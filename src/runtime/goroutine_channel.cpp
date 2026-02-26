#define NOMINMAX
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#include <cstdint>
#include <cstdlib>
#include <cstring>

// ============================================================================
// Channel implementation — unbuffered rendezvous via two semaphores
// ============================================================================

struct golangc_chan {
    CRITICAL_SECTION lock;      // Protects data_ptr assignment
    HANDLE           sender_ready; // Semaphore: sender has parked data (initial=0)
    HANDLE           recv_ready;   // Semaphore: receiver has consumed data (initial=0)
    void*            data_ptr;     // Points into sender's live stack frame
    int64_t          elem_size;
};

extern "C" {

golangc_chan* golangc_chan_make(int64_t elem_size) {
    auto* ch = static_cast<golangc_chan*>(std::malloc(sizeof(golangc_chan)));
    if (!ch) return nullptr;
    InitializeCriticalSection(&ch->lock);
    ch->sender_ready = CreateSemaphoreA(nullptr, 0, 1, nullptr);
    ch->recv_ready   = CreateSemaphoreA(nullptr, 0, 1, nullptr);
    ch->data_ptr     = nullptr;
    ch->elem_size    = elem_size;
    return ch;
}

void golangc_chan_send(golangc_chan* ch, void* val_ptr) {
    // Park the pointer to the sender's stack slot, then signal receiver.
    EnterCriticalSection(&ch->lock);
    ch->data_ptr = val_ptr;
    ReleaseSemaphore(ch->sender_ready, 1, nullptr);
    LeaveCriticalSection(&ch->lock);

    // Wait until receiver has consumed the value (then sender's frame is free).
    WaitForSingleObject(ch->recv_ready, INFINITE);
}

void golangc_chan_recv(golangc_chan* ch, void* out_ptr) {
    // Wait for sender to park a value.
    WaitForSingleObject(ch->sender_ready, INFINITE);

    EnterCriticalSection(&ch->lock);
    std::memcpy(out_ptr, ch->data_ptr, static_cast<size_t>(ch->elem_size));
    ch->data_ptr = nullptr;
    ReleaseSemaphore(ch->recv_ready, 1, nullptr);
    LeaveCriticalSection(&ch->lock);
}

// ============================================================================
// Goroutine launch block — heap-allocated, freed by thread proc
// ============================================================================

struct GoroutineLaunch {
    void*   func_ptr;
    int64_t arg_count;
    int64_t args[4]; // Up to 4 arguments (64-bit each)
};

static DWORD WINAPI goroutine_thread_proc(LPVOID param) {
    auto* launch = static_cast<GoroutineLaunch*>(param);
    void*   fp        = launch->func_ptr;
    int64_t argc      = launch->arg_count;
    int64_t a0 = argc > 0 ? launch->args[0] : 0;
    int64_t a1 = argc > 1 ? launch->args[1] : 0;
    int64_t a2 = argc > 2 ? launch->args[2] : 0;
    int64_t a3 = argc > 3 ? launch->args[3] : 0;
    std::free(launch);

    switch (argc) {
        case 0: reinterpret_cast<void(*)()>(fp)(); break;
        case 1: reinterpret_cast<void(*)(int64_t)>(fp)(a0); break;
        case 2: reinterpret_cast<void(*)(int64_t, int64_t)>(fp)(a0, a1); break;
        case 3: reinterpret_cast<void(*)(int64_t, int64_t, int64_t)>(fp)(a0, a1, a2); break;
        default: reinterpret_cast<void(*)(int64_t, int64_t, int64_t, int64_t)>(fp)(a0, a1, a2, a3); break;
    }
    return 0;
}

void golangc_go_spawn(void* func_ptr, int64_t arg_count, ...) {
    auto* launch = static_cast<GoroutineLaunch*>(std::malloc(sizeof(GoroutineLaunch)));
    if (!launch) return;
    launch->func_ptr  = func_ptr;
    launch->arg_count = arg_count;
    for (int i = 0; i < 4; ++i) launch->args[i] = 0;

    // Collect variadic arguments
    if (arg_count > 0) {
        va_list ap;
        va_start(ap, arg_count);
        int64_t n = arg_count < 4 ? arg_count : 4;
        for (int64_t i = 0; i < n; ++i) {
            launch->args[i] = va_arg(ap, int64_t);
        }
        va_end(ap);
    }

    HANDLE thread = CreateThread(nullptr, 0, goroutine_thread_proc, launch, 0, nullptr);
    if (thread) {
        CloseHandle(thread); // Fire-and-forget
    }
}

// ============================================================================
// Select — non-blocking poll across N channel cases
// ============================================================================

struct SelectCase {
    golangc_chan* ch;
    void*         val;   // send: ptr to value; recv: ptr to output buf; may be null
    int64_t       op;    // 0=recv, 1=send
};

int64_t golangc_select(SelectCase* cases, int64_t num_cases, int64_t has_default) {
    if (num_cases == 0) {
        // Only a default case (or empty select — block forever)
        if (has_default) return 0;
        Sleep(INFINITE);
        return 0;
    }

    for (;;) {
        for (int64_t i = 0; i < num_cases; ++i) {
            SelectCase* c = &cases[i];
            golangc_chan* ch = c->ch;
            if (!ch) continue;

            if (c->op == 0) {
                // Recv: try non-blocking wait on sender_ready semaphore
                if (WaitForSingleObject(ch->sender_ready, 0) == WAIT_OBJECT_0) {
                    EnterCriticalSection(&ch->lock);
                    if (c->val && ch->data_ptr)
                        std::memcpy(c->val, ch->data_ptr, static_cast<size_t>(ch->elem_size));
                    ch->data_ptr = nullptr;
                    ReleaseSemaphore(ch->recv_ready, 1, nullptr);
                    LeaveCriticalSection(&ch->lock);
                    return i;
                }
            } else {
                // Send: ready only if no other sender is currently occupying the channel.
                // Try to post the value and briefly wait for receiver acknowledgement.
                // This is a best-effort non-blocking send for select.
                EnterCriticalSection(&ch->lock);
                if (ch->data_ptr == nullptr) {
                    ch->data_ptr = c->val;
                    ReleaseSemaphore(ch->sender_ready, 1, nullptr);
                    LeaveCriticalSection(&ch->lock);
                    if (WaitForSingleObject(ch->recv_ready, 5) == WAIT_OBJECT_0) {
                        return i;
                    }
                    // No receiver within 5ms — undo by clearing data_ptr
                    // (sender_ready was already consumed if receiver came in; if not,
                    //  the channel is left in a bad state; this is an approximation)
                    EnterCriticalSection(&ch->lock);
                    ch->data_ptr = nullptr;
                    LeaveCriticalSection(&ch->lock);
                } else {
                    LeaveCriticalSection(&ch->lock);
                }
            }
        }
        if (has_default) return num_cases;
        Sleep(1); // 1ms backoff before retrying
    }
}

} // extern "C"
