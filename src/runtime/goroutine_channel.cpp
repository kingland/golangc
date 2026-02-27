#define NOMINMAX
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#include <cstdint>
#include <cstdlib>
#include <cstring>

// ============================================================================
// Channel implementation
// Unbuffered (buffer_cap == 0): two-semaphore rendezvous.
// Buffered  (buffer_cap  > 0): ring buffer with counting semaphores.
// ============================================================================

struct golangc_chan {
    CRITICAL_SECTION lock;
    int64_t          elem_size;
    int64_t          buffer_cap;   // 0 = unbuffered

    // --- Unbuffered fields (used when buffer_cap == 0) ---
    HANDLE  sender_ready;  // Semaphore: sender has parked data (initial=0)
    HANDLE  recv_ready;    // Semaphore: receiver has consumed data (initial=0)
    void*   data_ptr;      // Points into sender's live stack frame

    // --- Buffered fields (used when buffer_cap > 0) ---
    void*   buffer;        // heap: buffer_cap * elem_size bytes
    int64_t head;          // read index (mod buffer_cap)
    int64_t tail;          // write index (mod buffer_cap)
    HANDLE  not_full;      // counting semaphore, initial = buffer_cap
    HANDLE  not_empty;     // counting semaphore, initial = 0
};

extern "C" {

golangc_chan* golangc_chan_make(int64_t elem_size, int64_t buffer_cap) {
    auto* ch = static_cast<golangc_chan*>(std::malloc(sizeof(golangc_chan)));
    if (!ch) return nullptr;
    InitializeCriticalSection(&ch->lock);
    ch->elem_size   = elem_size;
    ch->buffer_cap  = buffer_cap;

    if (buffer_cap == 0) {
        // Unbuffered
        ch->sender_ready = CreateSemaphoreA(nullptr, 0, 1, nullptr);
        ch->recv_ready   = CreateSemaphoreA(nullptr, 0, 1, nullptr);
        ch->data_ptr     = nullptr;
        ch->buffer       = nullptr;
        ch->head = ch->tail = 0;
        ch->not_full  = nullptr;
        ch->not_empty = nullptr;
    } else {
        // Buffered
        ch->sender_ready = nullptr;
        ch->recv_ready   = nullptr;
        ch->data_ptr     = nullptr;
        ch->buffer = std::malloc(static_cast<size_t>(buffer_cap * elem_size));
        ch->head = ch->tail = 0;
        ch->not_full  = CreateSemaphoreA(nullptr, static_cast<LONG>(buffer_cap),
                                          static_cast<LONG>(buffer_cap), nullptr);
        ch->not_empty = CreateSemaphoreA(nullptr, 0,
                                          static_cast<LONG>(buffer_cap), nullptr);
    }
    return ch;
}

void golangc_chan_send(golangc_chan* ch, void* val_ptr) {
    if (ch->buffer_cap == 0) {
        // Unbuffered: park pointer, signal receiver, wait for ack.
        EnterCriticalSection(&ch->lock);
        ch->data_ptr = val_ptr;
        ReleaseSemaphore(ch->sender_ready, 1, nullptr);
        LeaveCriticalSection(&ch->lock);
        WaitForSingleObject(ch->recv_ready, INFINITE);
    } else {
        // Buffered: wait for space, then copy into ring buffer slot.
        WaitForSingleObject(ch->not_full, INFINITE);
        EnterCriticalSection(&ch->lock);
        auto* slot = static_cast<char*>(ch->buffer) +
                     (ch->tail % ch->buffer_cap) * ch->elem_size;
        std::memcpy(slot, val_ptr, static_cast<size_t>(ch->elem_size));
        ch->tail++;
        LeaveCriticalSection(&ch->lock);
        ReleaseSemaphore(ch->not_empty, 1, nullptr);
    }
}

void golangc_chan_recv(golangc_chan* ch, void* out_ptr) {
    if (ch->buffer_cap == 0) {
        // Unbuffered: wait for sender to park a value.
        WaitForSingleObject(ch->sender_ready, INFINITE);
        EnterCriticalSection(&ch->lock);
        std::memcpy(out_ptr, ch->data_ptr, static_cast<size_t>(ch->elem_size));
        ch->data_ptr = nullptr;
        ReleaseSemaphore(ch->recv_ready, 1, nullptr);
        LeaveCriticalSection(&ch->lock);
    } else {
        // Buffered: wait for data, then copy from ring buffer slot.
        WaitForSingleObject(ch->not_empty, INFINITE);
        EnterCriticalSection(&ch->lock);
        auto* slot = static_cast<char*>(ch->buffer) +
                     (ch->head % ch->buffer_cap) * ch->elem_size;
        std::memcpy(out_ptr, slot, static_cast<size_t>(ch->elem_size));
        ch->head++;
        LeaveCriticalSection(&ch->lock);
        ReleaseSemaphore(ch->not_full, 1, nullptr);
    }
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
                // Recv
                if (ch->buffer_cap == 0) {
                    // Unbuffered: try non-blocking wait on sender_ready
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
                    // Buffered: try non-blocking wait on not_empty
                    if (WaitForSingleObject(ch->not_empty, 0) == WAIT_OBJECT_0) {
                        EnterCriticalSection(&ch->lock);
                        if (c->val) {
                            auto* slot = static_cast<char*>(ch->buffer) +
                                         (ch->head % ch->buffer_cap) * ch->elem_size;
                            std::memcpy(c->val, slot, static_cast<size_t>(ch->elem_size));
                            ch->head++;
                        }
                        LeaveCriticalSection(&ch->lock);
                        ReleaseSemaphore(ch->not_full, 1, nullptr);
                        return i;
                    }
                }
            } else {
                // Send
                if (ch->buffer_cap == 0) {
                    // Unbuffered: best-effort non-blocking send
                    EnterCriticalSection(&ch->lock);
                    if (ch->data_ptr == nullptr) {
                        ch->data_ptr = c->val;
                        ReleaseSemaphore(ch->sender_ready, 1, nullptr);
                        LeaveCriticalSection(&ch->lock);
                        if (WaitForSingleObject(ch->recv_ready, 5) == WAIT_OBJECT_0) {
                            return i;
                        }
                        EnterCriticalSection(&ch->lock);
                        ch->data_ptr = nullptr;
                        LeaveCriticalSection(&ch->lock);
                    } else {
                        LeaveCriticalSection(&ch->lock);
                    }
                } else {
                    // Buffered: try non-blocking send
                    if (WaitForSingleObject(ch->not_full, 0) == WAIT_OBJECT_0) {
                        EnterCriticalSection(&ch->lock);
                        auto* slot = static_cast<char*>(ch->buffer) +
                                     (ch->tail % ch->buffer_cap) * ch->elem_size;
                        if (c->val)
                            std::memcpy(slot, c->val, static_cast<size_t>(ch->elem_size));
                        ch->tail++;
                        LeaveCriticalSection(&ch->lock);
                        ReleaseSemaphore(ch->not_empty, 1, nullptr);
                        return i;
                    }
                }
            }
        }
        if (has_default) return num_cases;
        Sleep(1); // 1ms backoff before retrying
    }
}

} // extern "C"
