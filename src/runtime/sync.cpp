// sync.cpp — runtime implementation of sync.Mutex and sync.WaitGroup
// Uses Windows CRITICAL_SECTION for mutexes and a manual-reset event for WaitGroup.

#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#include <windows.h>
#include <cstdlib>
#include <cstdint>

extern "C" {

// ============================================================================
// sync.Mutex
// ============================================================================

struct golangc_mutex {
    CRITICAL_SECTION lock;
};

golangc_mutex* golangc_mutex_make(void) {
    auto* m = static_cast<golangc_mutex*>(std::malloc(sizeof(golangc_mutex)));
    if (!m) return nullptr;
    InitializeCriticalSection(&m->lock);
    return m;
}

void golangc_mutex_lock(golangc_mutex* m) {
    if (!m) return;
    EnterCriticalSection(&m->lock);
}

void golangc_mutex_unlock(golangc_mutex* m) {
    if (!m) return;
    LeaveCriticalSection(&m->lock);
}

int64_t golangc_mutex_try_lock(golangc_mutex* m) {
    if (!m) return 0;
    return TryEnterCriticalSection(&m->lock) ? 1 : 0;
}

// ============================================================================
// sync.WaitGroup
// ============================================================================

struct golangc_waitgroup {
    CRITICAL_SECTION lock;
    int64_t          count;
    HANDLE           done_event; // manual-reset event; signaled when count == 0
};

golangc_waitgroup* golangc_waitgroup_make(void) {
    auto* wg = static_cast<golangc_waitgroup*>(std::malloc(sizeof(golangc_waitgroup)));
    if (!wg) return nullptr;
    InitializeCriticalSection(&wg->lock);
    wg->count = 0;
    // Manual-reset event, initially signaled (count starts at 0 → done).
    wg->done_event = CreateEventA(nullptr, TRUE, TRUE, nullptr);
    return wg;
}

void golangc_waitgroup_add(golangc_waitgroup* wg, int64_t delta) {
    if (!wg) return;
    EnterCriticalSection(&wg->lock);
    wg->count += delta;
    if (wg->count <= 0) {
        wg->count = 0;
        SetEvent(wg->done_event);   // signal: all goroutines done
    } else {
        ResetEvent(wg->done_event); // not done yet
    }
    LeaveCriticalSection(&wg->lock);
}

void golangc_waitgroup_done(golangc_waitgroup* wg) {
    golangc_waitgroup_add(wg, -1);
}

void golangc_waitgroup_wait(golangc_waitgroup* wg) {
    if (!wg) return;
    WaitForSingleObject(wg->done_event, INFINITE);
}

} // extern "C"
