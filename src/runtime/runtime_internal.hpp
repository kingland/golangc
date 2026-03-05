#pragma once
#define NOMINMAX
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <cstdint>

// Shared internal struct definitions used by both runtime.cpp and
// goroutine_channel.cpp (and rc.cpp for teardown logic).

struct MapBucket {
    void* key;  // malloc'd copy of the key bytes (nullptr = empty)
    void* val;  // malloc'd copy of the value bytes
};

struct golangc_map {
    int64_t    key_size;
    int64_t    val_size;
    int64_t    count;
    int64_t    capacity;   // Always a power of two
    MapBucket* buckets;
};

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
