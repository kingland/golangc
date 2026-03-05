#include "runtime/runtime_internal.hpp"

#include <cstdlib>
#include <cstring>

// ============================================================================
// Inline-prefix reference counting for heap-allocated Go objects.
//
// Every tracked allocation is prefixed with an 8-byte RCHeader:
//   [ RCHeader (8 bytes) ][ payload ... ]
//                          ^-- returned pointer
//
// type_tag values:
//   0 = string_buf   — no child pointers
//   1 = slice_buf    — no child pointers
//   2 = map          — has child pointers (buckets, key/val copies)
//   3 = chan         — has child handles (semaphores, CRITICAL_SECTION)
// ============================================================================

struct RCHeader {
    uint32_t refcount;
    uint32_t type_tag;
};

static void* rc_alloc(size_t payload_bytes, uint32_t tag) {
    auto* h = static_cast<RCHeader*>(malloc(sizeof(RCHeader) + payload_bytes));
    if (!h) return nullptr;
    memset(h + 1, 0, payload_bytes);
    h->refcount = 1;
    h->type_tag = tag;
    return h + 1;
}

static void rc_free(void* p) {
    if (!p) return;
    auto* h = static_cast<RCHeader*>(p) - 1;
    if (h->type_tag == 2) {
        // Map: free each bucket's key/val, then the buckets array
        auto* m = static_cast<golangc_map*>(p);
        if (m->buckets) {
            for (int64_t i = 0; i < m->capacity; ++i) {
                free(m->buckets[i].key);
                free(m->buckets[i].val);
            }
            free(m->buckets);
        }
    } else if (h->type_tag == 3) {
        // Chan: release handles and critical section
        auto* ch = static_cast<golangc_chan*>(p);
        if (ch->buffer_cap > 0 && ch->buffer) free(ch->buffer);
        if (ch->sender_ready) CloseHandle(ch->sender_ready);
        if (ch->recv_ready)   CloseHandle(ch->recv_ready);
        if (ch->not_full)     CloseHandle(ch->not_full);
        if (ch->not_empty)    CloseHandle(ch->not_empty);
        DeleteCriticalSection(&ch->lock);
    }
    // type 0 (string_buf) and 1 (slice_buf): no children — just free header
    free(h);
}

// ============================================================================
// Internal helpers — called by runtime.cpp and goroutine_channel.cpp
// ============================================================================

void* rc_alloc_string(size_t n) {
    return rc_alloc(n, 0);
}

void* rc_alloc_map() {
    return rc_alloc(sizeof(golangc_map), 2);
}

void* rc_alloc_chan() {
    return rc_alloc(sizeof(golangc_chan), 3);
}

// ============================================================================
// Public C API — called from generated code via Retain/Release IR opcodes
// ============================================================================

extern "C" {

void golangc_retain(void* p) {
    if (p) {
        (static_cast<RCHeader*>(p) - 1)->refcount++;
    }
}

void golangc_release(void* p) {
    if (!p) return;
    auto* h = static_cast<RCHeader*>(p) - 1;
    if (--h->refcount == 0) {
        rc_free(p);
    }
}

void* golangc_rc_slice_alloc(int64_t byte_count) {
    if (byte_count <= 0) byte_count = 8;
    return rc_alloc(static_cast<size_t>(byte_count), 1);
}

} // extern "C"
