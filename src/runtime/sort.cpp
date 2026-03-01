// sort.cpp — sort package runtime implementations.

#include "runtime/runtime.hpp"
#include <cstdlib>
#include <cstring>

extern "C" {

// sort.Ints: in-place sort of int64 elements (8 bytes each).
void golangc_sort_ints(void* ptr, int64_t len) {
    if (!ptr || len <= 1) return;
    qsort(ptr, static_cast<size_t>(len), sizeof(int64_t),
          [](const void* a, const void* b) -> int {
              int64_t x = *static_cast<const int64_t*>(a);
              int64_t y = *static_cast<const int64_t*>(b);
              return (x > y) - (x < y);
          });
}

// GoString layout: {ptr(8), len(8)} = 16 bytes per element.
struct SortGoString { const char* ptr; int64_t len; };

static int sort_strings_cmp(const void* a, const void* b) {
    auto* sa = static_cast<const SortGoString*>(a);
    auto* sb = static_cast<const SortGoString*>(b);
    if (!sa->ptr && !sb->ptr) return 0;
    if (!sa->ptr) return -1;
    if (!sb->ptr) return  1;
    int64_t minlen = sa->len < sb->len ? sa->len : sb->len;
    int cmp = memcmp(sa->ptr, sb->ptr, static_cast<size_t>(minlen));
    if (cmp != 0) return cmp;
    return (sa->len > sb->len) - (sa->len < sb->len);
}

// sort.Strings: in-place sort of GoString (16-byte) elements.
void golangc_sort_strings(void* ptr, int64_t len) {
    if (!ptr || len <= 1) return;
    qsort(ptr, static_cast<size_t>(len), sizeof(SortGoString), sort_strings_cmp);
}

// sort.Slice: sort using a Go function pointer as comparator.
// less_fn signature: int64_t less(int64_t i, int64_t j) — returns 1 if elem[i] < elem[j].
// Uses insertion sort so the comparator callback is called directly.
typedef int64_t (*SortLessFn)(int64_t, int64_t);

void golangc_sort_slice(void* ptr, int64_t len, void* less_fn_ptr, int64_t elem_size) {
    if (!ptr || len <= 1 || !less_fn_ptr || elem_size <= 0) return;
    auto less = reinterpret_cast<SortLessFn>(less_fn_ptr);
    char* data = static_cast<char*>(ptr);
    // Insertion sort: O(n²) but simple and correct with the callback.
    for (int64_t i = 1; i < len; ++i) {
        for (int64_t j = i; j > 0 && less(j, j - 1); --j) {
            // Swap elements j and j-1
            char* a = data + j       * elem_size;
            char* b = data + (j - 1) * elem_size;
            for (int64_t k = 0; k < elem_size; ++k) {
                char tmp = a[k]; a[k] = b[k]; b[k] = tmp;
            }
        }
    }
}

} // extern "C"
