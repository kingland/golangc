#include "runtime/runtime.hpp"

#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cstdint>

// Global closure environment pointer — set by ClosureMake before calling Ret,
// read at indirect call sites to pass as hidden env arg.
extern "C" void* golangc_closure_env = nullptr;

extern "C" {

void golangc_println_int(int64_t value) {
    printf("%lld\n", static_cast<long long>(value));
}

void golangc_println_string(const char* data, int64_t length) {
    if (data && length > 0) {
        printf("%.*s\n", static_cast<int>(length), data);
    } else {
        printf("\n");
    }
}

void golangc_println_bool(int64_t value) {
    printf("%s\n", value ? "true" : "false");
}

void golangc_print_int(int64_t value) {
    printf("%lld", static_cast<long long>(value));
}

void golangc_print_string(const char* data, int64_t length) {
    if (data && length > 0) {
        printf("%.*s", static_cast<int>(length), data);
    }
}

void golangc_print_bool(int64_t value) {
    printf("%s", value ? "true" : "false");
}

void golangc_print_space() {
    printf(" ");
}

void golangc_print_newline() {
    printf("\n");
}

void golangc_println_float(double value) {
    printf("%g\n", value);
}

void golangc_print_float(double value) {
    printf("%g", value);
}

void golangc_string_concat(char* sret_out,
                            const char* ptr1, int64_t len1,
                            const char* ptr2, int64_t len2) {
    int64_t total = len1 + len2;
    char* buf = static_cast<char*>(malloc(static_cast<size_t>(total) + 1));
    if (buf) {
        if (ptr1 && len1 > 0) memcpy(buf, ptr1, static_cast<size_t>(len1));
        if (ptr2 && len2 > 0) memcpy(buf + len1, ptr2, static_cast<size_t>(len2));
        buf[total] = '\0';
    }
    // Write {ptr, len} to sret buffer
    // sret_out is a 16-byte buffer: [ptr (8 bytes), len (8 bytes)]
    char** out_ptr = reinterpret_cast<char**>(sret_out);
    int64_t* out_len = reinterpret_cast<int64_t*>(sret_out + 8);
    *out_ptr = buf;
    *out_len = total;
}

int64_t golangc_string_eq(const char* ptr1, int64_t len1,
                           const char* ptr2, int64_t len2) {
    if (len1 != len2) return 0;
    if (len1 == 0) return 1;
    if (!ptr1 || !ptr2) return 0;
    return memcmp(ptr1, ptr2, static_cast<size_t>(len1)) == 0 ? 1 : 0;
}

[[noreturn]] void golangc_panic(const char* msg) {
    fprintf(stderr, "goroutine 1 [running]:\npanic: %s\n", msg ? msg : "unknown");
    exit(2);
}

// ============================================================================
// Map runtime — open-addressing hash table with FNV-1a key hashing
// ============================================================================

// Internal bucket — stores heap copies of key and value
struct MapBucket {
    void*  key;       // malloc'd copy of the key bytes (nullptr = empty)
    void*  val;       // malloc'd copy of the value bytes
};

struct golangc_map {
    int64_t    key_size;
    int64_t    val_size;
    int64_t    count;
    int64_t    capacity;   // Always a power of two
    MapBucket* buckets;
};

static uint64_t fnv1a(const void* data, size_t len) {
    const uint8_t* p = static_cast<const uint8_t*>(data);
    uint64_t h = 14695981039346656037ULL;
    for (size_t i = 0; i < len; ++i) {
        h ^= static_cast<uint64_t>(p[i]);
        h *= 1099511628211ULL;
    }
    return h;
}

// For map[string]int: key is a {const char* ptr, int64_t len} struct (16 bytes).
// Hash and compare the string content rather than the struct bytes.
static bool is_string_key(int64_t key_size) { return key_size == 16; }

static uint64_t hash_key(const void* key_ptr, int64_t key_size) {
    if (is_string_key(key_size)) {
        // Interpret key as {const char* ptr, int64_t len}
        const char* str_ptr = *reinterpret_cast<const char* const*>(key_ptr);
        int64_t     str_len = *reinterpret_cast<const int64_t*>(
                                  static_cast<const char*>(key_ptr) + 8);
        if (!str_ptr || str_len <= 0) return fnv1a("", 0);
        return fnv1a(str_ptr, static_cast<size_t>(str_len));
    }
    return fnv1a(key_ptr, static_cast<size_t>(key_size));
}

static bool keys_equal(const void* a, const void* b, int64_t key_size) {
    if (is_string_key(key_size)) {
        const char* pa = *reinterpret_cast<const char* const*>(a);
        int64_t     la = *reinterpret_cast<const int64_t*>(
                             static_cast<const char*>(a) + 8);
        const char* pb = *reinterpret_cast<const char* const*>(b);
        int64_t     lb = *reinterpret_cast<const int64_t*>(
                             static_cast<const char*>(b) + 8);
        if (la != lb) return false;
        if (!pa && !pb) return true;
        if (!pa || !pb) return false;
        return memcmp(pa, pb, static_cast<size_t>(la)) == 0;
    }
    return memcmp(a, b, static_cast<size_t>(key_size)) == 0;
}

static void map_resize(golangc_map* m) {
    int64_t new_cap = m->capacity * 2;
    MapBucket* new_buckets = static_cast<MapBucket*>(
        calloc(static_cast<size_t>(new_cap), sizeof(MapBucket)));
    if (!new_buckets) return;

    // Rehash all existing entries
    for (int64_t i = 0; i < m->capacity; ++i) {
        if (!m->buckets[i].key) continue;
        uint64_t h = hash_key(m->buckets[i].key, m->key_size);
        int64_t idx = static_cast<int64_t>(h & static_cast<uint64_t>(new_cap - 1));
        while (new_buckets[idx].key) {
            idx = (idx + 1) & (new_cap - 1);
        }
        new_buckets[idx].key = m->buckets[i].key;
        new_buckets[idx].val = m->buckets[i].val;
    }
    free(m->buckets);
    m->buckets = new_buckets;
    m->capacity = new_cap;
}

golangc_map* golangc_map_make(int64_t key_size, int64_t val_size) {
    auto* m = static_cast<golangc_map*>(malloc(sizeof(golangc_map)));
    if (!m) return nullptr;
    m->key_size = key_size;
    m->val_size = val_size;
    m->count    = 0;
    m->capacity = 16;
    m->buckets  = static_cast<MapBucket*>(calloc(16, sizeof(MapBucket)));
    return m;
}

// Returns pointer to the value slot for the given key, or nullptr if not found.
// Writes 1/0 to *out_ok.
void* golangc_map_get(golangc_map* m, void* key_ptr, int64_t* out_ok) {
    if (!m || !key_ptr) {
        if (out_ok) *out_ok = 0;
        return nullptr;
    }
    uint64_t h = hash_key(key_ptr, m->key_size);
    int64_t idx = static_cast<int64_t>(h & static_cast<uint64_t>(m->capacity - 1));
    for (int64_t probe = 0; probe < m->capacity; ++probe) {
        if (!m->buckets[idx].key) break; // Empty slot — key not present
        if (keys_equal(m->buckets[idx].key, key_ptr, m->key_size)) {
            if (out_ok) *out_ok = 1;
            return m->buckets[idx].val;
        }
        idx = (idx + 1) & (m->capacity - 1);
    }
    if (out_ok) *out_ok = 0;
    return nullptr;
}

int64_t golangc_map_len(golangc_map* m) {
    return m ? m->count : 0;
}

void golangc_map_delete(golangc_map* m, void* key_ptr) {
    if (!m || !key_ptr) return;

    uint64_t h = hash_key(key_ptr, m->key_size);
    int64_t idx = static_cast<int64_t>(h & static_cast<uint64_t>(m->capacity - 1));
    for (int64_t probe = 0; probe < m->capacity; ++probe) {
        if (!m->buckets[idx].key) return; // Not found
        if (keys_equal(m->buckets[idx].key, key_ptr, m->key_size)) {
            // Free and mark as deleted (nullptr key = empty slot)
            free(m->buckets[idx].key);
            free(m->buckets[idx].val);
            m->buckets[idx].key = nullptr;
            m->buckets[idx].val = nullptr;
            --m->count;

            // Robin Hood: shift subsequent entries back to fill the gap
            int64_t prev = idx;
            int64_t next = (idx + 1) & (m->capacity - 1);
            while (m->buckets[next].key) {
                uint64_t nh = hash_key(m->buckets[next].key, m->key_size);
                int64_t ideal = static_cast<int64_t>(nh & static_cast<uint64_t>(m->capacity - 1));
                // Check if next slot is displaced from its ideal position
                // (i.e., it belongs at or before prev)
                bool displaced = false;
                if (ideal <= prev) {
                    displaced = (prev >= ideal && next > prev) ||
                                (next < ideal);
                } else {
                    displaced = (prev >= ideal || next <= prev);
                }
                (void)displaced;
                // Simple approach: always backshift
                m->buckets[prev].key = m->buckets[next].key;
                m->buckets[prev].val = m->buckets[next].val;
                m->buckets[next].key = nullptr;
                m->buckets[next].val = nullptr;
                prev = next;
                next = (next + 1) & (m->capacity - 1);
            }
            return;
        }
        idx = (idx + 1) & (m->capacity - 1);
    }
}

// ============================================================================
// Map iterator
// ============================================================================

struct golangc_map_iter {
    golangc_map* map;
    int64_t      pos;  // current bucket index (0 = start)
};

golangc_map_iter* golangc_map_iter_make(golangc_map* m) {
    auto* it = static_cast<golangc_map_iter*>(malloc(sizeof(golangc_map_iter)));
    if (!it) return nullptr;
    it->map = m;
    it->pos = 0;
    return it;
}

int64_t golangc_map_iter_next(golangc_map_iter* it, void* out_key, void* out_val) {
    if (!it || !it->map) return 0;
    golangc_map* m = it->map;
    while (it->pos < m->capacity) {
        int64_t i = it->pos++;
        if (!m->buckets[i].key) continue;
        if (out_key) memcpy(out_key, m->buckets[i].key, static_cast<size_t>(m->key_size));
        if (out_val) memcpy(out_val, m->buckets[i].val, static_cast<size_t>(m->val_size));
        return 1;
    }
    return 0;
}

void golangc_map_iter_free(golangc_map_iter* it) {
    free(it);
}

// ============================================================================
// Slice append
// ============================================================================

void golangc_slice_append(void* slice_out, const void* elem_ptr, int64_t elem_size) {
    // slice_out points to {void* ptr, int64_t len, int64_t cap}
    void**   s_ptr = static_cast<void**>(slice_out);
    int64_t* s_len = reinterpret_cast<int64_t*>(static_cast<char*>(slice_out) + 8);
    int64_t* s_cap = reinterpret_cast<int64_t*>(static_cast<char*>(slice_out) + 16);

    if (*s_len >= *s_cap) {
        // Grow: double capacity (minimum 8)
        int64_t new_cap = *s_cap * 2;
        if (new_cap < 8) new_cap = 8;
        void* new_ptr = malloc(static_cast<size_t>(new_cap * elem_size));
        if (!new_ptr) return;
        if (*s_ptr && *s_len > 0)
            memcpy(new_ptr, *s_ptr, static_cast<size_t>(*s_len * elem_size));
        *s_ptr = new_ptr;
        *s_cap = new_cap;
    }

    // Append element
    char* dst = static_cast<char*>(*s_ptr) + (*s_len * elem_size);
    memcpy(dst, elem_ptr, static_cast<size_t>(elem_size));
    ++(*s_len);
}

void golangc_map_set(golangc_map* m, void* key_ptr, void* val_ptr) {
    if (!m || !key_ptr) return;

    // Resize when 75% full
    if (m->count * 4 >= m->capacity * 3) {
        map_resize(m);
    }

    uint64_t h = hash_key(key_ptr, m->key_size);
    int64_t idx = static_cast<int64_t>(h & static_cast<uint64_t>(m->capacity - 1));
    for (int64_t probe = 0; probe < m->capacity; ++probe) {
        if (!m->buckets[idx].key) {
            // Empty slot — insert new entry
            m->buckets[idx].key = malloc(static_cast<size_t>(m->key_size));
            m->buckets[idx].val = malloc(static_cast<size_t>(m->val_size));
            if (m->buckets[idx].key) memcpy(m->buckets[idx].key, key_ptr, static_cast<size_t>(m->key_size));
            if (m->buckets[idx].val) memcpy(m->buckets[idx].val, val_ptr, static_cast<size_t>(m->val_size));
            ++m->count;
            return;
        }
        if (keys_equal(m->buckets[idx].key, key_ptr, m->key_size)) {
            // Update existing entry
            if (m->buckets[idx].val) memcpy(m->buckets[idx].val, val_ptr, static_cast<size_t>(m->val_size));
            return;
        }
        idx = (idx + 1) & (m->capacity - 1);
    }
}

// ============================================================================
// String conversion (strconv)
// ============================================================================

// golangc_itoa: int64 → string, returned via sret (16-byte {ptr, len} buffer).
void golangc_itoa(char* sret_out, int64_t value) {
    char buf[32];
    int len = snprintf(buf, sizeof(buf), "%lld", static_cast<long long>(value));
    if (len < 0) len = 0;
    char* s = static_cast<char*>(malloc(static_cast<size_t>(len) + 1));
    if (s) {
        memcpy(s, buf, static_cast<size_t>(len) + 1);
    }
    *reinterpret_cast<char**>(sret_out) = s;
    *reinterpret_cast<int64_t*>(sret_out + 8) = static_cast<int64_t>(len);
}

// golangc_atoi: string (ptr+len) → int64. Sets *out_ok (1=success, 0=failure).
int64_t golangc_atoi(const char* ptr, int64_t len, int64_t* out_ok) {
    if (!ptr || len <= 0) {
        if (out_ok) *out_ok = 0;
        return 0;
    }
    char buf[64];
    if (len >= static_cast<int64_t>(sizeof(buf))) {
        if (out_ok) *out_ok = 0;
        return 0;
    }
    memcpy(buf, ptr, static_cast<size_t>(len));
    buf[len] = '\0';
    char* end = nullptr;
    long long val = strtoll(buf, &end, 10);
    if (out_ok) *out_ok = (end == buf + len) ? 1 : 0;
    return static_cast<int64_t>(val);
}

// ============================================================================
// String formatting (fmt)
// ============================================================================

// Custom format loop — handles Go's {ptr,len} string representation.
// Supports %d (int64), %s (string ptr+len), %v (same as %d for numbers).
// buf/buf_cap: output buffer; returns bytes written.
static int fmt_custom(char* buf, int buf_cap,
                      const char* fmt_ptr, int64_t fmt_len,
                      va_list ap) {
    int out = 0;
    for (int64_t i = 0; i < fmt_len && out < buf_cap - 1; ) {
        char c = fmt_ptr[i++];
        if (c != '%') {
            buf[out++] = c;
            continue;
        }
        if (i >= fmt_len) break;
        char verb = fmt_ptr[i++];
        if (verb == '%') {
            buf[out++] = '%';
            continue;
        }
        if (verb == 'd' || verb == 'v') {
            int64_t n = va_arg(ap, int64_t);
            char tmp[32];
            int w = snprintf(tmp, sizeof(tmp), "%lld", static_cast<long long>(n));
            if (w > 0 && out + w < buf_cap) {
                memcpy(buf + out, tmp, static_cast<size_t>(w));
                out += w;
            }
        } else if (verb == 's') {
            const char* sp = va_arg(ap, const char*);
            int64_t sl = va_arg(ap, int64_t);
            if (sp && sl > 0) {
                int copy = (int)sl;
                if (out + copy >= buf_cap) copy = buf_cap - out - 1;
                memcpy(buf + out, sp, static_cast<size_t>(copy));
                out += copy;
            }
        } else if (verb == 'f' || verb == 'g') {
            double d = va_arg(ap, double);
            char tmp[64];
            int w = snprintf(tmp, sizeof(tmp), verb == 'f' ? "%f" : "%g", d);
            if (w > 0 && out + w < buf_cap) {
                memcpy(buf + out, tmp, static_cast<size_t>(w));
                out += w;
            }
        } else {
            // Unknown verb — emit as-is
            if (out + 2 < buf_cap) { buf[out++] = '%'; buf[out++] = verb; }
        }
    }
    buf[out] = '\0';
    return out;
}

// golangc_sprintf: returns string via sret (16-byte {ptr, len} buffer).
void golangc_sprintf(char* sret_out, const char* fmt_ptr, int64_t fmt_len, ...) {
    char buf[4096];
    va_list ap;
    va_start(ap, fmt_len);
    int len = fmt_custom(buf, sizeof(buf), fmt_ptr, fmt_len, ap);
    va_end(ap);
    char* s = static_cast<char*>(malloc(static_cast<size_t>(len) + 1));
    if (s) memcpy(s, buf, static_cast<size_t>(len) + 1);
    *reinterpret_cast<char**>(sret_out) = s;
    *reinterpret_cast<int64_t*>(sret_out + 8) = static_cast<int64_t>(len);
}

// golangc_printf: format to stdout.
void golangc_printf(const char* fmt_ptr, int64_t fmt_len, ...) {
    char buf[4096];
    va_list ap;
    va_start(ap, fmt_len);
    int len = fmt_custom(buf, sizeof(buf), fmt_ptr, fmt_len, ap);
    va_end(ap);
    fwrite(buf, 1, static_cast<size_t>(len), stdout);
}

// ============================================================================
// Rune / character conversion
// ============================================================================

// golangc_rune_to_string: Unicode code point → UTF-8 string via sret.
void golangc_rune_to_string(char* sret_out, int32_t rune_val) {
    char buf[5] = {0};
    int len = 0;
    uint32_t r = static_cast<uint32_t>(rune_val);
    if (r < 0x80) {
        buf[0] = static_cast<char>(r); len = 1;
    } else if (r < 0x800) {
        buf[0] = static_cast<char>(0xC0 | (r >> 6));
        buf[1] = static_cast<char>(0x80 | (r & 0x3F)); len = 2;
    } else if (r < 0x10000) {
        buf[0] = static_cast<char>(0xE0 | (r >> 12));
        buf[1] = static_cast<char>(0x80 | ((r >> 6) & 0x3F));
        buf[2] = static_cast<char>(0x80 | (r & 0x3F)); len = 3;
    } else {
        buf[0] = static_cast<char>(0xF0 | (r >> 18));
        buf[1] = static_cast<char>(0x80 | ((r >> 18) & 0x3F));
        buf[2] = static_cast<char>(0x80 | ((r >> 6)  & 0x3F));
        buf[3] = static_cast<char>(0x80 | (r & 0x3F)); len = 4;
    }
    char* s = static_cast<char*>(malloc(static_cast<size_t>(len) + 1));
    if (s) {
        memcpy(s, buf, static_cast<size_t>(len) + 1);
    }
    *reinterpret_cast<char**>(sret_out) = s;
    *reinterpret_cast<int64_t*>(sret_out + 8) = static_cast<int64_t>(len);
}

// ============================================================================
// os.Args
// ============================================================================

// GoRuntimeSlice: matches the {ptr, int64 len, int64 cap} slice header layout.
struct GoRuntimeSlice {
    void*   ptr;
    int64_t len;
    int64_t cap;
};

// GoRuntimeString: matches the {const char* ptr, int64 len} string layout.
struct GoRuntimeString {
    const char* ptr;
    int64_t     len;
};

static GoRuntimeSlice g_os_args_slice = {nullptr, 0, 0};
GoRuntimeSlice* golangc_os_args = &g_os_args_slice;

void golangc_init_args(int argc, char** argv) {
    if (argc <= 0) return;
    auto* strings = static_cast<GoRuntimeString*>(
        malloc(static_cast<size_t>(argc) * sizeof(GoRuntimeString)));
    for (int i = 0; i < argc; ++i) {
        strings[i].ptr = argv[i];
        strings[i].len = static_cast<int64_t>(strlen(argv[i]));
    }
    g_os_args_slice.ptr = strings;
    g_os_args_slice.len = static_cast<int64_t>(argc);
    g_os_args_slice.cap = static_cast<int64_t>(argc);
}

// golangc_os_args_get: return os.Args slice by value via sret (24 bytes: ptr,len,cap).
void golangc_os_args_get(char* sret_out) {
    memcpy(sret_out, &g_os_args_slice, sizeof(GoRuntimeSlice));
}

} // extern "C"
