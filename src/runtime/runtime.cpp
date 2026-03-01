#include "runtime/runtime.hpp"

#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cstdint>
#include <cctype>
#include <cmath>

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
// String / rune iteration
// ============================================================================

// golangc_string_decode_rune: decode one UTF-8 code point at ptr[idx].
// Writes the rune value to *out_rune; returns byte width (1-4).
int64_t golangc_string_decode_rune(const char* ptr, int64_t len,
                                    int64_t idx, int32_t* out_rune) {
    if (!ptr || idx < 0 || idx >= len) {
        if (out_rune) *out_rune = 0xFFFD;
        return 1;
    }
    unsigned char b0 = static_cast<unsigned char>(ptr[idx]);
    if (b0 < 0x80) {
        if (out_rune) *out_rune = static_cast<int32_t>(b0);
        return 1;
    }
    auto cont = [&](int64_t i) -> unsigned char {
        return (i < len) ? static_cast<unsigned char>(ptr[i]) : 0x80;
    };
    if ((b0 & 0xE0) == 0xC0 && idx + 1 < len) {
        unsigned char b1 = cont(idx + 1);
        if ((b1 & 0xC0) == 0x80) {
            if (out_rune) *out_rune = ((b0 & 0x1F) << 6) | (b1 & 0x3F);
            return 2;
        }
    } else if ((b0 & 0xF0) == 0xE0 && idx + 2 < len) {
        unsigned char b1 = cont(idx + 1), b2 = cont(idx + 2);
        if ((b1 & 0xC0) == 0x80 && (b2 & 0xC0) == 0x80) {
            if (out_rune) *out_rune = ((b0 & 0x0F) << 12) | ((b1 & 0x3F) << 6) | (b2 & 0x3F);
            return 3;
        }
    } else if ((b0 & 0xF8) == 0xF0 && idx + 3 < len) {
        unsigned char b1 = cont(idx + 1), b2 = cont(idx + 2), b3 = cont(idx + 3);
        if ((b1 & 0xC0) == 0x80 && (b2 & 0xC0) == 0x80 && (b3 & 0xC0) == 0x80) {
            if (out_rune) *out_rune = ((b0 & 0x07) << 18) | ((b1 & 0x3F) << 12)
                                    | ((b2 & 0x3F) << 6)  | (b3 & 0x3F);
            return 4;
        }
    }
    // Invalid byte — replacement character
    if (out_rune) *out_rune = 0xFFFD;
    return 1;
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

// golangc_fprintf: format to an arbitrary file handle.
void golangc_fprintf(golangc_file* f, const char* fmt_ptr, int64_t fmt_len, ...) {
    if (!f || !f->f) return;
    char buf[4096];
    va_list ap;
    va_start(ap, fmt_len);
    int len = fmt_custom(buf, sizeof(buf), fmt_ptr, fmt_len, ap);
    va_end(ap);
    fwrite(buf, 1, static_cast<size_t>(len), f->f);
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

// ============================================================================
// strings package
// ============================================================================

// Helper: allocate a heap string of given length, return via sret {ptr,len}.
static void sret_string(char* sret_out, const char* data, int64_t len) {
    char* buf = static_cast<char*>(malloc(static_cast<size_t>(len) + 1));
    if (buf) {
        memcpy(buf, data, static_cast<size_t>(len));
        buf[len] = '\0';
    }
    memcpy(sret_out,     &buf, sizeof(char*));
    memcpy(sret_out + 8, &len, sizeof(int64_t));
}

int64_t golangc_strings_contains(const char* s_ptr, int64_t s_len,
                                   const char* sub_ptr, int64_t sub_len) {
    if (sub_len == 0) return 1;
    if (sub_len > s_len) return 0;
    for (int64_t i = 0; i <= s_len - sub_len; ++i) {
        if (memcmp(s_ptr + i, sub_ptr, static_cast<size_t>(sub_len)) == 0) return 1;
    }
    return 0;
}

int64_t golangc_strings_has_prefix(const char* s_ptr, int64_t s_len,
                                    const char* pre_ptr, int64_t pre_len) {
    if (pre_len > s_len) return 0;
    return memcmp(s_ptr, pre_ptr, static_cast<size_t>(pre_len)) == 0 ? 1 : 0;
}

int64_t golangc_strings_has_suffix(const char* s_ptr, int64_t s_len,
                                    const char* suf_ptr, int64_t suf_len) {
    if (suf_len > s_len) return 0;
    return memcmp(s_ptr + s_len - suf_len, suf_ptr, static_cast<size_t>(suf_len)) == 0 ? 1 : 0;
}

int64_t golangc_strings_index(const char* s_ptr, int64_t s_len,
                               const char* sub_ptr, int64_t sub_len) {
    if (sub_len == 0) return 0;
    if (sub_len > s_len) return -1;
    for (int64_t i = 0; i <= s_len - sub_len; ++i) {
        if (memcmp(s_ptr + i, sub_ptr, static_cast<size_t>(sub_len)) == 0) return i;
    }
    return -1;
}

void golangc_strings_to_upper(char* sret_out, const char* s_ptr, int64_t s_len) {
    char* buf = static_cast<char*>(malloc(static_cast<size_t>(s_len) + 1));
    if (buf) {
        for (int64_t i = 0; i < s_len; ++i)
            buf[i] = static_cast<char>(toupper(static_cast<unsigned char>(s_ptr[i])));
        buf[s_len] = '\0';
    }
    memcpy(sret_out,     &buf,   sizeof(char*));
    memcpy(sret_out + 8, &s_len, sizeof(int64_t));
}

void golangc_strings_to_lower(char* sret_out, const char* s_ptr, int64_t s_len) {
    char* buf = static_cast<char*>(malloc(static_cast<size_t>(s_len) + 1));
    if (buf) {
        for (int64_t i = 0; i < s_len; ++i)
            buf[i] = static_cast<char>(tolower(static_cast<unsigned char>(s_ptr[i])));
        buf[s_len] = '\0';
    }
    memcpy(sret_out,     &buf,   sizeof(char*));
    memcpy(sret_out + 8, &s_len, sizeof(int64_t));
}

void golangc_strings_trim_space(char* sret_out, const char* s_ptr, int64_t s_len) {
    int64_t start = 0, end = s_len;
    while (start < end && isspace(static_cast<unsigned char>(s_ptr[start]))) ++start;
    while (end > start && isspace(static_cast<unsigned char>(s_ptr[end - 1]))) --end;
    sret_string(sret_out, s_ptr + start, end - start);
}

void golangc_strings_repeat(char* sret_out, const char* s_ptr, int64_t s_len, int64_t count) {
    if (count <= 0 || s_len == 0) { sret_string(sret_out, "", 0); return; }
    int64_t total = s_len * count;
    char* buf = static_cast<char*>(malloc(static_cast<size_t>(total) + 1));
    if (buf) {
        for (int64_t i = 0; i < count; ++i)
            memcpy(buf + i * s_len, s_ptr, static_cast<size_t>(s_len));
        buf[total] = '\0';
    }
    memcpy(sret_out,     &buf,   sizeof(char*));
    memcpy(sret_out + 8, &total, sizeof(int64_t));
}

int64_t golangc_strings_count(const char* s_ptr, int64_t s_len,
                               const char* sub_ptr, int64_t sub_len) {
    if (sub_len == 0) return s_len + 1;
    if (sub_len > s_len) return 0;
    int64_t cnt = 0;
    for (int64_t i = 0; i <= s_len - sub_len; ) {
        if (memcmp(s_ptr + i, sub_ptr, static_cast<size_t>(sub_len)) == 0) {
            ++cnt; i += sub_len;
        } else {
            ++i;
        }
    }
    return cnt;
}

void golangc_strings_trim(char* sret_out, const char* s_ptr, int64_t s_len,
                           const char* cut_ptr, int64_t cut_len) {
    int64_t start = 0, end = s_len;
    auto in_cutset = [&](char c) {
        for (int64_t k = 0; k < cut_len; ++k)
            if (cut_ptr[k] == c) return true;
        return false;
    };
    while (start < end && in_cutset(s_ptr[start])) ++start;
    while (end > start && in_cutset(s_ptr[end - 1])) --end;
    sret_string(sret_out, s_ptr + start, end - start);
}

void golangc_strings_replace(char* sret_out,
                              const char* s_ptr, int64_t s_len,
                              const char* old_ptr, int64_t old_len,
                              const char* new_ptr, int64_t new_len,
                              int64_t n) {
    if (old_len == 0 || s_len == 0) { sret_string(sret_out, s_ptr, s_len); return; }
    // First pass: count replacements
    int64_t cnt = 0;
    for (int64_t i = 0; i <= s_len - old_len; ) {
        if ((n < 0 || cnt < n) && memcmp(s_ptr + i, old_ptr, static_cast<size_t>(old_len)) == 0) {
            ++cnt; i += old_len;
        } else {
            ++i;
        }
    }
    int64_t out_len = s_len + cnt * (new_len - old_len);
    char* buf = static_cast<char*>(malloc(static_cast<size_t>(out_len) + 1));
    if (!buf) { sret_string(sret_out, s_ptr, s_len); return; }
    int64_t ri = 0, wi = 0, done = 0;
    while (ri < s_len) {
        if ((n < 0 || done < cnt) && ri <= s_len - old_len &&
            memcmp(s_ptr + ri, old_ptr, static_cast<size_t>(old_len)) == 0) {
            memcpy(buf + wi, new_ptr, static_cast<size_t>(new_len));
            wi += new_len; ri += old_len; ++done;
        } else {
            buf[wi++] = s_ptr[ri++];
        }
    }
    buf[wi] = '\0';
    memcpy(sret_out,     &buf,    sizeof(char*));
    memcpy(sret_out + 8, &out_len, sizeof(int64_t));
}

// ---- strings.TrimPrefix ----
void golangc_strings_trim_prefix(char* sret_out,
                                   const char* s_ptr,   int64_t s_len,
                                   const char* pre_ptr, int64_t pre_len) {
    if (pre_len > 0 && s_len >= pre_len &&
        memcmp(s_ptr, pre_ptr, static_cast<size_t>(pre_len)) == 0) {
        sret_string(sret_out, s_ptr + pre_len, s_len - pre_len);
    } else {
        sret_string(sret_out, s_ptr, s_len);
    }
}

// ---- strings.TrimSuffix ----
void golangc_strings_trim_suffix(char* sret_out,
                                   const char* s_ptr,   int64_t s_len,
                                   const char* suf_ptr, int64_t suf_len) {
    if (suf_len > 0 && s_len >= suf_len &&
        memcmp(s_ptr + s_len - suf_len, suf_ptr, static_cast<size_t>(suf_len)) == 0) {
        sret_string(sret_out, s_ptr, s_len - suf_len);
    } else {
        sret_string(sret_out, s_ptr, s_len);
    }
}

// ---- strings.Fields ----
void golangc_strings_fields(char* sret_out, const char* s_ptr, int64_t s_len) {
    struct GoStr { const char* ptr; int64_t len; };
    struct GoSlice { void* ptr; int64_t len; int64_t cap; };

    // Count words
    int64_t count = 0;
    bool in_word = false;
    for (int64_t i = 0; i < s_len; ++i) {
        unsigned char c = static_cast<unsigned char>(s_ptr[i]);
        bool ws = (c == ' ' || c == '\t' || c == '\n' || c == '\r');
        if (!ws && !in_word) { ++count; in_word = true; }
        else if (ws)           { in_word = false; }
    }

    auto* arr = static_cast<GoStr*>(malloc(static_cast<size_t>(count) * sizeof(GoStr)));
    int64_t idx = 0, start = 0;
    in_word = false;
    for (int64_t i = 0; i <= s_len; ++i) {
        bool ws = (i == s_len);
        if (!ws) {
            unsigned char c = static_cast<unsigned char>(s_ptr[i]);
            ws = (c == ' ' || c == '\t' || c == '\n' || c == '\r');
        }
        if (!ws && !in_word) { start = i; in_word = true; }
        else if (ws && in_word) {
            int64_t wlen = i - start;
            char* buf = static_cast<char*>(malloc(static_cast<size_t>(wlen) + 1));
            if (buf) { memcpy(buf, s_ptr + start, static_cast<size_t>(wlen)); buf[wlen] = '\0'; }
            arr[idx++] = {buf, wlen};
            in_word = false;
        }
    }

    auto* out = reinterpret_cast<GoSlice*>(sret_out);
    out->ptr = arr; out->len = count; out->cap = count;
}

// ---- strings.Split ----
void golangc_strings_split(char* sret_out,
                            const char* s_ptr,   int64_t s_len,
                            const char* sep_ptr, int64_t sep_len) {
    struct GoStr  { const char* ptr; int64_t len; };
    struct GoSlice{ void* ptr; int64_t len; int64_t cap; };

    // Empty separator: split into individual bytes
    if (sep_len == 0) {
        auto* arr = static_cast<GoStr*>(malloc(static_cast<size_t>(s_len) * sizeof(GoStr)));
        for (int64_t i = 0; i < s_len; ++i) {
            char* c = static_cast<char*>(malloc(2));
            if (c) { c[0] = s_ptr[i]; c[1] = '\0'; }
            arr[i] = {c, 1};
        }
        auto* out = reinterpret_cast<GoSlice*>(sret_out);
        out->ptr = arr; out->len = s_len; out->cap = s_len;
        return;
    }

    // Count segments
    int64_t count = 1;
    for (int64_t i = 0; i <= s_len - sep_len; ) {
        if (memcmp(s_ptr + i, sep_ptr, static_cast<size_t>(sep_len)) == 0) {
            ++count; i += sep_len;
        } else { ++i; }
    }

    auto* arr = static_cast<GoStr*>(malloc(static_cast<size_t>(count) * sizeof(GoStr)));
    int64_t idx = 0, start = 0;
    for (int64_t i = 0; i <= s_len - sep_len; ) {
        if (memcmp(s_ptr + i, sep_ptr, static_cast<size_t>(sep_len)) == 0) {
            int64_t wlen = i - start;
            char* buf = static_cast<char*>(malloc(static_cast<size_t>(wlen) + 1));
            if (buf) { memcpy(buf, s_ptr + start, static_cast<size_t>(wlen)); buf[wlen] = '\0'; }
            arr[idx++] = {buf, wlen};
            start = i + sep_len; i += sep_len;
        } else { ++i; }
    }
    // Last segment
    {
        int64_t wlen = s_len - start;
        char* buf = static_cast<char*>(malloc(static_cast<size_t>(wlen) + 1));
        if (buf) { memcpy(buf, s_ptr + start, static_cast<size_t>(wlen)); buf[wlen] = '\0'; }
        arr[idx] = {buf, wlen};
    }

    auto* out = reinterpret_cast<GoSlice*>(sret_out);
    out->ptr = arr; out->len = count; out->cap = count;
}

// ---- strings.Join ----
void golangc_strings_join(char* sret_out,
                           void* elems_ptr, int64_t elems_len,
                           const char* sep_ptr, int64_t sep_len) {
    struct GoStr { const char* ptr; int64_t len; };
    auto* elems = static_cast<GoStr*>(elems_ptr);
    if (!elems || elems_len == 0) { sret_string(sret_out, "", 0); return; }

    // Compute total length
    int64_t total = 0;
    for (int64_t i = 0; i < elems_len; ++i) total += elems[i].len;
    total += sep_len * (elems_len - 1);

    char* buf = static_cast<char*>(malloc(static_cast<size_t>(total) + 1));
    if (!buf) { sret_string(sret_out, "", 0); return; }
    char* p = buf;
    for (int64_t i = 0; i < elems_len; ++i) {
        if (i > 0 && sep_len > 0 && sep_ptr) {
            memcpy(p, sep_ptr, static_cast<size_t>(sep_len)); p += sep_len;
        }
        if (elems[i].ptr && elems[i].len > 0) {
            memcpy(p, elems[i].ptr, static_cast<size_t>(elems[i].len));
            p += elems[i].len;
        }
    }
    *p = '\0';
    *reinterpret_cast<char**>(sret_out) = buf;
    *reinterpret_cast<int64_t*>(sret_out + 8) = total;
}

// ============================================================================
// math package
// ============================================================================

double golangc_math_abs(double x)          { return fabs(x); }
double golangc_math_sqrt(double x)         { return sqrt(x); }
double golangc_math_floor(double x)        { return floor(x); }
double golangc_math_ceil(double x)         { return ceil(x); }
double golangc_math_round(double x)        { return round(x); }
double golangc_math_max(double x, double y){ return x > y ? x : y; }
double golangc_math_min(double x, double y){ return x < y ? x : y; }
double golangc_math_pow(double x, double y){ return pow(x, y); }
double golangc_math_log(double x)          { return log(x); }
double golangc_math_log2(double x)         { return log2(x); }
double golangc_math_log10(double x)        { return log10(x); }
double golangc_math_sin(double x)          { return sin(x); }
double golangc_math_cos(double x)          { return cos(x); }
double golangc_math_tan(double x)          { return tan(x); }
double golangc_math_asin(double x)         { return asin(x); }
double golangc_math_acos(double x)         { return acos(x); }
double golangc_math_atan(double x)         { return atan(x); }
double golangc_math_atan2(double y, double x) { return atan2(y, x); }
double golangc_math_trunc(double x)        { return trunc(x); }
double golangc_math_exp(double x)          { return exp(x); }
double golangc_math_exp2(double x)         { return exp2(x); }
double golangc_math_mod(double x, double y){ return fmod(x, y); }
double golangc_math_hypot(double x, double y){ return hypot(x, y); }

// ============================================================================
// strings.Builder
// ============================================================================

struct golangc_builder {
    char*   buf;
    int64_t len;
    int64_t cap;
};

golangc_builder* golangc_builder_make(void) {
    auto* b = static_cast<golangc_builder*>(std::malloc(sizeof(golangc_builder)));
    if (!b) return nullptr;
    b->cap = 64;
    b->buf = static_cast<char*>(std::malloc(static_cast<size_t>(b->cap)));
    b->len = 0;
    if (!b->buf) { std::free(b); return nullptr; }
    return b;
}

static void builder_grow(golangc_builder* b, int64_t need) {
    if (b->len + need <= b->cap) return;
    int64_t new_cap = b->cap * 2;
    if (new_cap < b->len + need) new_cap = b->len + need + 64;
    b->buf = static_cast<char*>(std::realloc(b->buf, static_cast<size_t>(new_cap)));
    b->cap = new_cap;
}

void golangc_builder_write_string(golangc_builder* b, const char* ptr, int64_t len) {
    if (!b || !ptr || len <= 0) return;
    builder_grow(b, len);
    std::memcpy(b->buf + b->len, ptr, static_cast<size_t>(len));
    b->len += len;
}

void golangc_builder_write_byte(golangc_builder* b, int64_t byte_val) {
    if (!b) return;
    builder_grow(b, 1);
    b->buf[b->len++] = static_cast<char>(byte_val);
}

void golangc_builder_string(char* sret_out, golangc_builder* b) {
    if (!b || !sret_out) return;
    // Copy buffer into a heap string (caller does not own builder's buf).
    char* out = static_cast<char*>(std::malloc(static_cast<size_t>(b->len + 1)));
    if (!out) {
        std::memset(sret_out, 0, 16);
        return;
    }
    std::memcpy(out, b->buf, static_cast<size_t>(b->len));
    out[b->len] = '\0';
    std::memcpy(sret_out,     &out,    sizeof(char*));
    std::memcpy(sret_out + 8, &b->len, sizeof(int64_t));
}

void golangc_builder_reset(golangc_builder* b) {
    if (b) b->len = 0;
}

int64_t golangc_builder_len(golangc_builder* b) {
    return b ? b->len : 0;
}

// ============================================================================
// errors package
// ============================================================================

// Interface layout: { int64_t type_tag, void* data_ptr }
// type_tag == 0 → nil interface; type_tag == 1 → error value.

struct GoRuntimeString2 {
    const char* ptr;
    int64_t     len;
};

void golangc_errors_new(char* sret_out, const char* msg_ptr, int64_t msg_len) {
    // Allocate a GoRuntimeString2 on the heap to hold the error message.
    auto* s = static_cast<GoRuntimeString2*>(std::malloc(sizeof(GoRuntimeString2)));
    if (!s) { std::memset(sret_out, 0, 16); return; }
    // Copy the message bytes.
    char* buf = static_cast<char*>(std::malloc(static_cast<size_t>(msg_len + 1)));
    if (!buf) { std::free(s); std::memset(sret_out, 0, 16); return; }
    std::memcpy(buf, msg_ptr, static_cast<size_t>(msg_len));
    buf[msg_len] = '\0';
    s->ptr = buf;
    s->len = msg_len;
    int64_t tag = 1;
    std::memcpy(sret_out,     &tag, sizeof(int64_t));
    std::memcpy(sret_out + 8, &s,   sizeof(void*));
}

void golangc_fmt_errorf(char* sret_out, const char* fmt_ptr, int64_t fmt_len, ...) {
    // Format the message using the existing internal formatter, then wrap.
    char buf[1024];
    int64_t written = 0;
    // Simple copy-until-format-specifier loop (reuse golangc_sprintf logic).
    va_list ap;
    va_start(ap, fmt_len);
    const char* f = fmt_ptr;
    const char* end = fmt_ptr + fmt_len;
    char* out = buf;
    char* out_end = buf + sizeof(buf) - 1;
    while (f < end && out < out_end) {
        if (*f == '%' && f + 1 < end) {
            ++f;
            char spec = *f++;
            if (spec == 'd' || spec == 'v') {
                int64_t val = va_arg(ap, int64_t);
                int n = snprintf(out, static_cast<size_t>(out_end - out), "%lld",
                                 static_cast<long long>(val));
                if (n > 0) out += n;
            } else if (spec == 's') {
                const char* s_ptr = va_arg(ap, const char*);
                int64_t s_len = va_arg(ap, int64_t);
                int64_t copy = s_len < (out_end - out) ? s_len : (out_end - out);
                std::memcpy(out, s_ptr, static_cast<size_t>(copy));
                out += copy;
            } else {
                *out++ = '%'; *out++ = spec;
            }
        } else {
            *out++ = *f++;
        }
    }
    va_end(ap);
    *out = '\0';
    written = static_cast<int64_t>(out - buf);

    // Allocate and return as error interface.
    char* msg = static_cast<char*>(std::malloc(static_cast<size_t>(written + 1)));
    if (!msg) { std::memset(sret_out, 0, 16); return; }
    std::memcpy(msg, buf, static_cast<size_t>(written + 1));

    auto* s = static_cast<GoRuntimeString2*>(std::malloc(sizeof(GoRuntimeString2)));
    if (!s) { std::free(msg); std::memset(sret_out, 0, 16); return; }
    s->ptr = msg;
    s->len = written;
    int64_t tag = 1;
    std::memcpy(sret_out,     &tag, sizeof(int64_t));
    std::memcpy(sret_out + 8, &s,   sizeof(void*));
}

// ============================================================================
// time package
// ============================================================================

#ifdef _WIN32
#  ifndef WIN32_LEAN_AND_MEAN
#    define WIN32_LEAN_AND_MEAN
#  endif
#  include <windows.h>
#else
#  include <time.h>
#  include <unistd.h>
#endif

void golangc_time_sleep(int64_t nanoseconds) {
    if (nanoseconds <= 0) return;
#ifdef _WIN32
    DWORD ms = static_cast<DWORD>(nanoseconds / 1000000LL);
    if (ms == 0 && nanoseconds > 0) ms = 1;
    Sleep(ms);
#else
    struct timespec ts;
    ts.tv_sec  = nanoseconds / 1000000000LL;
    ts.tv_nsec = nanoseconds % 1000000000LL;
    nanosleep(&ts, nullptr);
#endif
}

int64_t golangc_time_now(void) {
#ifdef _WIN32
    FILETIME ft;
    GetSystemTimeAsFileTime(&ft);
    // FILETIME is 100-nanosecond intervals since 1601-01-01
    // Convert to nanoseconds since Unix epoch (1970-01-01)
    int64_t t = (static_cast<int64_t>(ft.dwHighDateTime) << 32) | ft.dwLowDateTime;
    // 11644473600 seconds between 1601 and 1970 epochs
    t -= 116444736000000000LL; // in 100-ns units
    return t * 100; // convert to nanoseconds
#else
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    return static_cast<int64_t>(ts.tv_sec) * 1000000000LL + ts.tv_nsec;
#endif
}

int64_t golangc_time_since(int64_t start_ns) {
    return golangc_time_now() - start_ns;
}

// ============================================================================
// math/rand package
// ============================================================================

#include <cstdlib>

void golangc_rand_seed(int64_t seed) {
    srand(static_cast<unsigned int>(seed));
}

int64_t golangc_rand_intn(int64_t n) {
    if (n <= 0) return 0;
    return static_cast<int64_t>(rand()) % n;
}

double golangc_rand_float64(void) {
    return static_cast<double>(rand()) / (static_cast<double>(RAND_MAX) + 1.0);
}

} // extern "C"
