#include "runtime/runtime.hpp"
#include "runtime/runtime_internal.hpp"

#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cstdint>
#include <cctype>
#include <cmath>
#include <vector>

// Forward declarations for RC allocators defined in rc.cpp
extern void* rc_alloc_string(size_t bytes);
extern void* rc_alloc_map();

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
    char* buf = static_cast<char*>(rc_alloc_string(static_cast<size_t>(total) + 1));
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
// (MapBucket and golangc_map structs are defined in runtime_internal.hpp)
// ============================================================================

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
    auto* m = static_cast<golangc_map*>(rc_alloc_map());
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

int64_t golangc_slice_copy(void* dst, const void* src, int64_t elem_size) {
    if (!dst || !src || elem_size <= 0) return 0;
    // Slice header: {void* ptr, int64_t len, int64_t cap}
    void*   d_ptr = *static_cast<void**>(dst);
    int64_t d_len = *reinterpret_cast<const int64_t*>(static_cast<const char*>(dst) + 8);
    void*   s_ptr = *static_cast<void* const*>(src);
    int64_t s_len = *reinterpret_cast<const int64_t*>(static_cast<const char*>(src) + 8);
    if (!d_ptr || !s_ptr) return 0;
    int64_t n = d_len < s_len ? d_len : s_len;
    if (n > 0) std::memcpy(d_ptr, s_ptr, static_cast<size_t>(n * elem_size));
    return n;
}

void golangc_slice_append(void* slice_out, const void* elem_ptr, int64_t elem_size) {
    // slice_out points to {void* ptr, int64_t len, int64_t cap}
    void**   s_ptr = static_cast<void**>(slice_out);
    int64_t* s_len = reinterpret_cast<int64_t*>(static_cast<char*>(slice_out) + 8);
    int64_t* s_cap = reinterpret_cast<int64_t*>(static_cast<char*>(slice_out) + 16);

    if (*s_len >= *s_cap) {
        // Grow: double capacity (minimum 8)
        int64_t new_cap = *s_cap * 2;
        if (new_cap < 8) new_cap = 8;
        void* old_ptr = *s_ptr;
        void* new_ptr = golangc_rc_slice_alloc(new_cap * elem_size);
        if (!new_ptr) return;
        if (old_ptr && *s_len > 0)
            memcpy(new_ptr, old_ptr, static_cast<size_t>(*s_len * elem_size));
        golangc_release(old_ptr);  // drop old backing array (refcount was 1)
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
    char* s = static_cast<char*>(rc_alloc_string(static_cast<size_t>(len) + 1));
    if (s) {
        memcpy(s, buf, static_cast<size_t>(len) + 1);
    }
    *reinterpret_cast<char**>(sret_out) = s;
    *reinterpret_cast<int64_t*>(sret_out + 8) = static_cast<int64_t>(len);
}

// golangc_atoi: called with a pointer to a Go string struct {char*, int64_t}
// as the first arg (codegen passes large structs by address via LEA).
// Second arg is ignored (null_ok from IR gen, not used since error is always nil).
int64_t golangc_atoi(const void* str_struct_ptr) {
    if (!str_struct_ptr) return 0;
    // String struct layout: {char* ptr, int64_t len}
    const char* ptr = *reinterpret_cast<const char* const*>(str_struct_ptr);
    int64_t len = *reinterpret_cast<const int64_t*>(
        static_cast<const char*>(str_struct_ptr) + 8);
    if (!ptr || len <= 0) return 0;
    char buf[64];
    if (len >= static_cast<int64_t>(sizeof(buf))) return 0;
    memcpy(buf, ptr, static_cast<size_t>(len));
    buf[len] = '\0';
    char* end = nullptr;
    long long val = strtoll(buf, &end, 10);
    return static_cast<int64_t>(val);
}

// ============================================================================
// String formatting (fmt)
// ============================================================================

// ============================================================================
// fmt_custom helpers
// ============================================================================

// fmt_binary: render int64 in base-2 into buf (cap bytes). Returns char count.
static int fmt_binary(char* buf, int cap, int64_t val) {
    if (cap < 2) return 0;
    if (val == 0) { buf[0] = '0'; buf[1] = '\0'; return 1; }
    uint64_t u = static_cast<uint64_t>(val);
    char tmp[65]; int i = 64; tmp[i] = '\0';
    while (u) { tmp[--i] = static_cast<char>('0' + (u & 1)); u >>= 1; }
    int len = 64 - i;
    if (len < cap) { memcpy(buf, tmp + i, static_cast<size_t>(len) + 1); }
    else { memcpy(buf, tmp + i, static_cast<size_t>(cap - 1)); buf[cap-1] = '\0'; len = cap - 1; }
    return len;
}

// fmt_quoted: render string as Go-quoted literal into buf. Returns char count.
static int fmt_quoted(char* buf, int cap, const char* s, int64_t slen) {
    int out = 0;
    auto emit = [&](char c) { if (out < cap - 1) buf[out++] = c; };
    emit('"');
    for (int64_t idx = 0; idx < slen; ++idx) {
        unsigned char c = static_cast<unsigned char>(s[idx]);
        switch (c) {
            case '"':  emit('\\'); emit('"');  break;
            case '\\': emit('\\'); emit('\\'); break;
            case '\n': emit('\\'); emit('n');  break;
            case '\t': emit('\\'); emit('t');  break;
            case '\r': emit('\\'); emit('r');  break;
            default:
                if (c < 32 || c == 127) {
                    int w = snprintf(buf + out, static_cast<size_t>(cap - out), "\\x%02x", c);
                    if (w > 0) out += w;
                } else {
                    emit(static_cast<char>(c));
                }
        }
    }
    emit('"');
    buf[out] = '\0';
    return out;
}

// Custom format loop — handles Go's {ptr,len} string representation.
// Supports full format grammar: %[flags][width][.prec]verb
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

        // --- Parse flags ---
        bool flag_plus  = false;
        bool flag_minus = false;
        bool flag_zero  = false;
        bool flag_space = false;
        bool flag_hash  = false;
        while (i < fmt_len) {
            char f = fmt_ptr[i];
            if      (f == '+') { flag_plus  = true; ++i; }
            else if (f == '-') { flag_minus = true; ++i; }
            else if (f == '0') { flag_zero  = true; ++i; }
            else if (f == ' ') { flag_space = true; ++i; }
            else if (f == '#') { flag_hash  = true; ++i; }
            else break;
        }
        (void)flag_space; (void)flag_hash; // reserved for future use

        // --- Parse width ---
        int width = 0;
        while (i < fmt_len && fmt_ptr[i] >= '0' && fmt_ptr[i] <= '9') {
            width = width * 10 + (fmt_ptr[i++] - '0');
        }

        // --- Parse precision ---
        int prec = -1;
        if (i < fmt_len && fmt_ptr[i] == '.') {
            ++i;
            prec = 0;
            while (i < fmt_len && fmt_ptr[i] >= '0' && fmt_ptr[i] <= '9') {
                prec = prec * 10 + (fmt_ptr[i++] - '0');
            }
        }

        if (i >= fmt_len) break;
        char verb = fmt_ptr[i++];

        // --- Generate value string ---
        char tmp[256];
        int  tmp_len = 0;
        const char* tmp_ptr = tmp; // may point elsewhere for %s

        if (verb == '%') {
            buf[out++] = '%';
            continue;
        } else if (verb == 'd' || verb == 'v' || verb == 'i') {
            int64_t n = va_arg(ap, int64_t);
            tmp_len = snprintf(tmp, sizeof(tmp),
                               flag_plus ? "%+lld" : "%lld",
                               static_cast<long long>(n));
        } else if (verb == 'b') {
            int64_t n = va_arg(ap, int64_t);
            tmp_len = fmt_binary(tmp, static_cast<int>(sizeof(tmp)), n);
        } else if (verb == 'o') {
            int64_t n = va_arg(ap, int64_t);
            tmp_len = snprintf(tmp, sizeof(tmp), "%llo",
                               static_cast<unsigned long long>(static_cast<uint64_t>(n)));
        } else if (verb == 'x') {
            int64_t n = va_arg(ap, int64_t);
            tmp_len = snprintf(tmp, sizeof(tmp), "%llx",
                               static_cast<unsigned long long>(static_cast<uint64_t>(n)));
        } else if (verb == 'X') {
            int64_t n = va_arg(ap, int64_t);
            tmp_len = snprintf(tmp, sizeof(tmp), "%llX",
                               static_cast<unsigned long long>(static_cast<uint64_t>(n)));
        } else if (verb == 'f') {
            double d = va_arg(ap, double);
            int p = (prec >= 0) ? prec : 6;
            tmp_len = snprintf(tmp, sizeof(tmp), "%.*f", p, d);
        } else if (verb == 'e') {
            double d = va_arg(ap, double);
            int p = (prec >= 0) ? prec : 6;
            tmp_len = snprintf(tmp, sizeof(tmp), "%.*e", p, d);
        } else if (verb == 'E') {
            double d = va_arg(ap, double);
            int p = (prec >= 0) ? prec : 6;
            tmp_len = snprintf(tmp, sizeof(tmp), "%.*E", p, d);
        } else if (verb == 'g') {
            double d = va_arg(ap, double);
            if (prec >= 0)
                tmp_len = snprintf(tmp, sizeof(tmp), "%.*g", prec, d);
            else
                tmp_len = snprintf(tmp, sizeof(tmp), "%g", d);
        } else if (verb == 'G') {
            double d = va_arg(ap, double);
            if (prec >= 0)
                tmp_len = snprintf(tmp, sizeof(tmp), "%.*G", prec, d);
            else
                tmp_len = snprintf(tmp, sizeof(tmp), "%G", d);
        } else if (verb == 's') {
            // String args arrive as GoString* (large struct passed by address).
            const GoString* gs = va_arg(ap, const GoString*);
            tmp_ptr = gs ? gs->ptr : "";
            tmp_len = static_cast<int>(gs ? gs->len : 0);
            // For %s we skip the tmp[] buffer and use tmp_ptr directly.
        } else if (verb == 'q') {
            const GoString* gs = va_arg(ap, const GoString*);
            const char* sp = gs ? gs->ptr : "";
            int64_t sl     = gs ? gs->len  : 0;
            tmp_len = fmt_quoted(tmp, static_cast<int>(sizeof(tmp)), sp, sl);
        } else if (verb == 't') {
            int64_t n = va_arg(ap, int64_t);
            tmp_ptr = n ? "true" : "false";
            tmp_len = static_cast<int>(strlen(tmp_ptr));
        } else if (verb == 'p') {
            int64_t n = va_arg(ap, int64_t);
            tmp_len = snprintf(tmp, sizeof(tmp), "0x%llx",
                               static_cast<unsigned long long>(static_cast<uint64_t>(n)));
        } else {
            // Unknown verb — emit as-is
            if (out + 2 < buf_cap) { buf[out++] = '%'; buf[out++] = verb; }
            continue;
        }

        if (tmp_len < 0) tmp_len = 0;

        // --- Apply padding ---
        int pad = width - tmp_len;
        if (pad > 0 && !flag_minus) {
            char pad_char = (flag_zero && (verb == 'd' || verb == 'i' || verb == 'v'
                                           || verb == 'o' || verb == 'x' || verb == 'X'
                                           || verb == 'f' || verb == 'e' || verb == 'E'
                                           || verb == 'g' || verb == 'G')) ? '0' : ' ';
            for (int p = 0; p < pad && out < buf_cap - 1; ++p)
                buf[out++] = pad_char;
        }
        // Emit value
        int copy = tmp_len;
        if (out + copy >= buf_cap) copy = buf_cap - out - 1;
        if (copy > 0) {
            memcpy(buf + out, tmp_ptr, static_cast<size_t>(copy));
            out += copy;
        }
        // Right-pad for left-align
        if (pad > 0 && flag_minus) {
            for (int p = 0; p < pad && out < buf_cap - 1; ++p)
                buf[out++] = ' ';
        }
    }
    buf[out] = '\0';
    return out;
}

// golangc_sprintf: returns string via sret (16-byte {ptr, len} buffer).
// fmt is a GoString* (large struct passed by address).
void golangc_sprintf(char* sret_out, const GoString* fmt, ...) {
    char buf[4096];
    va_list ap;
    va_start(ap, fmt);
    int len = fmt_custom(buf, sizeof(buf), fmt ? fmt->ptr : "", fmt ? fmt->len : 0, ap);
    va_end(ap);
    char* s = static_cast<char*>(rc_alloc_string(static_cast<size_t>(len) + 1));
    if (s) memcpy(s, buf, static_cast<size_t>(len) + 1);
    *reinterpret_cast<char**>(sret_out) = s;
    *reinterpret_cast<int64_t*>(sret_out + 8) = static_cast<int64_t>(len);
}

// golangc_printf: format to stdout.
void golangc_printf(const GoString* fmt, ...) {
    char buf[4096];
    va_list ap;
    va_start(ap, fmt);
    int len = fmt_custom(buf, sizeof(buf), fmt ? fmt->ptr : "", fmt ? fmt->len : 0, ap);
    va_end(ap);
    fwrite(buf, 1, static_cast<size_t>(len), stdout);
}

// golangc_fprintf: format to an arbitrary file handle.
void golangc_fprintf(golangc_file* f, const GoString* fmt, ...) {
    if (!f || !f->f) return;
    char buf[4096];
    va_list ap;
    va_start(ap, fmt);
    int len = fmt_custom(buf, sizeof(buf), fmt ? fmt->ptr : "", fmt ? fmt->len : 0, ap);
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
    char* s = static_cast<char*>(rc_alloc_string(static_cast<size_t>(len) + 1));
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

// ============================================================================
// Go string ABI helpers
//
// The codegen passes Go strings ({char*, int64_t}) as LARGE STRUCTS — i.e. by
// address via LEA.  So every runtime function that takes a "Go string" actually
// receives a POINTER to the {ptr, len} struct in its first (pointer) parameter.
// We unpack it with GS_PTR/GS_LEN macros.
// ============================================================================

// GoString is declared in runtime.hpp and included above.

// Unpack a GoString* received as a generic pointer argument.
static inline const char* gs_ptr(const void* gs) {
    return static_cast<const GoString*>(gs)->ptr;
}
static inline int64_t gs_len(const void* gs) {
    return static_cast<const GoString*>(gs)->len;
}

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

// All strings.* functions receive Go strings as GoString* (pointer to {ptr,len}).
// The codegen marshals large structs by address (LEA), so each "string" parameter
// is actually a pointer to the {char*, int64_t} pair on the caller's stack.

int64_t golangc_strings_contains(const GoString* s, const GoString* sub) {
    const char* s_ptr = s->ptr; int64_t s_len = s->len;
    const char* sub_ptr = sub->ptr; int64_t sub_len = sub->len;
    if (sub_len == 0) return 1;
    if (sub_len > s_len) return 0;
    for (int64_t i = 0; i <= s_len - sub_len; ++i) {
        if (memcmp(s_ptr + i, sub_ptr, static_cast<size_t>(sub_len)) == 0) return 1;
    }
    return 0;
}

int64_t golangc_strings_has_prefix(const GoString* s, const GoString* pre) {
    const char* s_ptr = s->ptr; int64_t s_len = s->len;
    const char* pre_ptr = pre->ptr; int64_t pre_len = pre->len;
    if (pre_len > s_len) return 0;
    return memcmp(s_ptr, pre_ptr, static_cast<size_t>(pre_len)) == 0 ? 1 : 0;
}

int64_t golangc_strings_has_suffix(const GoString* s, const GoString* suf) {
    const char* s_ptr = s->ptr; int64_t s_len = s->len;
    const char* suf_ptr = suf->ptr; int64_t suf_len = suf->len;
    if (suf_len > s_len) return 0;
    return memcmp(s_ptr + s_len - suf_len, suf_ptr, static_cast<size_t>(suf_len)) == 0 ? 1 : 0;
}

int64_t golangc_strings_index(const GoString* s, const GoString* sub) {
    const char* s_ptr = s->ptr; int64_t s_len = s->len;
    const char* sub_ptr = sub->ptr; int64_t sub_len = sub->len;
    if (sub_len == 0) return 0;
    if (sub_len > s_len) return -1;
    for (int64_t i = 0; i <= s_len - sub_len; ++i) {
        if (memcmp(s_ptr + i, sub_ptr, static_cast<size_t>(sub_len)) == 0) return i;
    }
    return -1;
}

void golangc_strings_to_upper(char* sret_out, const GoString* s) {
    const char* s_ptr = s->ptr; int64_t s_len = s->len;
    char* buf = static_cast<char*>(malloc(static_cast<size_t>(s_len) + 1));
    if (buf) {
        for (int64_t i = 0; i < s_len; ++i)
            buf[i] = static_cast<char>(toupper(static_cast<unsigned char>(s_ptr[i])));
        buf[s_len] = '\0';
    }
    memcpy(sret_out,     &buf,   sizeof(char*));
    memcpy(sret_out + 8, &s_len, sizeof(int64_t));
}

void golangc_strings_to_lower(char* sret_out, const GoString* s) {
    const char* s_ptr = s->ptr; int64_t s_len = s->len;
    char* buf = static_cast<char*>(malloc(static_cast<size_t>(s_len) + 1));
    if (buf) {
        for (int64_t i = 0; i < s_len; ++i)
            buf[i] = static_cast<char>(tolower(static_cast<unsigned char>(s_ptr[i])));
        buf[s_len] = '\0';
    }
    memcpy(sret_out,     &buf,   sizeof(char*));
    memcpy(sret_out + 8, &s_len, sizeof(int64_t));
}

void golangc_strings_trim_space(char* sret_out, const GoString* s) {
    const char* s_ptr = s->ptr; int64_t s_len = s->len;
    int64_t start = 0, end = s_len;
    while (start < end && isspace(static_cast<unsigned char>(s_ptr[start]))) ++start;
    while (end > start && isspace(static_cast<unsigned char>(s_ptr[end - 1]))) --end;
    sret_string(sret_out, s_ptr + start, end - start);
}

void golangc_strings_repeat(char* sret_out, const GoString* s, int64_t count) {
    const char* s_ptr = s->ptr; int64_t s_len = s->len;
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

int64_t golangc_strings_count(const GoString* s, const GoString* sub) {
    const char* s_ptr = s->ptr; int64_t s_len = s->len;
    const char* sub_ptr = sub->ptr; int64_t sub_len = sub->len;
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

void golangc_strings_trim(char* sret_out, const GoString* s, const GoString* cut) {
    const char* s_ptr = s->ptr; int64_t s_len = s->len;
    const char* cut_ptr = cut->ptr; int64_t cut_len = cut->len;
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
                              const GoString* s,
                              const GoString* old,
                              const GoString* nw,
                              int64_t n) {
    const char* s_ptr = s->ptr; int64_t s_len = s->len;
    const char* old_ptr = old->ptr; int64_t old_len = old->len;
    const char* new_ptr = nw->ptr; int64_t new_len = nw->len;
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
                                   const GoString* s,
                                   const GoString* pre) {
    const char* s_ptr = s->ptr; int64_t s_len = s->len;
    const char* pre_ptr = pre->ptr; int64_t pre_len = pre->len;
    if (pre_len > 0 && s_len >= pre_len &&
        memcmp(s_ptr, pre_ptr, static_cast<size_t>(pre_len)) == 0) {
        sret_string(sret_out, s_ptr + pre_len, s_len - pre_len);
    } else {
        sret_string(sret_out, s_ptr, s_len);
    }
}

// ---- strings.TrimSuffix ----
void golangc_strings_trim_suffix(char* sret_out,
                                   const GoString* s,
                                   const GoString* suf) {
    const char* s_ptr = s->ptr; int64_t s_len = s->len;
    const char* suf_ptr = suf->ptr; int64_t suf_len = suf->len;
    if (suf_len > 0 && s_len >= suf_len &&
        memcmp(s_ptr + s_len - suf_len, suf_ptr, static_cast<size_t>(suf_len)) == 0) {
        sret_string(sret_out, s_ptr, s_len - suf_len);
    } else {
        sret_string(sret_out, s_ptr, s_len);
    }
}

// ---- strings.ContainsRune ----
int64_t golangc_strings_contains_rune(const GoString* s, int64_t r) {
    const char* s_ptr = s->ptr; int64_t s_len = s->len;
    // Encode r as UTF-8 and search
    if (r < 0x80) {
        char c = static_cast<char>(r);
        for (int64_t i = 0; i < s_len; ++i)
            if (s_ptr[i] == c) return 1;
        return 0;
    }
    // Multi-byte rune: encode and use strstr-style search
    char buf[5] = {};
    int n = 0;
    uint32_t cp = static_cast<uint32_t>(r);
    if (cp < 0x800) {
        buf[0] = static_cast<char>(0xC0 | (cp >> 6));
        buf[1] = static_cast<char>(0x80 | (cp & 0x3F));
        n = 2;
    } else if (cp < 0x10000) {
        buf[0] = static_cast<char>(0xE0 | (cp >> 12));
        buf[1] = static_cast<char>(0x80 | ((cp >> 6) & 0x3F));
        buf[2] = static_cast<char>(0x80 | (cp & 0x3F));
        n = 3;
    } else {
        buf[0] = static_cast<char>(0xF0 | (cp >> 18));
        buf[1] = static_cast<char>(0x80 | ((cp >> 12) & 0x3F));
        buf[2] = static_cast<char>(0x80 | ((cp >> 6) & 0x3F));
        buf[3] = static_cast<char>(0x80 | (cp & 0x3F));
        n = 4;
    }
    for (int64_t i = 0; i <= s_len - n; ++i) {
        if (memcmp(s_ptr + i, buf, static_cast<size_t>(n)) == 0) return 1;
    }
    return 0;
}

// ---- strings.IndexByte ----
int64_t golangc_strings_index_byte(const GoString* s, int64_t c) {
    const char* s_ptr = s->ptr; int64_t s_len = s->len;
    char ch = static_cast<char>(c & 0xFF);
    for (int64_t i = 0; i < s_len; ++i)
        if (s_ptr[i] == ch) return i;
    return -1;
}

// ---- strings.LastIndex ----
int64_t golangc_strings_last_index(const GoString* s, const GoString* sub) {
    const char* s_ptr = s->ptr; int64_t s_len = s->len;
    const char* sub_ptr = sub->ptr; int64_t sub_len = sub->len;
    if (sub_len == 0) return s_len;
    if (sub_len > s_len) return -1;
    int64_t result = -1;
    for (int64_t i = 0; i <= s_len - sub_len; ++i) {
        if (memcmp(s_ptr + i, sub_ptr, static_cast<size_t>(sub_len)) == 0)
            result = i;
    }
    return result;
}

// ---- strings.IndexRune ----
int64_t golangc_strings_index_rune(const GoString* s, int64_t r) {
    const char* s_ptr = s->ptr; int64_t s_len = s->len;
    if (r < 0x80) {
        char c = static_cast<char>(r);
        for (int64_t i = 0; i < s_len; ++i)
            if (s_ptr[i] == c) return i;
        return -1;
    }
    char buf[5] = {};
    int n = 0;
    uint32_t cp = static_cast<uint32_t>(r);
    if (cp < 0x800) {
        buf[0] = static_cast<char>(0xC0 | (cp >> 6));
        buf[1] = static_cast<char>(0x80 | (cp & 0x3F));
        n = 2;
    } else if (cp < 0x10000) {
        buf[0] = static_cast<char>(0xE0 | (cp >> 12));
        buf[1] = static_cast<char>(0x80 | ((cp >> 6) & 0x3F));
        buf[2] = static_cast<char>(0x80 | (cp & 0x3F));
        n = 3;
    } else {
        buf[0] = static_cast<char>(0xF0 | (cp >> 18));
        buf[1] = static_cast<char>(0x80 | ((cp >> 12) & 0x3F));
        buf[2] = static_cast<char>(0x80 | ((cp >> 6) & 0x3F));
        buf[3] = static_cast<char>(0x80 | (cp & 0x3F));
        n = 4;
    }
    for (int64_t i = 0; i <= s_len - n; ++i) {
        if (memcmp(s_ptr + i, buf, static_cast<size_t>(n)) == 0) return i;
    }
    return -1;
}

// ---- strings.EqualFold ----
int64_t golangc_strings_equal_fold(const GoString* s, const GoString* t) {
    const char* s_ptr = s->ptr; int64_t s_len = s->len;
    const char* t_ptr = t->ptr; int64_t t_len = t->len;
    if (s_len != t_len) return 0;
    for (int64_t i = 0; i < s_len; ++i) {
        if (tolower((unsigned char)s_ptr[i]) != tolower((unsigned char)t_ptr[i]))
            return 0;
    }
    return 1;
}

// ---- strings.ContainsAny ----
int64_t golangc_strings_contains_any(const GoString* s, const GoString* chars) {
    const char* s_ptr = s->ptr; int64_t s_len = s->len;
    const char* chars_ptr = chars->ptr; int64_t chars_len = chars->len;
    for (int64_t i = 0; i < s_len; ++i)
        for (int64_t j = 0; j < chars_len; ++j)
            if (s_ptr[i] == chars_ptr[j]) return 1;
    return 0;
}

// ---- strings.Map ----
// mapping_fn is a pointer to a Go closure env struct; first field is the function pointer.
// For top-level funcs, mapping_fn itself is the function pointer (no env).
// Signature: int64_t (*)(int64_t rune)
void golangc_strings_map(char* sret_out, void* mapping_fn, const GoString* s) {
    const char* s_ptr = s->ptr; int64_t s_len = s->len;
    typedef int64_t (*MapFn)(int64_t);
    MapFn fn = reinterpret_cast<MapFn>(mapping_fn);
    // Worst case: every byte expands to 4-byte UTF-8 rune
    std::vector<char> out;
    out.reserve(static_cast<size_t>(s_len) * 4);
    int64_t i = 0;
    while (i < s_len) {
        // Decode one UTF-8 rune
        uint32_t cp;
        unsigned char b = static_cast<unsigned char>(s_ptr[i]);
        int nb;
        if (b < 0x80)       { cp = b; nb = 1; }
        else if (b < 0xE0)  { cp = b & 0x1F; nb = 2; }
        else if (b < 0xF0)  { cp = b & 0x0F; nb = 3; }
        else                { cp = b & 0x07; nb = 4; }
        for (int k = 1; k < nb && i + k < s_len; ++k)
            cp = (cp << 6) | (static_cast<unsigned char>(s_ptr[i + k]) & 0x3F);
        i += nb;
        int64_t mapped = fn(static_cast<int64_t>(cp));
        if (mapped < 0) continue; // negative rune = drop
        uint32_t mc = static_cast<uint32_t>(mapped);
        if (mc < 0x80) {
            out.push_back(static_cast<char>(mc));
        } else if (mc < 0x800) {
            out.push_back(static_cast<char>(0xC0 | (mc >> 6)));
            out.push_back(static_cast<char>(0x80 | (mc & 0x3F)));
        } else if (mc < 0x10000) {
            out.push_back(static_cast<char>(0xE0 | (mc >> 12)));
            out.push_back(static_cast<char>(0x80 | ((mc >> 6) & 0x3F)));
            out.push_back(static_cast<char>(0x80 | (mc & 0x3F)));
        } else {
            out.push_back(static_cast<char>(0xF0 | (mc >> 18)));
            out.push_back(static_cast<char>(0x80 | ((mc >> 12) & 0x3F)));
            out.push_back(static_cast<char>(0x80 | ((mc >> 6) & 0x3F)));
            out.push_back(static_cast<char>(0x80 | (mc & 0x3F)));
        }
    }
    sret_string(sret_out, out.data(), static_cast<int64_t>(out.size()));
}

// ---- strings.Title ----
void golangc_strings_title(char* sret_out, const GoString* s) {
    const char* s_ptr = s->ptr; int64_t s_len = s->len;
    std::vector<char> out(static_cast<size_t>(s_len));
    bool capitalize_next = true;
    for (int64_t i = 0; i < s_len; ++i) {
        unsigned char c = static_cast<unsigned char>(s_ptr[i]);
        if (isspace(c)) {
            out[static_cast<size_t>(i)] = static_cast<char>(c);
            capitalize_next = true;
        } else if (capitalize_next) {
            out[static_cast<size_t>(i)] = static_cast<char>(toupper(c));
            capitalize_next = false;
        } else {
            out[static_cast<size_t>(i)] = static_cast<char>(c);
        }
    }
    sret_string(sret_out, out.data(), s_len);
}

// ---- strings.TrimLeft ----
void golangc_strings_trim_left(char* sret_out, const GoString* s, const GoString* cut) {
    const char* s_ptr = s->ptr; int64_t s_len = s->len;
    const char* cut_ptr = cut->ptr; int64_t cut_len = cut->len;
    int64_t start = 0;
    while (start < s_len) {
        bool found = false;
        for (int64_t j = 0; j < cut_len; ++j) {
            if (s_ptr[start] == cut_ptr[j]) { found = true; break; }
        }
        if (!found) break;
        ++start;
    }
    sret_string(sret_out, s_ptr + start, s_len - start);
}

// ---- strings.TrimRight ----
void golangc_strings_trim_right(char* sret_out, const GoString* s, const GoString* cut) {
    const char* s_ptr = s->ptr; int64_t s_len = s->len;
    const char* cut_ptr = cut->ptr; int64_t cut_len = cut->len;
    int64_t end = s_len;
    while (end > 0) {
        bool found = false;
        for (int64_t j = 0; j < cut_len; ++j) {
            if (s_ptr[end - 1] == cut_ptr[j]) { found = true; break; }
        }
        if (!found) break;
        --end;
    }
    sret_string(sret_out, s_ptr, end);
}

// ---- strings.Fields ----
void golangc_strings_fields(char* sret_out, const GoString* s) {
    const char* s_ptr = s->ptr; int64_t s_len = s->len;
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
void golangc_strings_split(char* sret_out, const GoString* s, const GoString* sep) {
    const char* s_ptr = s->ptr; int64_t s_len = s->len;
    const char* sep_ptr = sep->ptr; int64_t sep_len = sep->len;
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
                           const GoString* sep) {
    const char* sep_ptr = sep->ptr; int64_t sep_len = sep->len;
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

void golangc_builder_write_string(golangc_builder* b, const GoString* s) {
    if (!b || !s || !s->ptr || s->len <= 0) return;
    builder_grow(b, s->len);
    std::memcpy(b->buf + b->len, s->ptr, static_cast<size_t>(s->len));
    b->len += s->len;
}

void golangc_builder_write_byte(golangc_builder* b, int64_t byte_val) {
    if (!b) return;
    builder_grow(b, 1);
    b->buf[b->len++] = static_cast<char>(byte_val);
}

void golangc_builder_string(char* sret_out, golangc_builder* b) {
    if (!b || !sret_out) return;
    // Copy buffer into a heap string (caller does not own builder's buf).
    char* out = static_cast<char*>(rc_alloc_string(static_cast<size_t>(b->len + 1)));
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

void golangc_errors_new(char* sret_out, const GoString* msg) {
    const char* msg_ptr = msg ? msg->ptr : nullptr;
    int64_t msg_len     = msg ? msg->len : 0;
    // Allocate a GoRuntimeString2 on the heap to hold the error message.
    auto* s = static_cast<GoRuntimeString2*>(std::malloc(sizeof(GoRuntimeString2)));
    if (!s) { std::memset(sret_out, 0, 16); return; }
    // Copy the message bytes.
    char* buf = static_cast<char*>(std::malloc(static_cast<size_t>(msg_len + 1)));
    if (!buf) { std::free(s); std::memset(sret_out, 0, 16); return; }
    if (msg_ptr && msg_len > 0) std::memcpy(buf, msg_ptr, static_cast<size_t>(msg_len));
    buf[msg_len] = '\0';
    s->ptr = buf;
    s->len = msg_len;
    int64_t tag = 1;
    std::memcpy(sret_out,     &tag, sizeof(int64_t));
    std::memcpy(sret_out + 8, &s,   sizeof(void*));
}

void golangc_fmt_errorf(char* sret_out, const GoString* fmt_gs, ...) {
    // Format the message using the existing internal formatter, then wrap.
    const char* fmt_ptr = fmt_gs ? fmt_gs->ptr : "";
    int64_t fmt_len     = fmt_gs ? fmt_gs->len : 0;
    char buf[1024];
    int64_t written = 0;
    // Simple copy-until-format-specifier loop (reuse golangc_sprintf logic).
    va_list ap;
    va_start(ap, fmt_gs);
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
                const GoString* gs = va_arg(ap, const GoString*);
                const char* s_ptr = gs ? gs->ptr : nullptr;
                int64_t s_len     = gs ? gs->len : 0;
                int64_t copy = s_len < (out_end - out) ? s_len : (out_end - out);
                if (s_ptr && copy > 0) std::memcpy(out, s_ptr, static_cast<size_t>(copy));
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

// ============================================================================
// unicode package
// ============================================================================

#include <cwctype>
#include <cwchar>

int64_t golangc_unicode_is_letter(int64_t r) { return iswalpha(static_cast<wint_t>(r)) ? 1 : 0; }
int64_t golangc_unicode_is_digit(int64_t r)  { return iswdigit(static_cast<wint_t>(r)) ? 1 : 0; }
int64_t golangc_unicode_is_space(int64_t r)  { return iswspace(static_cast<wint_t>(r)) ? 1 : 0; }
int64_t golangc_unicode_is_upper(int64_t r)  { return iswupper(static_cast<wint_t>(r)) ? 1 : 0; }
int64_t golangc_unicode_is_lower(int64_t r)  { return iswlower(static_cast<wint_t>(r)) ? 1 : 0; }
int64_t golangc_unicode_to_upper(int64_t r)  { return static_cast<int64_t>(towupper(static_cast<wint_t>(r))); }
int64_t golangc_unicode_to_lower(int64_t r)  { return static_cast<int64_t>(towlower(static_cast<wint_t>(r))); }

// ============================================================================
// bytes package
// ============================================================================

struct golangc_bytes_buffer {
    char*   data;
    int64_t len;
    int64_t cap;
};

static golangc_bytes_buffer* bytes_buf_alloc(void) {
    golangc_bytes_buffer* b = static_cast<golangc_bytes_buffer*>(malloc(sizeof(golangc_bytes_buffer)));
    b->data = static_cast<char*>(malloc(64));
    b->len  = 0;
    b->cap  = 64;
    return b;
}

static void bytes_buf_grow(golangc_bytes_buffer* b, int64_t need) {
    if (b->len + need <= b->cap) return;
    while (b->cap < b->len + need) b->cap *= 2;
    b->data = static_cast<char*>(realloc(b->data, static_cast<size_t>(b->cap)));
}

golangc_bytes_buffer* golangc_bytes_new_buffer(void) {
    return bytes_buf_alloc();
}

golangc_bytes_buffer* golangc_bytes_new_buffer_string(const GoString* s) {
    golangc_bytes_buffer* b = bytes_buf_alloc();
    if (s && s->ptr && s->len > 0) {
        bytes_buf_grow(b, s->len);
        memcpy(b->data, s->ptr, static_cast<size_t>(s->len));
        b->len = s->len;
    }
    return b;
}

void golangc_bytes_write_string(golangc_bytes_buffer* b, const GoString* s) {
    if (!b || !s || !s->ptr || s->len <= 0) return;
    bytes_buf_grow(b, s->len);
    memcpy(b->data + b->len, s->ptr, static_cast<size_t>(s->len));
    b->len += s->len;
}

void golangc_bytes_write_byte(golangc_bytes_buffer* b, int64_t byte_val) {
    bytes_buf_grow(b, 1);
    b->data[b->len++] = static_cast<char>(byte_val);
}

void golangc_bytes_write(golangc_bytes_buffer* b, void* slice_ptr, int64_t slice_len) {
    if (!slice_ptr || slice_len <= 0) return;
    bytes_buf_grow(b, slice_len);
    memcpy(b->data + b->len, slice_ptr, static_cast<size_t>(slice_len));
    b->len += slice_len;
}

void golangc_bytes_string(char* sret_out, golangc_bytes_buffer* b) {
    char* out_ptr = static_cast<char*>(malloc(static_cast<size_t>(b->len + 1)));
    memcpy(out_ptr, b->data, static_cast<size_t>(b->len));
    out_ptr[b->len] = '\0';
    int64_t* out = reinterpret_cast<int64_t*>(sret_out);
    out[0] = reinterpret_cast<int64_t>(out_ptr);
    out[1] = b->len;
}

void golangc_bytes_reset(golangc_bytes_buffer* b) {
    b->len = 0;
}

int64_t golangc_bytes_len(golangc_bytes_buffer* b) {
    return b->len;
}

} // extern "C"
