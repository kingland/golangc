// fmt_scan.cpp — fmt.Scan / fmt.Scanln / fmt.Scanf / fmt.Sscan / fmt.Sscanf
//               runtime implementations.

#ifdef _MSC_VER
#  define _CRT_SECURE_NO_WARNINGS
#endif

#include "runtime/runtime.hpp"
#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <cstring>

extern "C" {

// GoString: layout expected by the IR (16-byte {ptr, len}).
struct FmtScanGoString { const char* ptr; int64_t len; };

// ---------------------------------------------------------------------------
// Internal helper: scan n (tag, ptr) pairs from the given FILE*.
// tag: 0 = *int64, 1 = *double, 2 = *FmtScanGoString
// If stop_at_newline is true, consume the rest of the current line after
// all items have been read.
// ---------------------------------------------------------------------------
static int64_t scan_items(FILE* f, bool stop_at_newline, int64_t n, va_list ap) {
    int64_t count = 0;
    for (int64_t i = 0; i < n; ++i) {
        int64_t tag = va_arg(ap, int64_t);
        void*   ptr = va_arg(ap, void*);
        if (!ptr) continue;

        if (tag == 0) {
            long long val = 0;
            if (fscanf(f, "%lld", &val) == 1) {
                *static_cast<int64_t*>(ptr) = static_cast<int64_t>(val);
                ++count;
            }
        } else if (tag == 1) {
            double val = 0.0;
            if (fscanf(f, "%lf", &val) == 1) {
                *static_cast<double*>(ptr) = val;
                ++count;
            }
        } else {
            // String: read one whitespace-delimited token
            char buf[4096];
            if (fscanf(f, "%4095s", buf) == 1) {
                int64_t slen = static_cast<int64_t>(strlen(buf));
                char* s = static_cast<char*>(malloc(static_cast<size_t>(slen) + 1));
                if (s) { memcpy(s, buf, static_cast<size_t>(slen) + 1); }
                auto* gs = static_cast<FmtScanGoString*>(ptr);
                gs->ptr = s;
                gs->len = slen;
                ++count;
            }
        }
    }
    if (stop_at_newline) {
        // Consume remaining input on the current line
        int c;
        while ((c = fgetc(f)) != EOF && c != '\n') {}
    }
    return count;
}

int64_t golangc_fmt_scan(int64_t n, ...) {
    va_list ap;
    va_start(ap, n);
    int64_t r = scan_items(stdin, false, n, ap);
    va_end(ap);
    return r;
}

int64_t golangc_fmt_scanln(int64_t n, ...) {
    va_list ap;
    va_start(ap, n);
    int64_t r = scan_items(stdin, true, n, ap);
    va_end(ap);
    return r;
}

int64_t golangc_fmt_scanf(const char* /*fmt_ptr*/, int64_t /*fmt_len*/,
                           int64_t n, ...) {
    // Simplified: ignore format string, scan by type tags (same as Scan).
    va_list ap;
    va_start(ap, n);
    int64_t r = scan_items(stdin, false, n, ap);
    va_end(ap);
    return r;
}

// ---------------------------------------------------------------------------
// Sscan helpers: parse from a null-terminated buffer.
// ---------------------------------------------------------------------------
static int64_t sscan_items(const char* src, int64_t n, va_list ap) {
    int64_t count = 0;
    const char* cur = src;

    for (int64_t i = 0; i < n; ++i) {
        int64_t tag = va_arg(ap, int64_t);
        void*   ptr = va_arg(ap, void*);
        if (!ptr) continue;

        // Skip whitespace
        while (*cur == ' ' || *cur == '\t' || *cur == '\n' || *cur == '\r') ++cur;
        if (!*cur) break;

        int consumed = 0;
        if (tag == 0) {
            long long val = 0;
            if (sscanf(cur, "%lld%n", &val, &consumed) >= 1) {
                *static_cast<int64_t*>(ptr) = static_cast<int64_t>(val);
                cur += consumed; ++count;
            }
        } else if (tag == 1) {
            double val = 0.0;
            if (sscanf(cur, "%lf%n", &val, &consumed) >= 1) {
                *static_cast<double*>(ptr) = val;
                cur += consumed; ++count;
            }
        } else {
            char tmp[4096];
            if (sscanf(cur, "%4095s%n", tmp, &consumed) >= 1) {
                int64_t slen = static_cast<int64_t>(strlen(tmp));
                char* s = static_cast<char*>(malloc(static_cast<size_t>(slen) + 1));
                if (s) memcpy(s, tmp, static_cast<size_t>(slen) + 1);
                auto* gs = static_cast<FmtScanGoString*>(ptr);
                gs->ptr = s; gs->len = slen;
                cur += consumed; ++count;
            }
        }
    }
    return count;
}

int64_t golangc_fmt_sscan(const char* str_ptr, int64_t str_len, int64_t n, ...) {
    // Null-terminate a local copy
    char buf[65536];
    int64_t copy_len = str_len < 65535 ? str_len : 65535;
    memcpy(buf, str_ptr, static_cast<size_t>(copy_len));
    buf[copy_len] = '\0';

    va_list ap;
    va_start(ap, n);
    int64_t r = sscan_items(buf, n, ap);
    va_end(ap);
    return r;
}

int64_t golangc_fmt_sscanf(const char* str_ptr, int64_t str_len,
                             const char* /*fmt_ptr*/, int64_t /*fmt_len*/,
                             int64_t n, ...) {
    // Simplified: ignore format string, scan by type tags.
    char buf[65536];
    int64_t copy_len = str_len < 65535 ? str_len : 65535;
    memcpy(buf, str_ptr, static_cast<size_t>(copy_len));
    buf[copy_len] = '\0';

    va_list ap;
    va_start(ap, n);
    int64_t r = sscan_items(buf, n, ap);
    va_end(ap);
    return r;
}

} // extern "C"
