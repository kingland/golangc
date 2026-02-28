// bufio.cpp — bufio.Scanner and bufio.Reader runtime implementations

#include "runtime/runtime.hpp"
#include <cstdlib>
#include <cstring>
#include <cstdio>

extern "C" {

// ---- bufio.Scanner ----

struct golangc_scanner {
    golangc_file* file;
    char*   buf;      // current line (malloc'd, no newline)
    int64_t len;      // length of current line
    int64_t cap;      // buffer capacity
};

golangc_scanner* golangc_scanner_new(golangc_file* f) {
    auto* s = static_cast<golangc_scanner*>(malloc(sizeof(golangc_scanner)));
    s->file = f;
    s->cap  = 4096;
    s->buf  = static_cast<char*>(malloc(static_cast<size_t>(s->cap)));
    s->len  = 0;
    return s;
}

int64_t golangc_scanner_scan(golangc_scanner* s) {
    if (!s || !s->file || !s->file->f) return 0;
    if (!fgets(s->buf, static_cast<int>(s->cap), s->file->f)) return 0;
    s->len = static_cast<int64_t>(strlen(s->buf));
    // Strip trailing newline
    if (s->len > 0 && s->buf[s->len - 1] == '\n') { s->len--; s->buf[s->len] = '\0'; }
    if (s->len > 0 && s->buf[s->len - 1] == '\r') { s->len--; s->buf[s->len] = '\0'; }
    return 1;
}

void golangc_scanner_text(char* sret_out, golangc_scanner* s) {
    if (!s) {
        *reinterpret_cast<char**>(sret_out) = nullptr;
        *reinterpret_cast<int64_t*>(sret_out + 8) = 0;
        return;
    }
    // Return a malloc'd copy so the caller owns it
    char* copy = static_cast<char*>(malloc(static_cast<size_t>(s->len) + 1));
    if (copy) { memcpy(copy, s->buf, static_cast<size_t>(s->len)); copy[s->len] = '\0'; }
    *reinterpret_cast<char**>(sret_out) = copy;
    *reinterpret_cast<int64_t*>(sret_out + 8) = s->len;
}

// ---- bufio.Reader ----

struct golangc_breader {
    golangc_file* file;
};

golangc_breader* golangc_breader_new(golangc_file* f) {
    auto* r = static_cast<golangc_breader*>(malloc(sizeof(golangc_breader)));
    r->file = f;
    return r;
}

void golangc_breader_read_string(char* sret_out, golangc_breader* r, int64_t delim) {
    if (!r || !r->file || !r->file->f) {
        *reinterpret_cast<char**>(sret_out) = nullptr;
        *reinterpret_cast<int64_t*>(sret_out + 8) = 0;
        return;
    }
    // Read character by character until delim or EOF
    int64_t cap = 256;
    char* buf = static_cast<char*>(malloc(static_cast<size_t>(cap)));
    int64_t len = 0;
    int c;
    while ((c = fgetc(r->file->f)) != EOF) {
        if (len + 1 >= cap) {
            cap *= 2;
            buf = static_cast<char*>(realloc(buf, static_cast<size_t>(cap)));
        }
        buf[len++] = static_cast<char>(c);
        if (c == static_cast<int>(delim)) break;
    }
    buf[len] = '\0';
    *reinterpret_cast<char**>(sret_out) = buf;
    *reinterpret_cast<int64_t*>(sret_out + 8) = len;
}

// ---- os.ReadFile ----

// GoRuntimeSlice layout: {ptr, len, cap} (3 x int64 = 24 bytes)
struct GoRuntimeSlice3 { void* ptr; int64_t len; int64_t cap; };

void golangc_os_read_file(char* sret_out, const char* path_ptr, int64_t path_len) {
    auto* out = reinterpret_cast<GoRuntimeSlice3*>(sret_out);
    out->ptr = nullptr; out->len = 0; out->cap = 0;
    if (!path_ptr || path_len <= 0) return;

    char path_buf[4096];
    int64_t n = path_len < 4095 ? path_len : 4095;
    memcpy(path_buf, path_ptr, static_cast<size_t>(n));
    path_buf[n] = '\0';

    FILE* f = nullptr;
#ifdef _MSC_VER
    fopen_s(&f, path_buf, "rb");
#else
    f = fopen(path_buf, "rb");
#endif
    if (!f) return;

    fseek(f, 0, SEEK_END);
    long sz = ftell(f);
    fseek(f, 0, SEEK_SET);
    if (sz <= 0) { fclose(f); return; }

    char* data = static_cast<char*>(malloc(static_cast<size_t>(sz)));
    if (data) {
        fread(data, 1, static_cast<size_t>(sz), f);
        out->ptr = data;
        out->len = static_cast<int64_t>(sz);
        out->cap = static_cast<int64_t>(sz);
    }
    fclose(f);
}

} // extern "C"
