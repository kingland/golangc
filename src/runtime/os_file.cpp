// os_file.cpp — singleton file handles for os.Stdout, os.Stderr, os.Stdin
//               + os.Open, os.Create, file close/write, os.Exit

#include "runtime/runtime.hpp"  // for golangc_file struct
#include <cstdlib>
#include <cstring>

extern "C" {

static golangc_file g_stdout = { nullptr };
static golangc_file g_stderr = { nullptr };
static golangc_file g_stdin  = { nullptr };

golangc_file* golangc_os_stdout(void) {
    if (!g_stdout.f) g_stdout.f = stdout;
    return &g_stdout;
}
golangc_file* golangc_os_stderr(void) {
    if (!g_stderr.f) g_stderr.f = stderr;
    return &g_stderr;
}
golangc_file* golangc_os_stdin(void) {
    if (!g_stdin.f) g_stdin.f = stdin;
    return &g_stdin;
}

[[noreturn]] void golangc_os_exit(int64_t code) {
    exit(static_cast<int>(code));
}

golangc_file* golangc_os_open(const char* path_ptr, int64_t path_len) {
    char buf[4096];
    int64_t n = path_len < 4095 ? path_len : 4095;
    memcpy(buf, path_ptr, static_cast<size_t>(n));
    buf[n] = '\0';
    auto* gf = static_cast<golangc_file*>(malloc(sizeof(golangc_file)));
#ifdef _MSC_VER
    fopen_s(&gf->f, buf, "rb");
#else
    gf->f = fopen(buf, "rb");
#endif
    return gf;
}

golangc_file* golangc_os_create(const char* path_ptr, int64_t path_len) {
    char buf[4096];
    int64_t n = path_len < 4095 ? path_len : 4095;
    memcpy(buf, path_ptr, static_cast<size_t>(n));
    buf[n] = '\0';
    auto* gf = static_cast<golangc_file*>(malloc(sizeof(golangc_file)));
#ifdef _MSC_VER
    fopen_s(&gf->f, buf, "wb");
#else
    gf->f = fopen(buf, "wb");
#endif
    return gf;
}

void golangc_os_file_close(golangc_file* f) {
    if (f && f->f) { fclose(f->f); f->f = nullptr; }
}

int64_t golangc_os_file_write_string(golangc_file* f, const char* ptr, int64_t len) {
    if (!f || !f->f || !ptr || len <= 0) return 0;
    return static_cast<int64_t>(fwrite(ptr, 1, static_cast<size_t>(len), f->f));
}

} // extern "C"
