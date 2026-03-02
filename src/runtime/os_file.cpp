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

golangc_file* golangc_os_open(const GoString* path) {
    if (!path || !path->ptr || path->len <= 0) return nullptr;
    int64_t path_len = path->len;
    const char* path_ptr = path->ptr;
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

golangc_file* golangc_os_create(const GoString* path) {
    if (!path || !path->ptr || path->len <= 0) return nullptr;
    int64_t path_len = path->len;
    const char* path_ptr = path->ptr;
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

int64_t golangc_os_file_write_string(golangc_file* f, const GoString* s) {
    if (!f || !f->f || !s || !s->ptr || s->len <= 0) return 0;
    return static_cast<int64_t>(fwrite(s->ptr, 1, static_cast<size_t>(s->len), f->f));
}

// Slice header layout: {ptr, len, cap} — each 8 bytes
struct GoSliceHdr { void* ptr; int64_t len; int64_t cap; };

int64_t golangc_os_file_read(golangc_file* f, void* slice_header) {
    if (!f || !f->f || !slice_header) return 0;
    auto* sh = static_cast<GoSliceHdr*>(slice_header);
    if (!sh->ptr || sh->len <= 0) return 0;
    return static_cast<int64_t>(fread(sh->ptr, 1, static_cast<size_t>(sh->len), f->f));
}

int64_t golangc_os_file_write(golangc_file* f, void* slice_header) {
    if (!f || !f->f || !slice_header) return 0;
    auto* sh = static_cast<GoSliceHdr*>(slice_header);
    if (!sh->ptr || sh->len <= 0) return 0;
    return static_cast<int64_t>(fwrite(sh->ptr, 1, static_cast<size_t>(sh->len), f->f));
}

int64_t golangc_os_file_seek(golangc_file* f, int64_t offset, int64_t whence) {
    if (!f || !f->f) return -1;
    int w = (whence == 0) ? SEEK_SET : (whence == 1) ? SEEK_CUR : SEEK_END;
#ifdef _MSC_VER
    if (_fseeki64(f->f, offset, w) != 0) return -1;
    return static_cast<int64_t>(_ftelli64(f->f));
#else
    if (fseeko(f->f, static_cast<off_t>(offset), w) != 0) return -1;
    return static_cast<int64_t>(ftello(f->f));
#endif
}

void golangc_os_getenv(char* sret_out, const GoString* key) {
    const char* key_ptr = key ? key->ptr : nullptr;
    int64_t key_len     = key ? key->len : 0;
    char key_buf[1024];
    int64_t n = key_len < 1023 ? key_len : 1023;
    if (key_ptr && n > 0) memcpy(key_buf, key_ptr, static_cast<size_t>(n));
    key_buf[n] = '\0';

    const char* val = nullptr;
    size_t sz = 0;
#ifdef _MSC_VER
    char env_buf[4096];
    size_t env_sz = 0;
    if (getenv_s(&env_sz, env_buf, sizeof(env_buf), key_buf) == 0 && env_sz > 1) {
        val = env_buf;
        sz = env_sz - 1; // env_sz includes null terminator
    }
#else
    val = getenv(key_buf);
    sz = val ? strlen(val) : 0;
#endif

    if (!val || sz == 0) {
        *reinterpret_cast<const char**>(sret_out) = nullptr;
        *reinterpret_cast<int64_t*>(sret_out + 8) = 0;
        return;
    }
    char* buf = static_cast<char*>(malloc(sz + 1));
    if (buf) { memcpy(buf, val, sz + 1); }
    *reinterpret_cast<char**>(sret_out) = buf;
    *reinterpret_cast<int64_t*>(sret_out + 8) = static_cast<int64_t>(sz);
}

} // extern "C"
