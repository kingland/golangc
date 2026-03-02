// os extras: os.WriteFile, os.Remove, os.Mkdir/MkdirAll, os.TempDir, os.UserHomeDir
// strings.Reader, io.ReadAll, path/filepath

#define _CRT_SECURE_NO_WARNINGS
#define NOMINMAX
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#include "runtime.hpp"

#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <cstdio>

// ---------------------------------------------------------------------------
// Helpers: null-terminate a string header pair
// ---------------------------------------------------------------------------

namespace {

static char* dup_cstr(const char* ptr, int64_t len) {
    auto* s = static_cast<char*>(std::malloc(static_cast<size_t>(len) + 1));
    if (!s) return nullptr;
    std::memcpy(s, ptr, static_cast<size_t>(len));
    s[len] = '\0';
    return s;
}

// Write a {ptr, len} string pair into a 16-byte sret buffer
static void write_sret_string(char* sret_out, const char* ptr, int64_t len) {
    // Heap-dup the string so lifetime doesn't depend on stack
    char* dup = static_cast<char*>(std::malloc(static_cast<size_t>(len)));
    if (dup && len > 0) std::memcpy(dup, ptr, static_cast<size_t>(len));
    *reinterpret_cast<const char**>(sret_out)       = dup ? dup : "";
    *reinterpret_cast<int64_t*>(sret_out + 8)       = len;
}

// Slice header layout: {void* ptr, int64_t len, int64_t cap}
struct GoSlice {
    void*   ptr;
    int64_t len;
    int64_t cap;
};

} // namespace

extern "C" {

// ---------------------------------------------------------------------------
// os.WriteFile(name string, data []byte, perm int) error
// ---------------------------------------------------------------------------

void* golangc_os_write_file(const char* name_ptr, int64_t name_len, void* slice_header) {
    if (!name_ptr || !slice_header) return nullptr;
    char* path = dup_cstr(name_ptr, name_len);
    if (!path) return nullptr;

    auto* sl = static_cast<GoSlice*>(slice_header);
    FILE* f = std::fopen(path, "wb");
    std::free(path);
    if (!f) return reinterpret_cast<void*>(1); // non-null = error (simplified)
    if (sl->ptr && sl->len > 0)
        std::fwrite(sl->ptr, 1, static_cast<size_t>(sl->len), f);
    std::fclose(f);
    return nullptr; // nil error
}

// ---------------------------------------------------------------------------
// os.Remove(name string) error
// ---------------------------------------------------------------------------

void* golangc_os_remove(const char* name_ptr, int64_t name_len) {
    if (!name_ptr) return nullptr;
    char* path = dup_cstr(name_ptr, name_len);
    if (!path) return nullptr;
    int rc = std::remove(path);
    std::free(path);
    return rc == 0 ? nullptr : reinterpret_cast<void*>(1);
}

// ---------------------------------------------------------------------------
// os.Mkdir(name string, perm int) error
// ---------------------------------------------------------------------------

void* golangc_os_mkdir(const char* name_ptr, int64_t name_len, int64_t /*perm*/) {
    if (!name_ptr) return nullptr;
    char* path = dup_cstr(name_ptr, name_len);
    if (!path) return nullptr;
    BOOL ok = CreateDirectoryA(path, nullptr);
    std::free(path);
    return ok ? nullptr : reinterpret_cast<void*>(1);
}

// ---------------------------------------------------------------------------
// os.MkdirAll(path string, perm int) error
// ---------------------------------------------------------------------------

void* golangc_os_mkdir_all(const char* name_ptr, int64_t name_len, int64_t /*perm*/) {
    // Simplified: create one directory (no recursive creation)
    return golangc_os_mkdir(name_ptr, name_len, 0755);
}

// ---------------------------------------------------------------------------
// os.TempDir() string  — via sret
// ---------------------------------------------------------------------------

void golangc_os_temp_dir(char* sret_out) {
    char buf[MAX_PATH + 1];
    DWORD n = GetTempPathA(static_cast<DWORD>(sizeof(buf)), buf);
    if (n == 0) { write_sret_string(sret_out, ".", 1); return; }
    // Remove trailing backslash
    if (n > 0 && buf[n-1] == '\\') --n;
    write_sret_string(sret_out, buf, static_cast<int64_t>(n));
}

// ---------------------------------------------------------------------------
// os.UserHomeDir() (string, error)  — via sret (returns just string)
// ---------------------------------------------------------------------------

void golangc_os_user_home_dir(char* sret_out) {
    const char* home = std::getenv("USERPROFILE");
    if (!home) home = std::getenv("HOME");
    if (!home) home = ".";
    write_sret_string(sret_out, home, static_cast<int64_t>(std::strlen(home)));
}

// ---------------------------------------------------------------------------
// strings.Reader
// ---------------------------------------------------------------------------

struct golangc_strings_reader {
    const char* data;
    int64_t     total_len;
    int64_t     pos;
};

golangc_strings_reader* golangc_strings_reader_new(const char* ptr, int64_t len) {
    auto* r = static_cast<golangc_strings_reader*>(std::malloc(sizeof(golangc_strings_reader)));
    if (!r) return nullptr;
    // Dup the string so the reader owns it
    char* dup = static_cast<char*>(std::malloc(static_cast<size_t>(len)));
    if (dup && len > 0) std::memcpy(dup, ptr, static_cast<size_t>(len));
    r->data      = dup ? dup : "";
    r->total_len = len;
    r->pos       = 0;
    return r;
}

int64_t golangc_strings_reader_read(golangc_strings_reader* r, void* slice_header) {
    if (!r || !slice_header) return 0;
    auto* sl = static_cast<GoSlice*>(slice_header);
    if (!sl->ptr || sl->len <= 0) return 0;
    int64_t avail = r->total_len - r->pos;
    if (avail <= 0) return 0;
    int64_t n = avail < sl->len ? avail : sl->len;
    std::memcpy(sl->ptr, r->data + r->pos, static_cast<size_t>(n));
    r->pos += n;
    return n;
}

int64_t golangc_strings_reader_len(golangc_strings_reader* r) {
    if (!r) return 0;
    return r->total_len - r->pos;
}

// ---------------------------------------------------------------------------
// io.ReadAll(r) ([]byte, error)  — simplified: only handles strings.Reader
// Returns 24-byte {ptr, len, cap} via sret
// ---------------------------------------------------------------------------

void golangc_io_read_all(char* sret_out, void* reader_ptr) {
    auto* r = static_cast<golangc_strings_reader*>(reader_ptr);
    if (!r || !r->data) {
        // Return empty slice
        *reinterpret_cast<void**>(sret_out)         = nullptr;
        *reinterpret_cast<int64_t*>(sret_out + 8)   = 0;
        *reinterpret_cast<int64_t*>(sret_out + 16)  = 0;
        return;
    }
    int64_t remaining = r->total_len - r->pos;
    void* buf = nullptr;
    if (remaining > 0) {
        buf = std::malloc(static_cast<size_t>(remaining));
        if (buf) std::memcpy(buf, r->data + r->pos, static_cast<size_t>(remaining));
    }
    r->pos = r->total_len; // consumed
    *reinterpret_cast<void**>(sret_out)         = buf;
    *reinterpret_cast<int64_t*>(sret_out + 8)   = remaining;
    *reinterpret_cast<int64_t*>(sret_out + 16)  = remaining;
}

// ---------------------------------------------------------------------------
// path/filepath helpers
// ---------------------------------------------------------------------------

// Windows path separator is backslash; Go filepath.Join uses OS separator.
static const char kSep = '\\';

void golangc_filepath_join2(char* sret_out, const char* a_ptr, int64_t a_len,
                             const char* b_ptr, int64_t b_len) {
    // Trim trailing sep from a, add one sep, then b
    while (a_len > 0 && (a_ptr[a_len-1] == '\\' || a_ptr[a_len-1] == '/')) --a_len;
    // Skip leading sep from b
    while (b_len > 0 && (b_ptr[0] == '\\' || b_ptr[0] == '/')) { ++b_ptr; --b_len; }

    int64_t total = a_len + 1 + b_len;
    if (a_len == 0) { write_sret_string(sret_out, b_ptr, b_len); return; }
    if (b_len == 0) { write_sret_string(sret_out, a_ptr, a_len); return; }

    char* buf = static_cast<char*>(std::malloc(static_cast<size_t>(total)));
    if (!buf) { write_sret_string(sret_out, "", 0); return; }
    std::memcpy(buf, a_ptr, static_cast<size_t>(a_len));
    buf[a_len] = kSep;
    std::memcpy(buf + a_len + 1, b_ptr, static_cast<size_t>(b_len));
    *reinterpret_cast<const char**>(sret_out)     = buf;
    *reinterpret_cast<int64_t*>(sret_out + 8)     = total;
}

void golangc_filepath_dir(char* sret_out, const char* path_ptr, int64_t path_len) {
    // Find last '/' or '\\'
    int64_t last = -1;
    for (int64_t i = path_len - 1; i >= 0; --i) {
        if (path_ptr[i] == '/' || path_ptr[i] == '\\') { last = i; break; }
    }
    if (last < 0) { write_sret_string(sret_out, ".", 1); return; }
    if (last == 0) { write_sret_string(sret_out, "/", 1); return; }
    write_sret_string(sret_out, path_ptr, last);
}

void golangc_filepath_base(char* sret_out, const char* path_ptr, int64_t path_len) {
    // Remove trailing separators
    while (path_len > 1 && (path_ptr[path_len-1] == '/' || path_ptr[path_len-1] == '\\'))
        --path_len;
    // Find last separator
    int64_t last = -1;
    for (int64_t i = path_len - 1; i >= 0; --i) {
        if (path_ptr[i] == '/' || path_ptr[i] == '\\') { last = i; break; }
    }
    const char* base = path_ptr + last + 1;
    int64_t base_len = path_len - last - 1;
    write_sret_string(sret_out, base, base_len);
}

void golangc_filepath_ext(char* sret_out, const char* path_ptr, int64_t path_len) {
    // Find last '.' after last separator
    int64_t last_sep = -1;
    for (int64_t i = path_len - 1; i >= 0; --i) {
        if (path_ptr[i] == '/' || path_ptr[i] == '\\') { last_sep = i; break; }
    }
    for (int64_t i = path_len - 1; i > last_sep; --i) {
        if (path_ptr[i] == '.') {
            write_sret_string(sret_out, path_ptr + i, path_len - i);
            return;
        }
    }
    write_sret_string(sret_out, "", 0);
}

void golangc_filepath_abs(char* sret_out, const char* path_ptr, int64_t path_len) {
    // Simplified: just return the path as-is (no CWD resolution)
    write_sret_string(sret_out, path_ptr, path_len);
}

} // extern "C"
