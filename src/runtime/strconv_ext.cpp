// strconv_ext.cpp — extended strconv: ParseFloat, FormatFloat, ParseBool, FormatBool

#include "runtime/runtime.hpp"
#include <cstdlib>
#include <cstdio>
#include <cstring>

extern "C" {

double golangc_parse_float(const char* ptr, int64_t len) {
    if (!ptr || len <= 0) return 0.0;
    char buf[64];
    int64_t n = len < 63 ? len : 63;
    memcpy(buf, ptr, static_cast<size_t>(n));
    buf[n] = '\0';
    return strtod(buf, nullptr);
}

void golangc_format_float(char* sret_out, double value) {
    char* s = static_cast<char*>(malloc(64));
    if (!s) {
        *reinterpret_cast<char**>(sret_out) = nullptr;
        *reinterpret_cast<int64_t*>(sret_out + 8) = 0;
        return;
    }
    int len = snprintf(s, 64, "%g", value);
    *reinterpret_cast<char**>(sret_out) = s;
    *reinterpret_cast<int64_t*>(sret_out + 8) = static_cast<int64_t>(len);
}

int64_t golangc_parse_bool(const char* ptr, int64_t len) {
    if (!ptr || len <= 0) return 0;
    char buf[8];
    int64_t n = len < 7 ? len : 7;
    memcpy(buf, ptr, static_cast<size_t>(n));
    buf[n] = '\0';
    return (strcmp(buf, "1") == 0 || strcmp(buf, "t") == 0 || strcmp(buf, "T") == 0 ||
            strcmp(buf, "true") == 0 || strcmp(buf, "TRUE") == 0 || strcmp(buf, "True") == 0) ? 1 : 0;
}

void golangc_format_bool(char* sret_out, int64_t value) {
    const char* s = value ? "true" : "false";
    int64_t len = value ? 4 : 5;
    char* buf = static_cast<char*>(malloc(static_cast<size_t>(len) + 1));
    if (buf) memcpy(buf, s, static_cast<size_t>(len) + 1);
    *reinterpret_cast<char**>(sret_out) = buf;
    *reinterpret_cast<int64_t*>(sret_out + 8) = len;
}

} // extern "C"
