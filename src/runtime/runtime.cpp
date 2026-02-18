#include "runtime/runtime.hpp"

#include <cstdio>
#include <cstdlib>

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

[[noreturn]] void golangc_panic(const char* msg) {
    fprintf(stderr, "goroutine 1 [running]:\npanic: %s\n", msg ? msg : "unknown");
    exit(2);
}

} // extern "C"
