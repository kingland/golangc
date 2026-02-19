#pragma once

// Go compiler runtime library — extern "C" functions called by generated code.
// These are linked into the final executable as golangc_runtime.lib.

#include <cstdint>

extern "C" {

/// Print an integer followed by a newline.
void golangc_println_int(int64_t value);

/// Print a string (ptr + length) followed by a newline.
void golangc_println_string(const char* data, int64_t length);

/// Print a boolean ("true"/"false") followed by a newline.
void golangc_println_bool(int64_t value);

/// Print an integer without a newline.
void golangc_print_int(int64_t value);

/// Print a string (ptr + length) without a newline.
void golangc_print_string(const char* data, int64_t length);

/// Print a boolean without a newline.
void golangc_print_bool(int64_t value);

/// Print a single space.
void golangc_print_space();

/// Print a newline.
void golangc_print_newline();

/// Print a float followed by a newline.
void golangc_println_float(double value);

/// Print a float without a newline.
void golangc_print_float(double value);

/// String concatenation: returns {ptr, len} via sret.
/// sret_out points to a 16-byte buffer for {ptr, len}.
void golangc_string_concat(char* sret_out,
                            const char* ptr1, int64_t len1,
                            const char* ptr2, int64_t len2);

/// Panic with a message and exit.
[[noreturn]] void golangc_panic(const char* msg);

} // extern "C"
