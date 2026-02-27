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

/// Compare two strings by content. Returns 1 if equal, 0 otherwise.
int64_t golangc_string_eq(const char* ptr1, int64_t len1,
                           const char* ptr2, int64_t len2);

// ---- Select runtime ----

/// One case entry passed to golangc_select (layout must match SelectCase in goroutine_channel.cpp).
/// op: 0 = recv, 1 = send.
struct SelectCase {
    void*   ch;   ///< golangc_chan*
    void*   val;  ///< send: ptr to value; recv: ptr to output buffer; may be null
    int64_t op;   ///< 0=recv, 1=send
};

/// Poll N channel cases non-blockingly.
/// Returns 0..num_cases-1 for the fired channel case, or num_cases if default fires.
/// If has_default==0 and no case is ready, blocks until one becomes ready.
int64_t golangc_select(SelectCase* cases, int64_t num_cases, int64_t has_default);

/// Panic with a message and exit.
[[noreturn]] void golangc_panic(const char* msg);

// ---- Map runtime ----

struct golangc_map;

/// Create a hash map with given key and value sizes in bytes.
golangc_map* golangc_map_make(int64_t key_size, int64_t val_size);

/// Look up a key. Returns pointer to value slot (or nullptr if absent).
/// Writes 1/0 to *out_ok (out_ok may be nullptr).
void* golangc_map_get(golangc_map* m, void* key_ptr, int64_t* out_ok);

/// Insert or update key → value.
void golangc_map_set(golangc_map* m, void* key_ptr, void* val_ptr);

/// Return the number of entries in a map.
int64_t golangc_map_len(golangc_map* m);

/// Delete the entry for key_ptr (no-op if absent).
void golangc_map_delete(golangc_map* m, void* key_ptr);

// ---- Map iterator ----

struct golangc_map_iter;

/// Create an iterator over a map (snapshot of bucket position).
golangc_map_iter* golangc_map_iter_make(golangc_map* m);

/// Advance iterator: copies next key/val into out_key/out_val.
/// Returns 1 if a pair was produced, 0 if exhausted.
int64_t golangc_map_iter_next(golangc_map_iter* it, void* out_key, void* out_val);

/// Free an iterator created by golangc_map_iter_make.
void golangc_map_iter_free(golangc_map_iter* it);

// ---- Slice append ----

/// Append one element to a slice, growing if necessary.
/// slice_out: pointer to {ptr,len,cap} to update in-place.
/// elem_ptr:  pointer to the element bytes.
/// elem_size: size of one element in bytes.
void golangc_slice_append(void* slice_out, const void* elem_ptr, int64_t elem_size);

// ---- Goroutine / Channel runtime ----

struct golangc_chan;

/// Create an unbuffered channel with the given element size in bytes.
golangc_chan* golangc_chan_make(int64_t elem_size);

/// Send val_ptr to ch (blocks until receiver consumes).
void golangc_chan_send(golangc_chan* ch, void* val_ptr);

/// Receive from ch into out_ptr (blocks until sender delivers).
void golangc_chan_recv(golangc_chan* ch, void* out_ptr);

/// Spawn a goroutine: func_ptr called with arg_count variadic int64_t args.
void golangc_go_spawn(void* func_ptr, int64_t arg_count, ...);

// ---- String conversion (strconv) ----

/// Convert int64 to string. Returns {ptr, len} via sret (16-byte buffer).
void golangc_itoa(char* sret_out, int64_t value);

/// Convert string (ptr + len) to int64. Returns the integer value.
/// Sets *out_ok to 1 on success, 0 on failure (out_ok may be nullptr).
int64_t golangc_atoi(const char* ptr, int64_t len, int64_t* out_ok);

// ---- String formatting (fmt) ----

/// Simple sprintf: format string (ptr + len) + variadic int64/string args.
/// Supports %d (int64), %s (string = ptr+len pair), %v (same as %d/int).
/// Returns {ptr, len} via sret (16-byte buffer).
void golangc_sprintf(char* sret_out, const char* fmt_ptr, int64_t fmt_len, ...);

/// Simple printf to stdout — same format conventions as golangc_sprintf.
void golangc_printf(const char* fmt_ptr, int64_t fmt_len, ...);

// ---- Rune / character conversion ----

/// Convert a Unicode code point (rune) to a UTF-8 string.
/// Returns {ptr, len} via sret (16-byte buffer).
void golangc_rune_to_string(char* sret_out, int32_t rune_val);

// ---- os.Args ----

/// Pointer to the runtime []string slice for command-line arguments.
/// Populated by golangc_init_args() before main() is called.
extern struct GoRuntimeSlice* golangc_os_args;

/// Initialize os.Args from argc/argv.  Call this at program startup.
void golangc_init_args(int argc, char** argv);

/// Return the os.Args slice by value via sret (24-byte {ptr, len, cap} buffer).
void golangc_os_args_get(char* sret_out);

} // extern "C"

/// Global closure env pointer — set by ClosureMake, read at indirect call sites.
/// Allows closures to pass captured environment without modifying calling convention.
/// NOT thread-safe; adequate for single-threaded programs.
extern "C" void* golangc_closure_env;
