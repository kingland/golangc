#pragma once

// Go compiler runtime library — extern "C" functions called by generated code.
// These are linked into the final executable as golangc_runtime.lib.

#include <cstdint>
#include <cstdio>

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

/// Create a channel with the given element size in bytes.
/// buffer_cap == 0 → unbuffered rendezvous; buffer_cap > 0 → buffered ring buffer.
golangc_chan* golangc_chan_make(int64_t elem_size, int64_t buffer_cap);

/// Send val_ptr to ch (blocks until receiver consumes).
void golangc_chan_send(golangc_chan* ch, void* val_ptr);

/// Receive from ch into out_ptr (blocks until sender delivers).
void golangc_chan_recv(golangc_chan* ch, void* out_ptr);

/// Spawn a goroutine: func_ptr called with arg_count variadic int64_t args.
void golangc_go_spawn(void* func_ptr, int64_t arg_count, ...);

// ---- String / rune iteration ----

/// Decode one UTF-8 rune starting at ptr[idx].
/// Writes the rune value to *out_rune and returns the byte width (1–4).
/// Returns 1 and writes U+FFFD on invalid sequences.
int64_t golangc_string_decode_rune(const char* ptr, int64_t len,
                                    int64_t idx, int32_t* out_rune);

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

// ---- strings package ----

/// strings.Contains: returns 1 if s contains substr, 0 otherwise.
int64_t golangc_strings_contains(const char* s_ptr, int64_t s_len,
                                   const char* sub_ptr, int64_t sub_len);

/// strings.HasPrefix: returns 1 if s starts with prefix.
int64_t golangc_strings_has_prefix(const char* s_ptr, int64_t s_len,
                                    const char* pre_ptr, int64_t pre_len);

/// strings.HasSuffix: returns 1 if s ends with suffix.
int64_t golangc_strings_has_suffix(const char* s_ptr, int64_t s_len,
                                    const char* suf_ptr, int64_t suf_len);

/// strings.Index: returns byte index of first occurrence of substr in s, or -1.
int64_t golangc_strings_index(const char* s_ptr, int64_t s_len,
                               const char* sub_ptr, int64_t sub_len);

/// strings.ToUpper: returns uppercased copy of s via sret.
void golangc_strings_to_upper(char* sret_out, const char* s_ptr, int64_t s_len);

/// strings.ToLower: returns lowercased copy of s via sret.
void golangc_strings_to_lower(char* sret_out, const char* s_ptr, int64_t s_len);

/// strings.TrimSpace: returns s with leading/trailing whitespace removed via sret.
void golangc_strings_trim_space(char* sret_out, const char* s_ptr, int64_t s_len);

/// strings.Repeat: returns s repeated count times via sret.
void golangc_strings_repeat(char* sret_out, const char* s_ptr, int64_t s_len, int64_t count);

/// strings.Count: returns number of non-overlapping occurrences of substr in s.
int64_t golangc_strings_count(const char* s_ptr, int64_t s_len,
                               const char* sub_ptr, int64_t sub_len);

/// strings.Trim: returns s with all leading/trailing chars in cutset removed via sret.
void golangc_strings_trim(char* sret_out, const char* s_ptr, int64_t s_len,
                           const char* cut_ptr, int64_t cut_len);

/// strings.Replace: returns copy of s with old replaced by new (n=-1 for all) via sret.
void golangc_strings_replace(char* sret_out,
                              const char* s_ptr, int64_t s_len,
                              const char* old_ptr, int64_t old_len,
                              const char* new_ptr, int64_t new_len,
                              int64_t n);

// ---- math package ----

/// math.Abs
double golangc_math_abs(double x);
/// math.Sqrt
double golangc_math_sqrt(double x);
/// math.Floor
double golangc_math_floor(double x);
/// math.Ceil
double golangc_math_ceil(double x);
/// math.Round
double golangc_math_round(double x);
/// math.Max
double golangc_math_max(double x, double y);
/// math.Min
double golangc_math_min(double x, double y);
/// math.Pow
double golangc_math_pow(double x, double y);
/// math.Log
double golangc_math_log(double x);
/// math.Log2
double golangc_math_log2(double x);
/// math.Log10
double golangc_math_log10(double x);

// ---- strings.Builder ----

struct golangc_builder;

/// Create a new strings.Builder (empty).
golangc_builder* golangc_builder_make(void);

/// Append a string (ptr + len) to the builder.
void golangc_builder_write_string(golangc_builder* b, const char* ptr, int64_t len);

/// Append a single byte to the builder.
void golangc_builder_write_byte(golangc_builder* b, int64_t byte_val);

/// Return the accumulated string via sret (16-byte {ptr, len} buffer).
void golangc_builder_string(char* sret_out, golangc_builder* b);

/// Reset the builder to empty.
void golangc_builder_reset(golangc_builder* b);

/// Return the current length of the builder content.
int64_t golangc_builder_len(golangc_builder* b);

// ---- errors package ----

/// errors.New(msg string) error — returns interface{type_tag=1, data_ptr} via sret.
void golangc_errors_new(char* sret_out, const char* msg_ptr, int64_t msg_len);

/// fmt.Errorf(format string, ...) error — formatted error via sret.
void golangc_fmt_errorf(char* sret_out, const char* fmt_ptr, int64_t fmt_len, ...);

// ---- sync package ----

struct golangc_mutex;

/// Create a new sync.Mutex (backed by CRITICAL_SECTION).
golangc_mutex* golangc_mutex_make(void);

/// Lock the mutex (blocks until acquired).
void golangc_mutex_lock(golangc_mutex* m);

/// Unlock the mutex.
void golangc_mutex_unlock(golangc_mutex* m);

/// Try to lock the mutex without blocking. Returns 1 if acquired, 0 otherwise.
int64_t golangc_mutex_try_lock(golangc_mutex* m);

struct golangc_waitgroup;

/// Create a new sync.WaitGroup (backed by CRITICAL_SECTION + event HANDLE).
golangc_waitgroup* golangc_waitgroup_make(void);

/// Add delta to the WaitGroup counter. Signals done event when counter reaches 0.
void golangc_waitgroup_add(golangc_waitgroup* wg, int64_t delta);

/// Decrement the WaitGroup counter by 1 (equivalent to Add(-1)).
void golangc_waitgroup_done(golangc_waitgroup* wg);

/// Block until the WaitGroup counter reaches zero.
void golangc_waitgroup_wait(golangc_waitgroup* wg);

// ---- os.File handle ----

struct golangc_file { FILE* f; };

/// Return the singleton *os.File for stdout / stderr / stdin.
golangc_file* golangc_os_stdout(void);
golangc_file* golangc_os_stderr(void);
golangc_file* golangc_os_stdin(void);

// ---- fmt.Fprintf ----

/// Write formatted output to a file handle (same format conventions as golangc_printf).
void golangc_fprintf(golangc_file* f, const char* fmt_ptr, int64_t fmt_len, ...);

// ---- os.Exit ----
[[noreturn]] void golangc_os_exit(int64_t code);

// ---- os file open/create/close/write ----
/// Open a file for reading. Returns heap-allocated golangc_file*.
golangc_file* golangc_os_open(const char* path_ptr, int64_t path_len);
/// Create/truncate a file for writing. Returns heap-allocated golangc_file*.
golangc_file* golangc_os_create(const char* path_ptr, int64_t path_len);
/// Close the file and set f->f = nullptr.
void golangc_os_file_close(golangc_file* f);
/// Write a string (ptr+len) to f. Returns bytes written.
int64_t golangc_os_file_write_string(golangc_file* f, const char* ptr, int64_t len);

// ---- Extended strconv ----
/// strconv.ParseFloat: parse string to double.
double golangc_parse_float(const char* ptr, int64_t len);
/// strconv.FormatFloat: double to string via sret (16-byte {ptr,len}).
void golangc_format_float(char* sret_out, double value);
/// strconv.ParseBool: parse string to bool (1/0).
int64_t golangc_parse_bool(const char* ptr, int64_t len);
/// strconv.FormatBool: bool to "true"/"false" via sret (16-byte {ptr,len}).
void golangc_format_bool(char* sret_out, int64_t value);

// ---- bufio package ----

struct golangc_scanner;
struct golangc_breader;

/// Create a scanner wrapping a file handle.
golangc_scanner* golangc_scanner_new(golangc_file* f);
/// Advance scanner to next line. Returns 1 if a line was read, 0 at EOF.
int64_t golangc_scanner_scan(golangc_scanner* s);
/// Return the last scanned line (without newline) via sret (16-byte {ptr,len}).
void golangc_scanner_text(char* sret_out, golangc_scanner* s);

/// Create a buffered reader wrapping a file handle.
golangc_breader* golangc_breader_new(golangc_file* f);
/// Read until delim (inclusive), returns string via sret. EOF → empty string.
void golangc_breader_read_string(char* sret_out, golangc_breader* r, int64_t delim);

// ---- os.ReadFile ----
/// Read entire file into a slice (heap-allocated). Returns {ptr, len, cap} via sret.
void golangc_os_read_file(char* sret_out, const char* path_ptr, int64_t path_len);

} // extern "C"

/// Global closure env pointer — set by ClosureMake, read at indirect call sites.
/// Allows closures to pass captured environment without modifying calling convention.
/// NOT thread-safe; adequate for single-threaded programs.
extern "C" void* golangc_closure_env;
