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

/// Panic with "runtime error: integer overflow" — called by generated overflow checks.
[[noreturn]] void golangc_panic_overflow(void);

/// Panic with "runtime error: integer divide by zero" — called by generated div-zero checks.
[[noreturn]] void golangc_panic_divzero(void);

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

/// Copy elements from src slice into dst slice. Returns number of elements copied.
/// dst/src: pointer to {ptr,len,cap}. elem_size: bytes per element.
int64_t golangc_slice_copy(void* dst, const void* src, int64_t elem_size);

// ---- Goroutine / Channel runtime ----

struct golangc_chan;

/// Create a channel with the given element size in bytes.
/// buffer_cap == 0 → unbuffered rendezvous; buffer_cap > 0 → buffered ring buffer.
golangc_chan* golangc_chan_make(int64_t elem_size, int64_t buffer_cap);

/// Send val_ptr to ch (blocks until receiver consumes).
void golangc_chan_send(golangc_chan* ch, void* val_ptr);

/// Receive from ch into out_ptr (blocks until sender delivers).
void golangc_chan_recv(golangc_chan* ch, void* out_ptr);

/// Close a channel. Panics (exits) if ch is nullptr.
void golangc_chan_close(golangc_chan* ch);

/// Spawn a goroutine: func_ptr called with arg_count variadic int64_t args.
void golangc_go_spawn(void* func_ptr, int64_t arg_count, ...);

// ---- String / rune iteration ----

/// Decode one UTF-8 rune starting at ptr[idx].
/// Writes the rune value to *out_rune and returns the byte width (1–4).
/// Returns 1 and writes U+FFFD on invalid sequences.
int64_t golangc_string_decode_rune(const char* ptr, int64_t len,
                                    int64_t idx, int32_t* out_rune);

// ---- Go string ABI ----
// The codegen passes Go strings ({char*, int64_t}, 16 bytes) as large structs
// by address via LEA.  Runtime functions that accept a "string" parameter
// actually receive a single pointer to the {ptr, len} pair — i.e. a GoString*.
// There is NO extra "ignored" slot after each GoString* parameter.
struct GoString { const char* ptr; int64_t len; };

// ---- String conversion (strconv) ----

/// Convert int64 to string. Returns {ptr, len} via sret (16-byte buffer).
void golangc_itoa(char* sret_out, int64_t value);

/// Convert string to int64. str_struct_ptr points to a Go string {char*, int64_t}.
/// Codegen passes large-struct args by address (LEA), so the first param is a
/// pointer to the {ptr, len} pair on the caller's stack frame.
int64_t golangc_atoi(const void* str_struct_ptr);

// ---- String formatting (fmt) ----

/// Simple sprintf: format string (GoString*) + variadic args.
/// Supports %d (int64), %s (GoString* arg), %v (same as %d/int).
/// Returns {ptr, len} via sret (16-byte buffer).
void golangc_sprintf(char* sret_out, const GoString* fmt, ...);

/// Simple printf to stdout — same format conventions as golangc_sprintf.
void golangc_printf(const GoString* fmt, ...);

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
// All string parameters arrive as GoString* (large struct passed by address via LEA).

/// strings.Contains: returns 1 if s contains substr, 0 otherwise.
int64_t golangc_strings_contains(const GoString* s, const GoString* sub);

/// strings.HasPrefix: returns 1 if s starts with prefix.
int64_t golangc_strings_has_prefix(const GoString* s, const GoString* pre);

/// strings.HasSuffix: returns 1 if s ends with suffix.
int64_t golangc_strings_has_suffix(const GoString* s, const GoString* suf);

/// strings.Index: returns byte index of first occurrence of substr in s, or -1.
int64_t golangc_strings_index(const GoString* s, const GoString* sub);

/// strings.ToUpper: returns uppercased copy of s via sret.
void golangc_strings_to_upper(char* sret_out, const GoString* s);

/// strings.ToLower: returns lowercased copy of s via sret.
void golangc_strings_to_lower(char* sret_out, const GoString* s);

/// strings.TrimSpace: returns s with leading/trailing whitespace removed via sret.
void golangc_strings_trim_space(char* sret_out, const GoString* s);

/// strings.Repeat: returns s repeated count times via sret.
void golangc_strings_repeat(char* sret_out, const GoString* s, int64_t count);

/// strings.Count: returns number of non-overlapping occurrences of substr in s.
int64_t golangc_strings_count(const GoString* s, const GoString* sub);

/// strings.Trim: returns s with all leading/trailing chars in cutset removed via sret.
void golangc_strings_trim(char* sret_out, const GoString* s, const GoString* cut);

/// strings.Replace: returns copy of s with old replaced by new (n=-1 for all) via sret.
void golangc_strings_replace(char* sret_out,
                              const GoString* s,
                              const GoString* old,
                              const GoString* nw,
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
/// math.Sin
double golangc_math_sin(double x);
/// math.Cos
double golangc_math_cos(double x);
/// math.Tan
double golangc_math_tan(double x);
/// math.Asin
double golangc_math_asin(double x);
/// math.Acos
double golangc_math_acos(double x);
/// math.Atan
double golangc_math_atan(double x);
/// math.Atan2
double golangc_math_atan2(double y, double x);
/// math.Trunc
double golangc_math_trunc(double x);
/// math.Exp
double golangc_math_exp(double x);
/// math.Exp2
double golangc_math_exp2(double x);
/// math.Mod
double golangc_math_mod(double x, double y);
/// math.Hypot
double golangc_math_hypot(double x, double y);

// ---- strings.Builder ----

struct golangc_builder;

/// Create a new strings.Builder (empty).
golangc_builder* golangc_builder_make(void);

/// Append a string (GoString*) to the builder.
void golangc_builder_write_string(golangc_builder* b, const GoString* s);

/// Append a single byte to the builder.
void golangc_builder_write_byte(golangc_builder* b, int64_t byte_val);

/// Append a Unicode rune (encoded as UTF-8) to the builder.
void golangc_builder_write_rune(golangc_builder* b, int64_t r);

/// Return the accumulated string via sret (16-byte {ptr, len} buffer).
void golangc_builder_string(char* sret_out, golangc_builder* b);

/// Reset the builder to empty.
void golangc_builder_reset(golangc_builder* b);

/// Return the current length of the builder content.
int64_t golangc_builder_len(golangc_builder* b);
/// Free a strings.Builder and its internal buffer.
void golangc_builder_free(golangc_builder* b);

// ---- errors package ----

/// errors.New(msg string) error — returns interface{type_tag=1, data_ptr} via sret.
void golangc_errors_new(char* sret_out, const GoString* msg);

/// fmt.Errorf(format string, ...) error — formatted error via sret.
void golangc_fmt_errorf(char* sret_out, const GoString* fmt_gs, ...);

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
/// Free a mutex created by golangc_mutex_make.
void golangc_mutex_free(golangc_mutex* m);

struct golangc_waitgroup;

/// Create a new sync.WaitGroup (backed by CRITICAL_SECTION + event HANDLE).
golangc_waitgroup* golangc_waitgroup_make(void);

/// Add delta to the WaitGroup counter. Signals done event when counter reaches 0.
void golangc_waitgroup_add(golangc_waitgroup* wg, int64_t delta);

/// Decrement the WaitGroup counter by 1 (equivalent to Add(-1)).
void golangc_waitgroup_done(golangc_waitgroup* wg);

/// Block until the WaitGroup counter reaches zero.
void golangc_waitgroup_wait(golangc_waitgroup* wg);
/// Free a WaitGroup created by golangc_waitgroup_make.
void golangc_waitgroup_free(golangc_waitgroup* wg);

// ---- os.File handle ----

struct golangc_file { FILE* f; };

/// Return the singleton *os.File for stdout / stderr / stdin.
golangc_file* golangc_os_stdout(void);
golangc_file* golangc_os_stderr(void);
golangc_file* golangc_os_stdin(void);

// ---- fmt.Fprintf ----

/// Write formatted output to a file handle (same format conventions as golangc_printf).
void golangc_fprintf(golangc_file* f, const GoString* fmt, ...);

// ---- os.Exit ----
[[noreturn]] void golangc_os_exit(int64_t code);

// ---- os file open/create/close/write ----
/// Open a file for reading. Returns heap-allocated golangc_file*.
golangc_file* golangc_os_open(const GoString* path);
/// Create/truncate a file for writing. Returns heap-allocated golangc_file*.
golangc_file* golangc_os_create(const GoString* path);
/// Close the file and set f->f = nullptr.
void golangc_os_file_close(golangc_file* f);
/// Write a string (GoString*) to f. Returns bytes written.
int64_t golangc_os_file_write_string(golangc_file* f, const GoString* s);

// ---- Extended strconv ----
/// strconv.ParseFloat: parse string to double. Receives GoString* by address.
double golangc_parse_float(const GoString* s);
/// strconv.FormatFloat: double to string via sret (16-byte {ptr,len}).
void golangc_format_float(char* sret_out, double value);
/// strconv.ParseBool: parse string to bool (1/0). Receives GoString* by address.
int64_t golangc_parse_bool(const GoString* s);
/// strconv.FormatBool: bool to "true"/"false" via sret (16-byte {ptr,len}).
void golangc_format_bool(char* sret_out, int64_t value);

// ---- fmt.Scan* ----

/// fmt.Scan: read n items from stdin. Each item is (int64_t tag, void* ptr).
/// tag: 0=*int64, 1=*double, 2=*GoString (16-byte {ptr,len} header).
int64_t golangc_fmt_scan  (int64_t n, ...);
/// fmt.Scanln: same as Scan but stops at newline.
int64_t golangc_fmt_scanln(int64_t n, ...);
/// fmt.Scanf: format string (GoString*) first, then n, then (tag,ptr) pairs.
int64_t golangc_fmt_scanf (const GoString* fmt, int64_t n, ...);
/// fmt.Sscan: parse from string (GoString*), n (tag,ptr) pairs.
int64_t golangc_fmt_sscan (const GoString* str, int64_t n, ...);
/// fmt.Sscanf: format-directed parse from string (GoString* each).
int64_t golangc_fmt_sscanf(const GoString* str, const GoString* fmt, int64_t n, ...);

// ---- sort package ----

/// sort.Ints: in-place sort of int64 elements.
void golangc_sort_ints   (void* ptr, int64_t len);
/// sort.Strings: in-place sort of GoString (16-byte) elements.
void golangc_sort_strings(void* ptr, int64_t len);
/// sort.Slice: sort using callback. less_fn(i,j) returns 1 if elem[i] < elem[j].
void golangc_sort_slice  (void* ptr, int64_t len, void* less_fn, int64_t elem_size);

// ---- os.Getenv ----
/// os.Getenv: returns environment variable as string via sret (16-byte {ptr,len}).
void golangc_os_getenv(char* sret_out, const GoString* key);

// ---- strings extras ----

/// strings.Fields: split by whitespace, returns []string via sret (24-byte slice header).
void golangc_strings_fields(char* sret_out, const GoString* s);

/// strings.TrimPrefix: remove prefix if present, returns string via sret.
void golangc_strings_trim_prefix(char* sret_out, const GoString* s, const GoString* pre);

/// strings.TrimSuffix: remove suffix if present, returns string via sret.
void golangc_strings_trim_suffix(char* sret_out, const GoString* s, const GoString* suf);

/// strings.ContainsRune: returns 1 if s contains Unicode code point r, 0 otherwise.
int64_t golangc_strings_contains_rune(const GoString* s, int64_t r);

/// strings.IndexByte: returns index of first byte c in s, or -1.
int64_t golangc_strings_index_byte(const GoString* s, int64_t c);

/// strings.LastIndex: returns index of last occurrence of substr in s, or -1.
int64_t golangc_strings_last_index(const GoString* s, const GoString* sub);

/// strings.IndexRune: returns byte index of first occurrence of rune r in s, or -1.
int64_t golangc_strings_index_rune(const GoString* s, int64_t r);

/// strings.EqualFold: returns 1 if s and t are equal under Unicode case-folding.
int64_t golangc_strings_equal_fold(const GoString* s, const GoString* t);

/// strings.ContainsAny: returns 1 if s contains any Unicode code point in chars.
int64_t golangc_strings_contains_any(const GoString* s, const GoString* chars);

/// strings.Map: apply mapping to each rune in s, returns new string via sret.
/// mapping is a function pointer: int64_t (*)(int64_t rune)
void golangc_strings_map(char* sret_out, void* mapping_fn, const GoString* s);

/// strings.Title: returns s with each word's first letter title-cased, via sret.
void golangc_strings_title(char* sret_out, const GoString* s);

/// strings.TrimLeft: trim leading chars in cutset, returns string via sret.
void golangc_strings_trim_left(char* sret_out, const GoString* s, const GoString* cut);

/// strings.TrimRight: trim trailing chars in cutset, returns string via sret.
void golangc_strings_trim_right(char* sret_out, const GoString* s, const GoString* cut);

/// strings.Split: split s by sep, returns []string via sret (24-byte slice header).
void golangc_strings_split(char* sret_out, const GoString* s, const GoString* sep);

/// strings.Join: join []string elements with sep, returns string via sret.
void golangc_strings_join(char* sret_out,
                           void* elems_ptr, int64_t elems_len,
                           const GoString* sep);

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
void golangc_os_read_file(char* sret_out, const GoString* path);

// ---- time package ----
/// Sleep for the given nanoseconds.
void golangc_time_sleep(int64_t nanoseconds);
/// Return current time as nanoseconds since Unix epoch.
int64_t golangc_time_now(void);
/// Return nanoseconds elapsed since the given timestamp.
int64_t golangc_time_since(int64_t start_ns);

// ---- unicode package ----
/// unicode.IsLetter: returns 1 if r is a Unicode letter.
int64_t golangc_unicode_is_letter(int64_t r);
/// unicode.IsDigit: returns 1 if r is a Unicode decimal digit.
int64_t golangc_unicode_is_digit(int64_t r);
/// unicode.IsSpace: returns 1 if r is a Unicode whitespace character.
int64_t golangc_unicode_is_space(int64_t r);
/// unicode.IsUpper: returns 1 if r is an uppercase Unicode letter.
int64_t golangc_unicode_is_upper(int64_t r);
/// unicode.IsLower: returns 1 if r is a lowercase Unicode letter.
int64_t golangc_unicode_is_lower(int64_t r);
/// unicode.ToUpper: returns the uppercase mapping of r.
int64_t golangc_unicode_to_upper(int64_t r);
/// unicode.ToLower: returns the lowercase mapping of r.
int64_t golangc_unicode_to_lower(int64_t r);
/// unicode.IsPunct: returns 1 if r is a Unicode punctuation character.
int64_t golangc_unicode_is_punct(int64_t r);
/// unicode.IsControl: returns 1 if r is a Unicode control character.
int64_t golangc_unicode_is_control(int64_t r);
/// unicode.IsMark: returns 1 if r is a Unicode combining mark.
int64_t golangc_unicode_is_mark(int64_t r);
/// unicode.IsNumber: returns 1 if r is a Unicode number (decimal or other).
int64_t golangc_unicode_is_number(int64_t r);
/// unicode.IsPrint: returns 1 if r is a printable Unicode character.
int64_t golangc_unicode_is_print(int64_t r);
/// unicode.IsTitle: returns 1 if r is a Unicode title-case letter.
int64_t golangc_unicode_is_title(int64_t r);
/// unicode.IsSymbol: returns 1 if r is a Unicode symbol character.
int64_t golangc_unicode_is_symbol(int64_t r);

// ---- math/bits package ----
int64_t golangc_bits_len(int64_t x);
int64_t golangc_bits_ones_count(int64_t x);
int64_t golangc_bits_leading_zeros(int64_t x);
int64_t golangc_bits_trailing_zeros(int64_t x);
int64_t golangc_bits_rotate_left(int64_t x, int64_t k);
int64_t golangc_bits_rotate_left32(int64_t x, int64_t k);
int64_t golangc_bits_reverse64(int64_t x);
int64_t golangc_bits_reverse32(int64_t x);
int64_t golangc_bits_reverse16(int64_t x);
int64_t golangc_bits_reverse8(int64_t x);
int64_t golangc_bits_reverse_bytes64(int64_t x);
int64_t golangc_bits_reverse_bytes32(int64_t x);
int64_t golangc_bits_reverse_bytes16(int64_t x);

// ---- bytes package ----
struct golangc_bytes_buffer;
/// bytes.NewBuffer: allocate a new empty bytes.Buffer.
golangc_bytes_buffer* golangc_bytes_new_buffer(void);
/// bytes.NewBufferString: allocate a bytes.Buffer pre-filled with string content.
golangc_bytes_buffer* golangc_bytes_new_buffer_string(const GoString* s);
/// b.WriteString: append string to buffer.
void golangc_bytes_write_string(golangc_bytes_buffer* b, const GoString* s);
/// b.WriteByte: append single byte to buffer.
void golangc_bytes_write_byte(golangc_bytes_buffer* b, int64_t byte_val);
/// b.Write: append byte slice to buffer.
void golangc_bytes_write(golangc_bytes_buffer* b, void* slice_ptr, int64_t slice_len);
/// b.String: return accumulated string via sret (16-byte {ptr,len}).
void golangc_bytes_string(char* sret_out, golangc_bytes_buffer* b);
/// b.Reset: reset buffer to empty.
void golangc_bytes_reset(golangc_bytes_buffer* b);
/// b.Len: return current length of buffer content.
int64_t golangc_bytes_len(golangc_bytes_buffer* b);
/// b.WriteRune: append a UTF-8 encoded rune to buffer.
void golangc_bytes_write_rune(golangc_bytes_buffer* b, int64_t r);
/// b.Bytes: return current buffer contents as a byte slice (sret 24-byte {ptr,len,cap}).
void golangc_bytes_bytes(char* sret_out, golangc_bytes_buffer* b);
/// b.ReadByte: read and consume one byte from the buffer. Returns -1 if empty.
int64_t golangc_bytes_read_byte(golangc_bytes_buffer* b);
/// b.Grow: grow the buffer's capacity by n bytes.
void golangc_bytes_grow(golangc_bytes_buffer* b, int64_t n);
/// b.ReadFrom: read from a strings.Reader (simplified: ptr is golangc_strings_reader*).
int64_t golangc_bytes_read_from(golangc_bytes_buffer* b, void* reader_ptr);
/// Free a bytes.Buffer created by golangc_bytes_new_buffer / golangc_bytes_new_buffer_string.
void golangc_bytes_free(golangc_bytes_buffer* b);

// ---- strings.Replacer ----
struct golangc_strings_replacer;
/// strings.NewReplacer(old, new GoString*) — single-pair replacer.
golangc_strings_replacer* golangc_strings_new_replacer(const GoString* old_str, const GoString* new_str);
/// r.Replace(s) → replaced string via sret.
void golangc_strings_replacer_replace(char* sret_out, golangc_strings_replacer* r, const GoString* s);
/// r.WriteString(w, s) — write replaced s to a bufio.Writer.
void golangc_strings_replacer_write_string(golangc_strings_replacer* r, const GoString* s);
/// Free a Replacer created by golangc_strings_new_replacer.
void golangc_strings_replacer_free(golangc_strings_replacer* r);

// ---- sync.Map ----
struct golangc_sync_map;
/// Create a new sync.Map.
golangc_sync_map* golangc_sync_map_new(void);
/// m.Store(key, value int64) — simplified integer key/value map.
void golangc_sync_map_store(golangc_sync_map* m, int64_t key, int64_t val);
/// m.Load(key int64) → value int64 (0 if not found).
int64_t golangc_sync_map_load(golangc_sync_map* m, int64_t key);
/// m.Delete(key int64).
void golangc_sync_map_delete(golangc_sync_map* m, int64_t key);
/// m.LoadOrStore(key, value int64) → existing value (or stored value) int64.
int64_t golangc_sync_map_load_or_store(golangc_sync_map* m, int64_t key, int64_t val);
/// m.Range(f func(key, value int64) bool) — call f for each entry; stop if f returns 0.
void golangc_sync_map_range(golangc_sync_map* m, int64_t (*f)(int64_t key, int64_t val));
/// Free a sync.Map and all its entries.
void golangc_sync_map_free(golangc_sync_map* m);

// ---- regexp package (38G) ----
struct golangc_regexp;
/// regexp.Compile: compile pattern string into a *Regexp (null on error).
golangc_regexp* golangc_regexp_compile(const GoString* pattern);
/// regexp.MustCompile: compile pattern, abort on error.
golangc_regexp* golangc_regexp_must_compile(const GoString* pattern);
/// regexp.MatchString(pattern, s) → 1 if match, 0 otherwise.
int64_t golangc_regexp_match_string_pkg(const GoString* pattern, const GoString* s);
/// r.MatchString(s) → 1 if match, 0 otherwise.
int64_t golangc_regexp_match_string(golangc_regexp* r, const GoString* s);
/// r.FindString(s) → first match via sret GoString.
void golangc_regexp_find_string(char* sret_out, golangc_regexp* r, const GoString* s);
/// r.FindAllString(s, n) → all matches as []string via sret slice.
void golangc_regexp_find_all_string(char* sret_out, golangc_regexp* r, const GoString* s, int64_t n);
/// r.FindStringSubmatch(s) → first match+subgroups as []string via sret slice.
void golangc_regexp_find_string_submatch(char* sret_out, golangc_regexp* r, const GoString* s);
/// r.ReplaceAllString(s, repl) → replaced string via sret GoString.
void golangc_regexp_replace_all_string(char* sret_out, golangc_regexp* r, const GoString* s, const GoString* repl);
/// r.ReplaceAllLiteralString(s, repl) → replaced string via sret GoString (no backreferences).
void golangc_regexp_replace_all_literal_string(char* sret_out, golangc_regexp* r, const GoString* s, const GoString* repl);
/// r.Split(s, n) → parts as []string via sret slice.
void golangc_regexp_split(char* sret_out, golangc_regexp* r, const GoString* s, int64_t n);
/// r.String() → pattern string via sret GoString.
void golangc_regexp_string(char* sret_out, golangc_regexp* r);
/// r.NumSubexp() → number of parenthesized subexpressions.
int64_t golangc_regexp_num_subexp(golangc_regexp* r);
/// r.FindStringIndex(s) → [start, end] as []int via sret slice (empty slice if no match).
void golangc_regexp_find_string_index(char* sret_out, golangc_regexp* r, const GoString* s);
/// r.SubexpNames() → named subexpressions as []string via sret slice.
void golangc_regexp_subexp_names(char* sret_out, golangc_regexp* r);
/// Free a *Regexp created by golangc_regexp_compile / golangc_regexp_must_compile.
void golangc_regexp_free(golangc_regexp* r);

// ---- strings extra functions (38F) ----
/// strings.TrimFunc(s, f) — trims runes satisfying f from both ends.
void golangc_strings_trim_func(char* sret_out, const GoString* s, int64_t (*f)(int64_t));
/// strings.IndexFunc(s, f) — first index of rune satisfying f, or -1.
int64_t golangc_strings_index_func(const GoString* s, int64_t (*f)(int64_t));
/// strings.FieldsFunc(s, f) — split by runes satisfying f (sret slice).
void golangc_strings_fields_func(char* sret_out, const GoString* s, int64_t (*f)(int64_t));

// ---- os.File Read/Write/Seek ----
/// f.Read: read up to len(b) bytes into b slice. Returns bytes read.
int64_t golangc_os_file_read(golangc_file* f, void* slice_header);
/// f.Write: write byte slice to f. Returns bytes written.
int64_t golangc_os_file_write(golangc_file* f, void* slice_header);
/// f.Seek: seek to offset (whence: 0=start,1=current,2=end). Returns new offset.
int64_t golangc_os_file_seek(golangc_file* f, int64_t offset, int64_t whence);

// ---- os extras ----
/// os.Rename: rename/move a file. Returns 0 on success, 1 on error.
int64_t golangc_os_rename(const GoString* old_path, const GoString* new_path);
/// os.WriteFile: write []byte data to named file. Returns nil (error not exposed to codegen).
void* golangc_os_write_file(const GoString* name, void* slice_header);
/// os.Remove: delete a file by name. Returns nil error pointer (simplified).
void* golangc_os_remove(const GoString* name);
/// os.Mkdir: create directory. Returns nil error pointer (simplified).
void* golangc_os_mkdir(const GoString* name, int64_t perm);
/// os.MkdirAll: create directory and all parents. Returns nil error pointer (simplified).
void* golangc_os_mkdir_all(const GoString* name, int64_t perm);
/// os.TempDir: return temp directory string via sret (16-byte {ptr,len}).
void golangc_os_temp_dir(char* sret_out);
/// os.UserHomeDir: return home directory string via sret (16-byte {ptr,len}).
void golangc_os_user_home_dir(char* sret_out);

// ---- strings.Reader ----
struct golangc_strings_reader;
/// strings.NewReader: allocate a Reader from a string.
golangc_strings_reader* golangc_strings_reader_new(const GoString* s);
/// r.Read: read up to len(p) bytes into p, return n bytes read.
int64_t golangc_strings_reader_read(golangc_strings_reader* r, void* slice_header);
/// r.Len: remaining unread bytes.
int64_t golangc_strings_reader_len(golangc_strings_reader* r);

// ---- io.ReadAll ----
/// io.ReadAll: read all bytes from a generic reader. Simplified: only works for strings.Reader.
/// Returns []byte slice via sret (24-byte {ptr,len,cap}).
void golangc_io_read_all(char* sret_out, void* reader_ptr);

// ---- path/filepath ----
/// filepath.Join: join two path segments with OS separator via sret.
void golangc_filepath_join2(char* sret_out, const GoString* a, const GoString* b);
/// filepath.Dir: return directory part of path via sret.
void golangc_filepath_dir(char* sret_out, const GoString* path);
/// filepath.Base: return file name part of path via sret.
void golangc_filepath_base(char* sret_out, const GoString* path);
/// filepath.Ext: return file extension (including dot) via sret.
void golangc_filepath_ext(char* sret_out, const GoString* path);
/// filepath.Abs: return absolute path via sret (simplified: just cleans path).
void golangc_filepath_abs(char* sret_out, const GoString* path);

// ---- os.Stat / os.FileInfo ----
/// opaque file-info struct (heap-allocated by golangc_os_stat)
struct golangc_file_info {
    char     name[512];   ///< null-terminated file base name
    int64_t  size;        ///< file size in bytes
    int64_t  is_dir;      ///< 1 if directory, 0 otherwise
    int64_t  err_code;    ///< 0 = success, Windows error code on failure
};
/// os.Stat: stat a file. Returns pointer to heap-allocated golangc_file_info (never null).
/// On error, returned struct has err_code != 0.
golangc_file_info* golangc_os_stat(const GoString* name);
/// Extract error interface from a golangc_file_info*. Returns nil interface (16 bytes via sret)
/// if err_code==0, otherwise returns a non-nil error interface with the error message.
void golangc_os_stat_error(char* sret_out, golangc_file_info* fi);
/// os.IsNotExist: returns 1 if the error represents a "file not found" condition.
/// iface_val_ptr points to the 16-byte error interface {int64_t type_tag, void* data_ptr}.
int64_t golangc_os_is_not_exist(void* iface_val_ptr);
/// fi.Name(): return base name of file via sret string.
void golangc_file_info_name(char* sret_out, golangc_file_info* fi);
/// fi.Size(): return file size.
int64_t golangc_file_info_size(golangc_file_info* fi);
/// fi.IsDir(): return 1 if directory.
int64_t golangc_file_info_is_dir(golangc_file_info* fi);

// ---- math/rand package ----
/// Seed the random number generator.
void golangc_rand_seed(int64_t seed);
/// Return a pseudo-random int64 in [0, n).
int64_t golangc_rand_intn(int64_t n);
/// Return a pseudo-random float64 in [0.0, 1.0).
double golangc_rand_float64(void);

// ---- Reference counting ----

/// Increment the reference count of a heap-tracked object.
/// p must be a pointer returned by an RC-allocating runtime function (or nullptr).
void golangc_retain(void* ptr);

/// Decrement the reference count of a heap-tracked object.
/// Frees (and recursively tears down) the object when the count reaches zero.
void golangc_release(void* ptr);

/// Allocate a slice backing array of `byte_count` bytes with initial refcount=1.
/// Replaces raw malloc() in slice-make codegen.
void* golangc_rc_slice_alloc(int64_t byte_count);

// ---- strings extras (38C/38D) ----
/// strings.Cut(s, sep) -> (before, after string, found bool) via sret (40 bytes: 3 GoString fields + bool).
/// Writes {ptr,len,ptr,len,found(i64)} = 40 bytes into sret_out.
void golangc_strings_cut(char* sret_out, const GoString* s, const GoString* sep);
/// strings.LastIndexByte(s, c) -> int.
int64_t golangc_strings_last_index_byte(const GoString* s, int64_t c);

// ---- strconv extras (38C/38D) ----
/// strconv.FormatUint(i uint64, base int) string — formats unsigned integer in given base via sret.
void golangc_format_uint(char* sret_out, uint64_t i, int64_t base);
/// strconv.AppendInt(dst []byte, i int64, base int) []byte — appends formatted int to slice via sret.
void golangc_append_int(char* sret_out, void* dst_hdr, int64_t i, int64_t base);
/// strconv.Quote(s string) string — returns Go-syntax double-quoted string via sret.
void golangc_strconv_quote(char* sret_out, const GoString* s);
/// strconv.Unquote(s string) (string, error) — unquotes a Go-syntax quoted string via sret (32 bytes: GoString + interface).
void golangc_strconv_unquote(char* sret_out, const GoString* s);

// ---- os.ReadDir (38C) ----
/// os.ReadDir: list directory entries. Returns slice of golangc_file_info* via sret (24-byte slice header).
void golangc_os_read_dir(char* sret_out, const GoString* name);

// ---- bufio.Writer (38D) ----
struct golangc_bufio_writer;
golangc_bufio_writer* golangc_bufio_writer_new(void* file_ptr);
void   golangc_bufio_writer_write_string(golangc_bufio_writer* w, const GoString* s);
void   golangc_bufio_writer_write_byte(golangc_bufio_writer* w, int64_t c);
void   golangc_bufio_writer_write_rune(golangc_bufio_writer* w, int64_t r);
void   golangc_bufio_writer_flush(golangc_bufio_writer* w);
/// Flush + free the bufio.Writer (RAII cleanup).
void   golangc_bufio_writer_close(golangc_bufio_writer* w);
int64_t golangc_bufio_writer_buffered(golangc_bufio_writer* w);

// ---- sync.Once (38D) ----
struct golangc_sync_once;
golangc_sync_once* golangc_sync_once_new(void);
/// o.Do(f func()) — calls f exactly once.
/// Simplified: since codegen passes function pointer as raw ptr, we invoke it if not yet done.
void golangc_sync_once_do(golangc_sync_once* o, void* func_ptr);
/// Free a sync.Once created by golangc_sync_once_new.
void golangc_sync_once_free(golangc_sync_once* o);

} // extern "C"

/// Global closure env pointer — set by ClosureMake, read at indirect call sites.
/// Allows closures to pass captured environment without modifying calling convention.
/// NOT thread-safe; adequate for single-threaded programs.
extern "C" void* golangc_closure_env;
