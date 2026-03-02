#pragma once

#include "sema/scope.hpp"
#include "sema/types.hpp"
#include "common/arena_allocator.hpp"

namespace golangc {
namespace sema {

/// Built-in function IDs.
enum class BuiltinId : int {
    Println = 0,
    Print,
    Len,
    Cap,
    Make,
    New,
    Append,
    Copy,
    Delete,
    Close,
    Panic,
    Recover,
    // Pseudo-package builtins (fmt, strconv, os)
    FmtPrintln,    // fmt.Println(args...) — void, any types
    FmtPrintf,     // fmt.Printf(format string, args...) — void
    FmtSprintf,    // fmt.Sprintf(format string, args...) string
    StrconvItoa,   // strconv.Itoa(n int) string
    StrconvAtoi,   // strconv.Atoi(s string) (int, error)
    OsArgs,        // os.Args []string (global load)
    // strings pseudo-package
    StringsContains,    // strings.Contains(s, substr string) bool
    StringsHasPrefix,   // strings.HasPrefix(s, prefix string) bool
    StringsHasSuffix,   // strings.HasSuffix(s, suffix string) bool
    StringsIndex,       // strings.Index(s, substr string) int
    StringsToUpper,     // strings.ToUpper(s string) string
    StringsToLower,     // strings.ToLower(s string) string
    StringsTrimSpace,   // strings.TrimSpace(s string) string
    StringsRepeat,      // strings.Repeat(s string, count int) string
    StringsReplace,     // strings.Replace(s, old, new string, n int) string
    StringsCount,       // strings.Count(s, substr string) int
    StringsTrim,        // strings.Trim(s, cutset string) string
    StringsSplit,       // strings.Split(s, sep string) []string  (limited support)
    StringsJoin,        // strings.Join(elems []string, sep string) string
    // math pseudo-package
    MathAbs,   // math.Abs(x float64) float64
    MathSqrt,  // math.Sqrt(x float64) float64
    MathFloor, // math.Floor(x float64) float64
    MathCeil,  // math.Ceil(x float64) float64
    MathRound, // math.Round(x float64) float64
    MathMax,   // math.Max(x, y float64) float64
    MathMin,   // math.Min(x, y float64) float64
    MathPow,   // math.Pow(x, y float64) float64
    MathLog,   // math.Log(x float64) float64
    MathLog2,  // math.Log2(x float64) float64
    MathLog10, // math.Log10(x float64) float64
    // strings.Builder methods (called on a *Builder receiver)
    StringsBuilderWriteString, // b.WriteString(s string)
    StringsBuilderWriteByte,   // b.WriteByte(c byte)
    StringsBuilderString,      // b.String() string
    StringsBuilderReset,       // b.Reset()
    StringsBuilderLen,         // b.Len() int
    // errors pseudo-package
    ErrorsNew,   // errors.New(msg string) error
    ErrorsIs,    // errors.Is(err, target error) bool
    ErrorsAs,    // errors.As(err error, target interface{}) bool
    // fmt.Errorf
    FmtErrorf,   // fmt.Errorf(format string, args...) error
    // sync.Mutex methods (called on a *sync.Mutex receiver)
    SyncMutexLock,        // mu.Lock()
    SyncMutexUnlock,      // mu.Unlock()
    SyncMutexTryLock,     // mu.TryLock() bool
    // sync.WaitGroup methods (called on a *sync.WaitGroup receiver)
    SyncWaitGroupAdd,     // wg.Add(delta int)
    SyncWaitGroupDone,    // wg.Done()
    SyncWaitGroupWait,    // wg.Wait()
    // os file handles
    OsStdout,    // os.Stdout (*os.File)
    OsStderr,    // os.Stderr (*os.File)
    OsStdin,     // os.Stdin  (*os.File)
    // fmt output to writer
    FmtFprintf,  // fmt.Fprintf(w, format string, args...)
    FmtFprintln, // fmt.Fprintln(w, args...)
    // os functions
    OsExit,      // os.Exit(code int)
    OsOpen,      // os.Open(name string) (*os.File, error)
    OsCreate,    // os.Create(name string) (*os.File, error)
    // os.File methods
    OsFileClose,         // f.Close() error
    OsFileWriteString,   // f.WriteString(s string) (int, error)
    // extended strconv
    StrconvParseInt,     // strconv.ParseInt(s string, base, bitSize int) (int64, error)
    StrconvParseFloat,   // strconv.ParseFloat(s string, bitSize int) (float64, error)
    StrconvFormatInt,    // strconv.FormatInt(i int64, base int) string
    StrconvFormatFloat,  // strconv.FormatFloat(f float64, fmt byte, prec, bitSize int) string
    StrconvFormatBool,   // strconv.FormatBool(b bool) string
    StrconvParseBool,    // strconv.ParseBool(s string) (bool, error)
    // fmt.Scan family
    FmtScan,             // fmt.Scan(a ...interface{}) (int, error)
    FmtScanln,           // fmt.Scanln(a ...interface{}) (int, error)
    FmtScanf,            // fmt.Scanf(format string, a ...interface{}) (int, error)
    FmtSscan,            // fmt.Sscan(str string, a ...interface{}) (int, error)
    FmtSscanf,           // fmt.Sscanf(str, format string, a ...interface{}) (int, error)
    // sort pseudo-package
    SortInts,            // sort.Ints(a []int)
    SortStrings,         // sort.Strings(a []string)
    SortSlice,           // sort.Slice(slice interface{}, less func(i, j int) bool)
    // os.Getenv
    OsGetenv,            // os.Getenv(key string) string
    // strings extras
    StringsFields,       // strings.Fields(s string) []string
    StringsTrimPrefix,   // strings.TrimPrefix(s, prefix string) string
    StringsTrimSuffix,   // strings.TrimSuffix(s, suffix string) string
    StringsContainsRune, // strings.ContainsRune(s string, r rune) bool
    StringsIndexByte,    // strings.IndexByte(s string, c byte) int
    StringsLastIndex,    // strings.LastIndex(s, substr string) int
    StringsMap,          // strings.Map(mapping func(rune) rune, s string) string
    StringsTitle,        // strings.Title(s string) string  (deprecated but common)
    StringsEqualFold,    // strings.EqualFold(s, t string) bool
    StringsContainsAny,  // strings.ContainsAny(s, chars string) bool
    StringsIndexRune,    // strings.IndexRune(s string, r rune) int
    StringsReplaceAll,   // strings.ReplaceAll(s, old, new string) string
    StringsTrimLeft,     // strings.TrimLeft(s, cutset string) string
    StringsTrimRight,    // strings.TrimRight(s, cutset string) string
    // bufio pseudo-package
    BufioNewScanner,     // bufio.NewScanner(r io.Reader) *bufio.Scanner
    BufioNewReader,      // bufio.NewReader(r io.Reader) *bufio.Reader
    // bufio.Scanner methods
    BufioScannerScan,    // s.Scan() bool
    BufioScannerText,    // s.Text() string
    BufioScannerErr,     // s.Err() error
    // bufio.Reader methods
    BufioReaderReadString, // r.ReadString(delim byte) (string, error)
    // os.ReadFile
    OsReadFile,          // os.ReadFile(name string) ([]byte, error)
    // fmt.Sprint
    FmtSprint,           // fmt.Sprint(a ...interface{}) string
    FmtFprint,           // fmt.Fprint(w io.Writer, a ...interface{}) (int, error)
    // time pseudo-package
    TimeSleep,           // time.Sleep(d time.Duration)
    TimeNow,             // time.Now() time.Time
    TimeSince,           // time.Since(t time.Time) time.Duration
    // time.Duration type (namedtype sentinel — not called, used as constant multiplier)
    TimeDurationHour,    // time.Hour constant
    TimeDurationMinute,  // time.Minute constant
    TimeDurationSecond,  // time.Second constant
    TimeDurationMs,      // time.Millisecond constant
    // math/rand pseudo-package
    RandIntn,            // rand.Intn(n int) int
    RandFloat64,         // rand.Float64() float64
    RandSeed,            // rand.Seed(seed int64)
    RandNew,             // rand.New(src rand.Source) *rand.Rand (simplified)
    RandNewSource,       // rand.NewSource(seed int64) rand.Source
    // unicode pseudo-package
    UnicodeIsLetter,     // unicode.IsLetter(r rune) bool
    UnicodeIsDigit,      // unicode.IsDigit(r rune) bool
    UnicodeIsSpace,      // unicode.IsSpace(r rune) bool
    UnicodeIsUpper,      // unicode.IsUpper(r rune) bool
    UnicodeIsLower,      // unicode.IsLower(r rune) bool
    UnicodeToUpper,      // unicode.ToUpper(r rune) rune
    UnicodeToLower,      // unicode.ToLower(r rune) rune
    // bytes.Buffer methods (called on *bytes.Buffer receiver)
    BytesBufferWriteString, // b.WriteString(s string) (int, error)
    BytesBufferWriteByte,   // b.WriteByte(c byte) error
    BytesBufferWrite,       // b.Write(p []byte) (int, error)
    BytesBufferString,      // b.String() string
    BytesBufferReset,       // b.Reset()
    BytesBufferLen,         // b.Len() int
    BytesNewBuffer,         // bytes.NewBuffer(buf []byte) *bytes.Buffer
    BytesNewBufferString,   // bytes.NewBufferString(s string) *bytes.Buffer
    // os extras
    OsWriteFile,            // os.WriteFile(name string, data []byte, perm int) error
    OsRemove,               // os.Remove(name string) error
    OsMkdir,                // os.Mkdir(name string, perm int) error
    OsMkdirAll,             // os.MkdirAll(path string, perm int) error
    OsUserHomeDir,          // os.UserHomeDir() (string, error)
    OsTempDir,              // os.TempDir() string
    // os.File extras
    OsFileRead,             // f.Read(b []byte) (int, error)
    OsFileWrite,            // f.Write(b []byte) (int, error)
    OsFileSeek,             // f.Seek(offset int64, whence int) (int64, error)
    // strings.Reader
    StringsNewReader,       // strings.NewReader(s string) *strings.Reader
    StringsReaderReadString, // (not a real method, placeholder for read)
    // io
    IoReadAll,              // io.ReadAll(r io.Reader) ([]byte, error)
    // path/filepath
    FilepathJoin,           // filepath.Join(elem ...string) string
    FilepathDir,            // filepath.Dir(path string) string
    FilepathBase,           // filepath.Base(path string) string
    FilepathExt,            // filepath.Ext(path string) string
    FilepathAbs,            // filepath.Abs(path string) (string, error)
    // Sentinel
    Count
};

/// Initialize the universe scope with all predeclared Go identifiers.
/// Returns the universe scope. All types/symbols are arena-allocated.
[[nodiscard]] Scope* init_universe(ArenaAllocator& arena);

/// Get a cached basic type (singleton).
[[nodiscard]] Type* basic_type(BasicKind kind);

/// Get the predeclared 'error' interface type.
[[nodiscard]] Type* error_type();

/// Get the strings.Builder opaque pointer type (*strings.Builder).
/// Returns nullptr until init_universe() has been called.
[[nodiscard]] Type* strings_builder_ptr_type();

/// Get the sync.Mutex opaque pointer type (*sync.Mutex).
/// Returns nullptr until init_universe() has been called.
[[nodiscard]] Type* sync_mutex_ptr_type();

/// Get the sync.WaitGroup opaque pointer type (*sync.WaitGroup).
/// Returns nullptr until init_universe() has been called.
[[nodiscard]] Type* sync_waitgroup_ptr_type();

/// Get the os.File opaque pointer type (*os.File).
/// Returns nullptr until init_universe() has been called.
[[nodiscard]] Type* os_file_ptr_type();

/// Get the bufio.Scanner opaque pointer type (*bufio.Scanner).
/// Returns nullptr until init_universe() has been called.
[[nodiscard]] Type* bufio_scanner_ptr_type();

/// Get the bufio.Reader opaque pointer type (*bufio.Reader).
/// Returns nullptr until init_universe() has been called.
[[nodiscard]] Type* bufio_reader_ptr_type();

/// Get the bytes.Buffer opaque pointer type (*bytes.Buffer).
/// Returns nullptr until init_universe() has been called.
[[nodiscard]] Type* bytes_buffer_ptr_type();

/// Get the strings.Reader opaque pointer type (*strings.Reader).
/// Returns nullptr until init_universe() has been called.
[[nodiscard]] Type* strings_reader_ptr_type();

} // namespace sema
} // namespace golangc
