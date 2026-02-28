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

} // namespace sema
} // namespace golangc
