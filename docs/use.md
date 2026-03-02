# golangc — Usage Guide

`golangc` is a Go 1.21 compiler implemented in C++, targeting x86-64 Windows executables (PE format).
It compiles a single `.go` source file directly to a native `.exe` using MASM (ml64) and MSVC's linker.

---

## Prerequisites

- **Windows 10/11 x64**
- **Visual Studio 2022** (Community or higher) with the "Desktop development with C++" workload
  - Required tools: `ml64.exe`, `link.exe`
- `ml64` and `link` must be in `PATH` (run from a **VS x64 Native Tools Command Prompt**, or call `vcvars64.bat` first)

### Quick environment setup

```bat
:: Open a VS x64 Native Tools Command Prompt, or run:
"C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Auxiliary\Build\vcvars64.bat"
```

---

## Building golangc

```bash
# From the project root:
cmake -B build -S . -DCMAKE_TOOLCHAIN_FILE="C:/W/Code/AI/vcpkg/scripts/buildsystems/vcpkg.cmake"
cmake --build build --config Debug
# or Release:
cmake --build build --config Release
```

The compiler binary is at:
- `build/bin/Debug/golangc.exe`
- `build/bin/Release/golangc.exe`

---

## Command-Line Reference

```
golangc [options] <source.go>
```

| Option | Description |
|--------|-------------|
| `--help` | Show usage |
| `--version` | Show version (`golangc 0.5.0`) |
| `--dump-tokens` | Lex the file and print all tokens, then exit |
| `--dump-ast` | Parse the file and print the AST, then exit |
| `--check` | Run semantic analysis only (type-check), then exit |
| `--dump-ir` | Generate IR and print it, then exit |
| `--emit-asm` | Generate x64 MASM assembly and print it, then exit |
| `-o <file>` | Compile to a Windows executable |

---

## Compiling and Running a Program

```bat
:: Compile hello.go → hello.exe
golangc samples\hello.go -o hello.exe

:: Run it
hello.exe
```

Output:
```
Hello, World!
```

### Debug pipeline stages

```bat
:: See tokens
golangc samples\fibonacci.go --dump-tokens

:: See AST
golangc samples\fibonacci.go --dump-ast

:: Type-check only (no code generation)
golangc samples\fibonacci.go --check

:: See IR (SSA-style intermediate representation)
golangc samples\fibonacci.go --dump-ir

:: See generated x64 MASM assembly
golangc samples\fibonacci.go --emit-asm
```

---

## Supported Go Features

### Core language
| Feature | Status |
|---------|--------|
| `package main` / `func main()` | ✅ |
| All basic types (`int`, `int8`–`int64`, `uint*`, `float32/64`, `bool`, `string`, `byte`, `rune`, `uintptr`) | ✅ |
| Arithmetic, comparison, logical, bitwise operators | ✅ |
| `if` / `else if` / `else` | ✅ |
| `for` (C-style, `while`-style, infinite) | ✅ |
| `for range` over slice, array, string (runes), map | ✅ |
| `switch` (value, tagless/expression, string, fallthrough) | ✅ |
| `type switch` (`switch v := x.(type)`) | ✅ |
| Multiple return values | ✅ |
| Named return values | ✅ |
| Variadic functions (`...T`) and spread (`f(s...)`) | ✅ |
| Closures (capturing by reference) | ✅ |
| `defer` (LIFO, including in loops) | ✅ |
| `panic` | ✅ |
| Pointer types, `new`, `&expr`, `*ptr` | ✅ |
| `const` with `iota` | ✅ |
| Type conversions | ✅ |

### Composite types
| Feature | Status |
|---------|--------|
| Structs (fields, nested, composite literals) | ✅ |
| Methods (value and pointer receivers) | ✅ |
| Interfaces (structural typing, `interface{}`) | ✅ |
| Type assertion single-value (`v := i.(T)`, panics on mismatch) | ✅ |
| Type assertion two-value (`v, ok := i.(T)`) | ✅ |
| Slices (`make`, `append`, `copy`, `len`, `cap`, index, range) | ✅ |
| Arrays (fixed-size, index, range) | ✅ |
| Maps (`make`, index, assign, `delete`, `len`, range, two-value lookup) | ✅ |
| `[]byte(string)` and `string([]byte)` conversions | ✅ |
| `string(rune)` conversion | ✅ |

### Concurrency
| Feature | Status |
|---------|--------|
| `go` statement (goroutines) | ✅ |
| Channels: unbuffered (`make(chan T)`) | ✅ |
| Channels: buffered (`make(chan T, n)`) | ✅ |
| Send (`ch <- v`), receive (`v := <-ch`) | ✅ |
| `close(ch)` | ✅ |
| `select` with send/recv/default cases | ✅ |
| `sync.Mutex` (Lock/Unlock/TryLock) | ✅ |
| `sync.WaitGroup` (Add/Done/Wait) | ✅ |

### Standard library (pseudo-packages)

All standard library functions are built-in — **no separate package compilation needed**.
Import declarations are accepted but optional.

#### `fmt`
```go
fmt.Println(args...)
fmt.Printf(format, args...)
fmt.Sprintf(format, args...) string
fmt.Sprint(args...) string
fmt.Fprintf(w, format, args...)
fmt.Fprintln(w, args...)
fmt.Fprint(w, args...)
fmt.Errorf(format, args...) error
fmt.Scan(a...)
fmt.Scanln(a...)
fmt.Scanf(format, a...)
fmt.Sscan(str, a...)
fmt.Sscanf(str, format, a...)
```
`fmt.Println`, `fmt.Sprint`, `fmt.Sprintf` automatically call `.String()` on types that implement the `Stringer` interface.

#### `strings`
```go
strings.Contains, HasPrefix, HasSuffix, Index, LastIndex
strings.ToUpper, ToLower, TrimSpace, Trim, TrimLeft, TrimRight
strings.TrimPrefix, TrimSuffix
strings.Replace, ReplaceAll
strings.Repeat, Count
strings.Split, Join, Fields
strings.EqualFold, ContainsRune, ContainsAny, IndexRune, IndexByte
strings.Map, Title
strings.NewReader(s) *strings.Reader
// strings.Builder (value or pointer)
var b strings.Builder
b.WriteString(s)
b.WriteByte(c)
b.String() string
b.Reset()
b.Len() int
```

#### `strconv`
```go
strconv.Itoa(n) string
strconv.Atoi(s) (int, error)
strconv.ParseInt(s, base, bitSize) (int64, error)
strconv.ParseFloat(s, bitSize) (float64, error)
strconv.ParseBool(s) (bool, error)
strconv.FormatInt(i, base) string
strconv.FormatFloat(f, fmt, prec, bitSize) string
strconv.FormatBool(b) string
```

#### `os`
```go
os.Args []string
os.Stdout, os.Stderr, os.Stdin   // *os.File
os.Exit(code int)
os.Getenv(key) string
os.Open(name) (*os.File, error)
os.Create(name) (*os.File, error)
os.ReadFile(name) ([]byte, error)
os.WriteFile(name, data []byte, perm) error
os.Remove(name) error
os.Mkdir(name, perm) error
os.MkdirAll(path, perm) error
os.TempDir() string
os.UserHomeDir() (string, error)
// *os.File methods
f.Close() error
f.WriteString(s) (int, error)
```

#### `errors`
```go
errors.New(msg) error
errors.Is(err, target) bool
errors.As(err, target)
```

#### `math`
```go
math.Abs, Sqrt, Floor, Ceil, Round
math.Max, Min, Pow, Log, Log2, Log10
math.Sin, Cos, Tan, Asin, Acos, Atan, Atan2
math.Trunc, Exp, Exp2, Mod, Hypot
// Constants: math.Pi, math.E, math.Phi, math.Sqrt2
// Integer limits: math.MaxInt, math.MinInt, math.MaxInt8..64, etc.
// Float limits: math.MaxFloat64, math.SmallestNonzeroFloat64, etc.
```

#### `sort`
```go
sort.Ints(a []int)
sort.Strings(a []string)
sort.Slice(slice, less func(i, j int) bool)
```

#### `bufio`
```go
bufio.NewScanner(r) *bufio.Scanner
s.Scan() bool
s.Text() string
s.Err() error
bufio.NewReader(r) *bufio.Reader
r.ReadString(delim byte) (string, error)
```

#### `io`
```go
io.ReadAll(r) ([]byte, error)
```

#### `bytes`
```go
bytes.NewBuffer(nil) *bytes.Buffer
bytes.NewBufferString(s) *bytes.Buffer
var b bytes.Buffer   // zero-value works
b.WriteString(s)
b.WriteByte(c)
b.Write(p []byte)
b.String() string
b.Reset()
b.Len() int
```

#### `unicode`
```go
unicode.IsLetter(r) bool
unicode.IsDigit(r) bool
unicode.IsSpace(r) bool
unicode.IsUpper(r) bool
unicode.IsLower(r) bool
unicode.ToUpper(r) rune
unicode.ToLower(r) rune
```

#### `time`
```go
time.Sleep(d)
time.Now() time.Time
time.Since(t) time.Duration
time.Hour, time.Minute, time.Second, time.Millisecond
```

#### `rand` (math/rand)
```go
rand.Seed(seed int64)
rand.Intn(n int) int
rand.Float64() float64
```

#### `sync`
```go
var mu sync.Mutex
mu.Lock(); mu.Unlock(); mu.TryLock() bool
var wg sync.WaitGroup
wg.Add(n); wg.Done(); wg.Wait()
```

#### `path/filepath`
```go
filepath.Join(a, b string) string
filepath.Dir(path) string
filepath.Base(path) string
filepath.Ext(path) string
filepath.Abs(path) (string, error)
```

---

## Sample Programs

All sample programs are in the `samples/` directory. Compile and run any of them:

```bat
golangc samples\hello.go        -o hello.exe        && hello.exe
golangc samples\fibonacci.go    -o fib.exe          && fib.exe
golangc samples\structs.go      -o structs.exe       && structs.exe
golangc samples\interfaces.go   -o interfaces.exe    && interfaces.exe
golangc samples\closures.go     -o closures.exe      && closures.exe
golangc samples\goroutines.go   -o goroutines.exe    && goroutines.exe
golangc samples\maps.go         -o maps.exe          && maps.exe
golangc samples\switch.go       -o switch.exe        && switch.exe
golangc samples\variadic.go     -o variadic.exe      && variadic.exe
golangc samples\defer.go        -o defer.exe         && defer.exe
golangc samples\typeswitch.go   -o typeswitch.exe    && typeswitch.exe
golangc samples\floats.go       -o floats.exe        && floats.exe
```

---

## Running the Test Suite

Tests are built alongside the compiler and live in `build/bin/Debug/`.

```bat
:: Run all tests
cd build
ctest --output-on-failure -C Debug

:: Or run individual test suites directly:
build\bin\Debug\test_common.exe
build\bin\Debug\test_lexer.exe
build\bin\Debug\test_parser.exe
build\bin\Debug\test_sema.exe
build\bin\Debug\test_ir.exe
build\bin\Debug\test_codegen.exe
```

### Filtering tests with `--gtest_filter`

GoogleTest supports glob-style filter patterns:

```bat
:: Run only type switch tests
build\bin\Debug\test_codegen.exe --gtest_filter="TypeSwitch*"

:: Run all Phase 30+ tests
build\bin\Debug\test_codegen.exe --gtest_filter="Phase3*"

:: Run a specific test
build\bin\Debug\test_codegen.exe --gtest_filter="Phase31Test.CopyEmitsRuntime"

:: List all available tests
build\bin\Debug\test_codegen.exe --gtest_list_tests
```

### Current test counts (as of Phase 32)

| Suite | Binary | Tests |
|-------|--------|-------|
| Common utilities | `test_common.exe` | 30 |
| Lexer | `test_lexer.exe` | 89 |
| Parser | `test_parser.exe` | 87 |
| Semantic analysis | `test_sema.exe` | 110 |
| IR generation | `test_ir.exe` | 71 |
| Code generation | `test_codegen.exe` | 379 |
| **Total** | | **766** |

---

## Known Limitations

- **Single file only**: the compiler accepts one `.go` source file. Multi-file packages are not supported.
- **No generics**: Go 1.18+ parametric types (`[T any]`) are not implemented.
- **No reflection**: `reflect` package is not available.
- **No cgo**: C interop is not supported.
- **Composite literals as call arguments**: `f([]byte{1,2,3})` is not parsed correctly. Assign to a variable first:
  ```go
  data := []byte{1, 2, 3}
  f(data)
  ```
- **`os.MkdirAll`**: simplified — creates only one directory level (no recursive creation).
- **`errors.As`**: stub that always returns `false`.
- **`filepath.Join`**: only 2-argument form is supported; for 3+ paths, chain calls:
  ```go
  p := filepath.Join(filepath.Join(a, b), c)
  ```
- **Import declarations**: accepted by the parser but ignored — all pseudo-packages are always in scope.
- **`recover()`**: returns `nil`; not wired to actual panic unwinding.
- **Goroutine semantics**: goroutines run as Windows threads; the scheduler is not a full Go M:N scheduler.

---

## Writing New Go Programs for golangc

### Minimal program

```go
package main

func main() {
    println("Hello!")
}
```

### Using fmt.Println

```go
package main

import "fmt"

func main() {
    fmt.Println("Hello,", "World!")
    fmt.Printf("The answer is %d\n", 42)
    s := fmt.Sprintf("pi = %.4f", 3.14159)
    fmt.Println(s)
}
```

### Structs and methods

```go
package main

type Rectangle struct {
    Width, Height float64
}

func (r Rectangle) Area() float64 {
    return r.Width * r.Height
}

func main() {
    rect := Rectangle{Width: 10.0, Height: 5.0}
    println(rect.Area())
}
```

### Interfaces and type switches

```go
package main

import "fmt"

type Animal interface {
    Sound() string
}

type Dog struct{}
type Cat struct{}

func (d Dog) Sound() string { return "Woof" }
func (c Cat) Sound() string { return "Meow" }

func describe(a Animal) {
    switch v := a.(type) {
    case Dog:
        _ = v
        fmt.Println("Dog says", a.Sound())
    case Cat:
        _ = v
        fmt.Println("Cat says", a.Sound())
    }
}

func main() {
    describe(Dog{})
    describe(Cat{})
}
```

### Goroutines and channels

```go
package main

func sum(s []int, ch chan int) {
    total := 0
    for _, v := range s {
        total += v
    }
    ch <- total
}

func main() {
    s := []int{7, 2, 8, -9, 4, 0}
    ch := make(chan int)
    go sum(s[:len(s)/2], ch)
    go sum(s[len(s)/2:], ch)
    x, y := <-ch, <-ch
    println(x + y)
}
```

### Error handling

```go
package main

import (
    "errors"
    "fmt"
    "strconv"
)

var ErrNegative = errors.New("negative number")

func sqrt(n float64) (float64, error) {
    if n < 0 {
        return 0, ErrNegative
    }
    return n * n, nil
}

func main() {
    v, err := strconv.Atoi("123")
    if err == nil {
        fmt.Println(v)
    }

    result, err2 := sqrt(-1.0)
    _ = result
    if errors.Is(err2, ErrNegative) {
        fmt.Println("got negative error")
    }
}
```

---

## Troubleshooting

### `ml64` not found

The assembler is not in `PATH`. Open a **VS x64 Native Tools Command Prompt** or run:
```bat
"C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Auxiliary\Build\vcvars64.bat"
```

### `link` not found

Same as above — `link.exe` also requires the MSVC environment.

### Compilation error: `qualified types (X.Y) not yet supported`

The named type `X.Y` used in a type annotation is not registered. Use a supported type or report it as a missing feature.

### Program crashes at startup

Ensure the executable is linked with the CRT (the driver always adds `msvcrt.lib`/`ucrt.lib`). Do **not** use `/ENTRY:main` in custom link commands — it bypasses CRT initialization.

### Unexpected output / wrong values

Enable `--dump-ir` to inspect the intermediate representation and check that values and types are correct:
```bat
golangc myprog.go --dump-ir
```
