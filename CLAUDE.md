# Autonomous Golang Compiler Development Prompt (C++/MSVC/Windows)

```markdown
# GOLANG COMPILER IMPLEMENTATION PROJECT
## Autonomous Development Task for Claude

You are an expert compiler engineer tasked with implementing a complete Go (Golang) compiler in modern C++ targeting MSVC on Windows. This is a long-running autonomous development project requiring systematic, incremental progress with comprehensive documentation and testing.

---

## PROJECT OVERVIEW

**Objective**: Build a fully functional Go 1.21+ compiler that:
- Compiles Go source code to x86-64 Windows executables (PE format)
- Implements the complete Go language specification
- Uses modern C++20/23 features following C++ Core Guidelines
- Builds with MSVC (Visual Studio 2022) on Windows

**Architecture**: Multi-phase compiler pipeline
```
Source → Lexer → Parser → AST → Semantic Analysis → IR → Optimizer → Code Generator → PE Linker
```

---

## DEVELOPMENT PHASES

### Phase 1: Project Infrastructure
```
/golang-compiler/
├── CMakeLists.txt              # Root CMake configuration
├── vcpkg.json                  # Dependency manifest
├── .clang-format               # Code style
├── README.md
├── docs/
│   ├── architecture.md
│   ├── grammar.md
│   └── progress.md             # Track autonomous progress
├── src/
│   ├── common/                 # Shared utilities
│   │   ├── source_location.hpp
│   │   ├── diagnostic.hpp
│   │   ├── string_interner.hpp
│   │   └── arena_allocator.hpp
│   ├── lexer/
│   ├── parser/
│   ├── ast/
│   ├── sema/                   # Semantic analysis
│   ├── ir/                     # Intermediate representation
│   ├── codegen/                # x86-64 code generation
│   ├── linker/                 # PE format linker
│   └── driver/                 # Compiler driver
├── tests/
│   ├── lexer/
│   ├── parser/
│   ├── sema/
│   ├── codegen/
│   └── integration/            # Full compilation tests
└── samples/                    # Go test programs
```

### Phase 2: Lexer Implementation
Implement complete Go lexical analysis:
- All Go tokens (keywords, operators, literals)
- Unicode identifier support (Go allows Unicode)
- Automatic semicolon insertion (critical Go feature)
- String/rune literal parsing with escape sequences
- Numeric literals (int, float, imaginary, hex, octal, binary)
- Raw string literals (backtick strings)
- Comment handling (line and block)

```cpp
// Token categories to implement
enum class TokenKind : uint16_t {
    // Literals
    Identifier, IntLiteral, FloatLiteral, ImaginaryLiteral,
    RuneLiteral, StringLiteral, RawStringLiteral,
    
    // Keywords (25 total in Go)
    KW_break, KW_case, KW_chan, KW_const, KW_continue,
    KW_default, KW_defer, KW_else, KW_fallthrough, KW_for,
    KW_func, KW_go, KW_goto, KW_if, KW_import,
    KW_interface, KW_map, KW_package, KW_range, KW_return,
    KW_select, KW_struct, KW_switch, KW_type, KW_var,
    
    // Operators and delimiters...
};
```

### Phase 3: Parser & AST
Implement recursive descent parser for Go grammar:
- Package declarations
- Import declarations (single and grouped)
- Top-level declarations (const, type, var, func)
- Statements (all Go statement types)
- Expressions (with proper precedence)
- Type expressions (including generics if targeting Go 1.18+)

```cpp
// Core AST node hierarchy
namespace ast {
    struct Node { SourceRange range; };
    
    // Declarations
    struct PackageDecl;
    struct ImportDecl;
    struct ConstDecl;
    struct TypeDecl;
    struct VarDecl;
    struct FuncDecl;
    struct MethodDecl;
    
    // Types
    struct TypeExpr;
    struct ArrayType;
    struct SliceType;
    struct MapType;
    struct ChanType;
    struct StructType;
    struct InterfaceType;
    struct FuncType;
    struct PointerType;
    
    // Statements
    struct BlockStmt;
    struct IfStmt;
    struct ForStmt;
    struct SwitchStmt;
    struct SelectStmt;
    struct DeferStmt;
    struct GoStmt;
    // ... etc
    
    // Expressions
    struct BinaryExpr;
    struct UnaryExpr;
    struct CallExpr;
    struct IndexExpr;
    struct SliceExpr;
    struct TypeAssertExpr;
    struct SelectorExpr;
    struct CompositeLit;
    // ... etc
}
```

### Phase 4: Semantic Analysis
- Symbol table management (package-level and block scopes)
- Name resolution
- Type checking (Go's structural typing for interfaces)
- Method set computation
- Constant expression evaluation
- Escape analysis (for stack vs heap allocation)
- Interface satisfaction checking

### Phase 5: Intermediate Representation
Design SSA-based IR suitable for Go:
- Basic blocks with phi nodes
- Go-specific constructs (defer, goroutines, channels)
- Closure representation
- Interface method dispatch
- Slice/map/channel operations

### Phase 6: Code Generation (x86-64)
- Instruction selection
- Register allocation
- Stack frame layout (Go uses split stacks or segmented stacks)
- Calling convention (Go uses its own, not Windows x64)
- Runtime calls generation

### Phase 7: Runtime Library
Minimal Go runtime in C++/assembly:
- Goroutine scheduler (M:N threading)
- Memory allocator (Go uses tcmalloc-style)
- Garbage collector (concurrent, tri-color mark-sweep)
- Channel operations
- Interface type assertions
- Panic/recover mechanism
- Map implementation

### Phase 8: PE Linker
- Generate valid Windows PE executables
- Handle relocations
- Import table for Windows APIs
- Debug information (CodeView/PDB)

---

## AUTONOMOUS DEVELOPMENT PROTOCOL

### Work Session Structure
Each work session MUST:

1. **Resume Context**
   - Read `docs/progress.md` for current state
   - Review last completed milestone
   - Identify next immediate task

2. **Implement Incrementally**
   - Work on ONE component at a time
   - Write tests BEFORE or WITH implementation
   - Compile and verify after each significant change

3. **Document Progress**
   - Update `docs/progress.md` with:
     ```markdown
     ## Session [DATE/NUMBER]
     ### Completed
     - [Specific items completed]
     ### Current State
     - [What's working]
     - [Known issues]
     ### Next Steps
     - [Immediate next tasks]
     ```

4. **Checkpoint Commits**
   - Logical commit points with clear messages
   - Never leave code in broken state

### Progress Tracking File Template
```markdown
# Golang Compiler Progress Tracker

## Current Phase: [1-8]
## Current Milestone: [Description]
## Completion Estimate: [X%]

## Component Status
| Component | Status | Tests | Notes |
|-----------|--------|-------|-------|
| Lexer | 🟡 In Progress | 45/60 | Missing imaginary literals |
| Parser | ⬜ Not Started | 0/0 | |
| AST | ⬜ Not Started | 0/0 | |
| Sema | ⬜ Not Started | 0/0 | |
| IR | ⬜ Not Started | 0/0 | |
| CodeGen | ⬜ Not Started | 0/0 | |
| Runtime | ⬜ Not Started | 0/0 | |
| Linker | ⬜ Not Started | 0/0 | |

## Detailed Progress Log
[Session entries here]
```

---

## TECHNICAL REQUIREMENTS

### C++ Standards & Guidelines
```cpp
// Use modern C++ idioms
- std::string_view for non-owning strings
- std::span for array views  
- std::optional for nullable values
- std::variant for tagged unions (AST nodes)
- std::expected (C++23) or Result<T,E> for error handling
- constexpr where possible
- [[nodiscard]] on functions returning values
- Concepts for template constraints
- RAII for all resource management
- No raw new/delete - use smart pointers or arena allocators
```

### CMake Configuration
```cmake
cmake_minimum_required(VERSION 3.25)
project(golang-compiler VERSION 0.1.0 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# MSVC-specific flags
if(MSVC)
    add_compile_options(
        /W4 /WX           # Warnings as errors
        /permissive-      # Strict conformance
        /Zc:__cplusplus   # Correct __cplusplus macro
        /utf-8            # UTF-8 source and execution
        /EHsc             # Exception handling
    )
endif()

# Enable testing
include(CTest)
enable_testing()

# Use vcpkg for dependencies
find_package(fmt CONFIG REQUIRED)
find_package(GTest CONFIG REQUIRED)
```

### Testing Strategy
```cpp
// Unit test every component
// Example: Lexer tests
TEST(LexerTest, TokenizesKeywords) {
    Lexer lexer("func main() {}");
    EXPECT_EQ(lexer.next().kind, TokenKind::KW_func);
    EXPECT_EQ(lexer.next().kind, TokenKind::Identifier);
    // ...
}

TEST(LexerTest, AutomaticSemicolonInsertion) {
    Lexer lexer("return\n42");
    auto tokens = lexer.tokenize_all();
    // Verify semicolon inserted after 'return'
}
```

---

## GO LANGUAGE SPECIFICS TO HANDLE

### Critical Go Features
1. **Automatic Semicolon Insertion** - Lexer must handle
2. **Multiple Return Values** - ABI consideration
3. **Named Return Values** - Semantic analysis
4. **Defer Statements** - Stack unwinding mechanism
5. **Goroutines** - Runtime scheduler
6. **Channels** - Synchronization primitives
7. **Interfaces** - Structural typing, method sets
8. **Slices** - Fat pointers (ptr, len, cap)
9. **Maps** - Hash table implementation
10. **Garbage Collection** - Concurrent collector
11. **Panic/Recover** - Exception-like mechanism
12. **Reflection** - Runtime type information

### Go Calling Convention (not Windows x64!)
```
- Arguments passed in registers: AX, BX, CX, DI, SI, R8-R11
- Return values also in registers
- Stack grows downward
- Frame pointer in BP
- Goroutine pointer in R14 (or TLS)
```

---

## SAMPLE TEST PROGRAMS

### Milestone 1: Hello World
```go
package main

func main() {
    println("Hello, World!")
}
```

### Milestone 2: Functions & Control Flow
```go
package main

func fibonacci(n int) int {
    if n <= 1 {
        return n
    }
    return fibonacci(n-1) + fibonacci(n-2)
}

func main() {
    println(fibonacci(10))
}
```

### Milestone 3: Structs & Methods
```go
package main

type Point struct {
    X, Y int
}

func (p Point) Add(other Point) Point {
    return Point{p.X + other.X, p.Y + other.Y}
}

func main() {
    p1 := Point{1, 2}
    p2 := Point{3, 4}
    p3 := p1.Add(p2)
    println(p3.X, p3.Y)
}
```

### Milestone 4: Interfaces
```go
package main

type Stringer interface {
    String() string
}

type MyInt int

func (m MyInt) String() string {
    return "MyInt"
}

func Print(s Stringer) {
    println(s.String())
}

func main() {
    var x MyInt = 42
    Print(x)
}
```

### Milestone 5: Goroutines & Channels
```go
package main

func worker(ch chan int) {
    ch <- 42
}

func main() {
    ch := make(chan int)
    go worker(ch)
    println(<-ch)
}
```

---

## ERROR HANDLING & DIAGNOSTICS

```cpp
// Rich diagnostic system
class DiagnosticEngine {
public:
    void error(SourceLocation loc, std::string_view msg);
    void warning(SourceLocation loc, std::string_view msg);
    void note(SourceLocation loc, std::string_view msg);
    
    // Formatted diagnostics
    template<typename... Args>
    void error(SourceLocation loc, std::format_string<Args...> fmt, Args&&... args);
};

// Example output:
// main.go:10:5: error: undefined: foo
//     x := foo()
//          ^^^
// main.go:5:1: note: did you mean 'Foo' (exported from package bar)?
```

---

## WHEN STUCK OR UNCERTAIN

1. **Reference Materials**:
   - Go Language Specification: https://go.dev/ref/spec
   - Go Compiler Source: https://github.com/golang/go/tree/master/src/cmd/compile
   - SSA IR Design: https://github.com/golang/go/tree/master/src/cmd/compile/internal/ssa

2. **Simplification Strategy**:
   - Start with subset of Go (no generics, no reflection initially)
   - Use printf-style debugging for runtime
   - Generate C code initially if codegen is blocked

3. **Ask for Guidance**:
   - If blocked on design decisions, document options and trade-offs
   - Mark sections needing review in progress.md

---

## SUCCESS CRITERIA

✅ Phase 1 Complete: Project builds, basic infrastructure working
✅ Phase 2 Complete: All Go tokens lexed correctly, 100% test coverage
✅ Phase 3 Complete: Parser handles all Go syntax, AST pretty-printer works
✅ Phase 4 Complete: Type checking passes Go test suite subset
✅ Phase 5 Complete: IR generation for core constructs
✅ Phase 6 Complete: Simple programs compile to working executables
✅ Phase 7 Complete: Goroutines and channels functional
✅ Phase 8 Complete: Self-hosting capability (compile go compiler in go... eventually)

---

## BEGIN WORK

Start with Phase 1: Create project structure and implement basic infrastructure components. Document all progress in `docs/progress.md`. Work autonomously, making steady incremental progress. When you complete a logical unit of work, summarize what was done and what comes next.
```

---

This prompt provides a comprehensive blueprint for autonomous compiler development. Want me to expand any specific section (like the runtime implementation details or the SSA IR design)?