# Architecture

## Compiler Pipeline

```
Source (.go) → Lexer → Parser → AST → Sema → IR → CodeGen → PE Linker → Executable (.exe)
```

### 1. Lexer (`src/lexer/`)
Converts Go source text into a stream of tokens. Handles:
- All Go tokens (keywords, operators, literals)
- Unicode identifiers
- Automatic semicolon insertion
- String/rune literal escape sequences
- All numeric literal formats

### 2. Parser (`src/parser/`)
Recursive descent parser that converts tokens into an AST. Handles the complete Go grammar including packages, imports, declarations, statements, and expressions.

### 3. AST (`src/ast/`)
Abstract syntax tree node definitions. Uses arena allocation for efficient memory management. Nodes are immutable after construction.

### 4. Semantic Analysis (`src/sema/`)
Type checking and name resolution:
- Symbol table with lexical scoping
- Go structural interface typing
- Method set computation
- Constant expression evaluation
- Escape analysis

### 5. Intermediate Representation (`src/ir/`)
SSA-based IR for optimization and code generation. Supports Go-specific constructs (defer, goroutines, channels, closures).

### 6. Code Generation (`src/codegen/`)
x86-64 instruction selection, register allocation, and stack frame layout. Uses Go's calling convention (not Windows x64 ABI).

### 7. PE Linker (`src/linker/`)
Generates Windows PE executables with proper relocations, import tables, and debug information.

### 8. Runtime (`src/runtime/`)
Minimal Go runtime: goroutine scheduler, garbage collector, channel operations, map implementation, and panic/recover.

## Common Infrastructure (`src/common/`)

- **source_location.hpp**: Source positions and ranges for diagnostics
- **diagnostic.hpp**: Compiler error/warning/note reporting with formatted messages
- **string_interner.hpp**: String interning for efficient identifier storage
- **arena_allocator.hpp**: Arena-based memory allocation for AST nodes
- **result.hpp**: Result<T,E> type for error handling

## Dependencies

- **fmt**: Formatting library (used by diagnostics)
- **Google Test**: Unit testing framework
