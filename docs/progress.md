# Golang Compiler Progress Tracker

## Current Phase: 18 (Pseudo-packages: fmt, strconv, os) - Complete
## Current Milestone: Phase 18 complete - fmt.Println/Printf/Sprintf, strconv.Itoa/Atoi, os.Args, string(int) rune-to-string conversion, 11 new PseudoPkgTest tests — strconv_demo.go sample added (192 codegen tests, 579 total)
## Completion Estimate: 80%

## Component Status
| Component | Status | Tests | Notes |
|-----------|--------|-------|-------|
| Infrastructure | ✅ Complete | - | CMake, vcpkg, project structure |
| Common Utils | ✅ Complete | 30 | source_location, diagnostic, string_interner, arena_allocator, result |
| Lexer | ✅ Complete | 89 | All Go tokens, literals, operators, comments, auto-semicolons |
| AST | ✅ Complete | - | Full node hierarchy (16 expr, 21 stmt, 7 decl, 13 type kinds) |
| Parser | ✅ Complete | 87 | Recursive descent, all Go syntax, 7 sample programs parse; []T in call args fixed |
| Sema | ✅ Complete | 110 | Type system, scopes, name resolution, type checking, interface satisfaction; spread type check fix; unused param false positive fix |
| IR | ✅ Complete | 71 | SSA-style IR, multi-return tuple types, map ops, slice make/append/index-addr, StringEq, make_array_type public |
| CodeGen | ✅ Complete | 192 | x86-64 MASM, structs/methods/interfaces, floats, strings, slices (write+append), goroutines, channels, maps (len/delete/iter), multi-return, closures, defer, switch (int/tagless/string/fallthrough), select (recv/send/default), variadic functions (pack+spread), pointer-receiver methods, iota, method calls on named-type constants, fmt/strconv/os pseudo-packages, rune-to-string |
| Runtime | ✅ Complete | - | println/print/float/string_concat/panic + goroutine_channel + map (FNV-1a, string-aware, iter, delete) + slice_append + closure_env global + string_eq + golangc_select + golangc_itoa/atoi + golangc_sprintf/printf + golangc_rune_to_string + golangc_os_args/init_args/os_args_get |
| Linker | ✅ Complete | - | MASM ml64 → obj → link.exe → PE .exe (via driver -o flag) |

## Detailed Progress Log

### Session 1 - Phase 1: Project Infrastructure
#### Completed
- Created full project directory structure (src/, tests/, docs/, samples/)
- CMake build system with C++23, MSVC flags, CTest, vcpkg integration
- vcpkg.json with fmt and gtest dependencies
- .clang-format configuration
- Common utilities:
  - `source_location.hpp` - SourcePosition, SourceLocation, SourceRange
  - `diagnostic.hpp/cpp` - DiagnosticEngine with formatted messages, severity levels, custom handlers
  - `string_interner.hpp` - String interning with heterogeneous lookup
  - `arena_allocator.hpp` - Arena-based memory allocation for AST nodes
  - `result.hpp` - Result<T,E> error handling type
- Lexer infrastructure:
  - `token.hpp` - Complete TokenKind enum (all Go tokens), Token struct
  - `lexer.hpp/cpp` - Lexer class skeleton with stub methods, keyword lookup
- Parser skeleton:
  - `parser.hpp/cpp` - Parser class with advance/expect/match helpers
- Compiler driver:
  - `main.cpp` - CLI with --help, --version, --dump-tokens options
- Unit tests for all common utilities (25+ tests)
- Stub tests for lexer (7 tests) and parser (1 test)
- Documentation: README.md, architecture.md, grammar.md
- Sample Go programs for milestone testing

### Session 2 - Phase 2: Lexer Implementation
#### Completed
- Full Go lexer implementation (~875 lines in lexer.cpp)
- Character scanning:
  - `peek()`, `peek_next()`, `peek_at()`, `advance()`, `match_char()`
  - Whitespace handling with newline tracking for semicolon insertion
- Identifier and keyword recognition:
  - Unicode-aware letter/digit classification
  - Keyword lookup via `lookup_keyword()` from token.hpp
- Numeric literal parsing:
  - Decimal integers and floats
  - Hexadecimal (0x/0X) with hex float support
  - Octal (0o/0O) and legacy octal (leading 0)
  - Binary (0b/0B)
  - Underscore digit separators (Go 1.13+)
  - Imaginary suffix (i)
  - Exponents (e/E for decimal, p/P for hex)
- String and rune literal parsing:
  - Interpreted strings with full escape sequence support
  - Raw strings (backtick-delimited, no escaping)
  - Rune literals with escape sequences
  - Escape sequences: \a \b \f \n \r \t \v \\ \' \"
  - Octal escapes (\NNN), hex escapes (\xNN)
  - Unicode escapes (\uNNNN, \UNNNNNNNN)
- All Go operators and delimiters (~40 tokens)
- Comment handling:
  - Line comments (//) - act as newlines for semicolon insertion
  - Block comments (/* */) - newlines within trigger semicolon insertion
- Automatic semicolon insertion per Go specification:
  - After identifiers, literals, keywords (break, continue, fallthrough, return)
  - After ++, --, ), ], }
  - Triggered by newlines, line comments, block comments with newlines, and EOF
- Error recovery:
  - Unterminated strings, raw strings, rune literals
  - Unterminated block comments
  - Invalid escape sequences
  - Invalid numeric digit sequences
  - All errors reported via DiagnosticEngine
- 89 comprehensive lexer tests

### Session 3 - Phase 3: Parser & AST Implementation
#### Completed
- **AST Node Hierarchy** (`src/ast/ast.hpp`, `src/ast/ast.cpp` ~470 lines):
  - `List<T>` template for arena-allocated lists with `make_list()` helper
  - Expression nodes (16 kinds): Ident, BasicLit, CompositeLit, FuncLit, Unary, Binary,
    Paren, Selector, TypeAssert, Index, Slice, Call, Star, KeyValue, BadExpr, Ellipsis
  - Statement nodes (21 kinds): Expr, Send, IncDec, Assign, ShortVarDecl, Label, Branch,
    Block, If, For, Range, Switch, Select, Return, Go, Defer, Decl, Empty, BadStmt,
    CaseClause (via CaseClause*), CommClause (via CommClause*)
  - Declaration nodes (7 kinds): Bad, Import, Const, Type, Var, Func
  - Type expression nodes (13 kinds): Ident, Qualified, Array, Slice, Map, Chan, Struct,
    Interface, Func, Pointer, Paren, Ellipsis, Bad
  - Supporting types: Field, FieldList, PackageDecl, ImportSpec, ValueSpec, TypeSpec,
    CaseClause, CommClause, ChanDir enum, File (top-level)
  - `location()` methods for all node categories

- **Recursive Descent Parser** (`src/parser/parser.hpp`, `src/parser/parser.cpp` ~2400 lines):
  - Token navigation: advance, peek, at, at_end, expect, expect_semicolon, match, consume
  - Error handling: error, error_at, error_expected, sync_to_decl, sync_to_stmt, skip_to
  - File structure: parse_file, parse_package_clause
  - Declarations: parse_import_decl/spec, parse_top_level_decl, parse_const/type/var_decl,
    parse_func_or_method_decl, parse_const/type/var_spec
  - Type expressions: parse_type, parse_type_or_nil, parse_type_name,
    parse_array_or_slice_type, parse_map_type, parse_chan_type, parse_struct_type,
    parse_interface_type, parse_func_type, parse_pointer_type
  - Function signatures: parse_signature, parse_parameters (with two-pass name merging),
    parse_results, parse_param_decl, parse_field_list, parse_struct_field
  - Statements: parse_stmt, parse_simple_stmt (with no_semi flag), parse_block,
    parse_if_stmt, parse_for_stmt, parse_switch_stmt, parse_select_stmt,
    parse_return_stmt, parse_go_stmt, parse_defer_stmt, parse_branch_stmt,
    parse_case_clause, parse_comm_clause, parse_stmt_list
  - Expressions: parse_expr, parse_binary_expr (Pratt precedence, 5 levels),
    parse_unary_expr, parse_primary_expr, parse_operand, parse_composite_lit,
    parse_func_lit, parse_selector_or_type_assert, parse_index_or_slice,
    parse_call_expr, parse_expr_list, parse_ident_list, parse_element,
    parse_literal_value

  - Key design decisions:
    - `allow_composite_lit_` flag to resolve `if x {` vs `x{...}` ambiguity
    - `no_semi` parameter on parse_simple_stmt for if/for/switch conditions
    - Two-pass parameter parsing to handle `(a, b int)` vs `(int, int)` ambiguity
    - Special handling of type keywords in call expressions for `make(chan int)`
    - Arena allocation for all AST nodes (zero manual memory management)
    - Operator precedence via Pratt parsing: `|| → && → == != < > <= >= → + - | ^ → * / % << >> & &^`

- **AST Pretty-Printer** (`src/ast/ast_printer.hpp`, `src/ast/ast_printer.cpp` ~400 lines):
  - Indented tree output for all AST node types
  - Handles all expression, statement, declaration, and type node kinds
  - Used via `--dump-ast` compiler flag

- **Build system updates**:
  - New `golangc_ast` library (ast.cpp, ast_printer.cpp)
  - `golangc_parser` links `golangc_lexer` and `golangc_ast`
  - Test target links `golangc_parser`, `golangc_ast`, GTest
  - Driver updated with `--dump-ast` option, version bumped to 0.2.0

- **87 comprehensive parser tests** covering:
  - Package declarations (4): main, fmt, missing, missing name
  - Import declarations (4): single, grouped, named, dot
  - Function declarations (4): simple, params, multiple return, methods
  - Variable declarations (5): with type, with init, both, grouped, multi
  - Constant declarations (3): simple, grouped, typed
  - Type declarations (5): simple alias, struct, multi-field struct, interface, empty interface
  - Type expressions (8): slice, array, map, pointer, chan, send-only chan, recv-only chan, func
  - Expressions (15): binary precedence, left assoc, unary, paren, call, selector, index,
    slice, 3-index slice, composite lit, keyed composite lit, star, address-of, logical, comparison
  - Statements (22): return (3), if/else/else-if/init (4), for (4 variants), switch (2),
    go, defer, inc/dec, assign, compound assign, short var decl (2), send, branch (3),
    fallthrough, label, select
  - Full programs (5): hello world, fibonacci, structs+methods, interfaces, multi-decl
  - Error recovery (2): missing func body, bad expression
  - AST printer (2): basic output, expression tree

- **All 5 sample Go programs parse successfully**:
  - hello.go, fibonacci.go, structs.go, interfaces.go, goroutines.go

#### Current State
- Parser is feature-complete for Go 1.21+ core syntax
- All 206 total tests pass (30 common + 89 lexer + 87 parser)
- AST pretty-printer works via --dump-ast flag
- All sample Go programs parse and produce AST output

### Session 4 - Phase 4: Semantic Analysis Implementation
#### Completed
- **Type System** (`src/sema/types.hpp`, `src/sema/types.cpp` ~300 lines):
  - TypeKind enum: Invalid, Basic, Array, Slice, Map, Chan, Pointer, Func, Struct, Interface, Named, Tuple
  - BasicKind enum: 17 typed basics (bool, int..uint64, uintptr, float32/64, complex64/128, string) + 7 untyped constants + Invalid
  - BasicInfo table with size, numeric/integer/unsigned/float/complex/string/boolean/untyped flags
  - Complex type structs: FuncType, StructType, InterfaceType, NamedType, TupleType
  - Type as discriminated union with inline storage for small types, pointers for complex types
  - `type_string()` - human-readable formatting for all types
  - `identical()` - structural comparison (pointer equality for Named types)
  - `underlying()` - follows Named type chains
  - `default_type()` - converts untyped constants to default types (UntypedInt→int, etc.)
  - Type predicates: is_untyped, is_numeric, is_integer, is_boolean, is_string, is_ordered, is_comparable

- **Symbol Table & Scopes** (`src/sema/scope.hpp`, `src/sema/scope.cpp`):
  - SymbolKind: Bad, Const, Var, Type, Func, Label, Package, Builtin, Nil
  - Symbol struct with kind, name, type, location, decl_node, const_val, used flag, builtin_id
  - ScopeKind: Universe, Package, File, Function, Block
  - Scope class with lookup_local (this scope), lookup (parent chain), insert (with duplicate detection)
  - Scope hierarchy: Universe → Package → Function → Block → Block...

- **Universe Scope** (`src/sema/universe.hpp`, `src/sema/universe.cpp`):
  - 19 basic types + byte/rune aliases
  - Built-in constants: true, false (UntypedBool), nil
  - 12 built-in functions: println, print, len, cap, make, new, append, copy, delete, close, panic, recover
  - Error interface type: `interface{ Error() string }`
  - Singleton basic type cache via `basic_type()`

- **Constant Evaluation** (`src/sema/constant.hpp`, `src/sema/constant.cpp`):
  - ConstValue using std::variant<monostate, bool, int64_t, double, string>
  - Arithmetic: add, sub, mul, div, mod, neg (with int/float promotion)
  - Comparison: eq, neq, lt, le, gt, ge (bool/int/float/string)
  - Boolean: not
  - String concatenation
  - Type conversion helpers: to_int, to_float, to_bool_val
  - Division-by-zero safety

- **Checker** (`src/sema/checker.hpp`, `src/sema/checker.cpp` + 4 sub-files ~1800 lines total):
  - Two-pass top-level declaration handling (collect names → check bodies)
  - Type resolution (`checker_type.cpp`): AST TypeExpr → sema::Type for all type kinds
  - Declaration checking (`checker_decl.cpp`): func, method, var, const, type declarations
  - Expression checking (`checker_expr.cpp`): all 16 expression kinds
    - Identifier resolution with used-flag marking
    - Basic literal typing (untyped constants)
    - Composite literals with struct field matching
    - Function/method calls with argument type checking
    - Binary/unary operators with untyped promotion
    - Selector expressions (struct fields + methods + interface methods + pointer auto-deref)
    - Index expressions (array, slice, map, string)
    - Slice expressions, type assertions, star (deref), address-of
    - Built-in function special cases (println, len, cap, make, new, append, etc.)
    - Constant folding for arithmetic, comparison, string concatenation
  - Statement checking (`checker_stmt.cpp`): all 21 statement kinds
    - Block scoping with unused variable detection
    - Assignment type compatibility checking
    - Short var decl `:=` with at-least-one-new-var rule
    - Return value count and type checking (including naked returns)
    - If/for/switch condition boolean checking
    - Range loop with key/value type inference (array, slice, map, chan, string)
    - Send statement channel type validation
    - Go/defer must be call expression
    - IncDec numeric type checking
    - Break/continue loop context validation

- **Assignability** following Go spec:
  - Identical types; identical underlying where one unnamed
  - Untyped constant compatibility (numeric → numeric, bool → bool, string → string)
  - nil assignable to pointer, func, slice, map, chan, interface
  - Interface satisfaction

- **Interface Satisfaction**:
  - Method set matching (name + signature identity)
  - Value receivers available on both T and *T
  - Pointer receivers only on *T

- **Parser Improvements** (required for sema):
  - Fixed `for i, c := range x` parsing (range clause in short var decl)
  - Fixed `make(chan int)` — type keywords as call arguments stored as CompositeLit wrappers

- **Driver Updates**:
  - `--check` flag runs semantic analysis after parsing
  - Version bumped to 0.3.0
  - golangc_sema library linked into driver

- **110 comprehensive semantic analysis tests** covering:
  - Types (34): basic/untyped type strings, pointer/slice/array/map/chan/func/struct/named type strings,
    identity (structural, pointer-based for named), underlying, default_type, predicates
  - Scopes (14): create, insert/lookup, duplicate detection, parent chain, shadowing, nesting,
    universe types/constants/nil/builtins/error, symbol used flag
  - Constants (9): int/float arithmetic, string concat, negation, bool not, comparison,
    division by zero, toString, type conversions
  - Integration (53): all 5 sample programs, name resolution (undefined var/func, forward refs),
    type errors (wrong return type, arg count, return count, missing return, arith on string,
    invalid deref), short var decl (basic, multiple, no-new error), unused vars, blank ident,
    var decl (typed, inferred), control flow (if, for, switch), struct operations (field access,
    unknown field, lit with field names, unknown field name), method calls, channel send/recv,
    type declarations, assignments (compatible, multiple), inc/dec, go/defer, empty func,
    multiple returns, non-bool condition, send to non-channel, index non-indexable, redeclaration,
    const decl, string concat, inc/dec non-numeric, call non-function, builtin len, address-of/deref,
    interface satisfaction, for-range, blank assignment

- **All 5 sample Go programs pass `--check`**:
  - hello.go, fibonacci.go, structs.go, interfaces.go, goroutines.go

#### Current State
- Semantic analysis is feature-complete for single-file Go programs
- All 316 total tests pass (30 common + 89 lexer + 87 parser + 110 sema)
- Type checking, name resolution, and Go-specific validations all working
- Error diagnostics include source location and descriptive messages

### Session 5 - Phase 5: Intermediate Representation
#### Completed
- **IR Core Data Structures** (`src/ir/ir.hpp`, `src/ir/ir.cpp`):
  - IRTypeKind: Void, I1, I8, I16, I32, I64, F32, F64, Ptr, Struct, Array, Func
  - ~60 Opcode values: constants, arithmetic (int+float), bitwise, comparison, memory
    (alloca/load/store/getptr), conversions, aggregates, control flow (br/condbr/ret/switch/phi),
    calls, Go-specific (GoSpawn, DeferCall, ChanMake/Send/Recv, SliceMake/Len/Cap/Index,
    MapMake/Get/Set, StringLen/Index/Concat, InterfaceMake/Data/Type, Println, Panic, Recover)
  - Value base class (id, type, name), Instruction (extends Value with opcode, operands, targets),
    BasicBlock (label, instructions, predecessors/successors), Function (name, params, blocks,
    return type), Module (package name, functions, globals), GlobalVariable

- **IR Type Mapping** (`src/ir/ir_type_map.hpp`, `src/ir/ir_type_map.cpp`):
  - Singleton cached primitives: void, i1, i8, i16, i32, i64, f32, f64, ptr
  - Composite layouts: string→{ptr,i64}, slice→{ptr,i64,i64}, interface→{ptr,ptr}
  - Caching via unordered_map to avoid duplicate types
  - Maps all Go types: bool→I1, int/int64→I64, float32→F32, string→struct, pointer→ptr,
    chan→ptr, map→ptr, named→underlying, struct→struct of mapped fields

- **IR Builder** (`src/ir/ir_builder.hpp`, `src/ir/ir_builder.cpp`):
  - Fluent API for constructing IR instructions
  - Factory methods for every opcode category
  - CFG edge maintenance on Br/CondBr
  - Value ID management

- **IR Generator** (`src/ir/ir_gen.hpp`, `src/ir/ir_gen.cpp`, `ir_gen_decl.cpp`,
  `ir_gen_expr.cpp`, `ir_gen_stmt.cpp`):
  - Alloca-based SSA construction (no phi nodes needed during initial construction)
  - Two-pass function generation (register all functions, then generate bodies)
  - Method naming: "TypeName.MethodName", receiver as first parameter
  - Expression codegen: ident (load from alloca), basic_lit, binary (int/float dispatch),
    unary, call (with builtin detection fallback), selector (struct field GEP),
    index, composite_lit, star, paren, type_assert, func_lit
  - Short-circuit evaluation for && and ||
  - Builtin calls: println, len, cap, make, new, append, panic, recover
  - Statement codegen: block, assign (with compound +=/-= etc.), short_var_decl, return,
    if/else, for (with loop context for break/continue), range (simplified iterator),
    switch (chain of condbr), inc_dec, send, go, defer, branch
  - Declaration codegen: func_decl, global_var, local_var_spec
  - Uses `Checker::decl_symbol()` for IdentExpr* → Symbol* resolution (VarSpec names,
    function params, receiver params — these are standalone IdentExpr*, not Expr* unions)

- **IR Printer** (`src/ir/ir_printer.hpp`, `src/ir/ir_printer.cpp`):
  - Human-readable text output (LLVM IR style)
  - Module header, function signatures with params and return types
  - Block labels, instruction opcodes with type annotations

- **Checker Enhancement**:
  - Added `decl_sym_map_` (IdentExpr* → Symbol*) for declaration names not in expr_map
  - `decl_symbol()` public method for IR generator access
  - Records: function names, param names, receiver names, var spec names, const names, type names

- **Build system & Driver**:
  - `golangc_ir` library (8 source files), linked to sema/ast/common
  - `test_ir` executable with test_ir.cpp and test_ir_gen.cpp
  - `--dump-ir` flag, version bumped to 0.4.0

- **71 IR tests** (34 unit + 37 integration):
  - Unit tests: IR types, opcodes, basic blocks, functions, modules, type mapping, builder, printer
  - Integration: EmptyMain, HelloWorld, IntConstant, IntArithmetic, Comparison, IfStatement,
    IfElse, ForLoop, InfiniteFor, Fibonacci, MultipleParams, MultipleVarDecl, Assignment,
    IncDec, StringLiteral, BoolExpressions, StructLiteral, StructMethod, InterfaceCall,
    GoroutineAndChannel, all 5 sample programs, SwitchStatement, BreakContinue, VarDeclaration,
    DeferStatement, ReturnVoid, CompoundAssignment, MultipleFunctions, ModuleHasCorrectName,
    FunctionHasEntryBlock, AllBlocksTerminated, UnaryMinus, LenArray

- **All 5 sample programs generate IR via `--dump-ir`**:
  - hello.go: println with string constant
  - fibonacci.go: params, recursion, if/else, arithmetic
  - structs.go: struct params, field access, method calls
  - interfaces.go: interface params, method dispatch
  - goroutines.go: channel make/send/recv, goroutine spawn

#### Current State
- IR generation is feature-complete for all core Go constructs
- All 387 total tests pass (30 common + 89 lexer + 87 parser + 110 sema + 71 IR)
- SSA-style IR with alloca-based locals, ready for mem2reg optimization
- Error-free IR generation for all 5 sample programs

### Session 6 - Phase 6: x86-64 Code Generation
#### Completed
- **Code Generator** (`src/codegen/x64_codegen.hpp`, `x64_codegen.cpp`, `x64_codegen_inst.cpp`):
  - X64Reg enum (RAX-R15, XMM0-XMM15), RegSize (Byte/Word/DWord/QWord), reg_name() helper
  - FrameLayout class: maps IR allocas → [RBP-offset] stack slots, 16-byte aligned frames
  - Two-pass frame allocation: prescan all value-producing instructions before emitting prologue
  - MASM assembly output: _TEXT SEGMENT, PROC/ENDP, _DATA SEGMENT, EXTERN declarations
  - Function prologue/epilogue: push rbp, mov rbp rsp, sub rsp frame_size / add rsp, pop rbp, ret
  - Windows x64 ABI: params in RCX/RDX/R8/R9, shadow space, RAX return
  - Instruction selection for ~30 IR opcodes:
    - Constants: ConstInt, ConstBool, ConstString (data section with DB bytes)
    - Memory: Alloca (no-op, frame pre-allocated), Load, Store (scalar + string struct)
    - Arithmetic: Add, Sub, Mul (via r10 scratch register), Div/Rem (cqo + idiv), Neg
    - Bitwise: And, Or, Xor, Shl, Shr, AndNot, BitNot
    - Comparison: Eq/Ne/Lt/Le/Gt/Ge → CMP + SETcc + MOVZX
    - Logical: LogNot (test + sete)
    - Control flow: Br (jmp), CondBr (test + jne/jmp), Ret (load RAX + epilogue)
    - Calls: marshal args to registers, shadow space alloc, call, save RAX result
    - Println: dispatch to runtime based on arg type (int/string/bool)
    - Conversions: SExt, ZExt, Trunc (masking)
    - GetPtr: struct field access via LEA with offset
  - String handling: {ptr, i64} layout with separate data/length slots
  - Label sanitization: dots → $ for MASM compatibility

- **Runtime Library** (`src/runtime/runtime.hpp`, `src/runtime/runtime.cpp`):
  - `golangc_println_int(int64_t)` — printf %lld\n
  - `golangc_println_string(const char*, int64_t)` — printf %.*s\n
  - `golangc_println_bool(int64_t)` — "true"/"false"\n
  - `golangc_panic(const char*)` — fprintf + exit(2)
  - Built as static library golangc_runtime.lib

- **Driver Updates** (`src/driver/main.cpp`):
  - `--emit-asm` flag: generate MASM assembly to stdout
  - `-o output.exe` flag: full pipeline IR → asm → ml64 → link → .exe
  - Auto-discover golangc_runtime.lib relative to executable
  - Version bumped to 0.5.0

- **Build System**:
  - `golangc_codegen` library (x64_codegen.cpp, x64_codegen_inst.cpp), linked to ir/common
  - `golangc_runtime` static library (runtime.cpp)
  - `test_codegen` executable with 44 tests

- **44 codegen tests** covering:
  - Register names (4): qword, dword, byte, xmm
  - Frame layout (3): basic allocation, alignment, reset
  - Helpers (3): masm_name, type_size, is_string_type
  - Module structure (3): empty main, module header, text segment
  - Constants (2): int, bool
  - Arithmetic (6): add, sub, mul, div, rem, neg
  - Comparisons (2): equality, less-than
  - Control flow (2): if statement, for loop
  - Function calls (3): basic call, recursive call, multiple params
  - Println (3): int, string, expression
  - End-to-end assembly (2): hello world, fibonacci — full MASM structure verification
  - Bitwise (4): and, or, xor, shift
  - Logical not (1)
  - Return (2): int, void
  - Multiple functions (1)
  - Variables (1): local variable assignment
  - Complex programs (2): GCD, conditional

- **End-to-end executables**:
  - `golangc --emit-asm samples/hello.go` → valid MASM, assembles with ml64
  - hello.exe: prints "Hello, World!" and exits 0
  - `golangc --emit-asm samples/fibonacci.go` → valid MASM, assembles with ml64
  - fib.exe: prints "55" and exits 0

#### Current State
- Code generation is functional for integer programs with functions, control flow, recursion
- All 431 total tests pass (30 common + 89 lexer + 87 parser + 110 sema + 71 IR + 44 codegen)
- Two milestone programs compile and run correctly as native Windows x64 executables
- Stack-based code gen (no register allocation); future optimization opportunity

#### Next Steps
- **Phase 7**: Structs, Methods & Interfaces Codegen

### Session 7 - Phase 7: Structs, Methods & Interfaces Codegen
#### Completed
- **GetPtr + Load/Store Indirection** (`x64_codegen_inst.cpp`):
  - `emit_load`: detects GetPtr source, loads address from temp slot, dereferences through pointer
  - `emit_store`: detects GetPtr destination, stores value through computed pointer
  - Enables struct field read/write via `getptr` → `load`/`store` pattern

- **Multi-QWORD Struct Load/Store** (`x64_codegen_inst.cpp`, `x64_codegen.cpp`):
  - Generalized `emit_load`/`emit_store` to copy N QWORDs for struct-typed values
  - `type_qwords()` helper computes number of 8-byte slots for any IR type
  - `prescan_temps` allocates contiguous struct-sized temp slots with aliased extra QWORDs
  - `src_qword_off` lambda handles both temp slot aliases and contiguous layout fallback
  - String {ptr, i64} now handled as generic 2-QWORD struct (no special case)

- **Struct Passing in Calls + Returns** (Windows x64 ABI):
  - Large structs (>8 bytes) passed by pointer: caller passes `LEA` of value's stack slot
  - sret (struct return): caller allocates temp, passes address as implicit first arg (RCX)
  - Callee prologue: saves sret pointer to hidden slot, shifts param registers by 1
  - Callee return: copies struct value through sret pointer, returns pointer in RAX
  - `is_large_struct()`, `emit_struct_copy()` helpers

- **IR Enhancement** (`src/ir/ir.hpp`, `src/ir/ir_builder.cpp`):
  - Added `alloc_type` field to `Instruction` — tracks type allocated by Alloca
    (separate from `inst->type` which is always `ptr`)
  - `scan_allocas` uses `alloc_type` for correct struct sizing

- **Multi-arg Println with Spaces** (`x64_codegen_inst.cpp`):
  - New runtime: `golangc_print_int`, `golangc_print_string`, `golangc_print_bool`,
    `golangc_print_space`, `golangc_print_newline`
  - Single-arg: uses `golangc_println_*` (value + newline)
  - Multi-arg: uses `golangc_print_*` + `print_space` between + `print_newline` at end
  - No-arg: just `print_newline`

- **Interface Codegen** (`x64_codegen_inst.cpp`, `ir_gen_expr.cpp`):
  - `InterfaceMake`: stores {type_tag, data_value} in 2-QWORD temp slot
  - `InterfaceData`: extracts data pointer (second QWORD) from interface struct
  - IR gen: interface method calls resolved by searching `func_name_map_` for method name
  - IR gen: automatic interface boxing when passing concrete type as interface parameter
  - Static method dispatch (sufficient for single concrete type)

- **Runtime Library** (`src/runtime/runtime.hpp`, `src/runtime/runtime.cpp`):
  - Added 5 new functions: print_int, print_string, print_bool, print_space, print_newline

- **18 new codegen tests** (44 → 62 total):
  - Structs: StructCompositeLiteral, StructFieldAccess, StructFieldWrite, StructMethod,
    StructSretReturn, StructPassByPointer, StructsGoFull
  - Println: PrintlnMultiArg, PrintlnSingleArg, PrintlnNoArgs
  - Interfaces: InterfaceMake, InterfaceMethodCall, InterfacesGoFull
  - GetPtr: GetPtrLoadIndirection, GetPtrStoreIndirection
  - Type helpers: StructTypeSize, SmallStructNotLarge, TypeQwords

- **End-to-end executables**:
  - structs.exe: prints "4 6" (Point{1,2}.Add(Point{3,4})) ✅
  - interfaces.exe: prints "MyInt" (MyInt(42) implementing Stringer) ✅
  - hello.exe: prints "Hello, World!" (regression) ✅
  - fib.exe: prints "55" (regression) ✅

#### Current State
- Struct and interface codegen fully functional
- All 449 total tests pass (30 common + 89 lexer + 87 parser + 110 sema + 71 IR + 62 codegen)
- Four milestone programs compile and run correctly as native Windows x64 executables
- Windows x64 ABI compliance for struct passing/return (sret convention)

#### Next Steps
- **Phase 8**: Float, String, Slice codegen

### Session 8 - Phase 8: Float, String Ops & Slice Basics
#### Completed
- **Float Infrastructure** (`x64_codegen.hpp`, `x64_codegen.cpp`):
  - `FloatLiteral` struct and `float_pool_` for float constant data section
  - `is_float_type()`, `is_slice_type()` static helpers
  - `load_value_to_xmm()` helper — loads F64 from stack slot via `movsd`
  - Float constants emitted as `DQ <hex>h` (raw 64-bit hex via memcpy, avoids MASM precision)
  - `__f64_sign_mask DQ 8000000000000000h` emitted when float negation used
  - EXTERN declarations for `golangc_println_float`, `golangc_print_float`, `golangc_string_concat`

- **Float Arithmetic + Constants** (`x64_codegen_inst.cpp`):
  - `emit_const_float`: register in pool, `movsd xmm0, [__fltN]`, store to temp slot
  - `emit_float_arith`: `addsd`/`subsd`/`mulsd`/`divsd` with xmm0/xmm1 operands
  - `emit_float_neg`: `xorpd xmm0, [__f64_sign_mask]`

- **Float Comparisons + Conversions**:
  - `emit_float_compare`: `ucomisd` + NaN-safe SETcc patterns (FEq, FNe use setnp/setp)
  - `emit_sitofp`: `cvtsi2sd xmm0, rax`
  - `emit_fptosi`: `cvttsd2si rax, xmm0`

- **Float ABI (Windows x64)**:
  - Prologue: float params saved from XMM0-XMM3 at positional slots
  - `emit_ret`: float return via `movsd xmm0`
  - `emit_call`: float args loaded to XMMn, float results saved via `movsd`
  - `emit_println`: float detection dispatches to `golangc_print[ln]_float`

- **String Operations**:
  - `emit_string_len`: loads second QWORD (length field) from string struct
  - `emit_string_index`: loads ptr + index, `movzx rax, BYTE PTR [rcx+rax]`
  - `emit_string_concat`: calls `golangc_string_concat` with sret ({ptr,len} = 16 bytes)

- **Slice Operations**:
  - `emit_slice_len`: loads second QWORD from slice struct {ptr, len, cap}
  - `emit_slice_cap`: loads third QWORD
  - `emit_slice_index`: loads ptr + index*8, dereferences

- **Runtime Library** (`src/runtime/runtime.hpp`, `src/runtime/runtime.cpp`):
  - `golangc_println_float(double)` → `printf("%g\n")`
  - `golangc_print_float(double)` → `printf("%g")`
  - `golangc_string_concat(sret, ptr1, len1, ptr2, len2)` → malloc + memcpy + sret return

- **Bug Fix**: `ir_gen_expr.cpp` type conversion crash
  - `float64(x)` and `int(x)` caused null pointer dereference when `func_info` was null
  - Fixed: use `func_sym->type` (always valid via scope lookup) instead of `func_info->symbol->type`

- **22 new codegen tests** (62 → 84 total):
  - Float: FloatConstant, FloatAdd, FloatSub, FloatMul, FloatDiv, FloatNeg (6)
  - Float compare: FloatCompareLt, FloatCompareEq (2)
  - Float conversion: IntToFloat, FloatToInt (2)
  - Float ABI: PrintlnFloat, FloatFunctionParam, FloatReturn (3)
  - Strings: StringLen, StringIndex, StringConcat (3)
  - Slices: SliceLen (1)
  - End-to-end: FloatsGoFull, StringOpsFull, FloatMultiArgPrintln (3)
  - Helpers: IsFloatType, IsSliceType (2)

- **End-to-end executables**:
  - floats.exe: prints "5.14" and "6.28" (3.14+2.0, 3.14*2.0) ✅
  - hello.exe: prints "Hello, World!" (regression) ✅
  - fib.exe: prints "55" (regression) ✅
  - structs.exe: prints "4 6" (regression) ✅
  - interfaces.exe: prints "MyInt" (regression) ✅

#### Current State
- Float arithmetic, comparisons, conversions, and ABI fully functional
- String len/index/concat codegen working
- Slice len/cap/index codegen working
- All 471 total tests pass (30 common + 89 lexer + 87 parser + 110 sema + 71 IR + 84 codegen)
- Five milestone programs compile and run correctly as native Windows x64 executables

#### Next Steps (superseded by Session 9)
- Phase 9: Goroutines & Channels

---

### Session 9 - Phase 9: Goroutines & Channels (2026-02-22)
#### Completed
- **New file**: `src/runtime/goroutine_channel.cpp`
  - `golangc_chan_make(int64_t elem_size)` — malloc + CRITICAL_SECTION + 2 semaphores (initial count 0)
  - `golangc_chan_send(ch, val_ptr)` — sets data_ptr under lock, signals sender_ready, waits recv_ready
  - `golangc_chan_recv(ch, out_ptr)` — waits sender_ready, memcpy under lock, signals recv_ready
  - `golangc_go_spawn(func_ptr, arg_count, ...)` — heap-allocates GoroutineLaunch, CreateThread fire-and-forget
- **`src/runtime/runtime.hpp`**: Added `golangc_chan` forward decl + 4 extern "C" declarations
- **`src/CMakeLists.txt`**: Added `goroutine_channel.cpp` to `golangc_runtime` sources
- **`src/ir/ir_builder.hpp`**: Changed `create_chan_make` signature from `Value* buf_size` to `int64_t elem_size = 8`; added `create_slice_make`
- **`src/ir/ir_builder.cpp`**: Fixed `create_chan_make` to store `imm_int = elem_size`; added `create_slice_make` emitting `SliceMake` with `{length, capacity}` operands
- **`src/ir/ir_gen_expr.cpp`**: Fixed `make()` builtin for Chan (passes static elem_size); added Slice case
- **`src/codegen/x64_codegen.hpp`**: Added 5 private emit_ declarations
- **`src/codegen/x64_codegen.cpp`**: Added 5 EXTERN lines to `emit_module_header`
- **`src/codegen/x64_codegen_inst.cpp`**: Added 5 dispatch cases + full implementations:
  - `emit_chan_make`: `mov rcx, <elem_size>` → call → store RAX
  - `emit_chan_send`: spill constants if needed, load ch→RCX, lea &val→RDX, call
  - `emit_chan_recv`: get_temp_slot for output, load ch→RCX, lea &out→RDX, call
  - `emit_go_spawn`: load_value_to_reg(callee)→RCX, mov rdx=argc, R8/R9=args, call
  - `emit_slice_make`: malloc(len*8), store ptr/len/cap to 3 alias slots
- **`tests/ir/test_ir.cpp`**: Fixed `create_chan_make(nullptr→8)` call
- **`tests/codegen/test_codegen.cpp`**: Added 13 new tests (84→97 codegen tests)

#### Current State
- Goroutines and unbuffered channels fully functional via Windows CreateThread + semaphores
- goroutines.go compiles to a working .exe printing `42`
- All 5 milestone programs pass regression: hello, fibonacci, structs, interfaces, goroutines
- All 484 total tests pass (30+89+87+110+71+97)

#### Next Steps (superseded by Session 10)
- Phase 10: Maps + Multiple Return Values

---

### Session 10 - Phase 10: Maps + Multiple Return Values (2026-02-23)
#### Completed
- **Runtime (`runtime.cpp`)**: Added `golangc_map` struct (open-addressing FNV-1a hash table, initial capacity 16, 75% load resize), `golangc_map_make(key_size, val_size)`, `golangc_map_get(m, key_ptr, out_ok)`, `golangc_map_set(m, key_ptr, val_ptr)`. String keys (key_size==16) use content-based hashing and comparison via `hash_key`/`keys_equal` helpers.
- **`runtime.hpp`**: Added `golangc_map` forward decl + 3 `extern "C"` declarations
- **`ir_builder.hpp/cpp`**: Updated `create_map_make` to carry `key_size` in `imm_int` and `val_size` in `field_index`; added `make_tuple_type` to `IRTypeMap`
- **`ir_type_map.hpp/cpp`**: Added `make_tuple_type(fields)` and static `type_size(IRType*)` helper
- **`ir_gen_expr.cpp`**: Fixed `make(map[K]V)` to compute key/val sizes from sema types via `IRTypeMap::type_size`
- **`ir_gen_stmt.cpp`**:
  - `gen_assign`: detect map-index LHS (`m[k] = v`) and emit `MapSet`
  - `gen_return`: pack multiple return values into a struct via `InsertValue` sequence
  - `gen_short_var_decl`: unpack multi-return call via `ExtractValue`; handle `v, ok := m[k]`
  - `gen_range`: actually load and bind element value for slices/arrays/strings
- **`ir_gen_decl.cpp`**: Fixed `register_functions` to call `make_tuple_type` for multi-result functions
- **`x64_codegen.hpp/cpp/inst.cpp`**: 3 new emit functions + 3 EXTERN lines + `prescan_temps` allocates ok-slot for `MapGet` (id+300000)
- **`emit_map_make`**: `mov rcx, key_size` / `mov rdx, val_size` → `call golangc_map_make`
- **`emit_map_get`**: lea key addr→RDX, lea ok addr→R8, call, null-check → deref or store 0
- **`emit_map_set`**: spill constants if needed, lea key→RDX, lea val→R8, call
- **New samples**: `samples/maps.go` (string key map → prints 42, 99), `samples/multireturn.go` (divmod → prints 3, 2)
- **12 new codegen tests** (97→109)

#### Current State
- Maps with `int` and `string` keys fully functional
- Multiple return values pack/unpack via sret struct machinery (reuses existing struct ABI)
- For-range now loads actual element values for slices, arrays, strings
- `v, ok := m[k]` syntax supported
- All 7 milestone programs compile and run correctly
- All 496 total tests pass (30+89+87+110+71+109)

#### Next Steps
- ~~Phase 11: Append + slice element write, for-range map~~ → Complete

---

### Session 11 - Phase 11: Slice writes, append, map len/delete, for-range map
#### Completed
- **7 new IR opcodes**: `SliceIndexAddr`, `SliceAppend`, `MapLen`, `MapDelete`, `MapIterMake`, `MapIterNext`, `MapIterFree`
- **`src/ir/ir.hpp`**: Added 7 new opcodes; `ir.cpp` updated `opcode_name` for all 7
- **`src/ir/ir_builder.hpp/cpp`**: Added builder methods for all 7 new opcodes + `create_slice_append` (stores `elem_size` in `imm_int`)
- **`src/ir/ir_gen_expr.cpp`**:
  - `gen_addr` for `Index` on slices → emits `SliceIndexAddr` (enabling `s[i] = v`)
  - `len(map)` → `MapLen` opcode
  - `append(s, elem)` → `SliceAppend` opcode
  - `delete(m, k)` → `MapDelete` opcode (new builtin case)
- **`src/ir/ir_gen_stmt.cpp`**: `gen_range` map branch rewritten — `MapIterMake` → `MapIterNext` loop (passing key/val alloca operands) → `MapIterFree`; key/val variables bound via `var_map_` as before
- **`src/parser/parser.cpp`**: Fixed `parse_call_expr` to treat `[` as start of a type-arg (wraps `[]T`/`[N]T` in CompositeLit carrier, same as `chan`/`map`/`interface`) — enables `make([]int, n)`
- **`src/runtime/runtime.hpp`**: Added `golangc_map_len`, `golangc_map_delete`, `golangc_map_iter` struct forward-decl, `golangc_map_iter_make/next/free`, `golangc_slice_append`
- **`src/runtime/runtime.cpp`**: Implemented all 6 new runtime functions:
  - `golangc_map_len`: returns `m->count`
  - `golangc_map_delete`: linear probe find + backshift to maintain probe invariant
  - `golangc_map_iter_make/next/free`: malloc-based iterator advancing through bucket array
  - `golangc_slice_append`: doubles capacity on overflow, copies element at `ptr[len]`, increments `len`
- **`src/codegen/x64_codegen.hpp`**: 10 new private emit_ declarations
- **`src/codegen/x64_codegen.cpp`**: 9 new EXTERN lines; `prescan_temps` marks `MapDelete`/`MapIterFree` as non-value-producing; `is_getptr` extended to include `SliceIndexAddr`
- **`src/codegen/x64_codegen_inst.cpp`**: 11 new dispatch cases + 10 emit function implementations
  - `emit_slice_index_addr`: `mov rcx,[slice_ptr]` → `imul rax,8` → `add rax,rcx` → store address
  - `emit_slice_append`: copies slice triple to result slots, spills elem, calls `golangc_slice_append(&result, &elem, elem_size)` in-place
  - `emit_map_len`, `emit_map_delete`, `emit_map_iter_make/next/free`: straightforward runtime call wrappers
- **New sample**: `samples/wordfreq.go` — exercises map len, delete, for-range, slice append, slice index; prints `2 1 1 3 10 20 30`
- **11 new codegen tests** (109→120)

#### Current State
- Slices: make, len, cap, index (read + write), append all working
- Maps: make, get, set, len, delete, for-range all working
- For-range: slices, arrays, strings, maps all working
- All 5 milestone programs compile to working .exes
- All 507 total tests pass (30+89+87+110+71+120)

#### Next Steps
- **Phase 12**: Closures (func literals capturing variables), defer with arguments
- **Future**: Direct PE generation, buffered channels, GC

### Session 12 - Phase 12: Closures + defer
#### Completed
- **Root cause fix — `check_func_lit`** (`src/sema/checker_expr.cpp`): Populated `decl_sym_map_` for func-literal parameters, mirroring `check_func_decl`. This made func-literal params resolve correctly in IR gen (previously returned 0).
- **3 new IR opcodes** (`src/ir/ir.hpp`): `ClosureMake`, `ClosureEnv`, `Malloc`
- **`Function::has_env` flag**: Marks inner functions that receive a hidden env pointer as last parameter
- **`src/ir/ir_builder.hpp/cpp`**: `create_closure_make`, `create_closure_env`, `create_malloc`; updated `create_chan_make` to store `elem_size` in `imm_int`; added `create_slice_make`
- **`src/ir/ir_gen.hpp`**: Added `func_lit_counter_`, `collect_captures`, `collect_captures_expr` helpers; `#include <unordered_set>`
- **`src/ir/ir_gen_expr.cpp`** — major additions:
  - `collect_captures_expr`: AST walk of expressions finding outer-scope variable references (uses `binary.left/right`, `unary.x`, `paren.x`, `call.*`, `selector.x`, `index.*`, `composite_lit.elts`); skips nested `FuncLit`
  - `collect_captures`: AST walk of statements (`block`, `return_`, `expr`, `assign`, `short_var_decl`, `if_`, `for_`, `inc_dec`)
  - `gen_func_lit` rewritten: detect captures, malloc env struct in outer function, store captured values; inner function receives hidden `.env` param and loads captures via `[env+i*8]`; returns `ClosureMake(func_ptr, env_ptr)` when captures present, else `ClosureMake(func_ptr, nil)`
  - `gen_call`: appends `ClosureEnv` as extra arg for indirect calls (function-valued variables)
- **Global env variable approach** (`src/runtime/runtime.hpp/cpp`): `golangc_closure_env` global stores env ptr set by `ClosureMake`; `ClosureEnv` reads it back; works for sequential single-threaded programs
- **`src/codegen/x64_codegen.hpp/cpp/inst.cpp`**: `emit_closure_make`, `emit_closure_env`, `emit_malloc`; `EXTERN golangc_closure_env:QWORD`; `closure_env_slots_` map (cleared per function); dispatch cases for 3 new opcodes
- **Driver search path fix** (`src/driver/main.cpp`): Added `../../../lib/Release/golangc_runtime.lib` to runtime lib search paths (supports Release build layout `bin/Release/golangc.exe → lib/Release/golangc_runtime.lib`)
- **10 new codegen tests** (120→130): `DeferEmitsDeferBlock`, `DeferRunsBeforeReturn`, `DeferMultipleRunsLIFO`, `DeferCompilesNoTodos`, `FuncLitEmitsInnerProc`, `FuncLitIndirectCall`, `FuncLitAsArgument`, `ClosureCapture`, `ClosureEnvGlobal`, `ClosuresGoCompilesNoTodos`

#### Current State
- `defer.go` → `working\ndeferred 2\ndeferred 1\ndone` ✅
- `closures.go` → `10\n7\n15` ✅
- All 9 samples compile to working .exes: hello, fibonacci, structs, interfaces, maps, wordfreq, goroutines, defer, closures
- All 517 total tests pass (30+89+87+110+71+130)

#### Next Steps
- **Phase 13**: `fmt.Println` / `os.Exit` via import system; or switch/select statements; or string formatting
- **Future**: Direct PE generation, buffered channels, GC, self-hosting

### Session 13 - Phase 13: Switch Statements
#### Completed
- **`StringEq` IR opcode** (`src/ir/ir.hpp`, `src/ir/ir.cpp`): New opcode `StringEq` for runtime string content comparison; `opcode_name` returns `"string_eq"`
- **`create_string_eq`** (`src/ir/ir_builder.hpp/cpp`): Builder method producing an `i1` result, operands `{lhs, rhs}`
- **Fix string == / !=** (`src/ir/ir_gen_expr.cpp`): In `gen_binary`, `Equal`/`NotEqual` cases now check `is_string` and route through `create_string_eq`/`create_lognot` instead of raw pointer `create_eq`/`create_ne`
- **Fix `gen_switch` for string tag** (`src/ir/ir_gen_stmt.cpp`): Detect `tag_is_string` once before the loop; generate `create_string_eq` comparisons instead of `create_eq` for string-typed switch tags
- **Implement `fallthrough`** (`src/ir/ir_gen.hpp`, `src/ir/ir_gen_stmt.cpp`): Added `fallthrough_map_` member; `gen_switch` populates `case[i].block → case[i+1].block` entries; `gen_branch` handles `KW_fallthrough` by looking up the current block and branching to the next case
- **`golangc_string_eq` runtime function** (`src/runtime/runtime.hpp/cpp`): `memcmp`-based implementation; returns 1 if equal, 0 otherwise; handles null/zero-length edge cases
- **`emit_string_eq` codegen** (`src/codegen/x64_codegen.hpp/cpp/inst.cpp`): Loads ptr+len of both strings into RCX/RDX/R8/R9, calls `golangc_string_eq`, stores result; `EXTERN golangc_string_eq:PROC` added to module header
- **`samples/switch.go`**: New sample with `classify` (int switch, multi-value case) and `grade` (tagless switch) functions; expected output: `zero/small/ten/other/A/B/F`
- **12 new codegen tests** (130→142): `SwitchBasicInt`, `SwitchDefaultOnly`, `SwitchNoDefault`, `SwitchMultipleValues`, `SwitchTagless`, `SwitchWithInit`, `SwitchBreak`, `SwitchFallthrough`, `SwitchStringEqEmitsCall`, `SwitchStringEqExtern`, `SwitchCompilesNoTodos`, `SwitchGoFull`

#### Current State
- All 9 existing samples still compile; `switch.go` added as 10th sample
- All 529 total tests pass (30+89+87+110+71+142)
- String equality (`==`/`!=`) now works correctly via `golangc_string_eq`
- `fallthrough` generates correct branch to next case block
- String switch tag uses content comparison not pointer comparison

#### Next Steps
- **Phase 14**: `select` statement for channel multiplexing; or `fmt` package import system; or type switches
- **Future**: Direct PE generation, buffered channels, GC, self-hosting

### Session 14 - Phase 14: Select Statement
#### Completed
- **`golangc_select` runtime** (`src/runtime/goroutine_channel.cpp`, `runtime.hpp`): `SelectCase` struct `{ch*, val*, op}` (3 QWORDs); `golangc_select(cases, num_cases, has_default)` polls recv/send cases non-blockingly; returns fired case index (0..N-1) or N for default; 1ms Sleep backoff when blocking with no default; approximated non-blocking send (5ms timeout)
- **`make_array_type` public** (`src/ir/ir_type_map.hpp`): Moved from private section to public so IR gen can allocate typed flat arrays for the SelectCase stack buffer; removed duplicate private declaration
- **`gen_select`** (`src/ir/ir_gen_stmt.cpp`, `src/ir/ir_gen.hpp`): Full implementation classifying CommClauses (default/send/recv-assign/bare-recv), allocating flat `i64[N*3]` array on stack, filling `{ch_ptr, val_ptr, op}` via GetPtr+Store for each channel case, registering `golangc_select` as synthetic IR Function* in `func_name_map_`, calling it, building CondBr dispatch chain, emitting case bodies with recv-sym binding; break → merge via `loop_stack_`
- **`StmtKind::Select` dispatch** in `gen_stmt()` switch case
- **`EXTERN golangc_select:PROC`** added to `emit_module_header` (`x64_codegen.cpp`)
- **`samples/select.go`**: New sample: two-way recv select (goroutine sends on ch1), default branch, direct recv — 3 outputs
- **10 new codegen tests** (142→152): `SelectCompilesNoErrors`, `SelectCallsSelectRuntime`, `SelectExternDeclared`, `SelectDefaultOnly`, `SelectSingleRecvCase`, `SelectRecvAndDefault`, `SelectNoDefaultHasDefaultZero`, `SelectCaseBodyExecutes`, `SelectDefaultFires`, `SelectGoFull`

#### Current State
- All 10 samples compile (hello, fibonacci, structs, interfaces, maps, wordfreq, goroutines, defer, closures, switch, select)
- All 539 total tests pass (30+89+87+110+71+152)
- `select` with recv cases and `default` works end-to-end
- Known limitation: non-blocking send in `select` uses best-effort 5ms poll; multi-goroutine send-select patterns are approximate

#### Next Steps
- **Phase 15**: Variadic functions (`...T` params, `args...` spread) — unblocks many real Go patterns
- **Phase 16**: Type switches (`switch x.(type)`)
- **Future**: `fmt` package import system, buffered channels, GC, self-hosting

### Session 15 - Phase 15: Variadic Functions
#### Completed
- **`gen_call` variadic expansion** (`src/ir/ir_gen_expr.cpp`): Detects variadic callees via `callee_sema_func->is_variadic`; for normal calls packs trailing arguments into a `[]T` slice via `SliceMake`+`SliceAppend` chain; for spread calls (`f(s...)`) passes the last argument (already a slice) directly without wrapping; fixed param count computed as `params.size() - 1` for variadic functions
- **Sema fix — spread type check** (`src/sema/checker_expr.cpp`): In `check_call`, added `is_spread_arg` guard: when `expr.has_ellipsis && i == expr.args.count - 1`, skip unwrapping variadic param `[]T` → `T`; prevents false type-mismatch error on `f(s...)`
- **Sema fix — unused params** (`src/sema/checker_decl.cpp`): In `check_func_decl` and `check_method`, mark every parameter symbol `psym->used = true` immediately after declaration; Go's "declared and not used" rule applies only to local variables, not function parameters
- **`samples/variadic.go`**: New sample: `sum(...int) int` (for-range body), `max(first int, rest ...int) int` (fixed+variadic), calls with 0/many args and spread (`nums...`)
- **9 new codegen tests** (152→161): `VariadicDeclCompilesNoErrors`, `VariadicCallPacksSlice`, `VariadicCallZeroArgs`, `VariadicSpread`, `VariadicWithFixedParam`, `VariadicFixedPlusZeroVariadic`, `VariadicRangeOverParam`, `VariadicCompilesNoTodos`, `VariadicGoFull`

#### Current State
- All 11 samples compile (hello, fibonacci, structs, interfaces, maps, wordfreq, goroutines, defer, closures, switch, select, variadic)
- All 548 total tests pass (30+89+87+110+71+161)
- Variadic functions work end-to-end: pack, spread, fixed+variadic, zero-arg all correct

#### Next Steps
- **Phase 17**: Named types / type aliases, method sets on named types
- **Future**: `fmt` package import system, buffered channels, GC, self-hosting

---

### Session 16 — Phase 16: Type Switches

#### Completed
- **`src/parser/parser.cpp`**: `parse_switch_stmt` refactored to detect `x.(type)` and `v := x.(type)` type-switch guards (both bare and with init statement). Redirects to `TypeSwitchStmt` AST node. Also fixed a bug where `switch init; tag {` misparsed the tag as a composite literal (restored `allow_composite_lit_` only *after* parsing the tag).
- **`src/sema/checker.hpp`**: Added `check_type_switch` declaration.
- **`src/sema/checker_stmt.cpp`**: Replaced TODO stub with full `check_type_switch` implementation — checks interface expression, resolves per-case type names, declares bound variable (for `v := x.(type)` form) with the correct type in each case scope.
- **`src/sema/checker.cpp`**: Fixed `assignable_to` to accept untyped constants (`42`, `"hello"`, `true`) as valid arguments to `interface{}` parameters (previously returned false, blocking all type-switch test programs).
- **`src/ir/ir_gen.hpp`**: Added `type_id_map_`, `next_type_id_` (lazy int IDs starting at 2), `type_id_for`, `gen_type_switch` declarations.
- **`src/ir/ir_gen_stmt.cpp`**: Added `TypeSwitchStmt` dispatch, `type_id_for` implementation, `gen_type_switch` implementation — extracts type tag via `InterfaceType`, emits comparison chain against per-type IDs, emits case bodies with bound-variable data extraction.
- **`src/ir/ir_gen_expr.cpp`**: Fixed both interface-boxing sites to use `type_id_for(underlying_type)` instead of hardcoded `1`. Type IDs assigned at boxing time are now consistent with type-switch dispatch.
- **`src/codegen/x64_codegen.hpp`**: Added `emit_interface_type` declaration.
- **`src/codegen/x64_codegen_inst.cpp`**: Added `InterfaceType` opcode dispatch and `emit_interface_type` implementation — reads first QWORD of the interface pair (the type tag) into a result slot.
- **`samples/typeswitch.go`**: New sample — `typeOf(interface{}) string` returning `"int"`, `"string"`, `"bool"`, `"other"`.
- **10 new codegen tests** (161→171): `TypeSwitchCompilesNoErrors`, `TypeSwitchHasTsLabel`, `TypeSwitchIntBranch`, `TypeSwitchStringBranch`, `TypeSwitchBoolBranch`, `TypeSwitchDefaultOnly`, `TypeSwitchNoDefault`, `TypeSwitchBoundVar`, `TypeSwitchCompilesNoTodos`, `TypeSwitchGoFull`

#### Current State
- All 12 samples compile (hello, fibonacci, structs, interfaces, maps, wordfreq, goroutines, defer, closures, switch, select, variadic, typeswitch)
- All 558 total tests pass (30+89+87+110+71+171)
- Type switches work end-to-end: int/string/bool/default dispatch, bound variable, init form all correct

#### Next Steps
- **Phase 17**: Named types / type aliases, method sets on named types
- **Future**: `fmt` package import system, buffered channels, GC, self-hosting

---

### Session 17 — Phase 17: Named Types, Pointer Receivers, Iota

#### Completed

- **`src/sema/checker.hpp`**:
  - Added `needs_addr_for_recv = false` field to `ExprInfo` — signals to IR gen that a pointer-receiver method is being called on a value (so `&recv` must be taken at the call site).
  - Added `current_iota_ = 0` member to `Checker` — tracks the current `iota` value per const group.

- **`src/sema/checker.cpp`** — `lookup_method`:
  - Removed the `if (!m.pointer_receiver || is_pointer)` guard that silently dropped pointer-receiver methods when the receiver was a value type. Now returns the method unconditionally; the `needs_addr_for_recv` flag carries the "needs address-of" signal instead.
  - `eval_const_expr`: added `iota` identifier handling — when the identifier name is `"iota"`, returns `ConstValue(current_iota_)` immediately without scope lookup.

- **`src/sema/checker_decl.cpp`** — `check_const_decl` rewrite:
  - Implements Go's iota semantics: `current_iota_` is reset to 0 at the first spec, incremented after each spec.
  - Implicit spec repetition: specs with no value list inherit `last_type` from the previous explicit type and get `val = ConstValue(iota_val)` directly.
  - Guard against calling `check_expr` on the `iota` identifier (not in scope — would fire "undefined: iota"). Untyped iota assigned `UntypedInt` directly.

- **`src/sema/checker_expr.cpp`** — `check_selector`:
  - After finding a method, checks if it is a pointer-receiver method called on a non-pointer receiver; if so, sets `info.needs_addr_for_recv = true`.
  - Fixed aggregate initializer for `ExprInfo` blank identifier case (MSVC `C2440`).

- **`src/ir/ir_gen_expr.cpp`** — `gen_call`:
  - When `func_sel_info->needs_addr_for_recv` is true, calls `gen_addr(sel.x)` instead of `gen_expr(sel.x)` for the receiver. Falls back to alloca+store if the receiver is not directly addressable.

- **`samples/namedtypes.go`**: New sample — `Counter` struct with pointer-receiver `Inc`/`Value`, `Direction` iota enum with `Name()` value-receiver method.

- **10 new codegen tests** (171→181): `PtrRecvCompilesNoErrors`, `PtrRecvEmitsMethod`, `PtrRecvCallEmitted`, `IotaConstCompilesNoErrors`, `IotaValues`, `IotaUntyped`, `MethodOnConst`, `MultiplePtrRecv`, `CompilesNoTodos`, `GoFull`.

#### Current State
- All samples compile (hello, fibonacci, structs, interfaces, maps, wordfreq, goroutines, defer, closures, switch, select, variadic, typeswitch, namedtypes)
- All 568 total tests pass (30+89+87+110+71+181)
- Pointer-receiver methods work on addressable value receivers (auto-`&recv`)
- `iota` works in typed and untyped const groups, including implicit repetition
- Method calls on named-type constants work (`South.Name()`, `North.Name()`)

#### Next Steps
- **Phase 18**: String formatting, `fmt.Println`, package imports
- **Future**: Buffered channels, GC, self-hosting

---

### Session 18 — Phase 18: Pseudo-packages (fmt, strconv, os) + rune-to-string

#### Completed

- **`src/sema/scope.hpp`**: Added `SymbolKind::PseudoPkg` and `pkg_name` field to `Symbol`.

- **`src/sema/universe.hpp`**: Added new `BuiltinId` values: `FmtPrintln`, `FmtPrintf`, `FmtSprintf`, `StrconvItoa`, `StrconvAtoi`, `OsArgs`.

- **`src/sema/universe.cpp`**: Registered `fmt`, `strconv`, `os` as `PseudoPkg` symbols in the universe scope — always available without an explicit `import`.

- **`src/sema/checker.hpp`**: Added `check_pseudo_pkg_selector` declaration.

- **`src/sema/checker_expr.cpp`**:
  - New `check_pseudo_pkg_selector`: handles `fmt.{Println,Printf,Sprintf}`, `strconv.{Itoa,Atoi}`, `os.Args` — creates arena-stored `Builtin` symbols with correct return types (`string`, `(int,error)` tuple, `[]string`).
  - `check_selector`: early-exit for `PseudoPkg` receiver before the normal field/method path; records the package ident as resolved.
  - `check_call`: after `check_expr(expr.func)`, detects `Builtin` symbol on the result and routes args through permissive type-checking, returning the correct result type.

- **`src/ir/ir_gen.hpp`**: Added `get_or_declare_runtime` helper declaration.

- **`src/ir/ir_gen_expr.cpp`**:
  - `get_or_declare_runtime`: lazily forward-declares external runtime functions in `func_name_map_` so `emit_call` resolves them correctly.
  - `gen_builtin_call`: new cases for `fmt.Println` (reuses `Println` opcode), `fmt.Printf` → `golangc_printf`, `fmt.Sprintf` → `golangc_sprintf`, `strconv.Itoa` → `golangc_itoa`, `strconv.Atoi` → `golangc_atoi` + tuple pack, `os.Args` → `golangc_os_args_get`.
  - `gen_selector`: detects `os.Args` builtin on the selector ExprInfo (non-call value access) and emits `golangc_os_args_get()`.
  - `gen_call` type-conversion: `string(int)` now calls `golangc_rune_to_string` instead of doing a bitcast.

- **`src/runtime/runtime.hpp` / `runtime.cpp`**: Added:
  - `golangc_itoa(sret, int64)` — integer → string via `snprintf`
  - `golangc_atoi(ptr, len, out_ok)` — string → integer via `strtoll`
  - `golangc_sprintf(sret, fmt_ptr, fmt_len, ...)` — custom format loop supporting `%d`, `%s`, `%f`/`%g`
  - `golangc_printf(fmt_ptr, fmt_len, ...)` — same but writes to stdout
  - `golangc_rune_to_string(sret, rune)` — UTF-8 encodes a Unicode code point
  - `golangc_os_args`, `golangc_init_args(argc, argv)`, `golangc_os_args_get(sret)` — os.Args infrastructure

- **`samples/strconv_demo.go`**: New sample demonstrating `strconv.Itoa`, `strconv.Atoi`, `fmt.Sprintf`, `string(int)`, `os.Args`.

- **11 new codegen tests** (181→192): `FmtPrintlnInt`, `FmtPrintlnString`, `FmtPrintlnNoImport`, `StrconvItoa`, `StrconvAtoiCompilesNoErrors`, `FmtSprintf`, `FmtPrintf`, `RuneToString`, `OsArgs`, `FmtPrintlnMultiArg`, `GoFull`.

#### Current State
- All 579 total tests pass (30+89+87+110+71+192)
- `fmt.Println` / `fmt.Printf` / `fmt.Sprintf` work without a real import system
- `strconv.Itoa` / `strconv.Atoi` compile and emit correct runtime calls
- `string(65)` produces a valid UTF-8 string via `golangc_rune_to_string`
- `os.Args` loads the runtime-initialized `[]string` via `golangc_os_args_get`
- Import declarations (`import "fmt"` etc.) are silently accepted (not required)

#### Design Notes
- Pseudo-package approach chosen over a real package system: `fmt`/`strconv`/`os` registered as `PseudoPkg` in the universe scope, members resolved to `Builtin` symbols in `check_pseudo_pkg_selector`. Adds zero parsing complexity, minimal sema complexity.
- `fmt.Sprintf` uses a custom format loop (`fmt_custom` in runtime) that handles Go's `{ptr,len}` string representation — standard `vsnprintf` cannot be used directly since `%s` expects null-terminated pointers.
- `strconv.Atoi` returns a 2-field IR struct `{i64, interface}` matching the `(int, error)` Tuple expected by short-var-decl destructuring.

#### Next Steps
- **Phase 19**: `for range` over strings (UTF-8 rune iteration), string formatting improvements
- **Future**: Buffered channels, GC, self-hosting
