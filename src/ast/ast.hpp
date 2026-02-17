#pragma once

#include "common/arena_allocator.hpp"
#include "common/source_location.hpp"
#include "lexer/token.hpp"

#include <cstdint>
#include <span>
#include <string_view>

namespace golangc {
namespace ast {

// Forward declarations
struct Expr;
struct Stmt;
struct Decl;
struct TypeExpr;
struct Field;
struct File;

// ============================================================================
// Helper: Arena-allocated list (pointer + count)
// ============================================================================
template <typename T>
struct List {
    T* data    = nullptr;
    uint32_t count = 0;

    [[nodiscard]] std::span<T> span() { return {data, count}; }
    [[nodiscard]] std::span<const T> span() const { return {data, count}; }
    [[nodiscard]] bool empty() const { return count == 0; }
    [[nodiscard]] T& operator[](uint32_t i) { return data[i]; }
    [[nodiscard]] const T& operator[](uint32_t i) const { return data[i]; }
    [[nodiscard]] T* begin() { return data; }
    [[nodiscard]] T* end() { return data + count; }
    [[nodiscard]] const T* begin() const { return data; }
    [[nodiscard]] const T* end() const { return data + count; }
};

// ============================================================================
// Expressions
// ============================================================================

enum class ExprKind : uint8_t {
    Bad,            // Error placeholder
    Ident,          // x, fmt
    BasicLit,       // 42, 3.14, "hello", 'a'
    CompositeLit,   // T{...}
    FuncLit,        // func(x int) int { ... }
    Paren,          // (expr)
    Selector,       // x.y
    Index,          // x[i]
    Slice,          // x[lo:hi] or x[lo:hi:max]
    TypeAssert,     // x.(T)
    Call,           // f(args...)
    Unary,          // op x
    Binary,         // x op y
    KeyValue,       // key: value (in composite literals)
    Ellipsis,       // ... or ...T
    Star,           // *x (dereference, also used as pointer type in some contexts)
};

struct BadExpr {
    SourceLocation loc;
};

struct IdentExpr {
    SourceLocation loc;
    std::string_view name;
};

struct BasicLitExpr {
    SourceLocation loc;
    TokenKind kind; // IntLiteral, FloatLiteral, ImaginaryLiteral, RuneLiteral, StringLiteral
    std::string_view value;
};

struct CompositeLitExpr {
    SourceLocation loc;
    TypeExpr* type;        // May be null for nested literals
    List<Expr*> elts;      // Elements (may be KeyValue expressions)
    SourceLocation lbrace;
    SourceLocation rbrace;
};

struct FuncLitExpr {
    SourceLocation loc;
    TypeExpr* type;        // FuncType
    Stmt* body;            // BlockStmt
};

struct ParenExpr {
    SourceLocation loc;
    Expr* x;
    SourceLocation lparen;
    SourceLocation rparen;
};

struct SelectorExpr {
    SourceLocation loc;
    Expr* x;               // Expression before dot
    IdentExpr* sel;        // Field/method name after dot
};

struct IndexExpr {
    SourceLocation loc;
    Expr* x;               // Expression being indexed
    Expr* index;           // Index expression
    SourceLocation lbrack;
    SourceLocation rbrack;
};

struct SliceExpr {
    SourceLocation loc;
    Expr* x;               // Expression being sliced
    Expr* low;             // May be null
    Expr* high;            // May be null
    Expr* max;             // May be null (3-index slice)
    bool three_index;      // True if x[lo:hi:max]
    SourceLocation lbrack;
    SourceLocation rbrack;
};

struct TypeAssertExpr {
    SourceLocation loc;
    Expr* x;               // Expression
    TypeExpr* type;        // Asserted type (null for type switch x.(type))
    SourceLocation lparen;
    SourceLocation rparen;
};

struct CallExpr {
    SourceLocation loc;
    Expr* func;            // Function expression
    List<Expr*> args;      // Arguments
    bool has_ellipsis;     // True if last arg has ...
    SourceLocation lparen;
    SourceLocation rparen;
};

struct UnaryExpr {
    SourceLocation loc;
    TokenKind op;          // +, -, !, ^, &, <-
    Expr* x;
};

struct BinaryExpr {
    SourceLocation loc;
    TokenKind op;
    Expr* left;
    Expr* right;
    SourceLocation op_loc;
};

struct KeyValueExpr {
    SourceLocation loc;
    Expr* key;
    Expr* value;
    SourceLocation colon;
};

struct EllipsisExpr {
    SourceLocation loc;
    TypeExpr* elt;         // Element type (may be null for ... in call)
};

struct StarExpr {
    SourceLocation loc;
    Expr* x;
};

struct Expr {
    ExprKind kind;
    union {
        BadExpr bad;
        IdentExpr ident;
        BasicLitExpr basic_lit;
        CompositeLitExpr composite_lit;
        FuncLitExpr func_lit;
        ParenExpr paren;
        SelectorExpr selector;
        IndexExpr index;
        SliceExpr slice;
        TypeAssertExpr type_assert;
        CallExpr call;
        UnaryExpr unary;
        BinaryExpr binary;
        KeyValueExpr key_value;
        EllipsisExpr ellipsis;
        StarExpr star;
    };

    Expr() : kind(ExprKind::Bad), bad{} {}

    [[nodiscard]] SourceLocation location() const;
};

// ============================================================================
// Type Expressions
// ============================================================================

enum class TypeExprKind : uint8_t {
    Bad,
    Ident,          // Named type: int, string, MyType
    Qualified,      // Qualified type: pkg.Type
    Array,          // [N]T
    Slice,          // []T
    Map,            // map[K]V
    Chan,           // chan T, chan<- T, <-chan T
    Pointer,        // *T
    Func,           // func(...) (...)
    Struct,         // struct { ... }
    Interface,      // interface { ... }
    Paren,          // (T) - parenthesized type
    Ellipsis,       // ...T (variadic parameter type)
};

enum class ChanDir : uint8_t {
    SendRecv,  // chan T
    SendOnly,  // chan<- T
    RecvOnly,  // <-chan T
};

struct BadTypeExpr {
    SourceLocation loc;
};

struct IdentTypeExpr {
    SourceLocation loc;
    std::string_view name;
};

struct QualifiedTypeExpr {
    SourceLocation loc;
    std::string_view package;
    std::string_view name;
    SourceLocation dot_loc;
};

struct ArrayTypeExpr {
    SourceLocation loc;
    Expr* length;         // Array length expression
    TypeExpr* element;    // Element type
    SourceLocation lbrack;
    SourceLocation rbrack;
};

struct SliceTypeExpr {
    SourceLocation loc;
    TypeExpr* element;
    SourceLocation lbrack;
    SourceLocation rbrack;
};

struct MapTypeExpr {
    SourceLocation loc;
    TypeExpr* key;
    TypeExpr* value;
};

struct ChanTypeExpr {
    SourceLocation loc;
    ChanDir dir;
    TypeExpr* element;
};

struct PointerTypeExpr {
    SourceLocation loc;
    TypeExpr* base;
};

// Field in struct or parameter in function signature
struct Field {
    List<IdentExpr*> names; // Parameter/field names (may be empty for anonymous fields)
    TypeExpr* type;
    Expr* tag;              // Struct field tag (string literal, may be null)
    SourceLocation loc;
};

struct FieldList {
    List<Field*> fields;
    SourceLocation lparen; // or lbrace for struct/interface
    SourceLocation rparen; // or rbrace
};

struct FuncTypeExpr {
    SourceLocation loc;
    FieldList* params;      // Parameters
    FieldList* results;     // Results (may be null for no return)
};

struct StructTypeExpr {
    SourceLocation loc;
    FieldList* fields;
};

struct InterfaceMethod {
    IdentExpr* name;             // Method name (null for embedded type)
    FuncTypeExpr* signature;     // Method signature (null for embedded type)
    TypeExpr* embedded_type;     // Embedded type (null for method)
    SourceLocation loc;
};

struct InterfaceTypeExpr {
    SourceLocation loc;
    List<InterfaceMethod*> methods;
    SourceLocation lbrace;
    SourceLocation rbrace;
};

struct ParenTypeExpr {
    SourceLocation loc;
    TypeExpr* type;
};

struct EllipsisTypeExpr {
    SourceLocation loc;
    TypeExpr* element;
};

struct TypeExpr {
    TypeExprKind kind;
    union {
        BadTypeExpr bad;
        IdentTypeExpr ident;
        QualifiedTypeExpr qualified;
        ArrayTypeExpr array;
        SliceTypeExpr slice;
        MapTypeExpr map;
        ChanTypeExpr chan;
        PointerTypeExpr pointer;
        FuncTypeExpr func;
        StructTypeExpr struct_;
        InterfaceTypeExpr interface_;
        ParenTypeExpr paren;
        EllipsisTypeExpr ellipsis;
    };

    TypeExpr() : kind(TypeExprKind::Bad), bad{} {}

    [[nodiscard]] SourceLocation location() const;
};

// ============================================================================
// Statements
// ============================================================================

enum class StmtKind : uint8_t {
    Bad,
    Block,          // { stmts }
    Expr,           // expression statement
    Send,           // ch <- v
    IncDec,         // x++ or x--
    Assign,         // x = y, x, y = a, b
    ShortVarDecl,   // x := y
    Return,         // return [exprs]
    Branch,         // break, continue, goto, fallthrough
    If,             // if [init;] cond { body } [else elsebody]
    For,            // for [init; cond; post] { body } OR for range ...
    Switch,         // switch [init;] [tag] { cases }
    TypeSwitch,     // switch [init;] x.(type) { cases }
    Select,         // select { cases }
    CaseClause,     // case exprs: stmts  OR  default: stmts
    CommClause,     // case send/recv: stmts  OR  default: stmts
    Go,             // go expr
    Defer,          // defer expr
    Label,          // label: stmt
    Empty,          // ; (empty statement)
    Decl,           // Declaration statement (const, type, var)
    Range,          // for k, v := range x { body }
};

struct BadStmt {
    SourceLocation loc;
};

struct BlockStmt {
    SourceLocation loc;
    List<Stmt*> stmts;
    SourceLocation lbrace;
    SourceLocation rbrace;
};

struct ExprStmt {
    SourceLocation loc;
    Expr* x;
};

struct SendStmt {
    SourceLocation loc;
    Expr* chan;
    Expr* value;
    SourceLocation arrow; // <- location
};

struct IncDecStmt {
    SourceLocation loc;
    Expr* x;
    TokenKind tok;  // Increment or Decrement
};

struct AssignStmt {
    SourceLocation loc;
    List<Expr*> lhs;
    List<Expr*> rhs;
    TokenKind tok;  // Assign, PlusAssign, MinusAssign, etc.
    SourceLocation tok_loc;
};

struct ShortVarDeclStmt {
    SourceLocation loc;
    List<Expr*> lhs;   // Must be identifiers
    List<Expr*> rhs;
    SourceLocation tok_loc; // := location
};

struct ReturnStmt {
    SourceLocation loc;
    List<Expr*> results;
};

struct BranchStmt {
    SourceLocation loc;
    TokenKind tok;      // KW_break, KW_continue, KW_goto, KW_fallthrough
    IdentExpr* label;   // May be null
};

struct IfStmt {
    SourceLocation loc;
    Stmt* init;         // May be null
    Expr* cond;
    Stmt* body;         // BlockStmt
    Stmt* else_body;    // May be null (IfStmt or BlockStmt)
};

struct ForStmt {
    SourceLocation loc;
    Stmt* init;         // May be null
    Expr* cond;         // May be null
    Stmt* post;         // May be null
    Stmt* body;         // BlockStmt
};

struct RangeStmt {
    SourceLocation loc;
    Expr* key;          // May be null
    Expr* value;        // May be null
    Expr* x;            // Range expression
    TokenKind tok;      // Assign or ColonAssign (or Invalid if no key/value)
    SourceLocation tok_loc;
    Stmt* body;         // BlockStmt
};

struct CaseClause {
    SourceLocation loc;
    List<Expr*> values;    // Empty for default
    List<Stmt*> body;
    SourceLocation colon;
};

struct SwitchStmt {
    SourceLocation loc;
    Stmt* init;            // May be null
    Expr* tag;             // May be null
    List<CaseClause*> cases;
    SourceLocation lbrace;
    SourceLocation rbrace;
};

struct TypeSwitchStmt {
    SourceLocation loc;
    Stmt* init;            // May be null
    Stmt* assign;          // x := y.(type) or y.(type)
    List<CaseClause*> cases; // case types use TypeExpr stored as expressions
    SourceLocation lbrace;
    SourceLocation rbrace;
};

struct CommClause {
    SourceLocation loc;
    Stmt* comm;            // Send or receive statement, null for default
    List<Stmt*> body;
    SourceLocation colon;
};

struct SelectStmt {
    SourceLocation loc;
    List<CommClause*> cases;
    SourceLocation lbrace;
    SourceLocation rbrace;
};

struct GoStmt {
    SourceLocation loc;
    Expr* call;
};

struct DeferStmt {
    SourceLocation loc;
    Expr* call;
};

struct LabelStmt {
    SourceLocation loc;
    IdentExpr* label;
    Stmt* stmt;
    SourceLocation colon;
};

struct EmptyStmt {
    SourceLocation loc;
};

struct DeclStmt {
    SourceLocation loc;
    Decl* decl;
};

struct Stmt {
    StmtKind kind;
    union {
        BadStmt bad;
        BlockStmt block;
        ExprStmt expr;
        SendStmt send;
        IncDecStmt inc_dec;
        AssignStmt assign;
        ShortVarDeclStmt short_var_decl;
        ReturnStmt return_;
        BranchStmt branch;
        IfStmt if_;
        ForStmt for_;
        RangeStmt range;
        SwitchStmt switch_;
        TypeSwitchStmt type_switch;
        SelectStmt select;
        CaseClause case_clause;
        CommClause comm_clause;
        GoStmt go;
        DeferStmt defer;
        LabelStmt label;
        EmptyStmt empty;
        DeclStmt decl;
    };

    Stmt() : kind(StmtKind::Bad), bad{} {}

    [[nodiscard]] SourceLocation location() const;
};

// ============================================================================
// Declarations
// ============================================================================

enum class DeclKind : uint8_t {
    Bad,
    Package,
    Import,
    Const,
    Type,
    Var,
    Func,
};

struct BadDecl {
    SourceLocation loc;
};

struct PackageDecl {
    SourceLocation loc;
    IdentExpr* name;
};

struct ImportSpec {
    SourceLocation loc;
    IdentExpr* name;       // Local package name (null for default, "." ident for dot import)
    BasicLitExpr* path;    // Import path string literal
};

struct ImportDecl {
    SourceLocation loc;
    List<ImportSpec*> specs;
    SourceLocation lparen; // For grouped imports
    SourceLocation rparen;
};

struct ValueSpec {
    SourceLocation loc;
    List<IdentExpr*> names;
    TypeExpr* type;        // May be null
    List<Expr*> values;    // May be empty
};

struct ConstDecl {
    SourceLocation loc;
    List<ValueSpec*> specs;
    SourceLocation lparen;
    SourceLocation rparen;
};

struct TypeSpec {
    SourceLocation loc;
    IdentExpr* name;
    TypeExpr* type;
    bool is_alias;         // True for type X = Y
    SourceLocation assign_loc; // Location of = for alias
};

struct TypeDecl {
    SourceLocation loc;
    List<TypeSpec*> specs;
    SourceLocation lparen;
    SourceLocation rparen;
};

struct VarSpec {
    SourceLocation loc;
    List<IdentExpr*> names;
    TypeExpr* type;        // May be null
    List<Expr*> values;    // May be empty
};

struct VarDecl {
    SourceLocation loc;
    List<VarSpec*> specs;
    SourceLocation lparen;
    SourceLocation rparen;
};

struct FuncDecl {
    SourceLocation loc;
    FieldList* recv;       // Receiver (null for functions, non-null for methods)
    IdentExpr* name;       // Function/method name
    FuncTypeExpr* type;    // Function signature
    Stmt* body;            // BlockStmt (null for forward declarations)
};

struct Decl {
    DeclKind kind;
    union {
        BadDecl bad;
        PackageDecl package;
        ImportDecl import_;
        ConstDecl const_;
        TypeDecl type;
        VarDecl var;
        FuncDecl func;
    };

    Decl() : kind(DeclKind::Bad), bad{} {}

    [[nodiscard]] SourceLocation location() const;
};

// ============================================================================
// File (top-level AST node)
// ============================================================================

struct File {
    PackageDecl* package;
    List<Decl*> imports;        // ImportDecl nodes
    List<Decl*> decls;          // Top-level declarations
    SourceLocation loc;
};

// ============================================================================
// AST creation helpers (all arena-allocated)
// ============================================================================

// Create a list from a vector
template <typename T>
List<T> make_list(ArenaAllocator& arena, const std::vector<T>& vec) {
    if (vec.empty()) {
        return List<T>{nullptr, 0};
    }
    auto* data = arena.allocate_array<T>(vec.size());
    for (size_t i = 0; i < vec.size(); ++i) {
        data[i] = vec[i];
    }
    return List<T>{data, static_cast<uint32_t>(vec.size())};
}

} // namespace ast
} // namespace golangc
