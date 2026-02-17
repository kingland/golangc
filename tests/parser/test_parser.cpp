#include "ast/ast.hpp"
#include "ast/ast_printer.hpp"
#include "common/diagnostic.hpp"
#include "lexer/lexer.hpp"
#include "parser/parser.hpp"

#include <gtest/gtest.h>

#include <string>
#include <string_view>

using namespace golangc;
using namespace golangc::ast;

// ============================================================================
// Test helpers
// ============================================================================

struct ParseResult {
    DiagnosticEngine diag;
    std::unique_ptr<Lexer> lexer;
    std::unique_ptr<Parser> parser;
    bool success;

    [[nodiscard]] File* file() const { return parser->file(); }
    [[nodiscard]] bool has_errors() const { return diag.has_errors(); }
    [[nodiscard]] uint32_t error_count() const { return diag.error_count(); }
};

// Helper: parse source and return result
ParseResult parse_source(std::string_view source) {
    ParseResult result;
    // We must keep the source string alive - use a static string approach
    // Actually, the Lexer takes a string_view, so we need the source to be alive
    // during the Lexer and Parser lifetime. We'll store it in a member.
    result.lexer = std::make_unique<Lexer>(source, "test.go", result.diag);
    result.parser = std::make_unique<Parser>(*result.lexer, result.diag);
    result.success = result.parser->parse();
    return result;
}

// Helper: parse and expect success
ParseResult parse_ok(std::string_view source) {
    auto result = parse_source(source);
    EXPECT_TRUE(result.success) << "Expected parsing to succeed";
    EXPECT_FALSE(result.has_errors()) << "Expected no errors, got " << result.error_count();
    return result;
}

// Helper: parse and expect failure
ParseResult parse_fail(std::string_view source) {
    auto result = parse_source(source);
    EXPECT_TRUE(result.has_errors()) << "Expected parsing to fail";
    return result;
}

// Helper: get AST dump for comparison
std::string dump_ast(const File* file) {
    AstPrinter printer;
    return printer.print(file);
}

// ============================================================================
// Package declarations
// ============================================================================

TEST(ParserTest, PackageMain) {
    auto r = parse_ok("package main\n");
    ASSERT_NE(r.file(), nullptr);
    ASSERT_NE(r.file()->package, nullptr);
    EXPECT_EQ(r.file()->package->name->name, "main");
}

TEST(ParserTest, PackageFmt) {
    auto r = parse_ok("package fmt\n");
    EXPECT_EQ(r.file()->package->name->name, "fmt");
}

TEST(ParserTest, PackageMissing) {
    auto r = parse_fail("");
}

TEST(ParserTest, PackageMissingName) {
    auto r = parse_fail("package\n");
}

// ============================================================================
// Import declarations
// ============================================================================

TEST(ParserTest, SingleImport) {
    auto r = parse_ok("package main\nimport \"fmt\"\n");
    ASSERT_EQ(r.file()->imports.count, 1u);
    auto* imp = r.file()->imports[0];
    ASSERT_EQ(imp->kind, DeclKind::Import);
    ASSERT_EQ(imp->import_.specs.count, 1u);
    EXPECT_EQ(imp->import_.specs[0]->path->value, "\"fmt\"");
    EXPECT_EQ(imp->import_.specs[0]->name, nullptr);
}

TEST(ParserTest, GroupedImports) {
    auto r = parse_ok(R"(
package main

import (
    "fmt"
    "os"
)
)");
    ASSERT_EQ(r.file()->imports.count, 1u);
    auto* imp = r.file()->imports[0];
    ASSERT_EQ(imp->import_.specs.count, 2u);
    EXPECT_EQ(imp->import_.specs[0]->path->value, "\"fmt\"");
    EXPECT_EQ(imp->import_.specs[1]->path->value, "\"os\"");
}

TEST(ParserTest, NamedImport) {
    auto r = parse_ok("package main\nimport f \"fmt\"\n");
    ASSERT_EQ(r.file()->imports.count, 1u);
    auto* spec = r.file()->imports[0]->import_.specs[0];
    ASSERT_NE(spec->name, nullptr);
    EXPECT_EQ(spec->name->name, "f");
    EXPECT_EQ(spec->path->value, "\"fmt\"");
}

TEST(ParserTest, DotImport) {
    auto r = parse_ok("package main\nimport . \"fmt\"\n");
    auto* spec = r.file()->imports[0]->import_.specs[0];
    ASSERT_NE(spec->name, nullptr);
    EXPECT_EQ(spec->name->name, ".");
}

// ============================================================================
// Function declarations
// ============================================================================

TEST(ParserTest, SimpleFuncDecl) {
    auto r = parse_ok("package main\nfunc main() {}\n");
    ASSERT_EQ(r.file()->decls.count, 1u);
    auto* decl = r.file()->decls[0];
    ASSERT_EQ(decl->kind, DeclKind::Func);
    EXPECT_EQ(decl->func.name->name, "main");
    EXPECT_EQ(decl->func.recv, nullptr);
    ASSERT_NE(decl->func.body, nullptr);
}

TEST(ParserTest, FuncWithParams) {
    auto r = parse_ok("package main\nfunc add(a int, b int) int { return a + b }\n");
    auto* decl = r.file()->decls[0];
    ASSERT_EQ(decl->kind, DeclKind::Func);
    EXPECT_EQ(decl->func.name->name, "add");

    // Check params
    ASSERT_NE(decl->func.type, nullptr);
    ASSERT_NE(decl->func.type->params, nullptr);
    EXPECT_EQ(decl->func.type->params->fields.count, 2u);

    // Check return type
    ASSERT_NE(decl->func.type->results, nullptr);
    EXPECT_EQ(decl->func.type->results->fields.count, 1u);
}

TEST(ParserTest, FuncMultipleReturn) {
    auto r = parse_ok("package main\nfunc swap(a, b int) (int, int) { return b, a }\n");
    auto* decl = r.file()->decls[0];
    ASSERT_NE(decl->func.type->results, nullptr);
    EXPECT_EQ(decl->func.type->results->fields.count, 2u);
}

TEST(ParserTest, MethodDecl) {
    auto r = parse_ok(R"(
package main
type Foo struct {}
func (f Foo) Bar() {}
)");
    // Second decl should be the method
    ASSERT_GE(r.file()->decls.count, 2u);
    auto* method = r.file()->decls[1];
    ASSERT_EQ(method->kind, DeclKind::Func);
    EXPECT_EQ(method->func.name->name, "Bar");
    ASSERT_NE(method->func.recv, nullptr);
    EXPECT_EQ(method->func.recv->fields.count, 1u);
}

TEST(ParserTest, PointerReceiver) {
    auto r = parse_ok(R"(
package main
type Foo struct {}
func (f *Foo) Bar() {}
)");
    auto* method = r.file()->decls[1];
    ASSERT_NE(method->func.recv, nullptr);
    auto* recv_field = method->func.recv->fields[0];
    ASSERT_NE(recv_field->type, nullptr);
    EXPECT_EQ(recv_field->type->kind, TypeExprKind::Pointer);
}

// ============================================================================
// Variable declarations
// ============================================================================

TEST(ParserTest, VarDeclWithType) {
    auto r = parse_ok("package main\nvar x int\n");
    ASSERT_EQ(r.file()->decls.count, 1u);
    auto* decl = r.file()->decls[0];
    ASSERT_EQ(decl->kind, DeclKind::Var);
    ASSERT_EQ(decl->var.specs.count, 1u);
    EXPECT_EQ(decl->var.specs[0]->names[0]->name, "x");
    ASSERT_NE(decl->var.specs[0]->type, nullptr);
    EXPECT_EQ(decl->var.specs[0]->type->kind, TypeExprKind::Ident);
}

TEST(ParserTest, VarDeclWithInit) {
    auto r = parse_ok("package main\nvar x = 42\n");
    auto* spec = r.file()->decls[0]->var.specs[0];
    EXPECT_EQ(spec->type, nullptr);
    EXPECT_EQ(spec->values.count, 1u);
}

TEST(ParserTest, VarDeclTypeAndInit) {
    auto r = parse_ok("package main\nvar x int = 42\n");
    auto* spec = r.file()->decls[0]->var.specs[0];
    ASSERT_NE(spec->type, nullptr);
    EXPECT_EQ(spec->values.count, 1u);
}

TEST(ParserTest, GroupedVarDecl) {
    auto r = parse_ok(R"(
package main
var (
    x int
    y = 42
    z string = "hello"
)
)");
    auto* decl = r.file()->decls[0];
    ASSERT_EQ(decl->kind, DeclKind::Var);
    EXPECT_EQ(decl->var.specs.count, 3u);
}

TEST(ParserTest, MultiVarDecl) {
    auto r = parse_ok("package main\nvar a, b int\n");
    auto* spec = r.file()->decls[0]->var.specs[0];
    EXPECT_EQ(spec->names.count, 2u);
    EXPECT_EQ(spec->names[0]->name, "a");
    EXPECT_EQ(spec->names[1]->name, "b");
}

// ============================================================================
// Const declarations
// ============================================================================

TEST(ParserTest, ConstDecl) {
    auto r = parse_ok("package main\nconst x = 42\n");
    ASSERT_EQ(r.file()->decls.count, 1u);
    auto* decl = r.file()->decls[0];
    ASSERT_EQ(decl->kind, DeclKind::Const);
    ASSERT_EQ(decl->const_.specs.count, 1u);
    EXPECT_EQ(decl->const_.specs[0]->names[0]->name, "x");
}

TEST(ParserTest, GroupedConstDecl) {
    auto r = parse_ok(R"(
package main
const (
    a = 1
    b = 2
    c = 3
)
)");
    EXPECT_EQ(r.file()->decls[0]->const_.specs.count, 3u);
}

TEST(ParserTest, TypedConstDecl) {
    auto r = parse_ok("package main\nconst x int = 42\n");
    auto* spec = r.file()->decls[0]->const_.specs[0];
    ASSERT_NE(spec->type, nullptr);
}

// ============================================================================
// Type declarations
// ============================================================================

TEST(ParserTest, TypeDeclSimple) {
    auto r = parse_ok("package main\ntype MyInt int\n");
    ASSERT_EQ(r.file()->decls.count, 1u);
    auto* decl = r.file()->decls[0];
    ASSERT_EQ(decl->kind, DeclKind::Type);
    ASSERT_EQ(decl->type.specs.count, 1u);
    EXPECT_EQ(decl->type.specs[0]->name->name, "MyInt");
    EXPECT_FALSE(decl->type.specs[0]->is_alias);
}

TEST(ParserTest, TypeAlias) {
    auto r = parse_ok("package main\ntype MyInt = int\n");
    auto* spec = r.file()->decls[0]->type.specs[0];
    EXPECT_TRUE(spec->is_alias);
}

TEST(ParserTest, StructType) {
    auto r = parse_ok(R"(
package main
type Point struct {
    X int
    Y int
}
)");
    auto* spec = r.file()->decls[0]->type.specs[0];
    ASSERT_NE(spec->type, nullptr);
    EXPECT_EQ(spec->type->kind, TypeExprKind::Struct);
    EXPECT_EQ(spec->type->struct_.fields->fields.count, 2u);
}

TEST(ParserTest, StructMultiFieldSameLine) {
    auto r = parse_ok(R"(
package main
type Point struct {
    X, Y int
}
)");
    auto* spec = r.file()->decls[0]->type.specs[0];
    ASSERT_EQ(spec->type->kind, TypeExprKind::Struct);
    EXPECT_EQ(spec->type->struct_.fields->fields.count, 1u);
    EXPECT_EQ(spec->type->struct_.fields->fields[0]->names.count, 2u);
}

TEST(ParserTest, InterfaceType) {
    auto r = parse_ok(R"(
package main
type Stringer interface {
    String() string
}
)");
    auto* spec = r.file()->decls[0]->type.specs[0];
    ASSERT_EQ(spec->type->kind, TypeExprKind::Interface);
    EXPECT_EQ(spec->type->interface_.methods.count, 1u);
    EXPECT_EQ(spec->type->interface_.methods[0]->name->name, "String");
}

TEST(ParserTest, EmptyInterface) {
    auto r = parse_ok(R"(
package main
type Any interface {}
)");
    auto* spec = r.file()->decls[0]->type.specs[0];
    ASSERT_EQ(spec->type->kind, TypeExprKind::Interface);
    EXPECT_EQ(spec->type->interface_.methods.count, 0u);
}

// ============================================================================
// Type expressions
// ============================================================================

TEST(ParserTest, SliceType) {
    auto r = parse_ok("package main\nvar x []int\n");
    auto* spec = r.file()->decls[0]->var.specs[0];
    ASSERT_NE(spec->type, nullptr);
    EXPECT_EQ(spec->type->kind, TypeExprKind::Slice);
}

TEST(ParserTest, ArrayType) {
    auto r = parse_ok("package main\nvar x [10]int\n");
    auto* spec = r.file()->decls[0]->var.specs[0];
    ASSERT_NE(spec->type, nullptr);
    EXPECT_EQ(spec->type->kind, TypeExprKind::Array);
}

TEST(ParserTest, MapType) {
    auto r = parse_ok("package main\nvar x map[string]int\n");
    auto* spec = r.file()->decls[0]->var.specs[0];
    ASSERT_NE(spec->type, nullptr);
    EXPECT_EQ(spec->type->kind, TypeExprKind::Map);
}

TEST(ParserTest, PointerType) {
    auto r = parse_ok("package main\nvar x *int\n");
    auto* spec = r.file()->decls[0]->var.specs[0];
    ASSERT_NE(spec->type, nullptr);
    EXPECT_EQ(spec->type->kind, TypeExprKind::Pointer);
}

TEST(ParserTest, ChanType) {
    auto r = parse_ok("package main\nvar x chan int\n");
    auto* spec = r.file()->decls[0]->var.specs[0];
    ASSERT_NE(spec->type, nullptr);
    EXPECT_EQ(spec->type->kind, TypeExprKind::Chan);
    EXPECT_EQ(spec->type->chan.dir, ChanDir::SendRecv);
}

TEST(ParserTest, SendOnlyChan) {
    auto r = parse_ok("package main\nvar x chan<- int\n");
    auto* spec = r.file()->decls[0]->var.specs[0];
    EXPECT_EQ(spec->type->chan.dir, ChanDir::SendOnly);
}

TEST(ParserTest, RecvOnlyChan) {
    auto r = parse_ok("package main\nvar x <-chan int\n");
    auto* spec = r.file()->decls[0]->var.specs[0];
    EXPECT_EQ(spec->type->chan.dir, ChanDir::RecvOnly);
}

TEST(ParserTest, FuncType) {
    auto r = parse_ok("package main\nvar x func(int) string\n");
    auto* spec = r.file()->decls[0]->var.specs[0];
    ASSERT_NE(spec->type, nullptr);
    EXPECT_EQ(spec->type->kind, TypeExprKind::Func);
}

// ============================================================================
// Expressions
// ============================================================================

TEST(ParserTest, BinaryExprPrecedence) {
    // 1 + 2 * 3 should parse as 1 + (2 * 3)
    auto r = parse_ok("package main\nvar x = 1 + 2 * 3\n");
    auto* val = r.file()->decls[0]->var.specs[0]->values[0];
    ASSERT_EQ(val->kind, ExprKind::Binary);
    EXPECT_EQ(val->binary.op, TokenKind::Plus);
    // Right side should be 2 * 3
    ASSERT_EQ(val->binary.right->kind, ExprKind::Binary);
    EXPECT_EQ(val->binary.right->binary.op, TokenKind::Star);
}

TEST(ParserTest, BinaryExprLeftAssoc) {
    // 1 - 2 - 3 should parse as (1 - 2) - 3
    auto r = parse_ok("package main\nvar x = 1 - 2 - 3\n");
    auto* val = r.file()->decls[0]->var.specs[0]->values[0];
    ASSERT_EQ(val->kind, ExprKind::Binary);
    EXPECT_EQ(val->binary.op, TokenKind::Minus);
    // Left side should be 1 - 2
    ASSERT_EQ(val->binary.left->kind, ExprKind::Binary);
    EXPECT_EQ(val->binary.left->binary.op, TokenKind::Minus);
}

TEST(ParserTest, UnaryExpr) {
    auto r = parse_ok("package main\nvar x = -42\n");
    auto* val = r.file()->decls[0]->var.specs[0]->values[0];
    ASSERT_EQ(val->kind, ExprKind::Unary);
    EXPECT_EQ(val->unary.op, TokenKind::Minus);
}

TEST(ParserTest, ParenExpr) {
    auto r = parse_ok("package main\nvar x = (1 + 2) * 3\n");
    auto* val = r.file()->decls[0]->var.specs[0]->values[0];
    ASSERT_EQ(val->kind, ExprKind::Binary);
    EXPECT_EQ(val->binary.op, TokenKind::Star);
    ASSERT_EQ(val->binary.left->kind, ExprKind::Paren);
}

TEST(ParserTest, CallExpr) {
    auto r = parse_ok(R"(
package main
func main() {
    println("hello")
}
)");
    auto* body = r.file()->decls[0]->func.body;
    ASSERT_EQ(body->kind, StmtKind::Block);
    ASSERT_GE(body->block.stmts.count, 1u);
    auto* stmt = body->block.stmts[0];
    ASSERT_EQ(stmt->kind, StmtKind::Expr);
    ASSERT_EQ(stmt->expr.x->kind, ExprKind::Call);
    EXPECT_EQ(stmt->expr.x->call.args.count, 1u);
}

TEST(ParserTest, SelectorExpr) {
    auto r = parse_ok(R"(
package main
func main() {
    x.y()
}
)");
    auto* stmt = r.file()->decls[0]->func.body->block.stmts[0];
    ASSERT_EQ(stmt->kind, StmtKind::Expr);
    auto* call = stmt->expr.x;
    ASSERT_EQ(call->kind, ExprKind::Call);
    ASSERT_EQ(call->call.func->kind, ExprKind::Selector);
    EXPECT_EQ(call->call.func->selector.sel->name, "y");
}

TEST(ParserTest, IndexExpr) {
    auto r = parse_ok("package main\nfunc main() { x[0] }\n");
    auto* stmt = r.file()->decls[0]->func.body->block.stmts[0];
    ASSERT_EQ(stmt->kind, StmtKind::Expr);
    ASSERT_EQ(stmt->expr.x->kind, ExprKind::Index);
}

TEST(ParserTest, SliceExpr) {
    auto r = parse_ok("package main\nfunc main() { x[1:3] }\n");
    auto* stmt = r.file()->decls[0]->func.body->block.stmts[0];
    ASSERT_EQ(stmt->kind, StmtKind::Expr);
    ASSERT_EQ(stmt->expr.x->kind, ExprKind::Slice);
    EXPECT_NE(stmt->expr.x->slice.low, nullptr);
    EXPECT_NE(stmt->expr.x->slice.high, nullptr);
    EXPECT_FALSE(stmt->expr.x->slice.three_index);
}

TEST(ParserTest, ThreeIndexSlice) {
    auto r = parse_ok("package main\nfunc main() { x[1:3:5] }\n");
    auto* e = r.file()->decls[0]->func.body->block.stmts[0]->expr.x;
    ASSERT_EQ(e->kind, ExprKind::Slice);
    EXPECT_TRUE(e->slice.three_index);
    EXPECT_NE(e->slice.max, nullptr);
}

TEST(ParserTest, CompositeLiteral) {
    auto r = parse_ok(R"(
package main
func main() {
    p := Point{1, 2}
}
)");
    auto* stmt = r.file()->decls[0]->func.body->block.stmts[0];
    ASSERT_EQ(stmt->kind, StmtKind::ShortVarDecl);
    auto* rhs = stmt->short_var_decl.rhs[0];
    ASSERT_EQ(rhs->kind, ExprKind::CompositeLit);
    EXPECT_EQ(rhs->composite_lit.elts.count, 2u);
}

TEST(ParserTest, CompositeLitWithKeys) {
    auto r = parse_ok(R"(
package main
func main() {
    p := Point{X: 1, Y: 2}
}
)");
    auto* rhs = r.file()->decls[0]->func.body->block.stmts[0]->short_var_decl.rhs[0];
    ASSERT_EQ(rhs->kind, ExprKind::CompositeLit);
    ASSERT_EQ(rhs->composite_lit.elts.count, 2u);
    EXPECT_EQ(rhs->composite_lit.elts[0]->kind, ExprKind::KeyValue);
}

TEST(ParserTest, StarExpr) {
    auto r = parse_ok("package main\nfunc main() { *x = 5 }\n");
    auto* stmt = r.file()->decls[0]->func.body->block.stmts[0];
    ASSERT_EQ(stmt->kind, StmtKind::Assign);
    ASSERT_EQ(stmt->assign.lhs[0]->kind, ExprKind::Star);
}

TEST(ParserTest, AddressOfExpr) {
    auto r = parse_ok("package main\nvar x = &y\n");
    auto* val = r.file()->decls[0]->var.specs[0]->values[0];
    ASSERT_EQ(val->kind, ExprKind::Unary);
    EXPECT_EQ(val->unary.op, TokenKind::Ampersand);
}

TEST(ParserTest, LogicalOperators) {
    auto r = parse_ok("package main\nvar x = a && b || c\n");
    auto* val = r.file()->decls[0]->var.specs[0]->values[0];
    // || has lower precedence than &&, so: (a && b) || c
    ASSERT_EQ(val->kind, ExprKind::Binary);
    EXPECT_EQ(val->binary.op, TokenKind::LogicalOr);
    ASSERT_EQ(val->binary.left->kind, ExprKind::Binary);
    EXPECT_EQ(val->binary.left->binary.op, TokenKind::LogicalAnd);
}

TEST(ParserTest, ComparisonOperators) {
    auto r = parse_ok("package main\nvar x = a == b\n");
    auto* val = r.file()->decls[0]->var.specs[0]->values[0];
    ASSERT_EQ(val->kind, ExprKind::Binary);
    EXPECT_EQ(val->binary.op, TokenKind::Equal);
}

TEST(ParserTest, FuncLiteral) {
    auto r = parse_ok(R"(
package main
func main() {
    f := func(x int) int { return x + 1 }
}
)");
    auto* rhs = r.file()->decls[0]->func.body->block.stmts[0]->short_var_decl.rhs[0];
    ASSERT_EQ(rhs->kind, ExprKind::FuncLit);
    ASSERT_NE(rhs->func_lit.type, nullptr);
    ASSERT_NE(rhs->func_lit.body, nullptr);
}

// ============================================================================
// Statements
// ============================================================================

TEST(ParserTest, ReturnStmt) {
    auto r = parse_ok("package main\nfunc f() int { return 42 }\n");
    auto* body = r.file()->decls[0]->func.body;
    ASSERT_GE(body->block.stmts.count, 1u);
    auto* stmt = body->block.stmts[0];
    ASSERT_EQ(stmt->kind, StmtKind::Return);
    EXPECT_EQ(stmt->return_.results.count, 1u);
}

TEST(ParserTest, ReturnMultiple) {
    auto r = parse_ok("package main\nfunc f() (int, int) { return 1, 2 }\n");
    auto* stmt = r.file()->decls[0]->func.body->block.stmts[0];
    ASSERT_EQ(stmt->kind, StmtKind::Return);
    EXPECT_EQ(stmt->return_.results.count, 2u);
}

TEST(ParserTest, ReturnEmpty) {
    auto r = parse_ok("package main\nfunc f() { return }\n");
    auto* stmt = r.file()->decls[0]->func.body->block.stmts[0];
    ASSERT_EQ(stmt->kind, StmtKind::Return);
    EXPECT_EQ(stmt->return_.results.count, 0u);
}

TEST(ParserTest, IfStmt) {
    auto r = parse_ok(R"(
package main
func main() {
    if x > 0 {
        println(x)
    }
}
)");
    auto* stmt = r.file()->decls[0]->func.body->block.stmts[0];
    ASSERT_EQ(stmt->kind, StmtKind::If);
    EXPECT_EQ(stmt->if_.init, nullptr);
    ASSERT_NE(stmt->if_.cond, nullptr);
    ASSERT_NE(stmt->if_.body, nullptr);
    EXPECT_EQ(stmt->if_.else_body, nullptr);
}

TEST(ParserTest, IfElseStmt) {
    auto r = parse_ok(R"(
package main
func main() {
    if x > 0 {
        println("pos")
    } else {
        println("neg")
    }
}
)");
    auto* stmt = r.file()->decls[0]->func.body->block.stmts[0];
    ASSERT_EQ(stmt->kind, StmtKind::If);
    ASSERT_NE(stmt->if_.else_body, nullptr);
    EXPECT_EQ(stmt->if_.else_body->kind, StmtKind::Block);
}

TEST(ParserTest, IfElseIfStmt) {
    auto r = parse_ok(R"(
package main
func main() {
    if x > 0 {
        println("pos")
    } else if x < 0 {
        println("neg")
    } else {
        println("zero")
    }
}
)");
    auto* stmt = r.file()->decls[0]->func.body->block.stmts[0];
    ASSERT_EQ(stmt->kind, StmtKind::If);
    ASSERT_NE(stmt->if_.else_body, nullptr);
    EXPECT_EQ(stmt->if_.else_body->kind, StmtKind::If);
}

TEST(ParserTest, IfWithInit) {
    auto r = parse_ok(R"(
package main
func main() {
    if x := 5; x > 0 {
        println(x)
    }
}
)");
    auto* stmt = r.file()->decls[0]->func.body->block.stmts[0];
    ASSERT_EQ(stmt->kind, StmtKind::If);
    ASSERT_NE(stmt->if_.init, nullptr);
    ASSERT_NE(stmt->if_.cond, nullptr);
}

TEST(ParserTest, ForInfinite) {
    auto r = parse_ok(R"(
package main
func main() {
    for {
        break
    }
}
)");
    auto* stmt = r.file()->decls[0]->func.body->block.stmts[0];
    ASSERT_EQ(stmt->kind, StmtKind::For);
    EXPECT_EQ(stmt->for_.init, nullptr);
    EXPECT_EQ(stmt->for_.cond, nullptr);
    EXPECT_EQ(stmt->for_.post, nullptr);
}

TEST(ParserTest, ForCondition) {
    auto r = parse_ok(R"(
package main
func main() {
    for x < 10 {
        x++
    }
}
)");
    auto* stmt = r.file()->decls[0]->func.body->block.stmts[0];
    ASSERT_EQ(stmt->kind, StmtKind::For);
    EXPECT_EQ(stmt->for_.init, nullptr);
    ASSERT_NE(stmt->for_.cond, nullptr);
    EXPECT_EQ(stmt->for_.post, nullptr);
}

TEST(ParserTest, ForThreeClause) {
    auto r = parse_ok(R"(
package main
func main() {
    for i := 0; i < 10; i++ {
        println(i)
    }
}
)");
    auto* stmt = r.file()->decls[0]->func.body->block.stmts[0];
    ASSERT_EQ(stmt->kind, StmtKind::For);
    ASSERT_NE(stmt->for_.init, nullptr);
    ASSERT_NE(stmt->for_.cond, nullptr);
    ASSERT_NE(stmt->for_.post, nullptr);
}

TEST(ParserTest, ForRange) {
    auto r = parse_ok(R"(
package main
func main() {
    for range x {
        println("hi")
    }
}
)");
    auto* stmt = r.file()->decls[0]->func.body->block.stmts[0];
    ASSERT_EQ(stmt->kind, StmtKind::Range);
}

TEST(ParserTest, SwitchStmt) {
    auto r = parse_ok(R"(
package main
func main() {
    switch x {
    case 1:
        println("one")
    case 2:
        println("two")
    default:
        println("other")
    }
}
)");
    auto* stmt = r.file()->decls[0]->func.body->block.stmts[0];
    ASSERT_EQ(stmt->kind, StmtKind::Switch);
    EXPECT_EQ(stmt->switch_.cases.count, 3u);
}

TEST(ParserTest, SwitchNoTag) {
    auto r = parse_ok(R"(
package main
func main() {
    switch {
    case x > 0:
        println("pos")
    default:
        println("non-pos")
    }
}
)");
    auto* stmt = r.file()->decls[0]->func.body->block.stmts[0];
    ASSERT_EQ(stmt->kind, StmtKind::Switch);
    EXPECT_EQ(stmt->switch_.tag, nullptr);
}

TEST(ParserTest, GoStmt) {
    auto r = parse_ok(R"(
package main
func main() {
    go f()
}
)");
    auto* stmt = r.file()->decls[0]->func.body->block.stmts[0];
    ASSERT_EQ(stmt->kind, StmtKind::Go);
    ASSERT_NE(stmt->go.call, nullptr);
}

TEST(ParserTest, DeferStmt) {
    auto r = parse_ok(R"(
package main
func main() {
    defer f()
}
)");
    auto* stmt = r.file()->decls[0]->func.body->block.stmts[0];
    ASSERT_EQ(stmt->kind, StmtKind::Defer);
}

TEST(ParserTest, IncDecStmt) {
    auto r = parse_ok(R"(
package main
func main() {
    x++
    y--
}
)");
    auto* body = r.file()->decls[0]->func.body;
    ASSERT_GE(body->block.stmts.count, 2u);
    EXPECT_EQ(body->block.stmts[0]->kind, StmtKind::IncDec);
    EXPECT_EQ(body->block.stmts[0]->inc_dec.tok, TokenKind::Increment);
    EXPECT_EQ(body->block.stmts[1]->kind, StmtKind::IncDec);
    EXPECT_EQ(body->block.stmts[1]->inc_dec.tok, TokenKind::Decrement);
}

TEST(ParserTest, AssignStmt) {
    auto r = parse_ok(R"(
package main
func main() {
    x = 42
}
)");
    auto* stmt = r.file()->decls[0]->func.body->block.stmts[0];
    ASSERT_EQ(stmt->kind, StmtKind::Assign);
    EXPECT_EQ(stmt->assign.tok, TokenKind::Assign);
}

TEST(ParserTest, CompoundAssign) {
    auto r = parse_ok(R"(
package main
func main() {
    x += 1
}
)");
    auto* stmt = r.file()->decls[0]->func.body->block.stmts[0];
    ASSERT_EQ(stmt->kind, StmtKind::Assign);
    EXPECT_EQ(stmt->assign.tok, TokenKind::PlusAssign);
}

TEST(ParserTest, ShortVarDecl) {
    auto r = parse_ok(R"(
package main
func main() {
    x := 42
}
)");
    auto* stmt = r.file()->decls[0]->func.body->block.stmts[0];
    ASSERT_EQ(stmt->kind, StmtKind::ShortVarDecl);
    EXPECT_EQ(stmt->short_var_decl.lhs.count, 1u);
    EXPECT_EQ(stmt->short_var_decl.rhs.count, 1u);
}

TEST(ParserTest, MultiShortVarDecl) {
    auto r = parse_ok(R"(
package main
func main() {
    x, y := 1, 2
}
)");
    auto* stmt = r.file()->decls[0]->func.body->block.stmts[0];
    ASSERT_EQ(stmt->kind, StmtKind::ShortVarDecl);
    EXPECT_EQ(stmt->short_var_decl.lhs.count, 2u);
    EXPECT_EQ(stmt->short_var_decl.rhs.count, 2u);
}

TEST(ParserTest, SendStmt) {
    auto r = parse_ok(R"(
package main
func main() {
    ch <- 42
}
)");
    auto* stmt = r.file()->decls[0]->func.body->block.stmts[0];
    ASSERT_EQ(stmt->kind, StmtKind::Send);
}

TEST(ParserTest, BranchBreak) {
    auto r = parse_ok("package main\nfunc f() { break }\n");
    auto* stmt = r.file()->decls[0]->func.body->block.stmts[0];
    ASSERT_EQ(stmt->kind, StmtKind::Branch);
    EXPECT_EQ(stmt->branch.tok, TokenKind::KW_break);
    EXPECT_EQ(stmt->branch.label, nullptr);
}

TEST(ParserTest, BranchContinue) {
    auto r = parse_ok("package main\nfunc f() { continue }\n");
    auto* stmt = r.file()->decls[0]->func.body->block.stmts[0];
    EXPECT_EQ(stmt->branch.tok, TokenKind::KW_continue);
}

TEST(ParserTest, BranchGoto) {
    auto r = parse_ok("package main\nfunc f() { goto end }\n");
    auto* stmt = r.file()->decls[0]->func.body->block.stmts[0];
    ASSERT_EQ(stmt->kind, StmtKind::Branch);
    EXPECT_EQ(stmt->branch.tok, TokenKind::KW_goto);
    ASSERT_NE(stmt->branch.label, nullptr);
    EXPECT_EQ(stmt->branch.label->name, "end");
}

TEST(ParserTest, Fallthrough) {
    auto r = parse_ok(R"(
package main
func main() {
    switch x {
    case 1:
        fallthrough
    default:
        println("done")
    }
}
)");
    // Just verify it parses
    EXPECT_FALSE(r.has_errors());
}

TEST(ParserTest, LabelStmt) {
    auto r = parse_ok(R"(
package main
func main() {
    myLabel:
        println("at label")
}
)");
    auto* stmt = r.file()->decls[0]->func.body->block.stmts[0];
    ASSERT_EQ(stmt->kind, StmtKind::Label);
    EXPECT_EQ(stmt->label.label->name, "myLabel");
}

TEST(ParserTest, SelectStmt) {
    auto r = parse_ok(R"(
package main
func main() {
    select {
    case x := <-ch:
        println(x)
    default:
        println("no data")
    }
}
)");
    auto* stmt = r.file()->decls[0]->func.body->block.stmts[0];
    ASSERT_EQ(stmt->kind, StmtKind::Select);
    EXPECT_EQ(stmt->select.cases.count, 2u);
}

// ============================================================================
// Full program parsing (sample files)
// ============================================================================

TEST(ParserTest, HelloWorld) {
    auto r = parse_ok(R"(
package main

func main() {
    println("Hello, World!")
}
)");
    EXPECT_EQ(r.file()->package->name->name, "main");
    EXPECT_EQ(r.file()->decls.count, 1u);
}

TEST(ParserTest, Fibonacci) {
    auto r = parse_ok(R"(
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
)");
    EXPECT_EQ(r.file()->decls.count, 2u);
}

TEST(ParserTest, StructsAndMethods) {
    auto r = parse_ok(R"(
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
)");
    EXPECT_EQ(r.file()->decls.count, 3u); // type + method + main
}

TEST(ParserTest, Interfaces) {
    auto r = parse_ok(R"(
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
)");
    EXPECT_GE(r.file()->decls.count, 4u);
}

TEST(ParserTest, MultipleDeclarations) {
    auto r = parse_ok(R"(
package main

const Pi = 3.14

type Circle struct {
    Radius float64
}

var defaultCircle Circle

func main() {}
)");
    EXPECT_EQ(r.file()->decls.count, 4u); // const + type + var + func
}

// ============================================================================
// Error recovery
// ============================================================================

TEST(ParserTest, MissingFuncBody) {
    // Forward declaration - should parse ok
    auto r = parse_ok("package main\nfunc external(x int) int\n");
    auto* decl = r.file()->decls[0];
    ASSERT_EQ(decl->kind, DeclKind::Func);
    EXPECT_EQ(decl->func.body, nullptr);
}

TEST(ParserTest, BadExpression) {
    auto r = parse_fail("package main\nfunc main() { ++ }\n");
    EXPECT_TRUE(r.has_errors());
}

// ============================================================================
// AST Printer
// ============================================================================

TEST(ParserTest, AstPrinterBasic) {
    auto r = parse_ok("package main\nfunc main() {}\n");
    auto output = dump_ast(r.file());
    EXPECT_NE(output.find("File"), std::string::npos);
    EXPECT_NE(output.find("PackageDecl: main"), std::string::npos);
    EXPECT_NE(output.find("FuncDecl: main"), std::string::npos);
}

TEST(ParserTest, AstPrinterExpressions) {
    auto r = parse_ok("package main\nvar x = 1 + 2\n");
    auto output = dump_ast(r.file());
    EXPECT_NE(output.find("BinaryExpr: +"), std::string::npos);
    EXPECT_NE(output.find("BasicLit: IntLiteral 1"), std::string::npos);
    EXPECT_NE(output.find("BasicLit: IntLiteral 2"), std::string::npos);
}

