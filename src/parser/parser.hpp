#pragma once

#include "ast/ast.hpp"
#include "common/arena_allocator.hpp"
#include "common/diagnostic.hpp"
#include "lexer/lexer.hpp"
#include "lexer/token.hpp"

#include <string_view>
#include <vector>

namespace golangc {

/// Recursive descent parser for Go source code.
/// Converts a token stream from the Lexer into an AST.
class Parser {
public:
    Parser(Lexer& lexer, DiagnosticEngine& diag);

    /// Parse a complete Go source file.
    /// Returns true if parsing succeeded without errors.
    [[nodiscard]] bool parse();

    /// Get the parsed file AST (valid after parse() returns true).
    [[nodiscard]] ast::File* file() const { return file_; }

    /// Get the arena allocator (for tests that want to inspect allocations).
    [[nodiscard]] ArenaAllocator& arena() { return arena_; }

private:
    Lexer& lexer_;
    DiagnosticEngine& diag_;
    ArenaAllocator arena_;
    Token current_;
    Token prev_;
    ast::File* file_ = nullptr;
    bool allow_composite_lit_ = true; // false in if/for/switch conditions

    // ---- Token navigation ----
    void advance();
    [[nodiscard]] Token peek() const { return current_; }
    [[nodiscard]] bool at(TokenKind kind) const { return current_.kind == kind; }
    [[nodiscard]] bool at_end() const { return current_.kind == TokenKind::Eof; }
    bool expect(TokenKind kind);
    bool expect_semicolon();
    bool match(TokenKind kind);
    Token consume(TokenKind kind); // Like expect but returns the consumed token
    Token consume(); // Consume current token and return it

    // ---- Error handling ----
    void error(std::string_view msg);
    void error_at(SourceLocation loc, std::string_view msg);
    void error_expected(std::string_view what);
    void sync_to_decl();        // Synchronize to next top-level declaration
    void sync_to_stmt();        // Synchronize to next statement boundary
    void skip_to(TokenKind kind);

    // ---- Source file ----
    ast::File* parse_file();
    ast::PackageDecl* parse_package_clause();

    // ---- Declarations ----
    ast::Decl* parse_import_decl();
    ast::ImportSpec* parse_import_spec();
    ast::Decl* parse_top_level_decl();
    ast::Decl* parse_const_decl();
    ast::Decl* parse_type_decl();
    ast::Decl* parse_var_decl();
    ast::Decl* parse_func_or_method_decl();

    // Spec parsers
    ast::ValueSpec* parse_const_spec();
    ast::TypeSpec* parse_type_spec();
    ast::VarSpec* parse_var_spec();

    // ---- Types ----
    ast::TypeExpr* parse_type();
    ast::TypeExpr* parse_type_or_nil();   // Returns nullptr if no type
    ast::TypeExpr* parse_type_name();
    ast::TypeExpr* parse_array_or_slice_type();
    ast::TypeExpr* parse_map_type();
    ast::TypeExpr* parse_chan_type();
    ast::TypeExpr* parse_struct_type();
    ast::TypeExpr* parse_interface_type();
    ast::TypeExpr* parse_func_type();
    ast::TypeExpr* parse_pointer_type();

    // ---- Function signatures & fields ----
    ast::FuncTypeExpr* parse_signature();
    ast::FieldList* parse_parameters();
    ast::FieldList* parse_results();
    ast::Field* parse_param_decl();
    ast::FieldList* parse_field_list(TokenKind open, TokenKind close);
    ast::Field* parse_struct_field();

    // ---- Statements ----
    ast::Stmt* parse_stmt();
    ast::Stmt* parse_simple_stmt(bool no_semi = false);
    ast::Stmt* parse_block();
    ast::Stmt* parse_if_stmt();
    ast::Stmt* parse_for_stmt();
    ast::Stmt* parse_switch_stmt();
    ast::Stmt* parse_select_stmt();
    ast::Stmt* parse_return_stmt();
    ast::Stmt* parse_go_stmt();
    ast::Stmt* parse_defer_stmt();
    ast::Stmt* parse_branch_stmt(TokenKind keyword);
    ast::CaseClause* parse_case_clause();
    ast::CommClause* parse_comm_clause();
    void parse_stmt_list(std::vector<ast::Stmt*>& stmts);

    // ---- Expressions ----
    ast::Expr* parse_expr();
    ast::Expr* parse_binary_expr(int prec);
    ast::Expr* parse_unary_expr();
    ast::Expr* parse_primary_expr();
    ast::Expr* parse_operand();
    ast::Expr* parse_composite_lit(ast::TypeExpr* type);
    ast::Expr* parse_func_lit();

    // Expression helpers
    ast::Expr* parse_selector_or_type_assert(ast::Expr* x);
    ast::Expr* parse_index_or_slice(ast::Expr* x);
    ast::Expr* parse_call_expr(ast::Expr* func);
    ast::List<ast::Expr*> parse_expr_list();
    ast::List<ast::IdentExpr*> parse_ident_list();
    ast::Expr* parse_element();
    ast::Expr* parse_literal_value();

    // ---- Operator precedence ----
    [[nodiscard]] static int binary_prec(TokenKind kind);
    [[nodiscard]] static bool is_assign_op(TokenKind kind);

    // ---- AST node creation helpers ----
    template <typename T, typename... Args>
    T* alloc(Args&&... args) {
        return arena_.create<T>(std::forward<Args>(args)...);
    }

    ast::Expr* make_bad_expr(SourceLocation loc);
    ast::Stmt* make_bad_stmt(SourceLocation loc);
    ast::Decl* make_bad_decl(SourceLocation loc);
    ast::TypeExpr* make_bad_type(SourceLocation loc);

    ast::Expr* make_ident(Token tok);
    ast::IdentExpr* make_ident_node(Token tok);

    ast::Expr* make_expr(ast::ExprKind kind);
    ast::Stmt* make_stmt(ast::StmtKind kind);
    ast::Decl* make_decl(ast::DeclKind kind);
    ast::TypeExpr* make_type(ast::TypeExprKind kind);

    // ---- State query helpers ----
    [[nodiscard]] bool is_literal_type_start() const;
    [[nodiscard]] bool is_stmt_start() const;
    [[nodiscard]] bool is_decl_start() const;
    [[nodiscard]] bool could_be_type() const;
};

} // namespace golangc
