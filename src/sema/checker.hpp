#pragma once

#include "ast/ast.hpp"
#include "common/arena_allocator.hpp"
#include "common/diagnostic.hpp"
#include "sema/constant.hpp"
#include "sema/scope.hpp"
#include "sema/types.hpp"
#include "sema/universe.hpp"

#include <string_view>
#include <unordered_map>
#include <vector>

namespace golangc {
namespace sema {

/// Information about a checked expression.
struct ExprInfo {
    Type* type = nullptr;
    ConstValue* const_val = nullptr;
    bool is_lvalue = false;
    bool needs_addr_for_recv = false; // pointer-receiver method called on value: &recv needed
    Symbol* symbol = nullptr; // For identifier expressions
};

/// Main semantic checker.
/// Performs name resolution, type checking, and Go-specific validations
/// on a parsed AST.
class Checker {
public:
    Checker(DiagnosticEngine& diag);

    /// Check a parsed file. Returns true if no errors.
    [[nodiscard]] bool check(ast::File* file);

    /// Get the expression info for a checked expression.
    [[nodiscard]] const ExprInfo* expr_info(const ast::Expr* expr) const;

    /// Get the package scope (valid after check).
    [[nodiscard]] Scope* package_scope() const { return package_scope_; }

    /// Get the symbol declared for an IdentExpr* in a var/const spec.
    [[nodiscard]] Symbol* decl_symbol(const ast::IdentExpr* ident) const;

private:
    DiagnosticEngine& diag_;
    ArenaAllocator arena_;

    // Scope management
    Scope* universe_ = nullptr;
    Scope* package_scope_ = nullptr;
    Scope* current_scope_ = nullptr;

    // Current function context
    FuncType* current_func_ = nullptr;
    bool in_loop_ = false;

    // Expression type map
    using ExprMap = std::unordered_map<const ast::Expr*, ExprInfo>;
    ExprMap expr_map_;

    // Declaration symbol map (for IdentExpr* in var/const specs)
    std::unordered_map<const ast::IdentExpr*, Symbol*> decl_sym_map_;

    // Type cache for composite types to avoid duplicates
    std::vector<Type*> type_cache_;

    // Current iota value (reset per const group, incremented per spec)
    int64_t current_iota_ = 0;

    // ---- Scope helpers ----
    Scope* open_scope(ScopeKind kind);
    void close_scope();
    Symbol* declare(SymbolKind kind, std::string_view name, Type* type,
                    SourceLocation loc, ast::Decl* decl_node = nullptr);
    Symbol* lookup(std::string_view name);

    // ---- Type resolution (checker_type.cpp) ----
    Type* resolve_type(ast::TypeExpr* texpr);
    Type* resolve_func_type(ast::FuncTypeExpr* ftype);
    Type* resolve_struct_type(ast::StructTypeExpr* stype);
    Type* resolve_interface_type(ast::InterfaceTypeExpr* itype);

    // ---- Declaration checking (checker_decl.cpp) ----
    void collect_top_level_decls(ast::File* file);
    void check_top_level_decls(ast::File* file);
    void check_func_decl(ast::FuncDecl& decl);
    void check_var_decl(ast::VarDecl& decl);
    void check_var_spec(ast::VarSpec& spec);
    void check_const_decl(ast::ConstDecl& decl);
    void check_type_decl(ast::TypeDecl& decl);
    void check_method(ast::FuncDecl& decl);

    // ---- Expression checking (checker_expr.cpp) ----
    ExprInfo check_expr(ast::Expr* expr);
    ExprInfo check_ident(ast::IdentExpr& expr, SourceLocation loc);
    ExprInfo check_basic_lit(ast::BasicLitExpr& expr);
    ExprInfo check_composite_lit(ast::CompositeLitExpr& expr);
    ExprInfo check_func_lit(ast::FuncLitExpr& expr);
    ExprInfo check_paren(ast::ParenExpr& expr);
    ExprInfo check_selector(ast::SelectorExpr& expr);
    ExprInfo check_index(ast::IndexExpr& expr);
    ExprInfo check_call(ast::CallExpr& expr);
    ExprInfo check_unary(ast::UnaryExpr& expr);
    ExprInfo check_binary(ast::BinaryExpr& expr);
    ExprInfo check_star(ast::StarExpr& expr);
    ExprInfo check_type_assert(ast::TypeAssertExpr& expr);
    ExprInfo check_slice_expr(ast::SliceExpr& expr);
    ExprInfo check_builtin_call(ast::CallExpr& expr, Symbol* sym);

    // Expression helpers
    Type* promote_untyped(Type* a, Type* b);
    bool assignable_to(Type* src, Type* dst);
    bool representable(const ConstValue* val, Type* target);
    void record_expr(const ast::Expr* expr, ExprInfo info);

    // ---- Statement checking (checker_stmt.cpp) ----
    void check_stmt(ast::Stmt* stmt);
    void check_block(ast::BlockStmt& block);
    void check_assign(ast::AssignStmt& stmt);
    void check_short_var_decl(ast::ShortVarDeclStmt& stmt);
    void check_return(ast::ReturnStmt& stmt);
    void check_if(ast::IfStmt& stmt);
    void check_for(ast::ForStmt& stmt);
    void check_range(ast::RangeStmt& stmt);
    void check_switch(ast::SwitchStmt& stmt);
    void check_select(ast::SelectStmt& stmt);
    void check_send(ast::SendStmt& stmt);
    void check_inc_dec(ast::IncDecStmt& stmt);
    void check_go(ast::GoStmt& stmt);
    void check_defer(ast::DeferStmt& stmt);
    void check_branch(ast::BranchStmt& stmt);
    void check_expr_stmt(ast::ExprStmt& stmt);
    void check_decl_stmt(ast::DeclStmt& stmt);
    void check_unused_vars(Scope* scope);
    void check_type_switch(ast::TypeSwitchStmt& stmt);

    // ---- Type construction helpers ----
    Type* make_basic_type(BasicKind kind);
    Type* make_pointer_type(Type* base);
    Type* make_slice_type(Type* elem);
    Type* make_array_type(Type* elem, int64_t length);
    Type* make_map_type(Type* key, Type* value);
    Type* make_chan_type(Type* elem, ChanDir dir);
    Type* make_func_type(FuncType* ft);
    Type* make_named_type(std::string_view name, Type* underlying);
    Type* make_struct_type(StructType* st);
    Type* make_interface_type(InterfaceType* it);
    Type* make_tuple_type(const std::vector<Type*>& types);

    // ---- Pseudo-package selector handling ----
    ExprInfo check_pseudo_pkg_selector(const Symbol& pkg_sym, ast::SelectorExpr& expr);

    // ---- Interface satisfaction ----
    bool satisfies_interface(Type* concrete, InterfaceType* iface);
    Type* lookup_method(Type* t, std::string_view name);

    // ---- Constant evaluation ----
    ConstValue* eval_const_expr(ast::Expr* expr);
};

} // namespace sema
} // namespace golangc
