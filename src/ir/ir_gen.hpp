#pragma once

#include "ast/ast.hpp"
#include "ir/ir.hpp"
#include "ir/ir_builder.hpp"
#include "ir/ir_type_map.hpp"
#include "sema/checker.hpp"

#include <string>
#include <unordered_map>
#include <vector>

namespace golangc {
namespace ir {

/// Generates IR from a type-checked AST.
class IRGenerator {
public:
    explicit IRGenerator(sema::Checker& checker);

    /// Generate IR for a file. Returns the module.
    [[nodiscard]] std::unique_ptr<Module> generate(ast::File* file);

private:
    sema::Checker& checker_;
    IRTypeMap type_map_;
    IRBuilder builder_;
    Module* module_ = nullptr;

    // Maps from sema symbols to IR values
    std::unordered_map<const sema::Symbol*, Value*> var_map_;
    std::unordered_map<const sema::Symbol*, Function*> func_map_;
    std::unordered_map<std::string, Function*> func_name_map_;

    // Loop context for break/continue
    struct LoopContext {
        BasicBlock* break_target = nullptr;
        BasicBlock* continue_target = nullptr;
    };
    std::vector<LoopContext> loop_stack_;

    Function* current_func_ = nullptr;
    uint32_t block_counter_ = 0;

    // ---- Helpers ----
    std::string fresh_block_name(const std::string& prefix);
    IRType* map_sema_type(sema::Type* t);
    const sema::ExprInfo* expr_info(const ast::Expr* expr);
    sema::Type* expr_type(const ast::Expr* expr);

    // ---- Declaration generation (ir_gen_decl.cpp) ----
    void gen_file(ast::File* file);
    void register_functions(ast::File* file);
    void gen_func_decl(ast::FuncDecl& decl, bool is_method);
    void gen_global_var(ast::VarDecl& decl);

    // ---- Expression generation (ir_gen_expr.cpp) ----
    /// Generate code for an expression, returning the result Value*.
    Value* gen_expr(ast::Expr* expr);

    /// Generate address of an lvalue expression (for assignment).
    Value* gen_addr(ast::Expr* expr);

    // Expression helpers — all take the parent Expr* to access ExprInfo
    Value* gen_ident(ast::Expr* expr);
    Value* gen_basic_lit(ast::Expr* expr);
    Value* gen_binary(ast::Expr* expr);
    Value* gen_unary(ast::Expr* expr);
    Value* gen_call(ast::Expr* expr);
    Value* gen_selector(ast::Expr* expr);
    Value* gen_index(ast::Expr* expr);
    Value* gen_composite_lit(ast::Expr* expr);
    Value* gen_star(ast::Expr* expr);
    Value* gen_paren(ast::Expr* expr);
    Value* gen_type_assert(ast::Expr* expr);
    Value* gen_func_lit(ast::Expr* expr);

    Value* gen_builtin_call(ast::Expr* expr, const sema::ExprInfo* func_info);
    Value* gen_logical_and(ast::Expr* expr);
    Value* gen_logical_or(ast::Expr* expr);

    // ---- Statement generation (ir_gen_stmt.cpp) ----
    void gen_stmt(ast::Stmt* stmt);
    void gen_block(ast::BlockStmt& block);
    void gen_assign(ast::AssignStmt& stmt);
    void gen_short_var_decl(ast::ShortVarDeclStmt& stmt);
    void gen_return(ast::ReturnStmt& stmt);
    void gen_if(ast::IfStmt& stmt);
    void gen_for(ast::ForStmt& stmt);
    void gen_range(ast::RangeStmt& stmt);
    void gen_switch(ast::SwitchStmt& stmt);
    void gen_inc_dec(ast::IncDecStmt& stmt);
    void gen_send(ast::SendStmt& stmt);
    void gen_go(ast::GoStmt& stmt);
    void gen_defer(ast::DeferStmt& stmt);
    void gen_branch(ast::BranchStmt& stmt);
    void gen_expr_stmt(ast::ExprStmt& stmt);
    void gen_decl_stmt(ast::DeclStmt& stmt);
    void gen_local_var(ast::VarDecl& decl);
    void gen_local_var_spec(ast::VarSpec& spec);
};

} // namespace ir
} // namespace golangc
