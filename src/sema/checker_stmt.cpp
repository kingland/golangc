#include "sema/checker.hpp"

#include <fmt/format.h>

namespace golangc {
namespace sema {

// ============================================================================
// Main statement dispatcher
// ============================================================================

void Checker::check_stmt(ast::Stmt* stmt) {
    if (!stmt) return;

    switch (stmt->kind) {
        case ast::StmtKind::Bad:
            break;
        case ast::StmtKind::Block:
            check_block(stmt->block);
            break;
        case ast::StmtKind::Expr:
            check_expr_stmt(stmt->expr);
            break;
        case ast::StmtKind::Send:
            check_send(stmt->send);
            break;
        case ast::StmtKind::IncDec:
            check_inc_dec(stmt->inc_dec);
            break;
        case ast::StmtKind::Assign:
            check_assign(stmt->assign);
            break;
        case ast::StmtKind::ShortVarDecl:
            check_short_var_decl(stmt->short_var_decl);
            break;
        case ast::StmtKind::Return:
            check_return(stmt->return_);
            break;
        case ast::StmtKind::Branch:
            check_branch(stmt->branch);
            break;
        case ast::StmtKind::If:
            check_if(stmt->if_);
            break;
        case ast::StmtKind::For:
            check_for(stmt->for_);
            break;
        case ast::StmtKind::Range:
            check_range(stmt->range);
            break;
        case ast::StmtKind::Switch:
            check_switch(stmt->switch_);
            break;
        case ast::StmtKind::Select:
            check_select(stmt->select);
            break;
        case ast::StmtKind::Go:
            check_go(stmt->go);
            break;
        case ast::StmtKind::Defer:
            check_defer(stmt->defer);
            break;
        case ast::StmtKind::Decl:
            check_decl_stmt(stmt->decl);
            break;
        case ast::StmtKind::Label:
            // Check the labeled statement
            if (stmt->label.stmt) {
                check_stmt(stmt->label.stmt);
            }
            break;
        case ast::StmtKind::Empty:
            break;
        case ast::StmtKind::TypeSwitch:
            check_type_switch(stmt->type_switch);
            break;
        case ast::StmtKind::CaseClause:
        case ast::StmtKind::CommClause:
            // These are handled by switch/select
            break;
    }
}

// ============================================================================
// Block statement
// ============================================================================

void Checker::check_block(ast::BlockStmt& block) {
    auto* scope = open_scope(ScopeKind::Block);
    (void)scope;

    for (auto* stmt : block.stmts.span()) {
        check_stmt(stmt);
    }

    close_scope();
}

// ============================================================================
// Expression statement
// ============================================================================

void Checker::check_expr_stmt(ast::ExprStmt& stmt) {
    (void)check_expr(stmt.x);
}

// ============================================================================
// Assign statement
// ============================================================================

void Checker::check_assign(ast::AssignStmt& stmt) {
    // Check RHS first
    std::vector<ExprInfo> rhs_infos;
    for (auto* rhs : stmt.rhs.span()) {
        rhs_infos.push_back(check_expr(rhs));
    }

    // Check LHS
    std::vector<ExprInfo> lhs_infos;
    for (auto* lhs : stmt.lhs.span()) {
        lhs_infos.push_back(check_expr(lhs));
    }

    // For simple assignment (=), check type compatibility
    if (stmt.tok == TokenKind::Assign) {
        if (lhs_infos.size() == rhs_infos.size()) {
            for (size_t i = 0; i < lhs_infos.size(); ++i) {
                if (lhs_infos[i].type && rhs_infos[i].type) {
                    if (!assignable_to(rhs_infos[i].type, lhs_infos[i].type)) {
                        diag_.error(stmt.tok_loc,
                            "cannot use {} as {} in assignment",
                            type_string(rhs_infos[i].type),
                            type_string(lhs_infos[i].type));
                    }
                }
            }
        } else if (rhs_infos.size() == 1 && rhs_infos[0].type &&
                   rhs_infos[0].type->kind == TypeKind::Tuple) {
            // Multi-value return: x, y = f()
            // handled
        } else {
            diag_.error(stmt.tok_loc,
                "assignment count mismatch: {} = {}",
                lhs_infos.size(), rhs_infos.size());
        }
    }
    // For compound assignment (+=, -=, etc.) check numeric type
}

// ============================================================================
// Short variable declaration (:=)
// ============================================================================

void Checker::check_short_var_decl(ast::ShortVarDeclStmt& stmt) {
    // Check RHS
    std::vector<ExprInfo> rhs_infos;
    for (auto* rhs : stmt.rhs.span()) {
        rhs_infos.push_back(check_expr(rhs));
    }

    bool at_least_one_new = false;

    // Handle multi-value return on RHS
    bool multi_return = false;
    std::vector<Type*> result_types;
    if (rhs_infos.size() == 1 && rhs_infos[0].type &&
        rhs_infos[0].type->kind == TypeKind::Tuple &&
        rhs_infos[0].type->tuple) {
        multi_return = true;
        result_types = rhs_infos[0].type->tuple->types;
    }

    for (uint32_t i = 0; i < stmt.lhs.count; ++i) {
        auto* lhs = stmt.lhs[i];
        if (!lhs || lhs->kind != ast::ExprKind::Ident) continue;

        auto& ident = lhs->ident;
        if (ident.name == "_") {
            // Blank identifier always ok
            continue;
        }

        Type* var_type = nullptr;
        if (multi_return) {
            if (i < result_types.size()) {
                var_type = result_types[i];
            }
        } else if (i < rhs_infos.size()) {
            var_type = rhs_infos[i].type;
        }

        if (is_untyped(var_type)) {
            var_type = default_type(var_type);
        }

        // Check if this name already exists in current scope
        auto* existing = current_scope_->lookup_local(ident.name);
        if (existing) {
            // Re-assignment — just update type if needed
            if (var_type) {
                existing->type = var_type;
            }
            existing->used = true;
        } else {
            // New variable
            at_least_one_new = true;
            auto* sym = declare(SymbolKind::Var, ident.name,
                                var_type ? var_type : basic_type(BasicKind::Invalid),
                                ident.loc);
            if (sym) {
                // Record expression info so the ident is resolved
                ExprInfo info;
                info.type = var_type;
                info.symbol = sym;
                info.is_lvalue = true;
                record_expr(lhs, info);
            }
        }
    }

    if (!at_least_one_new) {
        diag_.error(stmt.tok_loc, "no new variables on left side of :=");
    }
}

// ============================================================================
// Return statement
// ============================================================================

void Checker::check_return(ast::ReturnStmt& stmt) {
    if (!current_func_) {
        diag_.error(stmt.loc, "return statement outside function");
        return;
    }

    size_t expected = current_func_->results.size();
    size_t got = stmt.results.count;

    // Naked return (allowed with named results)
    if (got == 0 && expected > 0) {
        // Check if there are named results
        bool all_named = true;
        for (const auto& r : current_func_->results) {
            if (r.name.empty()) {
                all_named = false;
                break;
            }
        }
        if (!all_named) {
            diag_.error(stmt.loc, "not enough return values (expected {}, got 0)", expected);
        }
        return;
    }

    if (got != expected) {
        diag_.error(stmt.loc, "wrong number of return values (expected {}, got {})",
                   expected, got);
        return;
    }

    for (uint32_t i = 0; i < stmt.results.count; ++i) {
        auto result_info = check_expr(stmt.results[i]);
        if (i < current_func_->results.size()) {
            Type* expected_type = current_func_->results[i].type;
            if (result_info.type && expected_type &&
                !assignable_to(result_info.type, expected_type)) {
                diag_.error(stmt.loc, "cannot use {} as {} in return statement",
                           type_string(result_info.type), type_string(expected_type));
            }
        }
    }
}

// ============================================================================
// If statement
// ============================================================================

void Checker::check_if(ast::IfStmt& stmt) {
    auto* scope = open_scope(ScopeKind::Block);
    (void)scope;

    if (stmt.init) {
        check_stmt(stmt.init);
    }

    if (stmt.cond) {
        auto cond_info = check_expr(stmt.cond);
        if (cond_info.type && !is_boolean(cond_info.type)) {
            diag_.error(stmt.loc, "non-boolean condition in if statement");
        }
    }

    if (stmt.body) {
        check_stmt(stmt.body);
    }

    if (stmt.else_body) {
        check_stmt(stmt.else_body);
    }

    close_scope();
}

// ============================================================================
// For statement
// ============================================================================

void Checker::check_for(ast::ForStmt& stmt) {
    auto* scope = open_scope(ScopeKind::Block);
    (void)scope;

    bool prev_in_loop = in_loop_;
    in_loop_ = true;

    if (stmt.init) {
        check_stmt(stmt.init);
    }

    if (stmt.cond) {
        auto cond_info = check_expr(stmt.cond);
        if (cond_info.type && !is_boolean(cond_info.type)) {
            diag_.error(stmt.loc, "non-boolean condition in for statement");
        }
    }

    if (stmt.post) {
        check_stmt(stmt.post);
    }

    if (stmt.body) {
        check_stmt(stmt.body);
    }

    in_loop_ = prev_in_loop;
    close_scope();
}

// ============================================================================
// Range statement
// ============================================================================

void Checker::check_range(ast::RangeStmt& stmt) {
    auto* scope = open_scope(ScopeKind::Block);
    (void)scope;

    bool prev_in_loop = in_loop_;
    in_loop_ = true;

    // Check range expression
    auto x_info = check_expr(stmt.x);

    // Determine key/value types
    Type* key_type = nullptr;
    Type* value_type = nullptr;

    if (x_info.type) {
        const Type* u = underlying(x_info.type);
        if (u) {
            switch (u->kind) {
                case TypeKind::Array:
                    key_type = basic_type(BasicKind::Int);
                    value_type = u->array.element;
                    break;
                case TypeKind::Slice:
                    key_type = basic_type(BasicKind::Int);
                    value_type = u->slice.element;
                    break;
                case TypeKind::Map:
                    key_type = u->map.key;
                    value_type = u->map.value;
                    break;
                case TypeKind::Chan:
                    key_type = u->chan.element;
                    break;
                case TypeKind::Basic:
                    if (u->basic == BasicKind::String ||
                        u->basic == BasicKind::UntypedString) {
                        key_type = basic_type(BasicKind::Int);
                        value_type = basic_type(BasicKind::Int32); // rune
                    }
                    break;
                default:
                    diag_.error(stmt.loc, "cannot range over {}",
                               type_string(x_info.type));
                    break;
            }
        }
    }

    // Declare or assign key/value
    if (stmt.tok == TokenKind::ColonAssign) {
        // Short var decl in range
        if (stmt.key && stmt.key->kind == ast::ExprKind::Ident &&
            stmt.key->ident.name != "_") {
            auto* sym = declare(SymbolKind::Var, stmt.key->ident.name,
                                key_type ? key_type : basic_type(BasicKind::Invalid),
                                stmt.key->ident.loc);
            ExprInfo ki;
            ki.type = key_type;
            ki.symbol = sym;
            ki.is_lvalue = true;
            record_expr(stmt.key, ki);
        }
        if (stmt.value && stmt.value->kind == ast::ExprKind::Ident &&
            stmt.value->ident.name != "_") {
            auto* sym = declare(SymbolKind::Var, stmt.value->ident.name,
                                value_type ? value_type : basic_type(BasicKind::Invalid),
                                stmt.value->ident.loc);
            ExprInfo vi;
            vi.type = value_type;
            vi.symbol = sym;
            vi.is_lvalue = true;
            record_expr(stmt.value, vi);
        }
    } else if (stmt.tok == TokenKind::Assign) {
        if (stmt.key) (void)check_expr(stmt.key);
        if (stmt.value) (void)check_expr(stmt.value);
    }

    if (stmt.body) {
        check_stmt(stmt.body);
    }

    in_loop_ = prev_in_loop;
    close_scope();
}

// ============================================================================
// Switch statement
// ============================================================================

void Checker::check_switch(ast::SwitchStmt& stmt) {
    auto* scope = open_scope(ScopeKind::Block);
    (void)scope;

    bool prev_in_loop = in_loop_;
    // switch allows break, so treat like a loop for break purposes
    in_loop_ = true;

    if (stmt.init) {
        check_stmt(stmt.init);
    }

    ExprInfo tag_info;
    if (stmt.tag) {
        tag_info = check_expr(stmt.tag);
    }

    for (auto* case_clause : stmt.cases.span()) {
        if (!case_clause) continue;

        // Check case values
        for (auto* val : case_clause->values.span()) {
            auto val_info = check_expr(val);
            // Check compatibility with tag if present
            if (tag_info.type && val_info.type) {
                // Types should be comparable
            }
        }

        // Open a scope for case body
        auto* case_scope = open_scope(ScopeKind::Block);
        (void)case_scope;

        for (auto* body_stmt : case_clause->body.span()) {
            check_stmt(body_stmt);
        }

        close_scope();
    }

    in_loop_ = prev_in_loop;
    close_scope();
}

// ============================================================================
// Select statement
// ============================================================================

void Checker::check_select(ast::SelectStmt& stmt) {
    for (auto* comm : stmt.cases.span()) {
        if (!comm) continue;

        auto* scope = open_scope(ScopeKind::Block);
        (void)scope;

        if (comm->comm) {
            check_stmt(comm->comm);
        }

        for (auto* body_stmt : comm->body.span()) {
            check_stmt(body_stmt);
        }

        close_scope();
    }
}

// ============================================================================
// Send statement
// ============================================================================

void Checker::check_send(ast::SendStmt& stmt) {
    auto chan_info = check_expr(stmt.chan);
    auto val_info = check_expr(stmt.value);

    if (!chan_info.type) return;

    const Type* u = underlying(chan_info.type);
    if (!u || u->kind != TypeKind::Chan) {
        diag_.error(stmt.loc, "invalid operation: cannot send to non-channel type {}",
                   type_string(chan_info.type));
        return;
    }

    if (u->chan.dir == ChanDir::RecvOnly) {
        diag_.error(stmt.loc, "invalid operation: cannot send to receive-only channel");
        return;
    }

    if (val_info.type && !assignable_to(val_info.type, u->chan.element)) {
        diag_.error(stmt.loc, "cannot use {} as {} in send",
                   type_string(val_info.type), type_string(u->chan.element));
    }
}

// ============================================================================
// IncDec statement
// ============================================================================

void Checker::check_inc_dec(ast::IncDecStmt& stmt) {
    auto x_info = check_expr(stmt.x);
    if (x_info.type && !is_numeric(x_info.type)) {
        diag_.error(stmt.loc, "invalid operation: {} on non-numeric type {}",
                   stmt.tok == TokenKind::Increment ? "++" : "--",
                   type_string(x_info.type));
    }
}

// ============================================================================
// Go statement
// ============================================================================

void Checker::check_go(ast::GoStmt& stmt) {
    if (!stmt.call) {
        diag_.error(stmt.loc, "expression in go must be function call");
        return;
    }
    auto call_info = check_expr(stmt.call);
    (void)call_info;
    // The expression should be a call
    if (stmt.call->kind != ast::ExprKind::Call) {
        diag_.error(stmt.loc, "expression in go must be function call");
    }
}

// ============================================================================
// Defer statement
// ============================================================================

void Checker::check_defer(ast::DeferStmt& stmt) {
    if (!stmt.call) {
        diag_.error(stmt.loc, "expression in defer must be function call");
        return;
    }
    auto call_info = check_expr(stmt.call);
    (void)call_info;
    if (stmt.call->kind != ast::ExprKind::Call) {
        diag_.error(stmt.loc, "expression in defer must be function call");
    }
}

// ============================================================================
// Branch statement (break, continue, goto, fallthrough)
// ============================================================================

void Checker::check_branch(ast::BranchStmt& stmt) {
    switch (stmt.tok) {
        case TokenKind::KW_break:
        case TokenKind::KW_continue:
            if (!in_loop_) {
                diag_.error(stmt.loc, "{} is not in a loop or switch",
                           stmt.tok == TokenKind::KW_break ? "break" : "continue");
            }
            break;
        case TokenKind::KW_goto:
            // Label checking would go here
            break;
        case TokenKind::KW_fallthrough:
            // Must be last statement in case clause
            break;
        default:
            break;
    }
}

// ============================================================================
// Declaration statement
// ============================================================================

void Checker::check_decl_stmt(ast::DeclStmt& stmt) {
    if (!stmt.decl) return;

    switch (stmt.decl->kind) {
        case ast::DeclKind::Var:
            check_var_decl(stmt.decl->var);
            break;
        case ast::DeclKind::Const:
            check_const_decl(stmt.decl->const_);
            break;
        case ast::DeclKind::Type:
            check_type_decl(stmt.decl->type);
            break;
        default:
            break;
    }
}

// ============================================================================
// Type switch statement
// ============================================================================

void Checker::check_type_switch(ast::TypeSwitchStmt& stmt) {
    auto* outer = open_scope(ScopeKind::Block);
    (void)outer;
    bool prev = in_loop_;
    in_loop_ = true;

    if (stmt.init) check_stmt(stmt.init);

    // Determine the interface expression and optional bound variable name
    ExprInfo iface_info;
    std::string_view bound_name;

    if (stmt.assign) {
        if (stmt.assign->kind == ast::StmtKind::Expr && stmt.assign->expr.x) {
            // switch x.(type) { ... }  — guard is just an expression stmt
            auto* ta = stmt.assign->expr.x;
            if (ta->kind == ast::ExprKind::TypeAssert) {
                iface_info = check_expr(ta->type_assert.x);
            }
        } else if (stmt.assign->kind == ast::StmtKind::ShortVarDecl) {
            // switch v := x.(type) { ... }
            auto& svd = stmt.assign->short_var_decl;
            if (svd.rhs.count > 0 && svd.rhs[0] &&
                svd.rhs[0]->kind == ast::ExprKind::TypeAssert) {
                iface_info = check_expr(svd.rhs[0]->type_assert.x);
            }
            if (svd.lhs.count > 0 && svd.lhs[0] &&
                svd.lhs[0]->kind == ast::ExprKind::Ident) {
                bound_name = svd.lhs[0]->ident.name;
            }
        }
    }

    for (auto* cc : stmt.cases.span()) {
        if (!cc) continue;
        auto* case_scope = open_scope(ScopeKind::Block);
        (void)case_scope;

        // For each value in case clause, check it as an expression (type name).
        // Single concrete type — resolve it for bound variable typing.
        Type* single_type = nullptr;
        for (auto* v : cc->values.span()) {
            if (!v) continue;
            auto vi = check_expr(v);
            if (cc->values.count == 1) {
                // The symbol for a type name has kind == SymbolKind::Type
                if (vi.symbol && vi.symbol->kind == SymbolKind::Type) {
                    single_type = vi.symbol->type;
                } else {
                    single_type = vi.type;
                }
            }
        }

        // Declare bound variable for this case (if bound name present)
        if (!bound_name.empty() && bound_name != "_") {
            Type* var_type = (cc->values.count == 1 && single_type)
                                 ? single_type
                                 : iface_info.type;
            if (!var_type) var_type = basic_type(BasicKind::Invalid);
            auto* sym = declare(SymbolKind::Var, bound_name, var_type, cc->loc);
            if (sym) sym->used = true;
        }

        for (auto* s : cc->body.span()) check_stmt(s);
        close_scope();
    }

    in_loop_ = prev;
    close_scope();
}

// ============================================================================
// Unused variable checking
// ============================================================================

void Checker::check_unused_vars(Scope* scope) {
    if (!scope) return;

    for (const auto& [name, sym] : scope->symbols()) {
        if (sym->kind == SymbolKind::Var && !sym->used && name != "_") {
            diag_.error(sym->loc, "{} declared and not used", name);
        }
    }
}

} // namespace sema
} // namespace golangc
