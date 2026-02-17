#include "sema/checker.hpp"

#include <fmt/format.h>

namespace golangc {
namespace sema {

// ============================================================================
// Pass 1: Collect top-level declarations (forward references)
// ============================================================================

void Checker::collect_top_level_decls(ast::File* file) {
    for (auto* decl : file->decls.span()) {
        if (!decl) continue;

        switch (decl->kind) {
            case ast::DeclKind::Func: {
                auto& fd = decl->func;
                if (fd.name && !fd.recv) {
                    // Regular function (not method)
                    (void)declare(SymbolKind::Func, fd.name->name, nullptr,
                                  fd.name->loc, decl);
                }
                break;
            }
            case ast::DeclKind::Type: {
                auto& td = decl->type;
                for (auto* spec : td.specs.span()) {
                    if (!spec || !spec->name) continue;
                    // Create a placeholder named type — will be filled in pass 2
                    auto* named = arena_.create<NamedType>();
                    named->name = spec->name->name;

                    auto* t = arena_.create<Type>();
                    t->kind = TypeKind::Named;
                    t->named = named;
                    type_cache_.push_back(t);

                    (void)declare(SymbolKind::Type, spec->name->name, t,
                                  spec->name->loc, decl);
                }
                break;
            }
            case ast::DeclKind::Var: {
                auto& vd = decl->var;
                for (auto* spec : vd.specs.span()) {
                    if (!spec) continue;
                    for (auto* name : spec->names.span()) {
                        if (!name) continue;
                        (void)declare(SymbolKind::Var, name->name, nullptr,
                                      name->loc, decl);
                    }
                }
                break;
            }
            case ast::DeclKind::Const: {
                auto& cd = decl->const_;
                for (auto* spec : cd.specs.span()) {
                    if (!spec) continue;
                    for (auto* name : spec->names.span()) {
                        if (!name) continue;
                        (void)declare(SymbolKind::Const, name->name, nullptr,
                                      name->loc, decl);
                    }
                }
                break;
            }
            default:
                break;
        }
    }
}

// ============================================================================
// Pass 2: Check top-level declarations
// ============================================================================

void Checker::check_top_level_decls(ast::File* file) {
    for (auto* decl : file->decls.span()) {
        if (!decl) continue;

        switch (decl->kind) {
            case ast::DeclKind::Func:
                if (decl->func.recv) {
                    check_method(decl->func);
                } else {
                    check_func_decl(decl->func);
                }
                break;
            case ast::DeclKind::Var:
                check_var_decl(decl->var);
                break;
            case ast::DeclKind::Const:
                check_const_decl(decl->const_);
                break;
            case ast::DeclKind::Type:
                check_type_decl(decl->type);
                break;
            default:
                break;
        }
    }
}

// ============================================================================
// Function declarations
// ============================================================================

void Checker::check_func_decl(ast::FuncDecl& decl) {
    if (!decl.name) return;

    // Resolve function type
    Type* func_type = resolve_func_type(decl.type);
    if (!func_type) return;

    // Update the symbol's type
    auto* sym = package_scope_->lookup_local(decl.name->name);
    if (sym) {
        sym->type = func_type;
    }

    // Check body
    if (decl.body) {
        auto* prev_func = current_func_;
        current_func_ = func_type->func;

        auto* func_scope = open_scope(ScopeKind::Function);

        // Add parameters to scope
        if (func_type->func) {
            for (const auto& param : func_type->func->params) {
                if (!param.name.empty() && param.name != "_") {
                    (void)declare(SymbolKind::Var, param.name, param.type,
                                  decl.loc);
                }
            }
            // Add named return values to scope
            for (const auto& result : func_type->func->results) {
                if (!result.name.empty() && result.name != "_") {
                    (void)declare(SymbolKind::Var, result.name, result.type,
                                  decl.loc);
                }
            }
        }

        // Check body statements
        if (decl.body->kind == ast::StmtKind::Block) {
            for (auto* stmt : decl.body->block.stmts.span()) {
                check_stmt(stmt);
            }
        }

        close_scope();
        (void)func_scope;
        current_func_ = prev_func;
    }
}

// ============================================================================
// Method declarations
// ============================================================================

void Checker::check_method(ast::FuncDecl& decl) {
    if (!decl.name || !decl.recv) return;

    // Resolve the function type (including receiver)
    Type* func_type = resolve_func_type(decl.type);
    if (!func_type || !func_type->func) return;

    // Determine receiver type
    if (decl.recv->fields.empty()) return;
    auto* recv_field = decl.recv->fields[0];
    if (!recv_field || !recv_field->type) return;

    Type* recv_type = resolve_type(recv_field->type);
    if (!recv_type) return;

    // Find the base named type
    Type* base_type = recv_type;
    bool pointer_receiver = false;
    if (recv_type->kind == TypeKind::Pointer) {
        base_type = recv_type->pointer.base;
        pointer_receiver = true;
    }

    if (base_type->kind != TypeKind::Named || !base_type->named) {
        diag_.error(decl.loc, "invalid receiver type");
        return;
    }

    // Register method on the named type
    base_type->named->methods.push_back(
        NamedType::Method{decl.name->name, func_type, pointer_receiver});

    // Check body
    if (decl.body) {
        auto* prev_func = current_func_;
        current_func_ = func_type->func;

        auto* func_scope = open_scope(ScopeKind::Function);

        // Add receiver to scope
        if (!recv_field->names.empty() && recv_field->names[0]) {
            auto recv_name = recv_field->names[0]->name;
            if (!recv_name.empty() && recv_name != "_") {
                auto* recv_sym = declare(SymbolKind::Var, recv_name, recv_type, decl.loc);
                // Mark receiver as used — Go doesn't require receivers to be used
                if (recv_sym) recv_sym->used = true;
            }
        }

        // Add parameters to scope
        if (func_type->func) {
            for (const auto& param : func_type->func->params) {
                if (!param.name.empty() && param.name != "_") {
                    (void)declare(SymbolKind::Var, param.name, param.type,
                                  decl.loc);
                }
            }
            for (const auto& result : func_type->func->results) {
                if (!result.name.empty() && result.name != "_") {
                    (void)declare(SymbolKind::Var, result.name, result.type,
                                  decl.loc);
                }
            }
        }

        // Check body
        if (decl.body->kind == ast::StmtKind::Block) {
            for (auto* stmt : decl.body->block.stmts.span()) {
                check_stmt(stmt);
            }
        }

        close_scope();
        (void)func_scope;
        current_func_ = prev_func;
    }
}

// ============================================================================
// Variable declarations
// ============================================================================

void Checker::check_var_decl(ast::VarDecl& decl) {
    for (auto* spec : decl.specs.span()) {
        if (spec) check_var_spec(*spec);
    }
}

void Checker::check_var_spec(ast::VarSpec& spec) {
    Type* declared_type = nullptr;
    if (spec.type) {
        declared_type = resolve_type(spec.type);
    }

    // Check values
    std::vector<ExprInfo> values;
    for (auto* val_expr : spec.values.span()) {
        values.push_back(check_expr(val_expr));
    }

    // Assign types to names
    for (uint32_t i = 0; i < spec.names.count; ++i) {
        auto* name = spec.names[i];
        if (!name) continue;

        Type* var_type = declared_type;

        if (!var_type && i < values.size()) {
            var_type = values[i].type;
            if (is_untyped(var_type)) {
                var_type = default_type(var_type);
            }
        }

        if (!var_type) {
            diag_.error(name->loc, "cannot determine type for '{}'", name->name);
            continue;
        }

        // Check type compatibility if both type and value present
        if (declared_type && i < values.size() && values[i].type) {
            if (!assignable_to(values[i].type, declared_type)) {
                diag_.error(name->loc, "cannot use {} as {} in assignment",
                           type_string(values[i].type), type_string(declared_type));
            }
        }

        // Update symbol type (may have been declared in pass 1 for top-level)
        auto* sym = current_scope_->lookup_local(name->name);
        if (sym) {
            sym->type = var_type;
        } else {
            (void)declare(SymbolKind::Var, name->name, var_type, name->loc);
        }
    }
}

// ============================================================================
// Constant declarations
// ============================================================================

void Checker::check_const_decl(ast::ConstDecl& decl) {
    for (auto* spec : decl.specs.span()) {
        if (!spec) continue;

        Type* declared_type = nullptr;
        if (spec->type) {
            declared_type = resolve_type(spec->type);
        }

        for (uint32_t i = 0; i < spec->names.count; ++i) {
            auto* name = spec->names[i];
            if (!name) continue;

            ConstValue* val = nullptr;
            Type* const_type = declared_type;

            if (i < spec->values.count) {
                val = eval_const_expr(spec->values[i]);
                if (!val) {
                    diag_.error(name->loc, "constant expression required");
                }

                if (!const_type && spec->values[i]) {
                    auto info = check_expr(spec->values[i]);
                    const_type = info.type;
                }
            }

            if (!const_type) {
                const_type = basic_type(BasicKind::UntypedInt);
            }

            auto* sym = current_scope_->lookup_local(name->name);
            if (sym) {
                sym->type = const_type;
                sym->const_val = val;
            } else {
                auto* new_sym = declare(SymbolKind::Const, name->name, const_type,
                                        name->loc);
                if (new_sym) new_sym->const_val = val;
            }
        }
    }
}

// ============================================================================
// Type declarations
// ============================================================================

void Checker::check_type_decl(ast::TypeDecl& decl) {
    for (auto* spec : decl.specs.span()) {
        if (!spec || !spec->name) continue;

        auto* sym = package_scope_->lookup_local(spec->name->name);
        if (!sym || !sym->type || sym->type->kind != TypeKind::Named) continue;

        // Resolve the underlying type
        Type* und = resolve_type(spec->type);
        if (!und) continue;

        // Fill in the named type's underlying
        sym->type->named->underlying = und;
    }
}

} // namespace sema
} // namespace golangc
