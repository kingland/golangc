#include "ir/ir_gen.hpp"

#include <cassert>
#include <string>

namespace golangc {
namespace ir {

void IRGenerator::gen_file(ast::File* file) {
    // First pass: register all functions (so calls can resolve forward references)
    register_functions(file);

    // Second pass: generate function bodies and global vars
    for (auto* decl : file->decls) {
        switch (decl->kind) {
            case ast::DeclKind::Func:
                gen_func_decl(decl->func, decl->func.recv != nullptr);
                break;
            case ast::DeclKind::Var:
                gen_global_var(decl->var);
                break;
            default:
                break;
        }
    }
}

/// Extract the receiver type name from a FuncDecl.
static std::string get_receiver_type_name(ast::FuncDecl& fd) {
    if (!fd.recv || fd.recv->fields.count == 0) return "";
    auto* field = fd.recv->fields[0];
    auto* texpr = field->type;
    if (texpr && texpr->kind == ast::TypeExprKind::Pointer) {
        texpr = texpr->pointer.base;
    }
    if (texpr && texpr->kind == ast::TypeExprKind::Ident) {
        return std::string(texpr->ident.name);
    }
    return "";
}

/// Build the full function name (TypeName.MethodName for methods).
static std::string build_func_name(ast::FuncDecl& fd) {
    if (fd.recv) {
        auto type_name = get_receiver_type_name(fd);
        if (!type_name.empty()) {
            return type_name + "." + std::string(fd.name->name);
        }
    }
    return std::string(fd.name->name);
}

void IRGenerator::register_functions(ast::File* file) {
    for (auto* decl : file->decls) {
        if (decl->kind != ast::DeclKind::Func) continue;
        auto& fd = decl->func;
        if (!fd.name) continue;

        std::string name = build_func_name(fd);

        // Get sema type info via the function name
        auto* func_sym = checker_.decl_symbol(fd.name);
        sema::Type* sema_func_type = nullptr;
        if (func_sym) {
            sema_func_type = func_sym->type;
        } else if (fd.recv) {
            // Method: look up on the receiver's named type
            auto type_name = get_receiver_type_name(fd);
            if (!type_name.empty()) {
                auto* scope = checker_.package_scope();
                if (scope) {
                    auto* type_sym = scope->lookup(type_name);
                    if (type_sym && type_sym->type &&
                        type_sym->type->kind == sema::TypeKind::Named &&
                        type_sym->type->named) {
                        for (auto& m : type_sym->type->named->methods) {
                            if (m.name == fd.name->name) {
                                sema_func_type = m.type;
                                break;
                            }
                        }
                    }
                }
            }
        }

        IRType* ir_func_type = sema_func_type ? map_sema_type(sema_func_type)
                                              : type_map_.void_type();

        auto* func = module_->create_function(builder_.next_id(), ir_func_type, name);
        func->is_method = fd.recv != nullptr;

        // Determine return type
        if (sema_func_type && sema_func_type->kind == sema::TypeKind::Func &&
            sema_func_type->func) {
            if (sema_func_type->func->results.size() == 1) {
                func->return_type = map_sema_type(sema_func_type->func->results[0].type);
            } else if (sema_func_type->func->results.size() > 1) {
                // Build an anonymous struct type for the tuple of return values
                std::vector<IRType*> ret_fields;
                for (const auto& r : sema_func_type->func->results) {
                    ret_fields.push_back(map_sema_type(r.type));
                }
                func->return_type = type_map_.make_tuple_type(std::move(ret_fields));
            } else {
                func->return_type = type_map_.void_type();
            }
        } else {
            func->return_type = type_map_.void_type();
        }

        func_name_map_[name] = func;
        if (func_sym) {
            func_map_[func_sym] = func;
        }
    }
}

void IRGenerator::gen_func_decl(ast::FuncDecl& decl, bool is_method) {
    if (!decl.body) return;

    std::string name = build_func_name(decl);

    auto it = func_name_map_.find(name);
    if (it == func_name_map_.end()) return;

    Function* func = it->second;
    current_func_ = func;
    block_counter_ = 0;
    var_map_.clear();
    loop_stack_.clear();

    auto* entry = func->create_block("entry");
    builder_.set_insert_block(entry);

    auto* func_sym = checker_.decl_symbol(decl.name);
    sema::Type* sema_func_type = nullptr;
    if (func_sym) {
        sema_func_type = func_sym->type;
    } else if (is_method && decl.recv) {
        // Method: look up on the receiver's named type
        auto type_name = get_receiver_type_name(decl);
        if (!type_name.empty()) {
            auto* scope = checker_.package_scope();
            if (scope) {
                auto* type_sym = scope->lookup(type_name);
                if (type_sym && type_sym->type &&
                    type_sym->type->kind == sema::TypeKind::Named &&
                    type_sym->type->named) {
                    for (auto& m : type_sym->type->named->methods) {
                        if (m.name == decl.name->name) {
                            sema_func_type = m.type;
                            break;
                        }
                    }
                }
            }
        }
    }

    // Handle receiver parameter
    if (is_method && decl.recv && decl.recv->fields.count > 0) {
        auto* field = decl.recv->fields[0];

        IRType* recv_ir_type = type_map_.ptr_type();
        if (field->type) {
            auto* texpr = field->type;
            bool is_ptr_recv = texpr->kind == ast::TypeExprKind::Pointer;
            if (is_ptr_recv) {
                recv_ir_type = type_map_.ptr_type();
            } else if (texpr->kind == ast::TypeExprKind::Ident) {
                auto* scope = checker_.package_scope();
                if (scope) {
                    auto* sym = scope->lookup(texpr->ident.name);
                    if (sym && sym->type) {
                        recv_ir_type = map_sema_type(sym->type);
                    }
                }
            }
        }

        auto param = std::make_unique<Value>(builder_.next_id(), recv_ir_type);
        if (field->names.count > 0) {
            param->name = std::string(field->names[0]->name);
        } else {
            param->name = "recv";
        }
        auto* param_ptr = param.get();
        func->params.push_back(std::move(param));

        auto* alloca_inst = builder_.create_alloca(recv_ir_type, param_ptr->name + ".addr");
        builder_.create_store(param_ptr, alloca_inst);

        if (field->names.count > 0) {
            auto* recv_sym = checker_.decl_symbol(field->names[0]);
            if (recv_sym) {
                var_map_[recv_sym] = alloca_inst;
            }
        }
    }

    // Handle regular parameters
    if (sema_func_type && sema_func_type->kind == sema::TypeKind::Func &&
        sema_func_type->func) {
        auto& sema_params = sema_func_type->func->params;

        size_t sema_idx = 0;
        if (decl.type && decl.type->params) {
            for (auto* field : decl.type->params->fields) {
                for (auto* name_expr : field->names) {
                    if (sema_idx >= sema_params.size()) break;

                    auto& sp = sema_params[sema_idx];
                    IRType* param_type = map_sema_type(sp.type);

                    auto param = std::make_unique<Value>(
                        builder_.next_id(), param_type,
                        std::string(name_expr->name));
                    auto* param_ptr = param.get();
                    func->params.push_back(std::move(param));

                    auto* alloca_inst = builder_.create_alloca(
                        param_type, std::string(name_expr->name) + ".addr");
                    builder_.create_store(param_ptr, alloca_inst);

                    auto* psym = checker_.decl_symbol(name_expr);
                    if (psym) {
                        var_map_[psym] = alloca_inst;
                    }

                    ++sema_idx;
                }
            }
        }
    }

    // Generate body
    assert(decl.body && decl.body->kind == ast::StmtKind::Block);
    gen_block(decl.body->block);

    // If the current block has no terminator, add a return
    if (builder_.insert_block() && !builder_.insert_block()->has_terminator()) {
        if (func->return_type && !func->return_type->is_void()) {
            auto* zero = builder_.create_const_int(func->return_type, 0);
            builder_.create_ret(zero);
        } else {
            builder_.create_ret();
        }
    }

    current_func_ = nullptr;
}

void IRGenerator::gen_global_var(ast::VarDecl& decl) {
    for (auto* spec : decl.specs) {
        for (uint32_t i = 0; i < spec->names.count; ++i) {
            auto* name_expr = spec->names[i];
            auto* sym = checker_.decl_symbol(name_expr);
            if (!sym) continue;

            IRType* var_type = map_sema_type(sym->type);
            auto gv = std::make_unique<GlobalVariable>(
                builder_.next_id(), var_type,
                std::string(name_expr->name));
            auto* gv_ptr = gv.get();
            module_->globals.push_back(std::move(gv));
            var_map_[sym] = gv_ptr;
        }
    }
}

} // namespace ir
} // namespace golangc
