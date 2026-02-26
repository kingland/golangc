#include "ir/ir_gen.hpp"
#include "ir/ir_type_map.hpp"

#include <cassert>
#include <charconv>
#include <string>

namespace golangc {
namespace ir {

Value* IRGenerator::gen_expr(ast::Expr* expr) {
    if (!expr) return nullptr;

    switch (expr->kind) {
        case ast::ExprKind::Ident:       return gen_ident(expr);
        case ast::ExprKind::BasicLit:    return gen_basic_lit(expr);
        case ast::ExprKind::Binary:      return gen_binary(expr);
        case ast::ExprKind::Unary:       return gen_unary(expr);
        case ast::ExprKind::Call:        return gen_call(expr);
        case ast::ExprKind::Selector:    return gen_selector(expr);
        case ast::ExprKind::Index:       return gen_index(expr);
        case ast::ExprKind::CompositeLit:return gen_composite_lit(expr);
        case ast::ExprKind::Star:        return gen_star(expr);
        case ast::ExprKind::Paren:       return gen_paren(expr);
        case ast::ExprKind::TypeAssert:  return gen_type_assert(expr);
        case ast::ExprKind::FuncLit:     return gen_func_lit(expr);
        case ast::ExprKind::KeyValue:
            return gen_expr(expr->key_value.value);
        default:
            return builder_.create_const_int(type_map_.i64_type(), 0, "todo");
    }
}

Value* IRGenerator::gen_addr(ast::Expr* expr) {
    if (!expr) return nullptr;

    switch (expr->kind) {
        case ast::ExprKind::Ident: {
            auto* info = expr_info(expr);
            if (info && info->symbol) {
                auto it = var_map_.find(info->symbol);
                if (it != var_map_.end()) {
                    return it->second;
                }
            }
            return nullptr;
        }
        case ast::ExprKind::Selector: {
            auto& sel = expr->selector;
            auto* base_addr = gen_addr(sel.x);
            if (!base_addr) {
                auto* base_val = gen_expr(sel.x);
                if (!base_val) return nullptr;
                base_addr = builder_.create_alloca(base_val->type, "tmp.addr");
                builder_.create_store(base_val, base_addr);
            }

            auto* base_sema_type = expr_type(sel.x);
            if (!base_sema_type) return nullptr;

            auto* ut = sema::underlying(base_sema_type);
            if (ut->kind == sema::TypeKind::Pointer) {
                base_addr = builder_.create_load(base_addr, type_map_.ptr_type(), "deref");
                ut = sema::underlying(ut->pointer.base);
            }

            if (ut->kind == sema::TypeKind::Struct && ut->struct_) {
                uint32_t idx = 0;
                for (const auto& f : ut->struct_->fields) {
                    if (f.name == sel.sel->name) {
                        IRType* field_type = map_sema_type(f.type);
                        return builder_.create_getptr_field(
                            base_addr, idx, field_type,
                            std::string(sel.sel->name) + ".addr");
                    }
                    ++idx;
                }
            }
            return nullptr;
        }
        case ast::ExprKind::Star:
            return gen_expr(expr->star.x);
        case ast::ExprKind::Index: {
            auto* base_info = expr_info(expr->index.x);
            if (!base_info || !base_info->type) return nullptr;
            auto* base_type = sema::underlying(base_info->type);
            if (base_type->kind == sema::TypeKind::Array) {
                auto* base_addr = gen_addr(expr->index.x);
                auto* idx = gen_expr(expr->index.index);
                if (!base_addr || !idx) return nullptr;
                return builder_.create_getptr(base_addr, idx, type_map_.ptr_type(), "elem.addr");
            }
            if (base_type->kind == sema::TypeKind::Slice) {
                auto* s = gen_expr(expr->index.x);
                auto* idx = gen_expr(expr->index.index);
                if (!s || !idx) return nullptr;
                return builder_.create_slice_index_addr(s, idx, "slice.elem.addr");
            }
            return nullptr;
        }
        default:
            return nullptr;
    }
}

Value* IRGenerator::gen_ident(ast::Expr* expr) {
    auto* info = expr_info(expr);
    if (!info || !info->symbol) {
        return builder_.create_const_int(type_map_.i64_type(), 0, "unknown");
    }

    auto* sym = info->symbol;

    // Constants
    if (sym->kind == sema::SymbolKind::Const && sym->const_val) {
        if (sym->const_val->is_int()) {
            return builder_.create_const_int(
                map_sema_type(sym->type), sym->const_val->as_int(), std::string(sym->name));
        }
        if (sym->const_val->is_float()) {
            return builder_.create_const_float(
                map_sema_type(sym->type), sym->const_val->as_float(), std::string(sym->name));
        }
        if (sym->const_val->is_bool()) {
            return builder_.create_const_bool(
                sym->const_val->as_bool(), std::string(sym->name));
        }
        if (sym->const_val->is_string()) {
            return builder_.create_const_string(
                sym->const_val->as_string(), std::string(sym->name));
        }
    }

    // Nil
    if (sym->kind == sema::SymbolKind::Nil) {
        return builder_.create_const_nil(type_map_.ptr_type(), "nil");
    }

    // Functions
    if (sym->kind == sema::SymbolKind::Func) {
        auto fit = func_map_.find(sym);
        if (fit != func_map_.end()) return fit->second;
        auto nit = func_name_map_.find(std::string(sym->name));
        if (nit != func_name_map_.end()) return nit->second;
        return nullptr;
    }

    // Variables: load from alloca
    auto it = var_map_.find(sym);
    if (it != var_map_.end()) {
        IRType* load_type = map_sema_type(sym->type);
        return builder_.create_load(it->second, load_type, std::string(sym->name));
    }

    return builder_.create_const_int(type_map_.i64_type(), 0, "unresolved");
}

Value* IRGenerator::gen_basic_lit(ast::Expr* expr) {
    auto& lit = expr->basic_lit;
    auto* info = expr_info(expr);

    switch (lit.kind) {
        case TokenKind::IntLiteral: {
            int64_t val = 0;
            auto sv = lit.value;
            if (sv.size() >= 2 && sv[0] == '0' && (sv[1] == 'x' || sv[1] == 'X')) {
                (void)std::from_chars(sv.data() + 2, sv.data() + sv.size(), val, 16);
            } else if (sv.size() >= 2 && sv[0] == '0' && (sv[1] == 'o' || sv[1] == 'O')) {
                (void)std::from_chars(sv.data() + 2, sv.data() + sv.size(), val, 8);
            } else if (sv.size() >= 2 && sv[0] == '0' && (sv[1] == 'b' || sv[1] == 'B')) {
                (void)std::from_chars(sv.data() + 2, sv.data() + sv.size(), val, 2);
            } else {
                (void)std::from_chars(sv.data(), sv.data() + sv.size(), val);
            }

            IRType* t = type_map_.i64_type();
            if (info && info->type) {
                t = map_sema_type(info->type);
            }
            return builder_.create_const_int(t, val);
        }
        case TokenKind::FloatLiteral: {
            double val = 0.0;
            auto sv = lit.value;
            (void)std::from_chars(sv.data(), sv.data() + sv.size(), val);
            IRType* t = type_map_.f64_type();
            if (info && info->type) {
                t = map_sema_type(info->type);
            }
            return builder_.create_const_float(t, val);
        }
        case TokenKind::StringLiteral: {
            auto sv = lit.value;
            std::string val;
            if (sv.size() >= 2) {
                val = std::string(sv.substr(1, sv.size() - 2));
            }
            return builder_.create_const_string(val);
        }
        case TokenKind::RuneLiteral: {
            auto sv = lit.value;
            int32_t val = 0;
            if (sv.size() >= 3) {
                if (sv[1] == '\\' && sv.size() >= 4) {
                    switch (sv[2]) {
                        case 'n': val = '\n'; break;
                        case 't': val = '\t'; break;
                        case 'r': val = '\r'; break;
                        case '\\': val = '\\'; break;
                        case '\'': val = '\''; break;
                        case '0': val = '\0'; break;
                        default: val = static_cast<int32_t>(sv[2]); break;
                    }
                } else {
                    val = static_cast<int32_t>(sv[1]);
                }
            }
            return builder_.create_const_int(type_map_.i32_type(), val);
        }
        default:
            return builder_.create_const_int(type_map_.i64_type(), 0);
    }
}

Value* IRGenerator::gen_binary(ast::Expr* expr) {
    auto& binop = expr->binary;

    // Short-circuit for logical operators
    if (binop.op == TokenKind::LogicalAnd) return gen_logical_and(expr);
    if (binop.op == TokenKind::LogicalOr) return gen_logical_or(expr);

    auto* lhs = gen_expr(binop.left);
    auto* rhs = gen_expr(binop.right);
    if (!lhs || !rhs) return builder_.create_const_int(type_map_.i64_type(), 0);

    bool is_float = lhs->type && lhs->type->is_float();

    auto* left_info = expr_info(binop.left);
    bool is_string = left_info && left_info->type && sema::is_string(left_info->type);

    if (is_string && binop.op == TokenKind::Plus) {
        return builder_.create_string_concat(lhs, rhs, "concat");
    }

    switch (binop.op) {
        case TokenKind::Plus:
            return is_float ? builder_.create_fadd(lhs, rhs, "add")
                            : builder_.create_add(lhs, rhs, "add");
        case TokenKind::Minus:
            return is_float ? builder_.create_fsub(lhs, rhs, "sub")
                            : builder_.create_sub(lhs, rhs, "sub");
        case TokenKind::Star:
            return is_float ? builder_.create_fmul(lhs, rhs, "mul")
                            : builder_.create_mul(lhs, rhs, "mul");
        case TokenKind::Slash:
            return is_float ? builder_.create_fdiv(lhs, rhs, "div")
                            : builder_.create_div(lhs, rhs, "div");
        case TokenKind::Percent:
            return builder_.create_rem(lhs, rhs, "rem");
        case TokenKind::Ampersand:
            return builder_.create_and(lhs, rhs, "and");
        case TokenKind::Pipe:
            return builder_.create_or(lhs, rhs, "or");
        case TokenKind::Caret:
            return builder_.create_xor(lhs, rhs, "xor");
        case TokenKind::ShiftLeft:
            return builder_.create_shl(lhs, rhs, "shl");
        case TokenKind::ShiftRight:
            return builder_.create_shr(lhs, rhs, "shr");
        case TokenKind::AmpCaret:
            return builder_.create_andnot(lhs, rhs, "andnot");
        case TokenKind::Equal:
            return is_float ? builder_.create_feq(lhs, rhs, "eq")
                            : builder_.create_eq(lhs, rhs, "eq");
        case TokenKind::NotEqual:
            return is_float ? builder_.create_fne(lhs, rhs, "ne")
                            : builder_.create_ne(lhs, rhs, "ne");
        case TokenKind::Less:
            return is_float ? builder_.create_flt(lhs, rhs, "lt")
                            : builder_.create_lt(lhs, rhs, "lt");
        case TokenKind::LessEqual:
            return is_float ? builder_.create_fle(lhs, rhs, "le")
                            : builder_.create_le(lhs, rhs, "le");
        case TokenKind::Greater:
            return is_float ? builder_.create_fgt(lhs, rhs, "gt")
                            : builder_.create_gt(lhs, rhs, "gt");
        case TokenKind::GreaterEqual:
            return is_float ? builder_.create_fge(lhs, rhs, "ge")
                            : builder_.create_ge(lhs, rhs, "ge");
        default:
            return builder_.create_const_int(type_map_.i64_type(), 0, "todo_binop");
    }
}

Value* IRGenerator::gen_unary(ast::Expr* expr) {
    auto& unary = expr->unary;

    if (unary.op == TokenKind::Arrow) {
        auto* ch = gen_expr(unary.x);
        if (!ch) return builder_.create_const_int(type_map_.i64_type(), 0);
        auto* info = expr_info(expr);
        IRType* result_type = type_map_.i64_type();
        if (info && info->type) result_type = map_sema_type(info->type);
        return builder_.create_chan_recv(ch, result_type, "recv");
    }

    auto* operand = gen_expr(unary.x);
    if (!operand) return builder_.create_const_int(type_map_.i64_type(), 0);

    bool is_float = operand->type && operand->type->is_float();

    switch (unary.op) {
        case TokenKind::Minus:
            return is_float ? builder_.create_fneg(operand, "neg")
                            : builder_.create_neg(operand, "neg");
        case TokenKind::Plus:
            return operand;
        case TokenKind::Not:
            return builder_.create_lognot(operand, "not");
        case TokenKind::Caret:
            return builder_.create_bitnot(operand, "bitnot");
        case TokenKind::Ampersand: {
            auto* addr = gen_addr(unary.x);
            if (addr) return addr;
            auto* alloca_inst = builder_.create_alloca(operand->type, "addr.tmp");
            builder_.create_store(operand, alloca_inst);
            return alloca_inst;
        }
        default:
            return operand;
    }
}

Value* IRGenerator::gen_call(ast::Expr* expr) {
    auto& call = expr->call;

    // Check if this is a builtin call or type conversion by looking up the name
    auto* func_info = expr_info(call.func);

    // For identifiers, also try looking up in scope (checker may not record expr_info for builtins)
    sema::Symbol* func_sym = nullptr;
    if (func_info && func_info->symbol) {
        func_sym = func_info->symbol;
    } else if (call.func && call.func->kind == ast::ExprKind::Ident) {
        auto* scope = checker_.package_scope();
        if (scope) {
            func_sym = scope->lookup(call.func->ident.name);
        }
    }

    if (func_sym && func_sym->kind == sema::SymbolKind::Builtin) {
        sema::ExprInfo builtin_info;
        builtin_info.symbol = func_sym;
        return gen_builtin_call(expr, &builtin_info);
    }

    if (func_sym && func_sym->kind == sema::SymbolKind::Type) {
        if (call.args.count == 1) {
            auto* val = gen_expr(call.args[0]);
            if (!val) return builder_.create_const_int(type_map_.i64_type(), 0);
            IRType* target_type = map_sema_type(func_sym->type);
            if (val->type == target_type) return val;

            bool src_float = val->type && val->type->is_float();
            bool dst_float = target_type && target_type->is_float();
            bool src_int = val->type && val->type->is_integer();
            bool dst_int = target_type && target_type->is_integer();

            if (src_int && dst_float) return builder_.create_sitofp(val, target_type, "conv");
            if (src_float && dst_int) return builder_.create_fptosi(val, target_type, "conv");
            if (src_float && dst_float) {
                if (target_type->kind == IRTypeKind::F64)
                    return builder_.create_fpext(val, target_type, "conv");
                return builder_.create_fptrunc(val, target_type, "conv");
            }
            return builder_.create_bitcast(val, target_type, "conv");
        }
    }

    // Method call detection
    std::string method_name;
    Value* receiver = nullptr;
    bool is_interface_call = false;

    if (call.func->kind == ast::ExprKind::Selector) {
        auto& sel = call.func->selector;
        auto* base_info = expr_info(sel.x);
        if (base_info && base_info->type) {
            auto* base_type = sema::underlying(base_info->type);
            bool is_ptr = base_type->kind == sema::TypeKind::Pointer;
            auto* concrete = is_ptr ? sema::underlying(base_type->pointer.base) : base_type;

            // Check if calling a method on an interface value
            if (concrete->kind == sema::TypeKind::Interface) {
                is_interface_call = true;
                receiver = gen_expr(sel.x);
                // For interface method calls, we need to find a concrete implementation.
                // Search func_name_map_ for any "TypeName.MethodName" that matches.
                std::string method_sel(sel.sel->name);
                for (auto& [fname, fval] : func_name_map_) {
                    auto dot = fname.find('.');
                    if (dot != std::string::npos && fname.substr(dot + 1) == method_sel) {
                        method_name = fname;
                        break;
                    }
                }
            } else {
                std::string type_name;
                if (base_info->type->kind == sema::TypeKind::Named) {
                    type_name = std::string(base_info->type->named->name);
                } else if (is_ptr && base_info->type->pointer.base->kind == sema::TypeKind::Named) {
                    type_name = std::string(base_info->type->pointer.base->named->name);
                }

                if (!type_name.empty()) {
                    method_name = type_name + "." + std::string(sel.sel->name);
                    receiver = gen_expr(sel.x);
                }
            }
        }
    }

    Value* callee = nullptr;
    if (!method_name.empty()) {
        auto it = func_name_map_.find(method_name);
        if (it != func_name_map_.end()) callee = it->second;
    }
    if (!callee) callee = gen_expr(call.func);
    if (!callee) return builder_.create_const_int(type_map_.i64_type(), 0, "bad_call");

    // For interface method calls, the receiver is the interface value's data.
    // Extract the concrete value from the interface before passing as receiver.
    if (is_interface_call && receiver) {
        // Extract the data portion from the interface value
        auto* iface_data = builder_.create_interface_data(receiver, type_map_.i64_type(), "iface.data");
        receiver = iface_data;
    }

    std::vector<Value*> args;
    if (receiver) args.push_back(receiver);

    // Get the callee's function type to detect interface parameters
    const sema::FuncType* callee_sema_func = nullptr;
    auto* callee_func_info = expr_info(call.func);
    if (callee_func_info && callee_func_info->type) {
        auto* ft = sema::underlying(callee_func_info->type);
        if (ft && ft->kind == sema::TypeKind::Func) {
            callee_sema_func = ft->func;
        }
    }

    for (uint32_t i = 0; i < call.args.count; ++i) {
        auto* arg = gen_expr(call.args[i]);
        if (!arg) continue;

        // Check if this argument needs interface boxing
        if (callee_sema_func && i < callee_sema_func->params.size()) {
            auto* param_type = sema::underlying(callee_sema_func->params[i].type);
            auto* arg_info = expr_info(call.args[i]);
            if (param_type && param_type->kind == sema::TypeKind::Interface &&
                arg_info && arg_info->type &&
                sema::underlying(arg_info->type)->kind != sema::TypeKind::Interface) {
                // Box the concrete value into an interface: InterfaceMake(type_tag, value)
                auto* type_tag = builder_.create_const_int(type_map_.i64_type(), 1, "type.tag");
                arg = builder_.create_interface_make(type_tag, arg, "iface");
            }
        }
        args.push_back(arg);
    }

    // For indirect calls (function-typed variables / closures), append the env ptr.
    // Direct IR Function* calls are never fat closures.
    bool is_indirect = (dynamic_cast<const ir::Function*>(callee) == nullptr);
    if (is_indirect && !is_interface_call) {
        auto* env = builder_.create_closure_env(callee, ".env");
        args.push_back(env);
    }

    auto* call_info = expr_info(expr);
    IRType* result_type = type_map_.void_type();
    if (call_info && call_info->type) result_type = map_sema_type(call_info->type);

    return builder_.create_call(callee, args, result_type, "call");
}

Value* IRGenerator::gen_selector(ast::Expr* expr) {
    auto& sel = expr->selector;
    auto* addr = gen_addr(expr);
    if (addr) {
        auto* info = expr_info(expr);
        IRType* field_type = type_map_.i64_type();
        if (info && info->type) field_type = map_sema_type(info->type);
        return builder_.create_load(addr, field_type, std::string(sel.sel->name));
    }
    return gen_expr(sel.x);
}

Value* IRGenerator::gen_index(ast::Expr* expr) {
    auto& idx = expr->index;
    auto* info = expr_info(expr);

    auto* base_info = expr_info(idx.x);
    if (!base_info || !base_info->type) {
        return builder_.create_const_int(type_map_.i64_type(), 0);
    }

    auto* base_type = sema::underlying(base_info->type);

    if (base_type->kind == sema::TypeKind::Map) {
        auto* m = gen_expr(idx.x);
        auto* key = gen_expr(idx.index);
        IRType* val_type = map_sema_type(base_type->map.value);
        return builder_.create_map_get(m, key, val_type, "map_get");
    }

    if (base_type->kind == sema::TypeKind::Slice) {
        auto* s = gen_expr(idx.x);
        auto* i = gen_expr(idx.index);
        IRType* elem_type = map_sema_type(base_type->slice.element);
        return builder_.create_slice_index(s, i, elem_type, "slice_idx");
    }

    if (sema::is_string(base_info->type)) {
        auto* s = gen_expr(idx.x);
        auto* i = gen_expr(idx.index);
        return builder_.create_string_index(s, i, "str_idx");
    }

    auto* addr = gen_addr(expr);
    if (addr) {
        IRType* elem_type = type_map_.i64_type();
        if (info && info->type) elem_type = map_sema_type(info->type);
        return builder_.create_load(addr, elem_type, "arr_idx");
    }

    return builder_.create_const_int(type_map_.i64_type(), 0);
}

Value* IRGenerator::gen_composite_lit(ast::Expr* expr) {
    auto& lit = expr->composite_lit;
    auto* info = expr_info(expr);

    if (!info || !info->type) {
        return builder_.create_const_int(type_map_.i64_type(), 0);
    }

    auto* sema_type = sema::underlying(info->type);
    IRType* ir_type = map_sema_type(info->type);

    if (sema_type->kind == sema::TypeKind::Struct && sema_type->struct_) {
        auto* alloca_inst = builder_.create_alloca(ir_type, "lit.addr");

        for (uint32_t i = 0; i < lit.elts.count; ++i) {
            auto* elt = lit.elts[i];
            Value* val = nullptr;
            uint32_t field_idx = i;

            if (elt->kind == ast::ExprKind::KeyValue) {
                auto& kv = elt->key_value;
                if (kv.key && kv.key->kind == ast::ExprKind::Ident) {
                    for (uint32_t j = 0; j < sema_type->struct_->fields.size(); ++j) {
                        if (sema_type->struct_->fields[j].name == kv.key->ident.name) {
                            field_idx = j;
                            break;
                        }
                    }
                }
                val = gen_expr(kv.value);
            } else {
                val = gen_expr(elt);
            }

            if (!val) continue;

            IRType* field_type = val->type;
            if (field_idx < sema_type->struct_->fields.size()) {
                field_type = map_sema_type(sema_type->struct_->fields[field_idx].type);
            }
            auto* field_addr = builder_.create_getptr_field(
                alloca_inst, field_idx, field_type, "field.addr");
            builder_.create_store(val, field_addr);
        }

        return builder_.create_load(alloca_inst, ir_type, "lit");
    }

    return builder_.create_const_nil(ir_type, "lit.todo");
}

Value* IRGenerator::gen_star(ast::Expr* expr) {
    auto* ptr = gen_expr(expr->star.x);
    if (!ptr) return builder_.create_const_int(type_map_.i64_type(), 0);

    auto* info = expr_info(expr);
    IRType* result_type = type_map_.i64_type();
    if (info && info->type) result_type = map_sema_type(info->type);
    return builder_.create_load(ptr, result_type, "deref");
}

Value* IRGenerator::gen_paren(ast::Expr* expr) {
    return gen_expr(expr->paren.x);
}

Value* IRGenerator::gen_type_assert(ast::Expr* expr) {
    auto* iface = gen_expr(expr->type_assert.x);
    if (!iface) return builder_.create_const_int(type_map_.i64_type(), 0);

    auto* info = expr_info(expr);
    IRType* result_type = type_map_.ptr_type();
    if (info && info->type) result_type = map_sema_type(info->type);
    return builder_.create_interface_data(iface, result_type, "assert");
}

// ---------------------------------------------------------------------------
// Capture collection helpers
// ---------------------------------------------------------------------------

void IRGenerator::collect_captures_expr(
        ast::Expr* expr,
        const std::unordered_map<const sema::Symbol*, Value*>& outer_map,
        const std::unordered_set<const sema::Symbol*>& inner_params,
        std::vector<const sema::Symbol*>& captures) {
    if (!expr) return;
    switch (expr->kind) {
        case ast::ExprKind::Ident: {
            auto* info = expr_info(expr);
            if (info && info->symbol) {
                auto* sym = info->symbol;
                if (inner_params.count(sym)) break; // own param
                if (outer_map.count(sym)) {
                    // Check not already added
                    bool found = false;
                    for (auto* c : captures) if (c == sym) { found = true; break; }
                    if (!found) captures.push_back(sym);
                }
            }
            break;
        }
        case ast::ExprKind::Binary:
            collect_captures_expr(expr->binary.left, outer_map, inner_params, captures);
            collect_captures_expr(expr->binary.right, outer_map, inner_params, captures);
            break;
        case ast::ExprKind::Unary:
        case ast::ExprKind::Star:
            collect_captures_expr(expr->unary.x, outer_map, inner_params, captures);
            break;
        case ast::ExprKind::Paren:
            collect_captures_expr(expr->paren.x, outer_map, inner_params, captures);
            break;
        case ast::ExprKind::Call:
            collect_captures_expr(expr->call.func, outer_map, inner_params, captures);
            for (auto* a : expr->call.args)
                collect_captures_expr(a, outer_map, inner_params, captures);
            break;
        case ast::ExprKind::Selector:
            collect_captures_expr(expr->selector.x, outer_map, inner_params, captures);
            break;
        case ast::ExprKind::Index:
            collect_captures_expr(expr->index.x, outer_map, inner_params, captures);
            collect_captures_expr(expr->index.index, outer_map, inner_params, captures);
            break;
        case ast::ExprKind::KeyValue:
            collect_captures_expr(expr->key_value.key, outer_map, inner_params, captures);
            collect_captures_expr(expr->key_value.value, outer_map, inner_params, captures);
            break;
        case ast::ExprKind::CompositeLit:
            for (auto* e : expr->composite_lit.elts)
                collect_captures_expr(e, outer_map, inner_params, captures);
            break;
        case ast::ExprKind::FuncLit:
            // Don't recurse into nested func lits — they'll handle their own captures
            break;
        default: break;
    }
}

void IRGenerator::collect_captures(
        ast::Stmt* stmt,
        const std::unordered_map<const sema::Symbol*, Value*>& outer_map,
        const std::unordered_set<const sema::Symbol*>& inner_params,
        std::vector<const sema::Symbol*>& captures) {
    if (!stmt) return;
    switch (stmt->kind) {
        case ast::StmtKind::Block:
            for (auto* s : stmt->block.stmts.span())
                collect_captures(s, outer_map, inner_params, captures);
            break;
        case ast::StmtKind::Return:
            for (auto* v : stmt->return_.results.span())
                collect_captures_expr(v, outer_map, inner_params, captures);
            break;
        case ast::StmtKind::Expr:
            collect_captures_expr(stmt->expr.x, outer_map, inner_params, captures);
            break;
        case ast::StmtKind::Assign:
            for (auto* l : stmt->assign.lhs.span())
                collect_captures_expr(l, outer_map, inner_params, captures);
            for (auto* r : stmt->assign.rhs.span())
                collect_captures_expr(r, outer_map, inner_params, captures);
            break;
        case ast::StmtKind::ShortVarDecl:
            for (auto* r : stmt->short_var_decl.rhs.span())
                collect_captures_expr(r, outer_map, inner_params, captures);
            break;
        case ast::StmtKind::If:
            collect_captures(stmt->if_.body, outer_map, inner_params, captures);
            collect_captures(stmt->if_.else_body, outer_map, inner_params, captures);
            collect_captures_expr(stmt->if_.cond, outer_map, inner_params, captures);
            break;
        case ast::StmtKind::For:
            collect_captures_expr(stmt->for_.cond, outer_map, inner_params, captures);
            collect_captures(stmt->for_.body, outer_map, inner_params, captures);
            break;
        case ast::StmtKind::IncDec:
            collect_captures_expr(stmt->inc_dec.x, outer_map, inner_params, captures);
            break;
        default: break;
    }
}

// ---------------------------------------------------------------------------
// gen_func_lit
// ---------------------------------------------------------------------------

Value* IRGenerator::gen_func_lit(ast::Expr* expr) {
    // Generate a func literal as a nested top-level function.
    // If the body captures variables from the enclosing scope, malloc an env
    // struct, store captures into it, and pass it as a hidden last parameter.
    auto& fl = expr->func_lit;
    if (!fl.body) return builder_.create_const_nil(type_map_.ptr_type(), "closure.nil");

    // Build a unique name: <enclosing>$lit<N>
    std::string parent_name = current_func_ ? current_func_->name : "anon";
    std::string lit_name = parent_name + "$lit" + std::to_string(func_lit_counter_++);

    // Resolve the func literal's type (from sema)
    const auto* info = expr_info(expr);
    sema::Type* sema_ft = info ? info->type : nullptr;

    // Create an IR function for the literal
    IRType* ir_func_type = sema_ft ? map_sema_type(sema_ft) : type_map_.void_type();
    auto* lit_func = module_->create_function(builder_.next_id(), ir_func_type, lit_name);

    // Determine return type
    if (sema_ft && sema_ft->kind == sema::TypeKind::Func && sema_ft->func) {
        if (sema_ft->func->results.size() == 1) {
            lit_func->return_type = map_sema_type(sema_ft->func->results[0].type);
        } else if (sema_ft->func->results.size() > 1) {
            std::vector<IRType*> ret_fields;
            for (const auto& r : sema_ft->func->results)
                ret_fields.push_back(map_sema_type(r.type));
            lit_func->return_type = type_map_.make_tuple_type(std::move(ret_fields));
        } else {
            lit_func->return_type = type_map_.void_type();
        }
    } else {
        lit_func->return_type = type_map_.void_type();
    }

    // ------------------------------------------------------------------
    // Collect captured variables (outer-scope vars referenced in body)
    // ------------------------------------------------------------------
    // Build the set of inner param symbols first
    std::unordered_set<const sema::Symbol*> inner_param_syms;
    if (fl.type && fl.type->func.params) {
        for (auto* field : fl.type->func.params->fields) {
            for (auto* name_node : field->names) {
                if (name_node) {
                    auto* sym = checker_.decl_symbol(name_node);
                    if (sym) inner_param_syms.insert(sym);
                }
            }
        }
    }

    std::vector<const sema::Symbol*> captures;
    if (fl.body) {
        collect_captures(fl.body, var_map_, inner_param_syms, captures);
    }

    // Save current generation context
    auto* saved_func      = current_func_;
    auto  saved_var_map   = var_map_;
    auto  saved_loops     = loop_stack_;
    auto  saved_counter   = block_counter_;
    auto* saved_block     = builder_.insert_block();

    // Set up context for the literal body (start fresh — no outer vars directly)
    current_func_ = lit_func;
    block_counter_ = 0;
    loop_stack_.clear();
    var_map_.clear();   // inner function has its own scope

    auto* entry = lit_func->create_block("entry");
    builder_.set_insert_block(entry);

    // Bind explicit parameters from the sema type
    if (sema_ft && sema_ft->kind == sema::TypeKind::Func && sema_ft->func) {
        const auto& params = sema_ft->func->params;
        size_t param_idx = 0;
        if (fl.type && fl.type->func.params) {
            for (auto* field : fl.type->func.params->fields) {
                if (param_idx >= params.size()) break;
                if (field->names.count > 0) {
                    for (auto* name_ident : field->names) {
                        if (param_idx >= params.size()) break;
                        IRType* pt = map_sema_type(params[param_idx].type);
                        auto pval = std::make_unique<Value>(builder_.next_id(), pt);
                        pval->name = name_ident ? std::string(name_ident->name) : "p";
                        auto* pv = pval.get();
                        lit_func->params.push_back(std::move(pval));
                        auto* alloca_p = builder_.create_alloca(pt, pv->name + ".addr");
                        builder_.create_store(pv, alloca_p);
                        if (name_ident) {
                            auto* sym = checker_.decl_symbol(name_ident);
                            if (sym) var_map_[sym] = alloca_p;
                        }
                        ++param_idx;
                    }
                } else {
                    IRType* pt = map_sema_type(params[param_idx].type);
                    auto pval = std::make_unique<Value>(builder_.next_id(), pt);
                    pval->name = "p";
                    lit_func->params.push_back(std::move(pval));
                    ++param_idx;
                }
            }
        }
    }

    // ------------------------------------------------------------------
    // If there are captures, add env ptr as hidden last param, then load
    // each capture from env[i*8].
    // ------------------------------------------------------------------
    Value* env_ptr_val = nullptr; // allocated in outer function, below

    if (!captures.empty()) {
        lit_func->has_env = true;

        // Add hidden env param
        auto env_pval = std::make_unique<Value>(builder_.next_id(), type_map_.ptr_type());
        env_pval->name = ".env";
        auto* env_pv = env_pval.get();
        lit_func->params.push_back(std::move(env_pval));

        // Alloca for env ptr
        auto* env_alloca = builder_.create_alloca(type_map_.ptr_type(), ".env.addr");
        builder_.create_store(env_pv, env_alloca);
        auto* env = builder_.create_load(env_alloca, type_map_.ptr_type(), ".env");

        // For each captured symbol, load from env[i*8]
        for (size_t ci = 0; ci < captures.size(); ++ci) {
            auto* sym = captures[ci];
            IRType* cap_type = map_sema_type(sym->type);
            // env is a ptr to an array of QWORDs; compute &env[ci]
            auto* offset = builder_.create_const_int(type_map_.i64_type(), (int64_t)(ci * 8));
            auto* field_ptr = builder_.create_getptr(env, offset, type_map_.ptr_type(),
                                                     std::string(sym->name) + ".cap.addr");
            // Alloca for local copy
            auto* cap_alloca = builder_.create_alloca(cap_type,
                                                      std::string(sym->name) + ".cap");
            auto* loaded = builder_.create_load(field_ptr, cap_type,
                                                std::string(sym->name) + ".cap.val");
            builder_.create_store(loaded, cap_alloca);
            var_map_[sym] = cap_alloca;
        }
    }

    // Generate body
    if (fl.body->kind == ast::StmtKind::Block) {
        gen_block(fl.body->block);
    }

    // Ensure terminator
    if (!builder_.insert_block()->has_terminator()) {
        builder_.create_ret(nullptr);
    }

    // Restore outer context
    current_func_ = saved_func;
    var_map_      = saved_var_map;
    loop_stack_   = saved_loops;
    block_counter_= saved_counter;
    builder_.set_insert_block(saved_block);

    // ------------------------------------------------------------------
    // In the outer function: if captures exist, malloc env and fill it.
    // ------------------------------------------------------------------
    if (!captures.empty()) {
        // malloc(N * 8)
        int64_t env_size = (int64_t)(captures.size() * 8);
        auto* size_val = builder_.create_const_int(type_map_.i64_type(), env_size, "env.size");
        env_ptr_val = builder_.create_malloc(size_val, "env");

        for (size_t ci = 0; ci < captures.size(); ++ci) {
            auto* sym = captures[ci];
            IRType* cap_type = map_sema_type(sym->type);
            // Load current value from outer var_map_
            auto it = var_map_.find(sym);
            Value* val = nullptr;
            if (it != var_map_.end()) {
                val = builder_.create_load(it->second, cap_type, std::string(sym->name));
            }
            if (!val) val = builder_.create_const_int(cap_type, 0);
            // Store into env[ci]
            auto* offset = builder_.create_const_int(type_map_.i64_type(), (int64_t)(ci * 8));
            auto* field_ptr = builder_.create_getptr(env_ptr_val, offset, type_map_.ptr_type(),
                                                     std::string(sym->name) + ".env.addr");
            builder_.create_store(val, field_ptr);
        }
    }

    return builder_.create_closure_make(lit_func, env_ptr_val, lit_name + ".val");
}

Value* IRGenerator::gen_builtin_call(ast::Expr* expr, const sema::ExprInfo* func_info) {
    auto& call = expr->call;
    auto* call_info = expr_info(expr);
    auto builtin_name = func_info->symbol->name;

    if (builtin_name == "println" || builtin_name == "print") {
        std::vector<Value*> args;
        for (auto* arg_expr : call.args) {
            auto* arg = gen_expr(arg_expr);
            if (arg) args.push_back(arg);
        }
        return builder_.create_println(args);
    }

    if (builtin_name == "len") {
        if (call.args.count >= 1) {
            auto* arg = gen_expr(call.args[0]);
            auto* arg_info = expr_info(call.args[0]);
            if (arg && arg_info && arg_info->type) {
                auto* t = sema::underlying(arg_info->type);
                if (t->kind == sema::TypeKind::Slice)
                    return builder_.create_slice_len(arg, "len");
                if (sema::is_string(arg_info->type))
                    return builder_.create_string_len(arg, "len");
                if (t->kind == sema::TypeKind::Array)
                    return builder_.create_const_int(type_map_.i64_type(), t->array.length, "len");
                if (t->kind == sema::TypeKind::Map)
                    return builder_.create_map_len(arg, "len");
            }
        }
        return builder_.create_const_int(type_map_.i64_type(), 0, "len");
    }

    if (builtin_name == "cap") {
        if (call.args.count >= 1) {
            auto* arg = gen_expr(call.args[0]);
            if (arg) return builder_.create_slice_cap(arg, "cap");
        }
        return builder_.create_const_int(type_map_.i64_type(), 0, "cap");
    }

    if (builtin_name == "make") {
        if (call_info && call_info->type) {
            auto* t = sema::underlying(call_info->type);
            if (t->kind == sema::TypeKind::Chan) {
                // Compute element size statically
                int64_t elem_size = 8;
                if (t->chan.element) {
                    auto* elem_ir = map_sema_type(t->chan.element);
                    if (elem_ir) {
                        switch (elem_ir->kind) {
                            case ir::IRTypeKind::I8:  elem_size = 1; break;
                            case ir::IRTypeKind::I16: elem_size = 2; break;
                            case ir::IRTypeKind::I32: elem_size = 4; break;
                            default:                  elem_size = 8; break;
                        }
                    }
                }
                return builder_.create_chan_make(type_map_.ptr_type(), elem_size, "make");
            }
            if (t->kind == sema::TypeKind::Map) {
                int64_t key_sz = 8, val_sz = 8;
                if (t->map.key)   key_sz = IRTypeMap::type_size(map_sema_type(t->map.key));
                if (t->map.value) val_sz = IRTypeMap::type_size(map_sema_type(t->map.value));
                return builder_.create_map_make(type_map_.ptr_type(), key_sz, val_sz, "make");
            }
            if (t->kind == sema::TypeKind::Slice) {
                Value* len_val = (call.args.count >= 2)
                    ? gen_expr(call.args[1])
                    : builder_.create_const_int(type_map_.i64_type(), 0);
                Value* cap_val = (call.args.count >= 3)
                    ? gen_expr(call.args[2])
                    : len_val;
                return builder_.create_slice_make(type_map_.slice_type(), len_val, cap_val, "make");
            }
        }
        return builder_.create_const_nil(type_map_.ptr_type(), "make");
    }

    if (builtin_name == "new") {
        return builder_.create_alloca(type_map_.i64_type(), "new");
    }

    if (builtin_name == "append") {
        if (call.args.count >= 2) {
            auto* slice = gen_expr(call.args[0]);
            auto* elem  = gen_expr(call.args[1]);
            if (slice && elem) {
                IRType* elem_type = elem->type ? elem->type : type_map_.i64_type();
                return builder_.create_slice_append(slice, elem, elem_type, "append");
            }
        }
        if (call.args.count >= 1) return gen_expr(call.args[0]);
        return builder_.create_const_nil(type_map_.slice_type(), "append");
    }

    if (builtin_name == "delete") {
        if (call.args.count >= 2) {
            auto* m   = gen_expr(call.args[0]);
            auto* key = gen_expr(call.args[1]);
            if (m && key) builder_.create_map_delete(m, key);
        }
        return builder_.create_const_int(type_map_.i64_type(), 0, "delete");
    }

    if (builtin_name == "panic") {
        Value* arg = nullptr;
        if (call.args.count >= 1) arg = gen_expr(call.args[0]);
        return builder_.create_panic(arg);
    }

    if (builtin_name == "recover") {
        return builder_.create_const_nil(type_map_.interface_type(), "recover");
    }

    return builder_.create_const_int(type_map_.i64_type(), 0, "builtin.todo");
}

Value* IRGenerator::gen_logical_and(ast::Expr* expr) {
    auto& binop = expr->binary;
    auto* lhs = gen_expr(binop.left);
    if (!lhs) return builder_.create_const_bool(false);

    auto* rhs_bb = current_func_->create_block(fresh_block_name("and.rhs"));
    auto* merge_bb = current_func_->create_block(fresh_block_name("and.merge"));

    builder_.create_condbr(lhs, rhs_bb, merge_bb);

    builder_.set_insert_block(rhs_bb);
    auto* rhs = gen_expr(binop.right);
    if (!rhs) rhs = builder_.create_const_bool(false);
    builder_.create_br(merge_bb);

    builder_.set_insert_block(merge_bb);
    return rhs ? rhs : builder_.create_const_bool(false);
}

Value* IRGenerator::gen_logical_or(ast::Expr* expr) {
    auto& binop = expr->binary;
    auto* lhs = gen_expr(binop.left);
    if (!lhs) return builder_.create_const_bool(true);

    auto* rhs_bb = current_func_->create_block(fresh_block_name("or.rhs"));
    auto* merge_bb = current_func_->create_block(fresh_block_name("or.merge"));

    builder_.create_condbr(lhs, merge_bb, rhs_bb);

    builder_.set_insert_block(rhs_bb);
    auto* rhs = gen_expr(binop.right);
    if (!rhs) rhs = builder_.create_const_bool(true);
    builder_.create_br(merge_bb);

    builder_.set_insert_block(merge_bb);
    return rhs ? rhs : builder_.create_const_bool(true);
}

} // namespace ir
} // namespace golangc
