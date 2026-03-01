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
            if (is_string)
                return builder_.create_string_eq(lhs, rhs, "seq");
            return is_float ? builder_.create_feq(lhs, rhs, "eq")
                            : builder_.create_eq(lhs, rhs, "eq");
        case TokenKind::NotEqual:
            if (is_string) {
                auto* eq = builder_.create_string_eq(lhs, rhs, "seq");
                return builder_.create_lognot(eq, "sne");
            }
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

    // Type conversion via type-expression carrier: []byte(s), []string(x), etc.
    // The parser represents these as CompositeLit with a TypeExpr but no elements.
    if (call.func && call.func->kind == ast::ExprKind::CompositeLit &&
        call.func->composite_lit.type != nullptr &&
        call.func->composite_lit.elts.count == 0 &&
        call.func->composite_lit.lbrace.filename.empty()) {
        auto* call_info = expr_info(expr);
        IRType* target_ir = call_info ? map_sema_type(call_info->type) : nullptr;
        if (!target_ir) target_ir = type_map_.slice_type();
        if (call.args.count == 1) {
            auto* val = gen_expr(call.args[0]);
            if (!val) return builder_.create_const_nil(target_ir, "conv.nil");
            if (val->type == target_ir) return val;
            return builder_.create_bitcast(val, target_ir, "typeconv");
        }
        return builder_.create_const_nil(target_ir, "conv.nil");
    }

    if (func_sym && func_sym->kind == sema::SymbolKind::Type) {
        if (call.args.count == 1) {
            auto* val = gen_expr(call.args[0]);
            if (!val) return builder_.create_const_int(type_map_.i64_type(), 0);
            IRType* target_type = map_sema_type(func_sym->type);

            // Special case: string(int) — rune to string via golangc_rune_to_string
            if (func_sym->type && sema::is_string(func_sym->type)) {
                auto* arg_sema = expr_info(call.args[0]);
                bool src_is_int = arg_sema && arg_sema->type &&
                                  sema::underlying(arg_sema->type)->kind == sema::TypeKind::Basic &&
                                  sema::is_integer(arg_sema->type);
                if (src_is_int) {
                    auto* fn = get_or_declare_runtime("golangc_rune_to_string",
                                                       type_map_.string_type());
                    // Truncate to i32 if needed (rune is int32)
                    Value* rune_val = val;
                    if (val->type && val->type->kind != IRTypeKind::I32) {
                        rune_val = builder_.create_bitcast(val, type_map_.i32_type(), "rune");
                    }
                    return builder_.create_call(fn, {rune_val},
                                                type_map_.string_type(), "rune2str");
                }
            }

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
        auto* func_sel_info = expr_info(call.func);
        bool needs_addr = func_sel_info && func_sel_info->needs_addr_for_recv;

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
                    if (needs_addr) {
                        // Pointer-receiver method on a value: pass &recv
                        receiver = gen_addr(sel.x);
                        if (!receiver) {
                            // Not addressable (e.g. constant) — alloca+store
                            auto* val = gen_expr(sel.x);
                            if (val) {
                                receiver = builder_.create_alloca(val->type, "recv.addr");
                                builder_.create_store(val, receiver);
                            }
                        }
                    } else {
                        receiver = gen_expr(sel.x);
                    }
                }
            }
        }
    }

    Value* callee = nullptr;
    if (!method_name.empty()) {
        auto it = func_name_map_.find(method_name);
        if (it != func_name_map_.end()) callee = it->second;

        // strings.Builder method dispatch — map to runtime functions
        if (!callee) {
            struct BuilderMethod { const char* suffix; const char* runtime_fn; bool returns_string; bool returns_int; };
            static const BuilderMethod builder_methods[] = {
                {"strings.Builder.WriteString", "golangc_builder_write_string", false, false},
                {"strings.Builder.WriteByte",   "golangc_builder_write_byte",   false, false},
                {"strings.Builder.String",      "golangc_builder_string",       true,  false},
                {"strings.Builder.Reset",       "golangc_builder_reset",        false, false},
                {"strings.Builder.Len",         "golangc_builder_len",          false, true},
            };
            for (const auto& bm : builder_methods) {
                if (method_name == bm.suffix) {
                    IRType* ret = bm.returns_string ? type_map_.string_type()
                                : bm.returns_int    ? type_map_.i64_type()
                                                    : type_map_.void_type();
                    callee = get_or_declare_runtime(bm.runtime_fn, ret);
                    break;
                }
            }
        }
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

    // Determine variadic info for this call
    bool callee_is_variadic = callee_sema_func && callee_sema_func->is_variadic;
    // Number of fixed (non-variadic) params (receiver already consumed from args if present)
    uint32_t fixed_param_count = callee_sema_func
        ? static_cast<uint32_t>(callee_sema_func->params.size()) - (callee_is_variadic ? 1 : 0)
        : call.args.count;
    // Adjust for receiver already in args
    uint32_t receiver_offset = receiver ? 1u : 0u;

    for (uint32_t i = 0; i < call.args.count; ++i) {
        // For variadic calls, stop the normal loop at the last fixed param
        if (callee_is_variadic && i >= fixed_param_count) break;

        auto* arg = gen_expr(call.args[i]);
        if (!arg) continue;

        // Check if this argument needs interface boxing
        if (callee_sema_func && i < callee_sema_func->params.size()) {
            auto* param_type = sema::underlying(callee_sema_func->params[i].type);
            auto* arg_info = expr_info(call.args[i]);
            if (param_type && param_type->kind == sema::TypeKind::Interface &&
                arg_info && arg_info->type &&
                sema::underlying(arg_info->type)->kind != sema::TypeKind::Interface) {
                int64_t tag_val = type_id_for(sema::underlying(arg_info->type));
                auto* type_tag = builder_.create_const_int(type_map_.i64_type(), tag_val, "type.tag");
                arg = builder_.create_interface_make(type_tag, arg, "iface");
            }
        }
        args.push_back(arg);
    }

    // Handle variadic trailing arguments
    if (callee_is_variadic) {
        // Determine element type of the variadic slice param
        IRType* elem_type = type_map_.i64_type();
        IRType* slice_type = type_map_.slice_type();
        auto& last_param = callee_sema_func->params.back();
        if (last_param.type && last_param.type->kind == sema::TypeKind::Slice) {
            elem_type = map_sema_type(last_param.type->slice.element);
            slice_type = map_sema_type(last_param.type);
        }

        if (call.has_ellipsis && call.args.count > 0) {
            // Spread: f(s...) — pass the slice directly
            auto* spread_arg = gen_expr(call.args[call.args.count - 1]);
            if (spread_arg) args.push_back(spread_arg);
        } else {
            // Pack trailing args into a new slice
            uint32_t variadic_count = call.args.count > fixed_param_count
                ? call.args.count - fixed_param_count : 0;
            auto* len_val = builder_.create_const_int(type_map_.i64_type(),
                                                       static_cast<int64_t>(variadic_count), "var.len");
            auto* slice = builder_.create_slice_make(slice_type, len_val, len_val, "var.slice");

            for (uint32_t i = fixed_param_count; i < call.args.count; ++i) {
                auto* elem = gen_expr(call.args[i]);
                if (!elem) continue;
                // Interface-box if needed
                if (elem_type && elem_type->kind == IRTypeKind::Struct) { // interface layout
                    auto* arg_info = expr_info(call.args[i]);
                    if (arg_info && arg_info->type &&
                        sema::underlying(arg_info->type)->kind != sema::TypeKind::Interface) {
                        int64_t tag_val = type_id_for(sema::underlying(arg_info->type));
                        auto* type_tag = builder_.create_const_int(type_map_.i64_type(), tag_val, "type.tag");
                        elem = builder_.create_interface_make(type_tag, elem, "iface");
                    }
                }
                slice = builder_.create_slice_append(slice, elem, elem_type, "var.append");
            }
            args.push_back(slice);
        }
    }
    (void)receiver_offset;

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

    // Check for pseudo-package value access (e.g. os.Args which is not a call).
    auto* info = expr_info(expr);
    if (info && info->symbol && info->symbol->kind == sema::SymbolKind::Builtin) {
        auto bname = info->symbol->name;
        if (bname == "os.Args") {
            auto* fn = get_or_declare_runtime("golangc_os_args_get", type_map_.slice_type());
            return builder_.create_call(fn, {}, type_map_.slice_type(), "os.Args");
        }
        if (bname == "os.Stdout") {
            auto* fn = get_or_declare_runtime("golangc_os_stdout", type_map_.ptr_type());
            return builder_.create_call(fn, {}, type_map_.ptr_type(), "os.Stdout");
        }
        if (bname == "os.Stderr") {
            auto* fn = get_or_declare_runtime("golangc_os_stderr", type_map_.ptr_type());
            return builder_.create_call(fn, {}, type_map_.ptr_type(), "os.Stderr");
        }
        if (bname == "os.Stdin") {
            auto* fn = get_or_declare_runtime("golangc_os_stdin", type_map_.ptr_type());
            return builder_.create_call(fn, {}, type_map_.ptr_type(), "os.Stdin");
        }
        // math constants — emit float64 or int64 constant directly
        if (bname == "math.Pi")         return builder_.create_const_float(type_map_.f64_type(), 3.14159265358979323846, "math.Pi");
        if (bname == "math.E")          return builder_.create_const_float(type_map_.f64_type(), 2.71828182845904523536, "math.E");
        if (bname == "math.Phi")        return builder_.create_const_float(type_map_.f64_type(), 1.61803398874989484820, "math.Phi");
        if (bname == "math.Sqrt2")      return builder_.create_const_float(type_map_.f64_type(), 1.41421356237309504880, "math.Sqrt2");
        if (bname == "math.SqrtE")      return builder_.create_const_float(type_map_.f64_type(), 1.64872127070012814684, "math.SqrtE");
        if (bname == "math.SqrtPi")     return builder_.create_const_float(type_map_.f64_type(), 1.77245385090551602730, "math.SqrtPi");
        if (bname == "math.SqrtPhi")    return builder_.create_const_float(type_map_.f64_type(), 1.27201964951406896425, "math.SqrtPhi");
        if (bname == "math.Ln2")        return builder_.create_const_float(type_map_.f64_type(), 0.69314718055994530941, "math.Ln2");
        if (bname == "math.Log2E")      return builder_.create_const_float(type_map_.f64_type(), 1.44269504088896340736, "math.Log2E");
        if (bname == "math.Ln10")       return builder_.create_const_float(type_map_.f64_type(), 2.30258509299404568402, "math.Ln10");
        if (bname == "math.Log10E")     return builder_.create_const_float(type_map_.f64_type(), 0.43429448190325182765, "math.Log10E");
        if (bname == "math.MaxFloat32") return builder_.create_const_float(type_map_.f64_type(), 3.4028234663852886e+38, "math.MaxFloat32");
        if (bname == "math.MaxFloat64") return builder_.create_const_float(type_map_.f64_type(), 1.7976931348623157e+308, "math.MaxFloat64");
        if (bname == "math.SmallestNonzeroFloat32") return builder_.create_const_float(type_map_.f64_type(), 1.401298464324817e-45, "math.SmallestNonzeroFloat32");
        if (bname == "math.SmallestNonzeroFloat64") return builder_.create_const_float(type_map_.f64_type(), 5e-324, "math.SmallestNonzeroFloat64");
        if (bname == "math.MaxInt" || bname == "math.MaxInt64")  return builder_.create_const_int(type_map_.i64_type(), INT64_MAX, "math.MaxInt64");
        if (bname == "math.MinInt" || bname == "math.MinInt64")  return builder_.create_const_int(type_map_.i64_type(), INT64_MIN, "math.MinInt64");
        if (bname == "math.MaxInt32")   return builder_.create_const_int(type_map_.i64_type(), 2147483647LL, "math.MaxInt32");
        if (bname == "math.MinInt32")   return builder_.create_const_int(type_map_.i64_type(), -2147483648LL, "math.MinInt32");
        if (bname == "math.MaxInt16")   return builder_.create_const_int(type_map_.i64_type(), 32767LL, "math.MaxInt16");
        if (bname == "math.MinInt16")   return builder_.create_const_int(type_map_.i64_type(), -32768LL, "math.MinInt16");
        if (bname == "math.MaxInt8")    return builder_.create_const_int(type_map_.i64_type(), 127LL, "math.MaxInt8");
        if (bname == "math.MinInt8")    return builder_.create_const_int(type_map_.i64_type(), -128LL, "math.MinInt8");
        if (bname == "math.MaxUint32")  return builder_.create_const_int(type_map_.i64_type(), 4294967295LL, "math.MaxUint32");
        if (bname == "math.MaxUint16")  return builder_.create_const_int(type_map_.i64_type(), 65535LL, "math.MaxUint16");
        if (bname == "math.MaxUint8")   return builder_.create_const_int(type_map_.i64_type(), 255LL, "math.MaxUint8");
        if (bname == "math.MaxUint64")  return builder_.create_const_int(type_map_.i64_type(), static_cast<int64_t>(UINT64_MAX), "math.MaxUint64");
    }

    auto* addr = gen_addr(expr);
    if (addr) {
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

// ---------------------------------------------------------------------------
// get_or_declare_runtime: look up or forward-declare a runtime function
// ---------------------------------------------------------------------------

Function* IRGenerator::get_or_declare_runtime(const std::string& name, IRType* ret_type) {
    auto it = func_name_map_.find(name);
    if (it != func_name_map_.end()) return it->second;

    // Create an external function declaration (no body).
    auto* f = module_->create_function(builder_.next_id(),
                                        ret_type ? ret_type : type_map_.void_type(),
                                        name);
    f->return_type = ret_type ? ret_type : type_map_.void_type();
    func_name_map_[name] = f;
    return f;
}

Value* IRGenerator::gen_builtin_call(ast::Expr* expr, const sema::ExprInfo* func_info) {
    auto& call = expr->call;
    auto* call_info = expr_info(expr);
    auto builtin_name = func_info->symbol->name;

    // ---- fmt.Println — reuse the existing Println IR opcode ----
    if (builtin_name == "fmt.Println") {
        std::vector<Value*> args;
        for (auto* arg_expr : call.args) {
            auto* arg = gen_expr(arg_expr);
            if (arg) args.push_back(arg);
        }
        return builder_.create_println(args);
    }

    // ---- fmt.Printf — call golangc_printf(fmt_ptr, fmt_len, ...) ----
    if (builtin_name == "fmt.Printf") {
        if (call.args.count >= 1) {
            auto* fmt_val = gen_expr(call.args[0]);
            if (fmt_val) {
                auto* fn = get_or_declare_runtime("golangc_printf", type_map_.void_type());
                std::vector<Value*> args = {fmt_val};
                for (uint32_t i = 1; i < call.args.count; ++i) {
                    auto* a = gen_expr(call.args[i]);
                    if (a) args.push_back(a);
                }
                return builder_.create_call(fn, args, type_map_.void_type(), "printf");
            }
        }
        return builder_.create_const_int(type_map_.i64_type(), 0, "printf.nop");
    }

    // ---- fmt.Sprintf — call golangc_sprintf via sret, returns string ----
    if (builtin_name == "fmt.Sprintf") {
        if (call.args.count >= 1) {
            auto* fmt_val = gen_expr(call.args[0]);
            if (fmt_val) {
                auto* fn = get_or_declare_runtime("golangc_sprintf", type_map_.string_type());
                std::vector<Value*> args = {fmt_val};
                for (uint32_t i = 1; i < call.args.count; ++i) {
                    auto* a = gen_expr(call.args[i]);
                    if (a) args.push_back(a);
                }
                return builder_.create_call(fn, args, type_map_.string_type(), "sprintf");
            }
        }
        return builder_.create_const_string("", "sprintf.empty");
    }

    // ---- strconv.Itoa — call golangc_itoa(n), returns string ----
    if (builtin_name == "strconv.Itoa") {
        if (call.args.count >= 1) {
            auto* n = gen_expr(call.args[0]);
            if (n) {
                auto* fn = get_or_declare_runtime("golangc_itoa", type_map_.string_type());
                return builder_.create_call(fn, {n}, type_map_.string_type(), "itoa");
            }
        }
        return builder_.create_const_string("0", "itoa.zero");
    }

    // ---- strconv.Atoi — call golangc_atoi(ptr, len, nullptr), returns (int, error) ----
    if (builtin_name == "strconv.Atoi") {
        if (call.args.count >= 1) {
            auto* s = gen_expr(call.args[0]);
            if (s) {
                // Pass a null pointer for out_ok (error ignored; 0 = success)
                auto* null_ok = builder_.create_const_nil(type_map_.ptr_type(), "no.ok");
                auto* fn = get_or_declare_runtime("golangc_atoi", type_map_.i64_type());
                auto* int_val = builder_.create_call(fn, {s, null_ok},
                                                      type_map_.i64_type(), "atoi");
                // Build a 2-field struct {i64, interface} = {int_val, nil_error}
                // to match the (int, error) tuple expected by the caller.
                auto* nil_err = builder_.create_const_nil(type_map_.interface_type(), "nil.err");
                std::vector<IRType*> fields = {type_map_.i64_type(), type_map_.interface_type()};
                IRType* tuple_ir = type_map_.make_tuple_type(std::move(fields));
                auto* packed = builder_.create_const_nil(tuple_ir, "atoi.pack");
                packed = builder_.create_insert_value(packed, int_val, 0, "atoi.pack");
                packed = builder_.create_insert_value(packed, nil_err, 1, "atoi.pack");
                return packed;
            }
        }
        return builder_.create_const_int(type_map_.i64_type(), 0, "atoi.zero");
    }

    // ---- strconv.ParseInt(s, base, bitSize) → (int64, error) — reuses golangc_atoi ----
    if (builtin_name == "strconv.ParseInt") {
        if (call.args.count >= 1) {
            auto* s = gen_expr(call.args[0]);
            if (s) {
                auto* null_ok = builder_.create_const_nil(type_map_.ptr_type(), "no.ok");
                auto* fn = get_or_declare_runtime("golangc_atoi", type_map_.i64_type());
                auto* int_val = builder_.create_call(fn, {s, null_ok}, type_map_.i64_type(), "parseInt");
                auto* nil_err = builder_.create_const_nil(type_map_.interface_type(), "nil.err");
                std::vector<IRType*> fields = {type_map_.i64_type(), type_map_.interface_type()};
                auto* packed = builder_.create_const_nil(type_map_.make_tuple_type(std::move(fields)), "parseInt.pack");
                packed = builder_.create_insert_value(packed, int_val, 0, "parseInt.pack");
                packed = builder_.create_insert_value(packed, nil_err, 1, "parseInt.pack");
                return packed;
            }
        }
        return builder_.create_const_int(type_map_.i64_type(), 0, "parseInt.zero");
    }

    // ---- strconv.FormatInt(i, base) → string — reuses golangc_itoa ----
    if (builtin_name == "strconv.FormatInt") {
        if (call.args.count >= 1) {
            auto* n = gen_expr(call.args[0]);
            if (n) {
                auto* fn = get_or_declare_runtime("golangc_itoa", type_map_.string_type());
                return builder_.create_call(fn, {n}, type_map_.string_type(), "formatInt");
            }
        }
        return builder_.create_const_string("0", "formatInt.zero");
    }

    // ---- strconv.ParseFloat(s, bitSize) → (float64, error) ----
    if (builtin_name == "strconv.ParseFloat") {
        if (call.args.count >= 1) {
            auto* s = gen_expr(call.args[0]);
            if (s) {
                auto* fn = get_or_declare_runtime("golangc_parse_float", type_map_.f64_type());
                auto* f_val = builder_.create_call(fn, {s}, type_map_.f64_type(), "parseFloat");
                auto* nil_err = builder_.create_const_nil(type_map_.interface_type(), "nil.err");
                std::vector<IRType*> fields = {type_map_.f64_type(), type_map_.interface_type()};
                auto* packed = builder_.create_const_nil(type_map_.make_tuple_type(std::move(fields)), "parseFloat.pack");
                packed = builder_.create_insert_value(packed, f_val, 0, "parseFloat.pack");
                packed = builder_.create_insert_value(packed, nil_err, 1, "parseFloat.pack");
                return packed;
            }
        }
        return builder_.create_const_int(type_map_.f64_type(), 0, "parseFloat.zero");
    }

    // ---- strconv.FormatFloat(f, fmt, prec, bitSize) → string ----
    if (builtin_name == "strconv.FormatFloat") {
        if (call.args.count >= 1) {
            auto* f = gen_expr(call.args[0]);
            if (f) {
                auto* fn = get_or_declare_runtime("golangc_format_float", type_map_.string_type());
                return builder_.create_call(fn, {f}, type_map_.string_type(), "formatFloat");
            }
        }
        return builder_.create_const_string("0", "formatFloat.zero");
    }

    // ---- strconv.ParseBool(s) → (bool, error) ----
    if (builtin_name == "strconv.ParseBool") {
        if (call.args.count >= 1) {
            auto* s = gen_expr(call.args[0]);
            if (s) {
                auto* fn = get_or_declare_runtime("golangc_parse_bool", type_map_.i64_type());
                auto* b_val = builder_.create_call(fn, {s}, type_map_.i64_type(), "parseBool");
                auto* nil_err = builder_.create_const_nil(type_map_.interface_type(), "nil.err");
                std::vector<IRType*> fields = {type_map_.i64_type(), type_map_.interface_type()};
                auto* packed = builder_.create_const_nil(type_map_.make_tuple_type(std::move(fields)), "parseBool.pack");
                packed = builder_.create_insert_value(packed, b_val, 0, "parseBool.pack");
                packed = builder_.create_insert_value(packed, nil_err, 1, "parseBool.pack");
                return packed;
            }
        }
        return builder_.create_const_int(type_map_.i64_type(), 0, "parseBool.zero");
    }

    // ---- strconv.FormatBool(b) → string ----
    if (builtin_name == "strconv.FormatBool") {
        if (call.args.count >= 1) {
            auto* b = gen_expr(call.args[0]);
            if (b) {
                auto* fn = get_or_declare_runtime("golangc_format_bool", type_map_.string_type());
                return builder_.create_call(fn, {b}, type_map_.string_type(), "formatBool");
            }
        }
        return builder_.create_const_string("false", "formatBool.zero");
    }

    // ---- os.Args — call golangc_os_args_get() which loads the global slice ----
    if (builtin_name == "os.Args") {
        // Emit a call to a tiny wrapper that returns the os.Args slice by value.
        auto* fn = get_or_declare_runtime("golangc_os_args_get", type_map_.slice_type());
        return builder_.create_call(fn, {}, type_map_.slice_type(), "os.Args");
    }

    // ---- os.Stdout / os.Stderr / os.Stdin ----
    if (builtin_name == "os.Stdout") {
        auto* fn = get_or_declare_runtime("golangc_os_stdout", type_map_.ptr_type());
        return builder_.create_call(fn, {}, type_map_.ptr_type(), "os.Stdout");
    }
    if (builtin_name == "os.Stderr") {
        auto* fn = get_or_declare_runtime("golangc_os_stderr", type_map_.ptr_type());
        return builder_.create_call(fn, {}, type_map_.ptr_type(), "os.Stderr");
    }
    if (builtin_name == "os.Stdin") {
        auto* fn = get_or_declare_runtime("golangc_os_stdin", type_map_.ptr_type());
        return builder_.create_call(fn, {}, type_map_.ptr_type(), "os.Stdin");
    }

    // ---- os.Exit(code) ----
    if (builtin_name == "os.Exit") {
        Value* code = call.args.count >= 1 ? gen_expr(call.args[0]) : nullptr;
        if (!code) code = builder_.create_const_int(type_map_.i64_type(), 0, "exit.code");
        auto* fn = get_or_declare_runtime("golangc_os_exit", type_map_.void_type());
        builder_.create_call(fn, {code}, type_map_.void_type(), "os.Exit");
        builder_.create_ret(nullptr);  // terminate basic block (exit never returns)
        return builder_.create_const_int(type_map_.i64_type(), 0, "exit.dead");
    }

    // ---- os.Open(path) → (*os.File, error) ----
    if (builtin_name == "os.Open") {
        if (call.args.count >= 1) {
            auto* path = gen_expr(call.args[0]);
            if (path) {
                auto* fn = get_or_declare_runtime("golangc_os_open", type_map_.ptr_type());
                auto* file_ptr = builder_.create_call(fn, {path}, type_map_.ptr_type(), "os.Open");
                auto* nil_err = builder_.create_const_nil(type_map_.interface_type(), "nil.err");
                std::vector<IRType*> fields = {type_map_.ptr_type(), type_map_.interface_type()};
                IRType* tuple_ir = type_map_.make_tuple_type(std::move(fields));
                auto* packed = builder_.create_const_nil(tuple_ir, "open.pack");
                packed = builder_.create_insert_value(packed, file_ptr, 0, "open.pack");
                packed = builder_.create_insert_value(packed, nil_err, 1, "open.pack");
                return packed;
            }
        }
        return builder_.create_const_nil(type_map_.ptr_type(), "open.nil");
    }

    // ---- os.ReadFile(name) → ([]byte, error) ----
    if (builtin_name == "os.ReadFile") {
        if (call.args.count >= 1) {
            auto* name = gen_expr(call.args[0]);
            if (name) {
                auto* fn = get_or_declare_runtime("golangc_os_read_file", type_map_.slice_type());
                auto* slice_val = builder_.create_call(fn, {name}, type_map_.slice_type(), "os.ReadFile");
                auto* nil_err = builder_.create_const_nil(type_map_.interface_type(), "nil.err");
                std::vector<IRType*> fields = {type_map_.slice_type(), type_map_.interface_type()};
                auto* packed = builder_.create_const_nil(type_map_.make_tuple_type(std::move(fields)), "readfile.pack");
                packed = builder_.create_insert_value(packed, slice_val, 0, "readfile.pack");
                packed = builder_.create_insert_value(packed, nil_err, 1, "readfile.pack");
                return packed;
            }
        }
        return builder_.create_const_nil(type_map_.slice_type(), "readfile.nil");
    }

    // ---- os.Create(path) → (*os.File, error) ----
    if (builtin_name == "os.Create") {
        if (call.args.count >= 1) {
            auto* path = gen_expr(call.args[0]);
            if (path) {
                auto* fn = get_or_declare_runtime("golangc_os_create", type_map_.ptr_type());
                auto* file_ptr = builder_.create_call(fn, {path}, type_map_.ptr_type(), "os.Create");
                auto* nil_err = builder_.create_const_nil(type_map_.interface_type(), "nil.err");
                std::vector<IRType*> fields = {type_map_.ptr_type(), type_map_.interface_type()};
                IRType* tuple_ir = type_map_.make_tuple_type(std::move(fields));
                auto* packed = builder_.create_const_nil(tuple_ir, "create.pack");
                packed = builder_.create_insert_value(packed, file_ptr, 0, "create.pack");
                packed = builder_.create_insert_value(packed, nil_err, 1, "create.pack");
                return packed;
            }
        }
        return builder_.create_const_nil(type_map_.ptr_type(), "create.nil");
    }

    // ---- os.Getenv(key string) string ----
    if (builtin_name == "os.Getenv") {
        if (call.args.count >= 1) {
            auto* key = gen_expr(call.args[0]);
            if (key) {
                auto* fn = get_or_declare_runtime("golangc_os_getenv", type_map_.string_type());
                return builder_.create_call(fn, {key}, type_map_.string_type(), "os.Getenv");
            }
        }
        return builder_.create_const_string("", "getenv.empty");
    }

    // ---- bufio.NewScanner(r) → *bufio.Scanner ----
    if (builtin_name == "bufio.NewScanner") {
        if (call.args.count >= 1) {
            auto* r = gen_expr(call.args[0]);
            if (r) {
                auto* fn = get_or_declare_runtime("golangc_scanner_new", type_map_.ptr_type());
                return builder_.create_call(fn, {r}, type_map_.ptr_type(), "bufio.NewScanner");
            }
        }
        return builder_.create_const_nil(type_map_.ptr_type(), "scanner.nil");
    }

    // ---- bufio.NewReader(r) → *bufio.Reader ----
    if (builtin_name == "bufio.NewReader") {
        if (call.args.count >= 1) {
            auto* r = gen_expr(call.args[0]);
            if (r) {
                auto* fn = get_or_declare_runtime("golangc_breader_new", type_map_.ptr_type());
                return builder_.create_call(fn, {r}, type_map_.ptr_type(), "bufio.NewReader");
            }
        }
        return builder_.create_const_nil(type_map_.ptr_type(), "breader.nil");
    }

    // ---- sort.Ints(a []int) ----
    if (builtin_name == "sort.Ints") {
        if (call.args.count >= 1) {
            auto* slice = gen_expr(call.args[0]);
            if (slice) {
                auto* data_ptr = builder_.create_extract_value(slice, 0, type_map_.ptr_type(), "sort.ptr");
                auto* data_len = builder_.create_slice_len(slice, "sort.len");
                auto* fn = get_or_declare_runtime("golangc_sort_ints", type_map_.void_type());
                return builder_.create_call(fn, {data_ptr, data_len}, type_map_.void_type(), "sort.Ints");
            }
        }
        return builder_.create_const_int(type_map_.i64_type(), 0, "sort.ints.nop");
    }

    // ---- sort.Strings(a []string) ----
    if (builtin_name == "sort.Strings") {
        if (call.args.count >= 1) {
            auto* slice = gen_expr(call.args[0]);
            if (slice) {
                auto* data_ptr = builder_.create_extract_value(slice, 0, type_map_.ptr_type(), "sort.ptr");
                auto* data_len = builder_.create_slice_len(slice, "sort.len");
                auto* fn = get_or_declare_runtime("golangc_sort_strings", type_map_.void_type());
                return builder_.create_call(fn, {data_ptr, data_len}, type_map_.void_type(), "sort.Strings");
            }
        }
        return builder_.create_const_int(type_map_.i64_type(), 0, "sort.strings.nop");
    }

    // ---- sort.Slice(slice, less func(i,j int) bool) ----
    if (builtin_name == "sort.Slice") {
        if (call.args.count >= 2) {
            auto* slice    = gen_expr(call.args[0]);
            auto* less_fn  = gen_expr(call.args[1]);
            if (slice && less_fn) {
                auto* data_ptr  = builder_.create_extract_value(slice, 0, type_map_.ptr_type(), "sort.ptr");
                auto* data_len  = builder_.create_slice_len(slice, "sort.len");
                auto* elem_size = builder_.create_const_int(type_map_.i64_type(), 8, "elem.sz");
                auto* fn = get_or_declare_runtime("golangc_sort_slice", type_map_.void_type());
                return builder_.create_call(fn, {data_ptr, data_len, less_fn, elem_size},
                                            type_map_.void_type(), "sort.Slice");
            }
        }
        return builder_.create_const_int(type_map_.i64_type(), 0, "sort.slice.nop");
    }

    // ---- fmt.Fprintf(w, format, args...) ----
    if (builtin_name == "fmt.Fprintf") {
        if (call.args.count >= 2) {
            auto* w   = gen_expr(call.args[0]);
            auto* fmt = gen_expr(call.args[1]);
            if (w && fmt) {
                auto* fn = get_or_declare_runtime("golangc_fprintf", type_map_.void_type());
                std::vector<Value*> args = {w, fmt};
                for (uint32_t i = 2; i < call.args.count; ++i) {
                    auto* a = gen_expr(call.args[i]);
                    if (a) args.push_back(a);
                }
                return builder_.create_call(fn, args, type_map_.void_type(), "fprintf");
            }
        }
        return builder_.create_const_int(type_map_.i64_type(), 0, "fprintf.nop");
    }

    // ---- fmt.Fprintln(w, args...) → golangc_fprintf(w, "<fmt>\n", args...) ----
    if (builtin_name == "fmt.Fprintln") {
        if (call.args.count >= 1) {
            auto* w = gen_expr(call.args[0]);
            if (w) {
                // Build format string: space-separated %d/%s/%g per remaining arg, then \n
                std::string fmt_str;
                std::vector<Value*> fmt_args = {w};
                for (uint32_t i = 1; i < call.args.count; ++i) {
                    if (i > 1) fmt_str += " ";
                    auto* a = gen_expr(call.args[i]);
                    if (!a) continue;
                    if (a->type == type_map_.string_type())       fmt_str += "%s";
                    else if (a->type && a->type->is_float())      fmt_str += "%g";
                    else                                           fmt_str += "%v";
                    fmt_args.push_back(a);
                }
                fmt_str += "\n";
                auto* fmt_val = builder_.create_const_string(fmt_str, "fprintln.fmt");
                // Insert format string as second argument (after w)
                fmt_args.insert(fmt_args.begin() + 1, fmt_val);
                auto* fn = get_or_declare_runtime("golangc_fprintf", type_map_.void_type());
                return builder_.create_call(fn, fmt_args, type_map_.void_type(), "fprintln");
            }
        }
        return builder_.create_const_int(type_map_.i64_type(), 0, "fprintln.nop");
    }

    // ---- strings package ----
    // Two-string-arg functions returning bool (i64)
    if (builtin_name == "strings.Contains" || builtin_name == "strings.HasPrefix" ||
        builtin_name == "strings.HasSuffix" || builtin_name == "strings.Index" ||
        builtin_name == "strings.Count") {
        if (call.args.count >= 2) {
            auto* s   = gen_expr(call.args[0]);
            auto* sub = gen_expr(call.args[1]);
            if (s && sub) {
                std::string fn_name;
                if      (builtin_name == "strings.Contains")  fn_name = "golangc_strings_contains";
                else if (builtin_name == "strings.HasPrefix")  fn_name = "golangc_strings_has_prefix";
                else if (builtin_name == "strings.HasSuffix")  fn_name = "golangc_strings_has_suffix";
                else if (builtin_name == "strings.Index")      fn_name = "golangc_strings_index";
                else                                           fn_name = "golangc_strings_count";
                IRType* ret = (builtin_name == "strings.Index" || builtin_name == "strings.Count")
                              ? type_map_.i64_type() : type_map_.i64_type(); // bool as i64
                auto* fn = get_or_declare_runtime(fn_name, ret);
                return builder_.create_call(fn, {s, sub}, ret, std::string(builtin_name));
            }
        }
        return builder_.create_const_int(type_map_.i64_type(), 0);
    }

    // One-string-arg functions returning string via sret
    if (builtin_name == "strings.ToUpper" || builtin_name == "strings.ToLower" ||
        builtin_name == "strings.TrimSpace") {
        if (call.args.count >= 1) {
            auto* s = gen_expr(call.args[0]);
            if (s) {
                std::string fn_name;
                if      (builtin_name == "strings.ToUpper")   fn_name = "golangc_strings_to_upper";
                else if (builtin_name == "strings.ToLower")   fn_name = "golangc_strings_to_lower";
                else                                          fn_name = "golangc_strings_trim_space";
                auto* fn = get_or_declare_runtime(fn_name, type_map_.string_type());
                return builder_.create_call(fn, {s}, type_map_.string_type(), std::string(builtin_name));
            }
        }
        return builder_.create_const_string("", std::string(builtin_name) + ".empty");
    }

    // strings.Repeat(s, count) → string
    if (builtin_name == "strings.Repeat") {
        if (call.args.count >= 2) {
            auto* s     = gen_expr(call.args[0]);
            auto* count = gen_expr(call.args[1]);
            if (s && count) {
                auto* fn = get_or_declare_runtime("golangc_strings_repeat", type_map_.string_type());
                return builder_.create_call(fn, {s, count}, type_map_.string_type(), "strings.Repeat");
            }
        }
        return builder_.create_const_string("", "repeat.empty");
    }

    // strings.Trim(s, cutset) → string
    if (builtin_name == "strings.Trim") {
        if (call.args.count >= 2) {
            auto* s   = gen_expr(call.args[0]);
            auto* cut = gen_expr(call.args[1]);
            if (s && cut) {
                auto* fn = get_or_declare_runtime("golangc_strings_trim", type_map_.string_type());
                return builder_.create_call(fn, {s, cut}, type_map_.string_type(), "strings.Trim");
            }
        }
        return builder_.create_const_string("", "trim.empty");
    }

    // strings.Replace(s, old, new, n) → string
    if (builtin_name == "strings.Replace") {
        if (call.args.count >= 4) {
            auto* s   = gen_expr(call.args[0]);
            auto* old = gen_expr(call.args[1]);
            auto* nw  = gen_expr(call.args[2]);
            auto* n   = gen_expr(call.args[3]);
            if (s && old && nw && n) {
                auto* fn = get_or_declare_runtime("golangc_strings_replace", type_map_.string_type());
                return builder_.create_call(fn, {s, old, nw, n}, type_map_.string_type(), "strings.Replace");
            }
        }
        return builder_.create_const_string("", "replace.empty");
    }

    // ---- strings.Split(s, sep) → []string ----
    if (builtin_name == "strings.Split") {
        if (call.args.count >= 2) {
            auto* s   = gen_expr(call.args[0]);
            auto* sep = gen_expr(call.args[1]);
            if (s && sep) {
                auto* fn = get_or_declare_runtime("golangc_strings_split", type_map_.slice_type());
                return builder_.create_call(fn, {s, sep}, type_map_.slice_type(), "strings.Split");
            }
        }
        return builder_.create_const_nil(type_map_.slice_type(), "split.nil");
    }

    // ---- strings.Join(elems []string, sep string) → string ----
    if (builtin_name == "strings.Join") {
        if (call.args.count >= 2) {
            auto* elems = gen_expr(call.args[0]);
            auto* sep   = gen_expr(call.args[1]);
            if (elems && sep) {
                auto* fn = get_or_declare_runtime("golangc_strings_join", type_map_.string_type());
                return builder_.create_call(fn, {elems, sep}, type_map_.string_type(), "strings.Join");
            }
        }
        return builder_.create_const_string("", "join.empty");
    }

    // ---- strings.Fields(s) → []string ----
    if (builtin_name == "strings.Fields") {
        if (call.args.count >= 1) {
            auto* s = gen_expr(call.args[0]);
            if (s) {
                auto* fn = get_or_declare_runtime("golangc_strings_fields", type_map_.slice_type());
                return builder_.create_call(fn, {s}, type_map_.slice_type(), "strings.Fields");
            }
        }
        return builder_.create_const_nil(type_map_.slice_type(), "fields.nil");
    }

    // ---- strings.TrimPrefix / strings.TrimSuffix ----
    if (builtin_name == "strings.TrimPrefix" || builtin_name == "strings.TrimSuffix") {
        if (call.args.count >= 2) {
            auto* s   = gen_expr(call.args[0]);
            auto* fix = gen_expr(call.args[1]);
            if (s && fix) {
                std::string fn_name = (builtin_name == "strings.TrimPrefix")
                                    ? "golangc_strings_trim_prefix"
                                    : "golangc_strings_trim_suffix";
                auto* fn = get_or_declare_runtime(fn_name, type_map_.string_type());
                return builder_.create_call(fn, {s, fix}, type_map_.string_type(),
                                            std::string(builtin_name));
            }
        }
        return builder_.create_const_string("", std::string(builtin_name) + ".empty");
    }

    // ---- fmt.Sprint(args...) → string ----
    if (builtin_name == "fmt.Sprint") {
        // Concatenate args using golangc_fmt_sprintf with auto format string
        // Build a format string like "%v %v %v" (space-separated)
        std::string fmt_str;
        std::vector<Value*> fmt_args;
        for (uint32_t i = 0; i < call.args.count; ++i) {
            auto* a = gen_expr(call.args[i]);
            if (!a) continue;
            if (i > 0) fmt_str += " ";
            if (a->type == type_map_.string_type())  fmt_str += "%s";
            else if (a->type && a->type->is_float())  fmt_str += "%g";
            else                                      fmt_str += "%v";
            fmt_args.push_back(a);
        }
        auto* fmt_val = builder_.create_const_string(fmt_str, "sprint.fmt");
        fmt_args.insert(fmt_args.begin(), fmt_val);
        auto* fn = get_or_declare_runtime("golangc_sprintf", type_map_.string_type());
        return builder_.create_call(fn, fmt_args, type_map_.string_type(), "fmt.Sprint");
    }

    // ---- fmt.Fprint(w, args...) ----
    if (builtin_name == "fmt.Fprint") {
        if (call.args.count >= 1) {
            auto* w = gen_expr(call.args[0]);
            if (w) {
                std::string fmt_str;
                std::vector<Value*> fargs = {w};
                for (uint32_t i = 1; i < call.args.count; ++i) {
                    auto* a = gen_expr(call.args[i]);
                    if (!a) continue;
                    if (i > 1) fmt_str += " ";
                    if (a->type == type_map_.string_type())  fmt_str += "%s";
                    else if (a->type && a->type->is_float())  fmt_str += "%g";
                    else                                      fmt_str += "%v";
                    fargs.push_back(a);
                }
                auto* fmt_val = builder_.create_const_string(fmt_str, "fprint.fmt");
                fargs.insert(fargs.begin() + 1, fmt_val);
                auto* fn = get_or_declare_runtime("golangc_fprintf", type_map_.void_type());
                return builder_.create_call(fn, fargs, type_map_.void_type(), "fmt.Fprint");
            }
        }
        return builder_.create_const_int(type_map_.i64_type(), 0, "fprint.nop");
    }

    // ---- time package ----
    if (builtin_name == "time.Sleep") {
        if (call.args.count >= 1) {
            auto* dur = gen_expr(call.args[0]);
            if (dur) {
                auto* fn = get_or_declare_runtime("golangc_time_sleep", type_map_.void_type());
                return builder_.create_call(fn, {dur}, type_map_.void_type(), "time.Sleep");
            }
        }
        return builder_.create_const_int(type_map_.i64_type(), 0, "sleep.nop");
    }

    if (builtin_name == "time.Now") {
        auto* fn = get_or_declare_runtime("golangc_time_now", type_map_.i64_type());
        return builder_.create_call(fn, {}, type_map_.i64_type(), "time.Now");
    }

    if (builtin_name == "time.Since") {
        if (call.args.count >= 1) {
            auto* t = gen_expr(call.args[0]);
            if (t) {
                auto* fn = get_or_declare_runtime("golangc_time_since", type_map_.i64_type());
                return builder_.create_call(fn, {t}, type_map_.i64_type(), "time.Since");
            }
        }
        return builder_.create_const_int(type_map_.i64_type(), 0, "since.zero");
    }

    // time duration constants — just return the nanosecond value as i64
    if (builtin_name == "time.Hour")        return builder_.create_const_int(type_map_.i64_type(), 3600000000000LL, "time.Hour");
    if (builtin_name == "time.Minute")      return builder_.create_const_int(type_map_.i64_type(), 60000000000LL,   "time.Minute");
    if (builtin_name == "time.Second")      return builder_.create_const_int(type_map_.i64_type(), 1000000000LL,    "time.Second");
    if (builtin_name == "time.Millisecond") return builder_.create_const_int(type_map_.i64_type(), 1000000LL,       "time.Ms");

    // ---- math/rand package ----
    if (builtin_name == "rand.Intn" || builtin_name == "rand.Int63n" || builtin_name == "rand.Int31n") {
        if (call.args.count >= 1) {
            auto* n = gen_expr(call.args[0]);
            if (n) {
                auto* fn = get_or_declare_runtime("golangc_rand_intn", type_map_.i64_type());
                return builder_.create_call(fn, {n}, type_map_.i64_type(), "rand.Intn");
            }
        }
        return builder_.create_const_int(type_map_.i64_type(), 0, "rand.zero");
    }

    if (builtin_name == "rand.Float64") {
        auto* fn = get_or_declare_runtime("golangc_rand_float64", type_map_.f64_type());
        return builder_.create_call(fn, {}, type_map_.f64_type(), "rand.Float64");
    }

    if (builtin_name == "rand.Seed") {
        if (call.args.count >= 1) {
            auto* seed = gen_expr(call.args[0]);
            if (seed) {
                auto* fn = get_or_declare_runtime("golangc_rand_seed", type_map_.void_type());
                return builder_.create_call(fn, {seed}, type_map_.void_type(), "rand.Seed");
            }
        }
        return builder_.create_const_int(type_map_.i64_type(), 0, "seed.nop");
    }

    if (builtin_name == "rand.New" || builtin_name == "rand.NewSource") {
        // Simplified: NewSource returns seed as i64; New just returns its arg
        if (call.args.count >= 1) {
            auto* a = gen_expr(call.args[0]);
            if (a) return a;
        }
        return builder_.create_const_int(type_map_.i64_type(), 0, "rand.new.zero");
    }

    // ---- math package ----
    // Single float64 argument → float64
    if (builtin_name == "math.Abs"   || builtin_name == "math.Sqrt"  ||
        builtin_name == "math.Floor" || builtin_name == "math.Ceil"  ||
        builtin_name == "math.Round" || builtin_name == "math.Log"   ||
        builtin_name == "math.Log2"  || builtin_name == "math.Log10" ||
        builtin_name == "math.Sin"   || builtin_name == "math.Cos"   ||
        builtin_name == "math.Tan"   || builtin_name == "math.Asin"  ||
        builtin_name == "math.Acos"  || builtin_name == "math.Atan"  ||
        builtin_name == "math.Trunc" || builtin_name == "math.Exp"   ||
        builtin_name == "math.Exp2") {
        if (call.args.count >= 1) {
            auto* x = gen_expr(call.args[0]);
            if (x) {
                std::string fn_name;
                if      (builtin_name == "math.Abs")   fn_name = "golangc_math_abs";
                else if (builtin_name == "math.Sqrt")  fn_name = "golangc_math_sqrt";
                else if (builtin_name == "math.Floor") fn_name = "golangc_math_floor";
                else if (builtin_name == "math.Ceil")  fn_name = "golangc_math_ceil";
                else if (builtin_name == "math.Round") fn_name = "golangc_math_round";
                else if (builtin_name == "math.Log")   fn_name = "golangc_math_log";
                else if (builtin_name == "math.Log2")  fn_name = "golangc_math_log2";
                else if (builtin_name == "math.Log10") fn_name = "golangc_math_log10";
                else if (builtin_name == "math.Sin")   fn_name = "golangc_math_sin";
                else if (builtin_name == "math.Cos")   fn_name = "golangc_math_cos";
                else if (builtin_name == "math.Tan")   fn_name = "golangc_math_tan";
                else if (builtin_name == "math.Asin")  fn_name = "golangc_math_asin";
                else if (builtin_name == "math.Acos")  fn_name = "golangc_math_acos";
                else if (builtin_name == "math.Atan")  fn_name = "golangc_math_atan";
                else if (builtin_name == "math.Trunc") fn_name = "golangc_math_trunc";
                else if (builtin_name == "math.Exp")   fn_name = "golangc_math_exp";
                else                                   fn_name = "golangc_math_exp2";
                auto* fn = get_or_declare_runtime(fn_name, type_map_.f64_type());
                return builder_.create_call(fn, {x}, type_map_.f64_type(), std::string(builtin_name));
            }
        }
        return builder_.create_const_float(type_map_.f64_type(), 0.0, "math.zero");
    }

    // Two float64 arguments → float64
    if (builtin_name == "math.Max"  || builtin_name == "math.Min"  ||
        builtin_name == "math.Pow"  || builtin_name == "math.Mod"  ||
        builtin_name == "math.Hypot"|| builtin_name == "math.Atan2") {
        if (call.args.count >= 2) {
            auto* x = gen_expr(call.args[0]);
            auto* y = gen_expr(call.args[1]);
            if (x && y) {
                std::string fn_name;
                if      (builtin_name == "math.Max")   fn_name = "golangc_math_max";
                else if (builtin_name == "math.Min")   fn_name = "golangc_math_min";
                else if (builtin_name == "math.Pow")   fn_name = "golangc_math_pow";
                else if (builtin_name == "math.Mod")   fn_name = "golangc_math_mod";
                else if (builtin_name == "math.Hypot") fn_name = "golangc_math_hypot";
                else                                   fn_name = "golangc_math_atan2";
                auto* fn = get_or_declare_runtime(fn_name, type_map_.f64_type());
                return builder_.create_call(fn, {x, y}, type_map_.f64_type(), std::string(builtin_name));
            }
        }
        return builder_.create_const_float(type_map_.f64_type(), 0.0, "math.zero");
    }

    // math.Inf(sign int) float64 — return +Inf or -Inf approximated by MaxFloat64
    if (builtin_name == "math.Inf") {
        // Return MaxFloat64 as approximation (full infinity needs special FP encoding)
        return builder_.create_const_float(type_map_.f64_type(), 1.7976931348623157e+308, "math.Inf");
    }
    // math.NaN() float64
    if (builtin_name == "math.NaN") {
        return builder_.create_const_float(type_map_.f64_type(), 0.0, "math.NaN"); // simplified
    }
    // math.IsInf(f, sign) bool / math.IsNaN(f) bool — simplified, return false
    if (builtin_name == "math.IsInf" || builtin_name == "math.IsNaN") {
        return builder_.create_const_int(type_map_.i64_type(), 0, "math.IsFalse");
    }

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
                // Extract optional buffer capacity: make(chan T, n)
                int64_t buf_cap = 0;
                if (call.args.count >= 2) {
                    auto* cap_info = expr_info(call.args[1]);
                    if (cap_info && cap_info->const_val && cap_info->const_val->is_int()) {
                        buf_cap = cap_info->const_val->as_int();
                    } else if (call.args[1]->kind == ast::ExprKind::BasicLit) {
                        // Try to parse as integer literal directly
                        auto& lit = call.args[1]->basic_lit;
                        if (lit.kind == TokenKind::IntLiteral) {
                            buf_cap = std::stoll(std::string(lit.value));
                        }
                    }
                }
                return builder_.create_chan_make(type_map_.ptr_type(), elem_size, buf_cap, "make");
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

    // ---- strings.Builder methods ----
    // These are dispatched as builtins; extract receiver from call.func->selector.x
    if (builtin_name.size() > 16 &&
        builtin_name.compare(0, 16, "strings.Builder.") == 0) {
        Value* recv = nullptr;
        if (call.func->kind == ast::ExprKind::Selector) {
            recv = gen_expr(call.func->selector.x);
        }
        if (!recv) return builder_.create_const_int(type_map_.i64_type(), 0, "builder.norecv");

        if (builtin_name == "strings.Builder.WriteString") {
            Value* s = call.args.count >= 1 ? gen_expr(call.args[0]) : nullptr;
            if (!s) return builder_.create_const_int(type_map_.i64_type(), 0, "builder.noarg");
            auto* fn = get_or_declare_runtime("golangc_builder_write_string", type_map_.void_type());
            return builder_.create_call(fn, {recv, s}, type_map_.void_type(), "WriteString");
        }
        if (builtin_name == "strings.Builder.WriteByte") {
            Value* b_val = call.args.count >= 1 ? gen_expr(call.args[0]) : nullptr;
            if (!b_val) return builder_.create_const_int(type_map_.i64_type(), 0, "builder.noarg");
            auto* fn = get_or_declare_runtime("golangc_builder_write_byte", type_map_.void_type());
            return builder_.create_call(fn, {recv, b_val}, type_map_.void_type(), "WriteByte");
        }
        if (builtin_name == "strings.Builder.String") {
            auto* fn = get_or_declare_runtime("golangc_builder_string", type_map_.string_type());
            return builder_.create_call(fn, {recv}, type_map_.string_type(), "String");
        }
        if (builtin_name == "strings.Builder.Reset") {
            auto* fn = get_or_declare_runtime("golangc_builder_reset", type_map_.void_type());
            return builder_.create_call(fn, {recv}, type_map_.void_type(), "Reset");
        }
        if (builtin_name == "strings.Builder.Len") {
            auto* fn = get_or_declare_runtime("golangc_builder_len", type_map_.i64_type());
            return builder_.create_call(fn, {recv}, type_map_.i64_type(), "Len");
        }
        return builder_.create_const_int(type_map_.i64_type(), 0, "builder.unknown");
    }

    // ---- sync.Mutex methods ----
    if (builtin_name.size() > 11 &&
        builtin_name.compare(0, 11, "sync.Mutex.") == 0) {
        Value* recv = nullptr;
        if (call.func->kind == ast::ExprKind::Selector) {
            recv = gen_expr(call.func->selector.x);
        }
        if (!recv) return builder_.create_const_int(type_map_.i64_type(), 0, "mutex.norecv");

        if (builtin_name == "sync.Mutex.Lock") {
            auto* fn = get_or_declare_runtime("golangc_mutex_lock", type_map_.void_type());
            return builder_.create_call(fn, {recv}, type_map_.void_type(), "Lock");
        }
        if (builtin_name == "sync.Mutex.Unlock") {
            auto* fn = get_or_declare_runtime("golangc_mutex_unlock", type_map_.void_type());
            return builder_.create_call(fn, {recv}, type_map_.void_type(), "Unlock");
        }
        if (builtin_name == "sync.Mutex.TryLock") {
            auto* fn = get_or_declare_runtime("golangc_mutex_try_lock", type_map_.i64_type());
            return builder_.create_call(fn, {recv}, type_map_.i64_type(), "TryLock");
        }
        return builder_.create_const_int(type_map_.i64_type(), 0, "mutex.unknown");
    }

    // ---- sync.WaitGroup methods ----
    if (builtin_name.size() > 15 &&
        builtin_name.compare(0, 15, "sync.WaitGroup.") == 0) {
        Value* recv = nullptr;
        if (call.func->kind == ast::ExprKind::Selector) {
            recv = gen_expr(call.func->selector.x);
        }
        if (!recv) return builder_.create_const_int(type_map_.i64_type(), 0, "wg.norecv");

        if (builtin_name == "sync.WaitGroup.Add") {
            Value* delta = call.args.count >= 1 ? gen_expr(call.args[0]) : nullptr;
            if (!delta) delta = builder_.create_const_int(type_map_.i64_type(), 0, "wg.delta");
            auto* fn = get_or_declare_runtime("golangc_waitgroup_add", type_map_.void_type());
            return builder_.create_call(fn, {recv, delta}, type_map_.void_type(), "Add");
        }
        if (builtin_name == "sync.WaitGroup.Done") {
            auto* fn = get_or_declare_runtime("golangc_waitgroup_done", type_map_.void_type());
            return builder_.create_call(fn, {recv}, type_map_.void_type(), "Done");
        }
        if (builtin_name == "sync.WaitGroup.Wait") {
            auto* fn = get_or_declare_runtime("golangc_waitgroup_wait", type_map_.void_type());
            return builder_.create_call(fn, {recv}, type_map_.void_type(), "Wait");
        }
        return builder_.create_const_int(type_map_.i64_type(), 0, "wg.unknown");
    }

    // ---- os.File methods ----
    if (builtin_name.size() > 8 && builtin_name.compare(0, 8, "os.File.") == 0) {
        Value* recv = nullptr;
        if (call.func->kind == ast::ExprKind::Selector) {
            recv = gen_expr(call.func->selector.x);
        }
        if (!recv) return builder_.create_const_int(type_map_.i64_type(), 0, "file.norecv");

        if (builtin_name == "os.File.Close") {
            auto* fn = get_or_declare_runtime("golangc_os_file_close", type_map_.void_type());
            builder_.create_call(fn, {recv}, type_map_.void_type(), "Close");
            return builder_.create_const_nil(type_map_.interface_type(), "close.nil.err");
        }
        if (builtin_name == "os.File.WriteString") {
            Value* s = call.args.count >= 1 ? gen_expr(call.args[0]) : nullptr;
            if (!s) return builder_.create_const_int(type_map_.i64_type(), 0, "file.noarg");
            auto* fn = get_or_declare_runtime("golangc_os_file_write_string", type_map_.i64_type());
            auto* n = builder_.create_call(fn, {recv, s}, type_map_.i64_type(), "WriteString");
            auto* nil_err = builder_.create_const_nil(type_map_.interface_type(), "nil.err");
            std::vector<IRType*> fields = {type_map_.i64_type(), type_map_.interface_type()};
            auto* packed = builder_.create_const_nil(type_map_.make_tuple_type(std::move(fields)), "ws.pack");
            packed = builder_.create_insert_value(packed, n, 0, "ws.pack");
            packed = builder_.create_insert_value(packed, nil_err, 1, "ws.pack");
            return packed;
        }
        return builder_.create_const_int(type_map_.i64_type(), 0, "file.unknown");
    }

    // ---- bufio.Scanner methods ----
    if (builtin_name.size() > 14 && builtin_name.compare(0, 14, "bufio.Scanner.") == 0) {
        Value* recv = nullptr;
        if (call.func->kind == ast::ExprKind::Selector) {
            recv = gen_expr(call.func->selector.x);
        }
        if (!recv) return builder_.create_const_int(type_map_.i64_type(), 0, "scanner.norecv");

        if (builtin_name == "bufio.Scanner.Scan") {
            auto* fn = get_or_declare_runtime("golangc_scanner_scan", type_map_.i64_type());
            return builder_.create_call(fn, {recv}, type_map_.i64_type(), "Scan");
        }
        if (builtin_name == "bufio.Scanner.Text") {
            auto* fn = get_or_declare_runtime("golangc_scanner_text", type_map_.string_type());
            return builder_.create_call(fn, {recv}, type_map_.string_type(), "Text");
        }
        if (builtin_name == "bufio.Scanner.Err") {
            return builder_.create_const_nil(type_map_.interface_type(), "scanner.err.nil");
        }
        return builder_.create_const_int(type_map_.i64_type(), 0, "scanner.unknown");
    }

    // ---- bufio.Reader methods ----
    if (builtin_name.size() > 13 && builtin_name.compare(0, 13, "bufio.Reader.") == 0) {
        Value* recv = nullptr;
        if (call.func->kind == ast::ExprKind::Selector) {
            recv = gen_expr(call.func->selector.x);
        }
        if (!recv) return builder_.create_const_int(type_map_.i64_type(), 0, "breader.norecv");

        if (builtin_name == "bufio.Reader.ReadString") {
            Value* delim = call.args.count >= 1 ? gen_expr(call.args[0]) : nullptr;
            if (!delim) delim = builder_.create_const_int(type_map_.i64_type(), '\n', "delim.nl");
            auto* fn = get_or_declare_runtime("golangc_breader_read_string", type_map_.string_type());
            auto* s = builder_.create_call(fn, {recv, delim}, type_map_.string_type(), "ReadString");
            auto* nil_err = builder_.create_const_nil(type_map_.interface_type(), "nil.err");
            std::vector<IRType*> fields = {type_map_.string_type(), type_map_.interface_type()};
            auto* packed = builder_.create_const_nil(type_map_.make_tuple_type(std::move(fields)), "rs.pack");
            packed = builder_.create_insert_value(packed, s, 0, "rs.pack");
            packed = builder_.create_insert_value(packed, nil_err, 1, "rs.pack");
            return packed;
        }
        return builder_.create_const_int(type_map_.i64_type(), 0, "breader.unknown");
    }

    // ---- fmt.Scan / fmt.Scanln / fmt.Scanf ----
    if (builtin_name == "fmt.Scan" || builtin_name == "fmt.Scanln" ||
        builtin_name == "fmt.Scanf") {
        bool is_scanf = (builtin_name == "fmt.Scanf");
        uint32_t first_ptr_arg = is_scanf ? 1 : 0;
        std::string rt_name = is_scanf ? "golangc_fmt_scanf"
                            : (builtin_name == "fmt.Scanln") ? "golangc_fmt_scanln"
                                                              : "golangc_fmt_scan";
        std::vector<Value*> args;
        if (is_scanf && call.args.count >= 1) {
            auto* fmt_val = gen_expr(call.args[0]);
            if (fmt_val) args.push_back(fmt_val);
        }
        int64_t nargs = static_cast<int64_t>(call.args.count) - static_cast<int64_t>(first_ptr_arg);
        if (nargs < 0) nargs = 0;
        args.push_back(builder_.create_const_int(type_map_.i64_type(), nargs, "scan.nargs"));
        for (uint32_t i = first_ptr_arg; i < call.args.count; ++i) {
            int64_t tag = 0; // 0=int64, 1=float64, 2=string
            auto* ai = expr_info(call.args[i]);
            if (ai && ai->type) {
                auto* pt = sema::underlying(ai->type);
                if (pt && pt->kind == sema::TypeKind::Pointer && pt->pointer.base) {
                    auto* base = sema::underlying(pt->pointer.base);
                    if (base && base->kind == sema::TypeKind::Basic) {
                        if (base->basic == sema::BasicKind::Float32 ||
                            base->basic == sema::BasicKind::Float64)  tag = 1;
                        else if (sema::is_string(pt->pointer.base))   tag = 2;
                    }
                }
            }
            args.push_back(builder_.create_const_int(type_map_.i64_type(), tag, "scan.tag"));
            auto* ptr = gen_expr(call.args[i]);
            if (ptr) args.push_back(ptr);
        }
        auto* fn = get_or_declare_runtime(rt_name, type_map_.i64_type());
        auto* n = builder_.create_call(fn, args, type_map_.i64_type(), std::string(builtin_name));
        auto* nil_err = builder_.create_const_nil(type_map_.interface_type(), "nil.err");
        std::vector<IRType*> fields = {type_map_.i64_type(), type_map_.interface_type()};
        auto* packed = builder_.create_const_nil(type_map_.make_tuple_type(std::move(fields)), "scan.pack");
        packed = builder_.create_insert_value(packed, n, 0, "scan.pack");
        packed = builder_.create_insert_value(packed, nil_err, 1, "scan.pack");
        return packed;
    }

    // ---- fmt.Sscan / fmt.Sscanf ----
    if (builtin_name == "fmt.Sscan" || builtin_name == "fmt.Sscanf") {
        bool is_sscanf = (builtin_name == "fmt.Sscanf");
        uint32_t first_ptr_arg = is_sscanf ? 2 : 1;
        std::string rt_name = is_sscanf ? "golangc_fmt_sscanf" : "golangc_fmt_sscan";
        std::vector<Value*> args;
        // Input string (first arg always)
        if (call.args.count >= 1) {
            auto* str = gen_expr(call.args[0]);
            if (str) args.push_back(str);
        }
        // Format string (second arg for Sscanf)
        if (is_sscanf && call.args.count >= 2) {
            auto* fmt_val = gen_expr(call.args[1]);
            if (fmt_val) args.push_back(fmt_val);
        }
        int64_t nargs = static_cast<int64_t>(call.args.count) - static_cast<int64_t>(first_ptr_arg);
        if (nargs < 0) nargs = 0;
        args.push_back(builder_.create_const_int(type_map_.i64_type(), nargs, "sscan.nargs"));
        for (uint32_t i = first_ptr_arg; i < call.args.count; ++i) {
            int64_t tag = 0; // 0=int64, 1=float64, 2=string
            auto* ai = expr_info(call.args[i]);
            if (ai && ai->type) {
                auto* pt = sema::underlying(ai->type);
                if (pt && pt->kind == sema::TypeKind::Pointer && pt->pointer.base) {
                    auto* base = sema::underlying(pt->pointer.base);
                    if (base && base->kind == sema::TypeKind::Basic) {
                        if (base->basic == sema::BasicKind::Float32 ||
                            base->basic == sema::BasicKind::Float64)    tag = 1;
                        else if (sema::is_string(pt->pointer.base))     tag = 2;
                    }
                }
            }
            args.push_back(builder_.create_const_int(type_map_.i64_type(), tag, "sscan.tag"));
            auto* ptr = gen_expr(call.args[i]);
            if (ptr) args.push_back(ptr);
        }
        auto* fn = get_or_declare_runtime(rt_name, type_map_.i64_type());
        auto* n = builder_.create_call(fn, args, type_map_.i64_type(), std::string(builtin_name));
        auto* nil_err = builder_.create_const_nil(type_map_.interface_type(), "nil.err");
        std::vector<IRType*> fields = {type_map_.i64_type(), type_map_.interface_type()};
        auto* packed = builder_.create_const_nil(type_map_.make_tuple_type(std::move(fields)), "sscan.pack");
        packed = builder_.create_insert_value(packed, n, 0, "sscan.pack");
        packed = builder_.create_insert_value(packed, nil_err, 1, "sscan.pack");
        return packed;
    }

    // ---- errors.New(msg) → golangc_errors_new(ptr, len) returns interface via sret ----
    if (builtin_name == "errors.New") {
        if (call.args.count >= 1) {
            auto* msg = gen_expr(call.args[0]);
            if (msg) {
                auto* fn = get_or_declare_runtime("golangc_errors_new", type_map_.interface_type());
                return builder_.create_call(fn, {msg}, type_map_.interface_type(), "errors.New");
            }
        }
        return builder_.create_const_nil(type_map_.interface_type(), "errors.New.nil");
    }

    // ---- fmt.Errorf(fmt, ...) → golangc_fmt_errorf(ptr, len, ...) returns interface via sret ----
    if (builtin_name == "fmt.Errorf") {
        if (call.args.count >= 1) {
            auto* fmt_val = gen_expr(call.args[0]);
            if (fmt_val) {
                auto* fn = get_or_declare_runtime("golangc_fmt_errorf", type_map_.interface_type());
                std::vector<Value*> args = {fmt_val};
                for (uint32_t i = 1; i < call.args.count; ++i) {
                    auto* a = gen_expr(call.args[i]);
                    if (a) args.push_back(a);
                }
                return builder_.create_call(fn, args, type_map_.interface_type(), "fmt.Errorf");
            }
        }
        return builder_.create_const_nil(type_map_.interface_type(), "fmt.Errorf.nil");
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
