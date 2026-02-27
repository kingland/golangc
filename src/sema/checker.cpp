#include "sema/checker.hpp"

#include <fmt/format.h>

namespace golangc {
namespace sema {

Checker::Checker(DiagnosticEngine& diag)
    : diag_(diag) {}

bool Checker::check(ast::File* file) {
    if (!file) return false;

    // Initialize universe scope
    universe_ = init_universe(arena_);
    current_scope_ = universe_;

    // Create package scope (child of universe)
    package_scope_ = open_scope(ScopeKind::Package);

    // Two-pass approach for top-level declarations
    // Pass 1: collect all names (forward declarations)
    collect_top_level_decls(file);

    // Pass 2: check all declarations
    check_top_level_decls(file);

    return !diag_.has_errors();
}

const ExprInfo* Checker::expr_info(const ast::Expr* expr) const {
    auto it = expr_map_.find(expr);
    if (it != expr_map_.end()) {
        return &it->second;
    }
    return nullptr;
}

Symbol* Checker::decl_symbol(const ast::IdentExpr* ident) const {
    auto it = decl_sym_map_.find(ident);
    if (it != decl_sym_map_.end()) {
        return it->second;
    }
    return nullptr;
}

// ============================================================================
// Scope helpers
// ============================================================================

Scope* Checker::open_scope(ScopeKind kind) {
    auto* scope = arena_.create<Scope>(kind, current_scope_);
    current_scope_ = scope;
    return scope;
}

void Checker::close_scope() {
    if (current_scope_) {
        // Check for unused variables in block/function scopes
        if (current_scope_->kind() == ScopeKind::Block ||
            current_scope_->kind() == ScopeKind::Function) {
            check_unused_vars(current_scope_);
        }
        current_scope_ = current_scope_->parent();
    }
}

Symbol* Checker::declare(SymbolKind kind, std::string_view name, Type* type,
                         SourceLocation loc, ast::Decl* decl_node) {
    auto* sym = arena_.create<Symbol>();
    sym->kind = kind;
    sym->name = name;
    sym->type = type;
    sym->loc = loc;
    sym->decl_node = decl_node;

    if (auto* existing = current_scope_->insert(sym)) {
        diag_.error(loc, "{} redeclared in this block", name);
        diag_.note(existing->loc, "previous declaration here");
        return existing;
    }
    return sym;
}

Symbol* Checker::lookup(std::string_view name) {
    return current_scope_->lookup(name);
}

// ============================================================================
// Type construction helpers
// ============================================================================

Type* Checker::make_basic_type(BasicKind kind) {
    return basic_type(kind);
}

Type* Checker::make_pointer_type(Type* base) {
    auto* t = arena_.create<Type>();
    t->kind = TypeKind::Pointer;
    t->pointer.base = base;
    type_cache_.push_back(t);
    return t;
}

Type* Checker::make_slice_type(Type* elem) {
    auto* t = arena_.create<Type>();
    t->kind = TypeKind::Slice;
    t->slice.element = elem;
    type_cache_.push_back(t);
    return t;
}

Type* Checker::make_array_type(Type* elem, int64_t length) {
    auto* t = arena_.create<Type>();
    t->kind = TypeKind::Array;
    t->array.element = elem;
    t->array.length = length;
    type_cache_.push_back(t);
    return t;
}

Type* Checker::make_map_type(Type* key, Type* value) {
    auto* t = arena_.create<Type>();
    t->kind = TypeKind::Map;
    t->map.key = key;
    t->map.value = value;
    type_cache_.push_back(t);
    return t;
}

Type* Checker::make_chan_type(Type* elem, ChanDir dir) {
    auto* t = arena_.create<Type>();
    t->kind = TypeKind::Chan;
    t->chan.element = elem;
    t->chan.dir = dir;
    type_cache_.push_back(t);
    return t;
}

Type* Checker::make_func_type(FuncType* ft) {
    auto* t = arena_.create<Type>();
    t->kind = TypeKind::Func;
    t->func = ft;
    type_cache_.push_back(t);
    return t;
}

Type* Checker::make_named_type(std::string_view name, Type* und) {
    auto* nt = arena_.create<NamedType>();
    nt->name = name;
    nt->underlying = und;

    auto* t = arena_.create<Type>();
    t->kind = TypeKind::Named;
    t->named = nt;
    type_cache_.push_back(t);
    return t;
}

Type* Checker::make_struct_type(StructType* st) {
    auto* t = arena_.create<Type>();
    t->kind = TypeKind::Struct;
    t->struct_ = st;
    type_cache_.push_back(t);
    return t;
}

Type* Checker::make_interface_type(InterfaceType* it) {
    auto* t = arena_.create<Type>();
    t->kind = TypeKind::Interface;
    t->interface_ = it;
    type_cache_.push_back(t);
    return t;
}

Type* Checker::make_tuple_type(const std::vector<Type*>& types) {
    auto* tt = arena_.create<TupleType>();
    tt->types = types;

    auto* t = arena_.create<Type>();
    t->kind = TypeKind::Tuple;
    t->tuple = tt;
    type_cache_.push_back(t);
    return t;
}

// ============================================================================
// Assignability
// ============================================================================

bool Checker::assignable_to(Type* src, Type* dst) {
    if (!src || !dst) return false;
    if (identical(src, dst)) return true;

    // Untyped nil can be assigned to pointer, func, slice, map, chan, interface
    if (src->kind == TypeKind::Basic && src->basic == BasicKind::UntypedNil) {
        const Type* u = underlying(dst);
        if (!u) return false;
        switch (u->kind) {
            case TypeKind::Pointer:
            case TypeKind::Func:
            case TypeKind::Slice:
            case TypeKind::Map:
            case TypeKind::Chan:
            case TypeKind::Interface:
                return true;
            default:
                return false;
        }
    }

    // Untyped constant assignable if representable
    if (is_untyped(src) && !is_untyped(dst)) {
        // Check basic type compatibility
        const Type* du = underlying(dst);
        if (!du) return false;
        // Untyped constants are always assignable to interface{} (empty interface).
        // Any non-nil value satisfies interface{}, so this is valid in Go.
        if (du->kind == TypeKind::Interface) {
            return satisfies_interface(src, du->interface_);
        }
        if (du->kind == TypeKind::Basic) {
            const auto& si = basic_info(src->basic);
            const auto& di = basic_info(du->basic);
            // Untyped bool → bool
            if (si.is_boolean && di.is_boolean) return true;
            // Untyped string → string
            if (si.is_string && di.is_string) return true;
            // Untyped numeric → compatible numeric
            if (si.is_numeric && di.is_numeric) return true;
        }
        // Untyped int → named type with int underlying
        return false;
    }

    // Identical underlying types where at least one is unnamed
    const Type* su = underlying(src);
    const Type* du = underlying(dst);
    if (su && du && identical(su, du)) {
        // At least one must be unnamed
        if (src->kind != TypeKind::Named || dst->kind != TypeKind::Named) {
            return true;
        }
    }

    // Interface satisfaction: src implements dst interface
    if (dst->kind == TypeKind::Named && dst->named && dst->named->underlying) {
        const Type* di = underlying(dst);
        if (di && di->kind == TypeKind::Interface && di->interface_) {
            return satisfies_interface(src, di->interface_);
        }
    }
    if (dst->kind == TypeKind::Interface && dst->interface_) {
        return satisfies_interface(src, dst->interface_);
    }

    return false;
}

bool Checker::representable(const ConstValue* val, Type* target) {
    if (!val || !val->is_valid() || !target) return false;

    const Type* u = underlying(target);
    if (!u || u->kind != TypeKind::Basic) return false;

    const auto& info = basic_info(u->basic);

    if (val->is_int() && info.is_integer) return true;
    if (val->is_int() && info.is_float) return true;
    if (val->is_float() && info.is_float) return true;
    if (val->is_float() && info.is_integer) {
        // Float representable as int only if it's a whole number
        int64_t i = 0;
        return val->to_int(i);
    }
    if (val->is_bool() && info.is_boolean) return true;
    if (val->is_string() && info.is_string) return true;

    return false;
}

// ============================================================================
// Interface satisfaction
// ============================================================================

bool Checker::satisfies_interface(Type* concrete, InterfaceType* iface) {
    if (!iface) return true; // empty interface
    if (iface->methods.empty() && iface->embedded.empty()) return true;

    for (const auto& method : iface->methods) {
        Type* m = lookup_method(concrete, method.name);
        if (!m) return false;

        // m must be a func type matching the interface method signature
        if (m->kind != TypeKind::Func || !m->func || !method.signature) return false;
        // Build a temporary type for comparison
        Type iface_method_type;
        iface_method_type.kind = TypeKind::Func;
        iface_method_type.func = method.signature;
        if (!identical(m, &iface_method_type)) return false;
    }
    return true;
}

Type* Checker::lookup_method(Type* t, std::string_view name) {
    if (!t) return nullptr;

    // Check named type methods
    Type* base = t;
    if (t->kind == TypeKind::Pointer) {
        base = t->pointer.base;
    }

    if (base->kind == TypeKind::Named && base->named) {
        for (const auto& m : base->named->methods) {
            if (m.name == name) {
                // Return the method regardless of pointer_receiver.
                // The caller (check_selector) will set needs_addr_for_recv if
                // a pointer-receiver method is called on a non-pointer value.
                return m.type;
            }
        }
    }

    return nullptr;
}

// ============================================================================
// Untyped promotion
// ============================================================================

Type* Checker::promote_untyped(Type* a, Type* b) {
    if (!a || !b) return nullptr;

    // If both typed, they must be identical
    if (!is_untyped(a) && !is_untyped(b)) {
        if (identical(a, b)) return a;
        return nullptr;
    }

    // If one is typed and the other untyped, result is the typed one
    if (!is_untyped(a)) return a;
    if (!is_untyped(b)) return b;

    // Both untyped — pick the "wider" one
    // Order: complex > float > rune > int > bool
    // (string is separate)
    auto rank = [](BasicKind k) -> int {
        switch (k) {
            case BasicKind::UntypedComplex: return 5;
            case BasicKind::UntypedFloat:   return 4;
            case BasicKind::UntypedRune:    return 3;
            case BasicKind::UntypedInt:     return 2;
            case BasicKind::UntypedBool:    return 1;
            case BasicKind::UntypedString:  return 0;
            default: return -1;
        }
    };

    int ra = rank(a->basic);
    int rb = rank(b->basic);
    return ra >= rb ? a : b;
}

// ============================================================================
// Expression recording
// ============================================================================

void Checker::record_expr(const ast::Expr* expr, ExprInfo info) {
    expr_map_[expr] = info;
}

// ============================================================================
// Constant evaluation
// ============================================================================

ConstValue* Checker::eval_const_expr(ast::Expr* expr) {
    if (!expr) return nullptr;

    switch (expr->kind) {
        case ast::ExprKind::BasicLit: {
            auto& lit = expr->basic_lit;
            if (lit.kind == TokenKind::IntLiteral) {
                try {
                    int64_t val = std::stoll(std::string(lit.value));
                    return arena_.create<ConstValue>(val);
                } catch (...) {
                    return nullptr;
                }
            }
            if (lit.kind == TokenKind::FloatLiteral) {
                try {
                    double val = std::stod(std::string(lit.value));
                    return arena_.create<ConstValue>(val);
                } catch (...) {
                    return nullptr;
                }
            }
            if (lit.kind == TokenKind::StringLiteral) {
                // Strip quotes
                auto sv = lit.value;
                if (sv.size() >= 2) {
                    sv = sv.substr(1, sv.size() - 2);
                }
                return arena_.create<ConstValue>(std::string(sv));
            }
            if (lit.kind == TokenKind::RuneLiteral) {
                auto sv = lit.value;
                if (sv.size() >= 3) { // 'x'
                    return arena_.create<ConstValue>(static_cast<int64_t>(sv[1]));
                }
                return nullptr;
            }
            return nullptr;
        }

        case ast::ExprKind::Ident: {
            // iota is a predeclared identifier in const context
            if (expr->ident.name == "iota") {
                return arena_.create<ConstValue>(current_iota_);
            }
            auto* sym = lookup(expr->ident.name);
            if (sym && sym->kind == SymbolKind::Const && sym->const_val) {
                return sym->const_val;
            }
            return nullptr;
        }

        case ast::ExprKind::Paren:
            return eval_const_expr(expr->paren.x);

        case ast::ExprKind::Unary: {
            auto* operand = eval_const_expr(expr->unary.x);
            if (!operand) return nullptr;
            switch (expr->unary.op) {
                case TokenKind::Minus: {
                    auto r = const_neg(*operand);
                    if (r.is_valid()) return arena_.create<ConstValue>(r);
                    return nullptr;
                }
                case TokenKind::Not: {
                    auto r = const_not(*operand);
                    if (r.is_valid()) return arena_.create<ConstValue>(r);
                    return nullptr;
                }
                case TokenKind::Plus:
                    return operand;
                default:
                    return nullptr;
            }
        }

        case ast::ExprKind::Binary: {
            auto* left = eval_const_expr(expr->binary.left);
            auto* right = eval_const_expr(expr->binary.right);
            if (!left || !right) return nullptr;

            ConstValue result;
            switch (expr->binary.op) {
                case TokenKind::Plus:     result = const_add(*left, *right); break;
                case TokenKind::Minus:    result = const_sub(*left, *right); break;
                case TokenKind::Star:     result = const_mul(*left, *right); break;
                case TokenKind::Slash:    result = const_div(*left, *right); break;
                case TokenKind::Percent:  result = const_mod(*left, *right); break;
                case TokenKind::Equal:    result = const_eq(*left, *right); break;
                case TokenKind::NotEqual: result = const_neq(*left, *right); break;
                case TokenKind::Less:     result = const_lt(*left, *right); break;
                case TokenKind::LessEqual:result = const_le(*left, *right); break;
                case TokenKind::Greater:  result = const_gt(*left, *right); break;
                case TokenKind::GreaterEqual: result = const_ge(*left, *right); break;
                default: return nullptr;
            }
            if (result.is_valid()) return arena_.create<ConstValue>(result);
            return nullptr;
        }

        default:
            return nullptr;
    }
}

} // namespace sema
} // namespace golangc
