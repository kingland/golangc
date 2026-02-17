#include "sema/types.hpp"

#include <fmt/format.h>

#include <array>

namespace golangc {
namespace sema {

// ============================================================================
// BasicInfo table
// ============================================================================

namespace {

// clang-format off
constexpr std::array<BasicInfo, static_cast<size_t>(BasicKind::Invalid) + 1> kBasicInfoTable = {{
    // kind                  name              size  num   int   uns   flt   cplx  str   bool  untyped
    {BasicKind::Bool,        "bool",           1,    false,false,false,false,false,false,true, false},
    {BasicKind::Int,         "int",            8,    true, true, false,false,false,false,false,false},
    {BasicKind::Int8,        "int8",           1,    true, true, false,false,false,false,false,false},
    {BasicKind::Int16,       "int16",          2,    true, true, false,false,false,false,false,false},
    {BasicKind::Int32,       "int32",          4,    true, true, false,false,false,false,false,false},
    {BasicKind::Int64,       "int64",          8,    true, true, false,false,false,false,false,false},
    {BasicKind::Uint,        "uint",           8,    true, true, true, false,false,false,false,false},
    {BasicKind::Uint8,       "uint8",          1,    true, true, true, false,false,false,false,false},
    {BasicKind::Uint16,      "uint16",         2,    true, true, true, false,false,false,false,false},
    {BasicKind::Uint32,      "uint32",         4,    true, true, true, false,false,false,false,false},
    {BasicKind::Uint64,      "uint64",         8,    true, true, true, false,false,false,false,false},
    {BasicKind::Uintptr,     "uintptr",        8,    true, true, true, false,false,false,false,false},
    {BasicKind::Float32,     "float32",        4,    true, false,false,true, false,false,false,false},
    {BasicKind::Float64,     "float64",        8,    true, false,false,true, false,false,false,false},
    {BasicKind::Complex64,   "complex64",      8,    true, false,false,false,true, false,false,false},
    {BasicKind::Complex128,  "complex128",     16,   true, false,false,false,true, false,false,false},
    {BasicKind::String,      "string",         16,   false,false,false,false,false,true, false,false},
    {BasicKind::UntypedBool, "untyped bool",   0,    false,false,false,false,false,false,true, true},
    {BasicKind::UntypedInt,  "untyped int",    0,    true, true, false,false,false,false,false,true},
    {BasicKind::UntypedRune, "untyped rune",   0,    true, true, false,false,false,false,false,true},
    {BasicKind::UntypedFloat,"untyped float",  0,    true, false,false,true, false,false,false,true},
    {BasicKind::UntypedComplex,"untyped complex",0,  true, false,false,false,true, false,false,true},
    {BasicKind::UntypedString,"untyped string",0,    false,false,false,false,false,true, false,true},
    {BasicKind::UntypedNil,  "untyped nil",    0,    false,false,false,false,false,false,false,true},
    {BasicKind::Invalid,     "<invalid>",      0,    false,false,false,false,false,false,false,false},
}};
// clang-format on

} // namespace

const BasicInfo& basic_info(BasicKind kind) {
    auto idx = static_cast<size_t>(kind);
    if (idx >= kBasicInfoTable.size()) {
        return kBasicInfoTable.back();
    }
    return kBasicInfoTable[idx];
}

// ============================================================================
// type_string
// ============================================================================

std::string type_string(const Type* t) {
    if (!t) {
        return "<nil>";
    }

    switch (t->kind) {
        case TypeKind::Invalid:
            return "<invalid>";

        case TypeKind::Basic:
            return std::string(basic_info(t->basic).name);

        case TypeKind::Array:
            return fmt::format("[{}]{}", t->array.length, type_string(t->array.element));

        case TypeKind::Slice:
            return fmt::format("[]{}", type_string(t->slice.element));

        case TypeKind::Map:
            return fmt::format("map[{}]{}", type_string(t->map.key),
                               type_string(t->map.value));

        case TypeKind::Chan: {
            switch (t->chan.dir) {
                case ChanDir::SendRecv:
                    return fmt::format("chan {}", type_string(t->chan.element));
                case ChanDir::SendOnly:
                    return fmt::format("chan<- {}", type_string(t->chan.element));
                case ChanDir::RecvOnly:
                    return fmt::format("<-chan {}", type_string(t->chan.element));
            }
            return "<invalid chan>";
        }

        case TypeKind::Pointer:
            return fmt::format("*{}", type_string(t->pointer.base));

        case TypeKind::Func: {
            if (!t->func) return "func()";
            std::string result = "func(";
            for (size_t i = 0; i < t->func->params.size(); ++i) {
                if (i > 0) result += ", ";
                if (t->func->params[i].is_variadic) {
                    result += "...";
                    // Variadic param type is a slice; print element type
                    if (t->func->params[i].type &&
                        t->func->params[i].type->kind == TypeKind::Slice) {
                        result += type_string(t->func->params[i].type->slice.element);
                    } else {
                        result += type_string(t->func->params[i].type);
                    }
                } else {
                    result += type_string(t->func->params[i].type);
                }
            }
            result += ")";
            if (t->func->results.size() == 1) {
                result += " " + type_string(t->func->results[0].type);
            } else if (t->func->results.size() > 1) {
                result += " (";
                for (size_t i = 0; i < t->func->results.size(); ++i) {
                    if (i > 0) result += ", ";
                    result += type_string(t->func->results[i].type);
                }
                result += ")";
            }
            return result;
        }

        case TypeKind::Struct: {
            if (!t->struct_) return "struct{}";
            std::string result = "struct{";
            for (size_t i = 0; i < t->struct_->fields.size(); ++i) {
                if (i > 0) result += "; ";
                const auto& f = t->struct_->fields[i];
                if (!f.name.empty()) {
                    result += std::string(f.name) + " ";
                }
                result += type_string(f.type);
            }
            result += "}";
            return result;
        }

        case TypeKind::Interface: {
            if (!t->interface_) return "interface{}";
            if (t->interface_->methods.empty() && t->interface_->embedded.empty()) {
                return "interface{}";
            }
            std::string result = "interface{";
            for (size_t i = 0; i < t->interface_->methods.size(); ++i) {
                if (i > 0) result += "; ";
                result += std::string(t->interface_->methods[i].name) + "()";
            }
            result += "}";
            return result;
        }

        case TypeKind::Named:
            if (t->named) {
                return std::string(t->named->name);
            }
            return "<named?>";

        case TypeKind::Tuple: {
            if (!t->tuple) return "()";
            std::string result = "(";
            for (size_t i = 0; i < t->tuple->types.size(); ++i) {
                if (i > 0) result += ", ";
                result += type_string(t->tuple->types[i]);
            }
            result += ")";
            return result;
        }
    }

    return "<unknown>";
}

// ============================================================================
// identical
// ============================================================================

bool identical(const Type* a, const Type* b) {
    if (a == b) return true;
    if (!a || !b) return false;
    if (a->kind != b->kind) return false;

    switch (a->kind) {
        case TypeKind::Invalid:
            return true;

        case TypeKind::Basic:
            return a->basic == b->basic;

        case TypeKind::Array:
            return a->array.length == b->array.length &&
                   identical(a->array.element, b->array.element);

        case TypeKind::Slice:
            return identical(a->slice.element, b->slice.element);

        case TypeKind::Map:
            return identical(a->map.key, b->map.key) &&
                   identical(a->map.value, b->map.value);

        case TypeKind::Chan:
            return a->chan.dir == b->chan.dir &&
                   identical(a->chan.element, b->chan.element);

        case TypeKind::Pointer:
            return identical(a->pointer.base, b->pointer.base);

        case TypeKind::Func: {
            if (!a->func || !b->func) return a->func == b->func;
            if (a->func->params.size() != b->func->params.size()) return false;
            if (a->func->results.size() != b->func->results.size()) return false;
            if (a->func->is_variadic != b->func->is_variadic) return false;
            for (size_t i = 0; i < a->func->params.size(); ++i) {
                if (!identical(a->func->params[i].type, b->func->params[i].type))
                    return false;
            }
            for (size_t i = 0; i < a->func->results.size(); ++i) {
                if (!identical(a->func->results[i].type, b->func->results[i].type))
                    return false;
            }
            return true;
        }

        case TypeKind::Struct: {
            if (!a->struct_ || !b->struct_) return a->struct_ == b->struct_;
            if (a->struct_->fields.size() != b->struct_->fields.size()) return false;
            for (size_t i = 0; i < a->struct_->fields.size(); ++i) {
                if (a->struct_->fields[i].name != b->struct_->fields[i].name)
                    return false;
                if (!identical(a->struct_->fields[i].type, b->struct_->fields[i].type))
                    return false;
                if (a->struct_->fields[i].tag != b->struct_->fields[i].tag)
                    return false;
            }
            return true;
        }

        case TypeKind::Interface: {
            if (!a->interface_ || !b->interface_)
                return a->interface_ == b->interface_;
            if (a->interface_->methods.size() != b->interface_->methods.size())
                return false;
            for (size_t i = 0; i < a->interface_->methods.size(); ++i) {
                if (a->interface_->methods[i].name != b->interface_->methods[i].name)
                    return false;
                // Compare method signatures as func types
                // Build temporary Type wrappers for comparison
                Type fa, fb;
                fa.kind = TypeKind::Func;
                fa.func = a->interface_->methods[i].signature;
                fb.kind = TypeKind::Func;
                fb.func = b->interface_->methods[i].signature;
                if (!identical(&fa, &fb)) return false;
            }
            return true;
        }

        case TypeKind::Named:
            // Named types are identical only if they are the same named type
            return a->named == b->named;

        case TypeKind::Tuple: {
            if (!a->tuple || !b->tuple) return a->tuple == b->tuple;
            if (a->tuple->types.size() != b->tuple->types.size()) return false;
            for (size_t i = 0; i < a->tuple->types.size(); ++i) {
                if (!identical(a->tuple->types[i], b->tuple->types[i]))
                    return false;
            }
            return true;
        }
    }

    return false;
}

// ============================================================================
// underlying
// ============================================================================

const Type* underlying(const Type* t) {
    while (t && t->kind == TypeKind::Named && t->named && t->named->underlying) {
        t = t->named->underlying;
    }
    return t;
}

Type* underlying(Type* t) {
    while (t && t->kind == TypeKind::Named && t->named && t->named->underlying) {
        t = t->named->underlying;
    }
    return t;
}

// ============================================================================
// default_type
// ============================================================================

namespace {
// These are lazily created singletons for default types
Type g_basic_types[static_cast<size_t>(BasicKind::Invalid) + 1];
bool g_basic_types_init = false;

void ensure_basic_types() {
    if (g_basic_types_init) return;
    g_basic_types_init = true;
    for (size_t i = 0; i <= static_cast<size_t>(BasicKind::Invalid); ++i) {
        g_basic_types[i].kind = TypeKind::Basic;
        g_basic_types[i].basic = static_cast<BasicKind>(i);
    }
}
} // namespace

Type* default_type(Type* t) {
    ensure_basic_types();
    if (!t || t->kind != TypeKind::Basic) return t;

    switch (t->basic) {
        case BasicKind::UntypedBool:
            return &g_basic_types[static_cast<size_t>(BasicKind::Bool)];
        case BasicKind::UntypedInt:
            return &g_basic_types[static_cast<size_t>(BasicKind::Int)];
        case BasicKind::UntypedRune:
            return &g_basic_types[static_cast<size_t>(BasicKind::Int32)];
        case BasicKind::UntypedFloat:
            return &g_basic_types[static_cast<size_t>(BasicKind::Float64)];
        case BasicKind::UntypedComplex:
            return &g_basic_types[static_cast<size_t>(BasicKind::Complex128)];
        case BasicKind::UntypedString:
            return &g_basic_types[static_cast<size_t>(BasicKind::String)];
        case BasicKind::UntypedNil:
            return t; // nil has no default type
        default:
            return t;
    }
}

// ============================================================================
// Type predicates
// ============================================================================

bool is_untyped(const Type* t) {
    if (!t || t->kind != TypeKind::Basic) return false;
    return basic_info(t->basic).is_untyped;
}

bool is_numeric(const Type* t) {
    const Type* u = underlying(t);
    if (!u || u->kind != TypeKind::Basic) return false;
    return basic_info(u->basic).is_numeric;
}

bool is_integer(const Type* t) {
    const Type* u = underlying(t);
    if (!u || u->kind != TypeKind::Basic) return false;
    return basic_info(u->basic).is_integer;
}

bool is_boolean(const Type* t) {
    const Type* u = underlying(t);
    if (!u || u->kind != TypeKind::Basic) return false;
    return basic_info(u->basic).is_boolean;
}

bool is_string(const Type* t) {
    const Type* u = underlying(t);
    if (!u || u->kind != TypeKind::Basic) return false;
    return basic_info(u->basic).is_string;
}

bool is_ordered(const Type* t) {
    const Type* u = underlying(t);
    if (!u || u->kind != TypeKind::Basic) return false;
    const auto& info = basic_info(u->basic);
    return info.is_integer || info.is_float || info.is_string;
}

bool is_comparable(const Type* t) {
    const Type* u = underlying(t);
    if (!u) return false;

    switch (u->kind) {
        case TypeKind::Basic:
            return u->basic != BasicKind::Invalid;
        case TypeKind::Pointer:
        case TypeKind::Chan:
        case TypeKind::Interface:
            return true;
        case TypeKind::Array:
            return is_comparable(u->array.element);
        case TypeKind::Struct: {
            if (!u->struct_) return true;
            for (const auto& f : u->struct_->fields) {
                if (!is_comparable(f.type)) return false;
            }
            return true;
        }
        default:
            return false; // Slice, Map, Func are not comparable
    }
}

} // namespace sema
} // namespace golangc
