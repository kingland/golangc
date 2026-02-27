#include "sema/universe.hpp"
#include "sema/constant.hpp"

#include <array>

namespace golangc {
namespace sema {

namespace {

// Singleton basic types
std::array<Type, static_cast<size_t>(BasicKind::Invalid) + 1> g_basic_types{};
bool g_basic_types_initialized = false;

// Singleton error type
Type* g_error_type = nullptr;

void ensure_basic_types_initialized() {
    if (g_basic_types_initialized) return;
    g_basic_types_initialized = true;
    for (size_t i = 0; i <= static_cast<size_t>(BasicKind::Invalid); ++i) {
        g_basic_types[i].kind = TypeKind::Basic;
        g_basic_types[i].basic = static_cast<BasicKind>(i);
    }
}

Symbol* make_type_symbol(ArenaAllocator& arena, std::string_view name, Type* type) {
    auto* sym = arena.create<Symbol>();
    sym->kind = SymbolKind::Type;
    sym->name = name;
    sym->type = type;
    sym->used = true; // Universe symbols are always "used"
    return sym;
}

Symbol* make_const_symbol(ArenaAllocator& arena, std::string_view name, Type* type,
                          ConstValue* val) {
    auto* sym = arena.create<Symbol>();
    sym->kind = SymbolKind::Const;
    sym->name = name;
    sym->type = type;
    sym->const_val = val;
    sym->used = true;
    return sym;
}

Symbol* make_builtin_symbol(ArenaAllocator& arena, std::string_view name, BuiltinId id) {
    auto* sym = arena.create<Symbol>();
    sym->kind = SymbolKind::Builtin;
    sym->name = name;
    sym->builtin_id = static_cast<int>(id);
    sym->used = true;
    return sym;
}

Symbol* make_nil_symbol(ArenaAllocator& arena) {
    auto* sym = arena.create<Symbol>();
    sym->kind = SymbolKind::Nil;
    sym->name = "nil";
    sym->type = basic_type(BasicKind::UntypedNil);
    sym->used = true;
    return sym;
}

} // namespace

Type* basic_type(BasicKind kind) {
    ensure_basic_types_initialized();
    auto idx = static_cast<size_t>(kind);
    if (idx >= g_basic_types.size()) {
        return &g_basic_types.back(); // Invalid
    }
    return &g_basic_types[idx];
}

Type* error_type() {
    return g_error_type;
}

Scope* init_universe(ArenaAllocator& arena) {
    ensure_basic_types_initialized();

    auto* scope = arena.create<Scope>(ScopeKind::Universe, nullptr);

    // ---- Basic types ----
    struct TypeEntry {
        std::string_view name;
        BasicKind kind;
    };
    constexpr TypeEntry basic_entries[] = {
        {"bool", BasicKind::Bool},
        {"int", BasicKind::Int},
        {"int8", BasicKind::Int8},
        {"int16", BasicKind::Int16},
        {"int32", BasicKind::Int32},
        {"int64", BasicKind::Int64},
        {"uint", BasicKind::Uint},
        {"uint8", BasicKind::Uint8},
        {"uint16", BasicKind::Uint16},
        {"uint32", BasicKind::Uint32},
        {"uint64", BasicKind::Uint64},
        {"uintptr", BasicKind::Uintptr},
        {"float32", BasicKind::Float32},
        {"float64", BasicKind::Float64},
        {"complex64", BasicKind::Complex64},
        {"complex128", BasicKind::Complex128},
        {"string", BasicKind::String},
    };

    for (const auto& entry : basic_entries) {
        (void)scope->insert(
            make_type_symbol(arena, entry.name, basic_type(entry.kind)));
    }

    // byte = uint8, rune = int32
    (void)scope->insert(make_type_symbol(arena, "byte", basic_type(BasicKind::Uint8)));
    (void)scope->insert(make_type_symbol(arena, "rune", basic_type(BasicKind::Int32)));

    // ---- Constants: true, false ----
    auto* true_val = arena.create<ConstValue>(true);
    auto* false_val = arena.create<ConstValue>(false);
    (void)scope->insert(
        make_const_symbol(arena, "true", basic_type(BasicKind::UntypedBool), true_val));
    (void)scope->insert(
        make_const_symbol(arena, "false", basic_type(BasicKind::UntypedBool), false_val));

    // ---- nil ----
    (void)scope->insert(make_nil_symbol(arena));

    // ---- Built-in functions ----
    struct BuiltinEntry {
        std::string_view name;
        BuiltinId id;
    };
    constexpr BuiltinEntry builtin_entries[] = {
        {"println", BuiltinId::Println},
        {"print", BuiltinId::Print},
        {"len", BuiltinId::Len},
        {"cap", BuiltinId::Cap},
        {"make", BuiltinId::Make},
        {"new", BuiltinId::New},
        {"append", BuiltinId::Append},
        {"copy", BuiltinId::Copy},
        {"delete", BuiltinId::Delete},
        {"close", BuiltinId::Close},
        {"panic", BuiltinId::Panic},
        {"recover", BuiltinId::Recover},
    };

    for (const auto& entry : builtin_entries) {
        (void)scope->insert(make_builtin_symbol(arena, entry.name, entry.id));
    }

    // ---- Pseudo-packages (fmt, strconv, os) ----
    // These are always in scope so that fmt.Println etc. work without
    // a real package system. An explicit `import "fmt"` is not required
    // (and import declarations are currently ignored by the checker).
    auto make_pseudo_pkg = [&](std::string_view name) -> Symbol* {
        auto* sym = arena.create<Symbol>();
        sym->kind = SymbolKind::PseudoPkg;
        sym->name = name;
        sym->pkg_name = name;
        sym->used = true;
        return sym;
    };
    (void)scope->insert(make_pseudo_pkg("fmt"));
    (void)scope->insert(make_pseudo_pkg("strconv"));
    (void)scope->insert(make_pseudo_pkg("os"));
    (void)scope->insert(make_pseudo_pkg("strings"));
    (void)scope->insert(make_pseudo_pkg("math"));

    // ---- error interface type ----
    // type error interface { Error() string }
    auto* error_iface = arena.create<InterfaceType>();
    auto* error_method_sig = arena.create<FuncType>();
    error_method_sig->results.push_back(FuncParam{"", basic_type(BasicKind::String)});
    error_iface->methods.push_back(InterfaceMethod{"Error", error_method_sig});

    auto* error_ty = arena.create<Type>();
    error_ty->kind = TypeKind::Interface;
    error_ty->interface_ = error_iface;
    g_error_type = error_ty;

    (void)scope->insert(make_type_symbol(arena, "error", error_ty));

    return scope;
}

} // namespace sema
} // namespace golangc
