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

// Singleton strings.Builder pointer type (*strings.Builder)
Type* g_strings_builder_ptr_type = nullptr;

// Singleton sync.Mutex pointer type (*sync.Mutex)
Type* g_sync_mutex_ptr_type = nullptr;

// Singleton sync.WaitGroup pointer type (*sync.WaitGroup)
Type* g_sync_waitgroup_ptr_type = nullptr;

// Singleton os.File pointer type (*os.File)
Type* g_os_file_ptr_type = nullptr;

// Singleton bufio.Scanner pointer type (*bufio.Scanner)
Type* g_bufio_scanner_ptr_type = nullptr;

// Singleton bufio.Reader pointer type (*bufio.Reader)
Type* g_bufio_reader_ptr_type = nullptr;

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

Type* strings_builder_ptr_type() {
    return g_strings_builder_ptr_type;
}

Type* sync_mutex_ptr_type() {
    return g_sync_mutex_ptr_type;
}

Type* sync_waitgroup_ptr_type() {
    return g_sync_waitgroup_ptr_type;
}

Type* os_file_ptr_type() {
    return g_os_file_ptr_type;
}

Type* bufio_scanner_ptr_type() {
    return g_bufio_scanner_ptr_type;
}

Type* bufio_reader_ptr_type() {
    return g_bufio_reader_ptr_type;
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
    (void)scope->insert(make_pseudo_pkg("errors"));
    (void)scope->insert(make_pseudo_pkg("sync"));
    (void)scope->insert(make_pseudo_pkg("io"));
    (void)scope->insert(make_pseudo_pkg("bufio"));

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

    // ---- strings.Builder opaque named type ----
    // Implemented as an opaque pointer in the runtime (like golangc_map).
    // We represent it as a Named type wrapping a pointer-to-void (uintptr).
    // The pointer type itself is what user code holds.
    auto* builder_underlying = arena.create<Type>();
    builder_underlying->kind = TypeKind::Basic;
    builder_underlying->basic = BasicKind::Uintptr; // opaque handle

    auto* builder_named = arena.create<NamedType>();
    builder_named->name = "strings.Builder";
    builder_named->underlying = builder_underlying;
    // Register pointer-receiver methods on the Named type.
    // We register them with pointer_receiver=true; the IR dispatch
    // detects "strings.Builder.X" and calls the runtime.
    auto make_method = [&](std::string_view name, Type* func_type) {
        builder_named->methods.push_back(NamedType::Method{name, func_type, true});
    };
    // Build minimal func types for each method (return types used in sema checks).
    auto* void_ft = arena.create<FuncType>();
    auto* string_ft_raw = arena.create<FuncType>();
    string_ft_raw->results.push_back(FuncParam{"", basic_type(BasicKind::String)});
    auto* int_ft_raw = arena.create<FuncType>();
    int_ft_raw->results.push_back(FuncParam{"", basic_type(BasicKind::Int)});

    auto* void_ty = arena.create<Type>();
    void_ty->kind = TypeKind::Func;
    void_ty->func = void_ft;

    auto* str_ty2 = arena.create<Type>();
    str_ty2->kind = TypeKind::Func;
    str_ty2->func = string_ft_raw;

    auto* int_ty2 = arena.create<Type>();
    int_ty2->kind = TypeKind::Func;
    int_ty2->func = int_ft_raw;

    make_method("WriteString", void_ty);
    make_method("WriteByte",   void_ty);
    make_method("String",      str_ty2);
    make_method("Reset",       void_ty);
    make_method("Len",         int_ty2);

    auto* builder_named_ty = arena.create<Type>();
    builder_named_ty->kind = TypeKind::Named;
    builder_named_ty->named = builder_named;

    auto* builder_ptr_ty = arena.create<Type>();
    builder_ptr_ty->kind = TypeKind::Pointer;
    builder_ptr_ty->pointer.base = builder_named_ty;

    g_strings_builder_ptr_type = builder_ptr_ty;

    // ---- sync.Mutex opaque named type ----
    auto* mutex_underlying = arena.create<Type>();
    mutex_underlying->kind = TypeKind::Basic;
    mutex_underlying->basic = BasicKind::Uintptr; // opaque handle

    auto* mutex_named = arena.create<NamedType>();
    mutex_named->name = "sync.Mutex";
    mutex_named->underlying = mutex_underlying;

    // Build bool func type for TryLock
    auto* bool_ft_raw = arena.create<FuncType>();
    bool_ft_raw->results.push_back(FuncParam{"", basic_type(BasicKind::Bool)});
    auto* bool_ty = arena.create<Type>();
    bool_ty->kind = TypeKind::Func;
    bool_ty->func = bool_ft_raw;

    mutex_named->methods.push_back(NamedType::Method{"Lock",    void_ty,  true});
    mutex_named->methods.push_back(NamedType::Method{"Unlock",  void_ty,  true});
    mutex_named->methods.push_back(NamedType::Method{"TryLock", bool_ty,  true});

    auto* mutex_named_ty = arena.create<Type>();
    mutex_named_ty->kind = TypeKind::Named;
    mutex_named_ty->named = mutex_named;

    auto* mutex_ptr_ty = arena.create<Type>();
    mutex_ptr_ty->kind = TypeKind::Pointer;
    mutex_ptr_ty->pointer.base = mutex_named_ty;

    g_sync_mutex_ptr_type = mutex_ptr_ty;

    // ---- sync.WaitGroup opaque named type ----
    auto* wg_underlying = arena.create<Type>();
    wg_underlying->kind = TypeKind::Basic;
    wg_underlying->basic = BasicKind::Uintptr; // opaque handle

    auto* wg_named = arena.create<NamedType>();
    wg_named->name = "sync.WaitGroup";
    wg_named->underlying = wg_underlying;

    // Add(delta int) — void; Done() — void; Wait() — void
    wg_named->methods.push_back(NamedType::Method{"Add",  void_ty,  true});
    wg_named->methods.push_back(NamedType::Method{"Done", void_ty,  true});
    wg_named->methods.push_back(NamedType::Method{"Wait", void_ty,  true});

    auto* wg_named_ty = arena.create<Type>();
    wg_named_ty->kind = TypeKind::Named;
    wg_named_ty->named = wg_named;

    auto* wg_ptr_ty = arena.create<Type>();
    wg_ptr_ty->kind = TypeKind::Pointer;
    wg_ptr_ty->pointer.base = wg_named_ty;

    g_sync_waitgroup_ptr_type = wg_ptr_ty;

    // ---- os.File opaque named type ----
    auto* file_underlying = arena.create<Type>();
    file_underlying->kind = TypeKind::Basic;
    file_underlying->basic = BasicKind::Uintptr; // opaque handle

    auto* file_named = arena.create<NamedType>();
    file_named->name = "os.File";
    file_named->underlying = file_underlying;

    // Add stub methods so lookup_method() succeeds and check_selector enters if(method).
    file_named->methods.push_back(NamedType::Method{"Close",       void_ty, true});
    file_named->methods.push_back(NamedType::Method{"WriteString", void_ty, true});

    auto* file_named_ty = arena.create<Type>();
    file_named_ty->kind = TypeKind::Named;
    file_named_ty->named = file_named;

    auto* file_ptr_ty = arena.create<Type>();
    file_ptr_ty->kind = TypeKind::Pointer;
    file_ptr_ty->pointer.base = file_named_ty;

    g_os_file_ptr_type = file_ptr_ty;

    // ---- bufio.Scanner opaque named type ----
    auto* scanner_underlying = arena.create<Type>();
    scanner_underlying->kind = TypeKind::Basic;
    scanner_underlying->basic = BasicKind::Uintptr;

    auto* scanner_named = arena.create<NamedType>();
    scanner_named->name = "bufio.Scanner";
    scanner_named->underlying = scanner_underlying;
    scanner_named->methods.push_back(NamedType::Method{"Scan", void_ty, true});
    scanner_named->methods.push_back(NamedType::Method{"Text", void_ty, true});
    scanner_named->methods.push_back(NamedType::Method{"Err",  void_ty, true});

    auto* scanner_named_ty = arena.create<Type>();
    scanner_named_ty->kind = TypeKind::Named;
    scanner_named_ty->named = scanner_named;

    auto* scanner_ptr_ty = arena.create<Type>();
    scanner_ptr_ty->kind = TypeKind::Pointer;
    scanner_ptr_ty->pointer.base = scanner_named_ty;

    g_bufio_scanner_ptr_type = scanner_ptr_ty;

    // ---- bufio.Reader opaque named type ----
    auto* breader_underlying = arena.create<Type>();
    breader_underlying->kind = TypeKind::Basic;
    breader_underlying->basic = BasicKind::Uintptr;

    auto* breader_named = arena.create<NamedType>();
    breader_named->name = "bufio.Reader";
    breader_named->underlying = breader_underlying;
    breader_named->methods.push_back(NamedType::Method{"ReadString", void_ty, true});

    auto* breader_named_ty = arena.create<Type>();
    breader_named_ty->kind = TypeKind::Named;
    breader_named_ty->named = breader_named;

    auto* breader_ptr_ty = arena.create<Type>();
    breader_ptr_ty->kind = TypeKind::Pointer;
    breader_ptr_ty->pointer.base = breader_named_ty;

    g_bufio_reader_ptr_type = breader_ptr_ty;

    return scope;
}

} // namespace sema
} // namespace golangc
