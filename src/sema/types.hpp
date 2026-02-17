#pragma once

#include "common/arena_allocator.hpp"
#include "common/source_location.hpp"

#include <cstdint>
#include <string>
#include <string_view>
#include <vector>

namespace golangc {
namespace sema {

// Forward declarations
struct Type;
struct FuncType;
struct StructType;
struct InterfaceType;
struct NamedType;
struct TupleType;

// ============================================================================
// Type Kinds
// ============================================================================

enum class TypeKind : uint8_t {
    Invalid,
    Basic,
    Array,
    Slice,
    Map,
    Chan,
    Pointer,
    Func,
    Struct,
    Interface,
    Named,
    Tuple,    // For multiple return values
};

enum class BasicKind : uint8_t {
    // Typed basics
    Bool,
    Int, Int8, Int16, Int32, Int64,
    Uint, Uint8, Uint16, Uint32, Uint64,
    Uintptr,
    Float32, Float64,
    Complex64, Complex128,
    String,

    // Untyped constants
    UntypedBool,
    UntypedInt,
    UntypedRune,
    UntypedFloat,
    UntypedComplex,
    UntypedString,
    UntypedNil,

    Invalid,
};

/// Information about a basic type.
struct BasicInfo {
    BasicKind kind;
    std::string_view name;
    uint32_t size;      // Size in bytes (0 for platform-dependent)
    bool is_numeric;
    bool is_integer;
    bool is_unsigned;
    bool is_float;
    bool is_complex;
    bool is_string;
    bool is_boolean;
    bool is_untyped;
};

/// Get info for a basic kind.
[[nodiscard]] const BasicInfo& basic_info(BasicKind kind);

// ============================================================================
// Channel direction (mirrors ast::ChanDir)
// ============================================================================

enum class ChanDir : uint8_t {
    SendRecv = 0,
    SendOnly = 1,
    RecvOnly = 2,
};

// ============================================================================
// Complex type structs (heap-allocated in arena, contain vectors)
// ============================================================================

struct FuncParam {
    std::string_view name;
    Type* type = nullptr;
    bool is_variadic = false;
};

struct FuncType {
    std::vector<FuncParam> params;
    std::vector<FuncParam> results;
    bool is_variadic = false; // last param is ...T
};

struct StructField {
    std::string_view name;
    Type* type = nullptr;
    std::string_view tag;
    bool anonymous = false;  // embedded field
};

struct StructType {
    std::vector<StructField> fields;
};

struct InterfaceMethod {
    std::string_view name;
    FuncType* signature = nullptr;
};

struct InterfaceType {
    std::vector<InterfaceMethod> methods;
    std::vector<Type*> embedded;  // Embedded interfaces
};

struct NamedType {
    std::string_view name;
    Type* underlying = nullptr;

    struct Method {
        std::string_view name;
        Type* type = nullptr;       // Func type
        bool pointer_receiver = false;
    };
    std::vector<Method> methods;
};

struct TupleType {
    std::vector<Type*> types;
};

// ============================================================================
// The Type struct — discriminated union
// ============================================================================

struct Type {
    TypeKind kind = TypeKind::Invalid;

    union {
        // Basic type
        BasicKind basic;

        // Array: element + length
        struct {
            Type* element;
            int64_t length;
        } array;

        // Slice: element
        struct {
            Type* element;
        } slice;

        // Map: key + value
        struct {
            Type* key;
            Type* value;
        } map;

        // Chan: element + direction
        struct {
            Type* element;
            ChanDir dir;
        } chan;

        // Pointer: base type
        struct {
            Type* base;
        } pointer;

        // Complex types (heap-allocated)
        FuncType* func;
        StructType* struct_;
        InterfaceType* interface_;
        NamedType* named;
        TupleType* tuple;
    };

    Type() : kind(TypeKind::Invalid), basic(BasicKind::Invalid) {}
};

// ============================================================================
// Type operations
// ============================================================================

/// Return a human-readable string for a type.
[[nodiscard]] std::string type_string(const Type* t);

/// Check if two types are identical (pointer equality for canonicalized types,
/// but also handles structural comparison for non-canonicalized types).
[[nodiscard]] bool identical(const Type* a, const Type* b);

/// Follow Named types to get the underlying type.
[[nodiscard]] const Type* underlying(const Type* t);
[[nodiscard]] Type* underlying(Type* t);

/// Get the default typed version of an untyped type.
/// UntypedBool -> Bool, UntypedInt -> Int, etc.
[[nodiscard]] Type* default_type(Type* t);

/// Check if a type is an untyped type.
[[nodiscard]] bool is_untyped(const Type* t);

/// Check if a type is numeric.
[[nodiscard]] bool is_numeric(const Type* t);

/// Check if a type is integer.
[[nodiscard]] bool is_integer(const Type* t);

/// Check if a type is boolean.
[[nodiscard]] bool is_boolean(const Type* t);

/// Check if a type is string.
[[nodiscard]] bool is_string(const Type* t);

/// Check if a type is ordered (supports < > <= >=).
[[nodiscard]] bool is_ordered(const Type* t);

/// Check if a type is comparable (supports == !=).
[[nodiscard]] bool is_comparable(const Type* t);

} // namespace sema
} // namespace golangc
