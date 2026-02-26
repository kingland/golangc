#pragma once

#include "ir/ir.hpp"
#include "sema/types.hpp"

#include <unordered_map>

namespace golangc {
namespace ir {

/// Maps sema::Type* to ir::IRType*, with caching and ownership of created types.
class IRTypeMap {
public:
    IRTypeMap();

    /// Map a semantic type to an IR type.
    [[nodiscard]] IRType* map_type(sema::Type* t);

    // Singleton cached primitive types
    [[nodiscard]] IRType* void_type() const { return void_; }
    [[nodiscard]] IRType* i1_type() const { return i1_; }
    [[nodiscard]] IRType* i8_type() const { return i8_; }
    [[nodiscard]] IRType* i16_type() const { return i16_; }
    [[nodiscard]] IRType* i32_type() const { return i32_; }
    [[nodiscard]] IRType* i64_type() const { return i64_; }
    [[nodiscard]] IRType* f32_type() const { return f32_; }
    [[nodiscard]] IRType* f64_type() const { return f64_; }
    [[nodiscard]] IRType* ptr_type() const { return ptr_; }

    /// Get the string layout type: {ptr, i64}
    [[nodiscard]] IRType* string_type() const { return string_; }

    /// Get the slice layout type: {ptr, i64, i64}
    [[nodiscard]] IRType* slice_type() const { return slice_; }

    /// Get the interface layout type: {ptr, ptr}
    [[nodiscard]] IRType* interface_type() const { return iface_; }

    /// Get the fat-closure layout type: {func_ptr, env_ptr} = {ptr, ptr}
    [[nodiscard]] IRType* closure_type() const { return iface_; } // same layout as iface

    /// Get size in bytes of an IR type (for map key/value sizing).
    [[nodiscard]] static int64_t type_size(const IRType* t);

    /// Create an anonymous struct type for multiple return values.
    [[nodiscard]] IRType* make_tuple_type(std::vector<IRType*> fields);

private:
    // Owned types
    std::vector<std::unique_ptr<IRType>> owned_types_;

    // Cache: sema::Type* -> IRType*
    std::unordered_map<const sema::Type*, IRType*> cache_;

    // Primitive singletons
    IRType* void_;
    IRType* i1_;
    IRType* i8_;
    IRType* i16_;
    IRType* i32_;
    IRType* i64_;
    IRType* f32_;
    IRType* f64_;
    IRType* ptr_;

    // Composite singletons
    IRType* string_;    // {ptr, i64}
    IRType* slice_;     // {ptr, i64, i64}
    IRType* iface_;     // {ptr, ptr}

    IRType* make_type(IRTypeKind kind);
    IRType* make_struct_type(std::vector<IRType*> fields, std::string name = "");
    IRType* make_array_type(IRType* elem, int64_t count);
    IRType* make_func_type(IRType* ret, std::vector<IRType*> params);

    IRType* map_basic_type(sema::BasicKind kind);
};

} // namespace ir
} // namespace golangc
