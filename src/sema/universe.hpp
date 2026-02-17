#pragma once

#include "sema/scope.hpp"
#include "sema/types.hpp"
#include "common/arena_allocator.hpp"

namespace golangc {
namespace sema {

/// Built-in function IDs.
enum class BuiltinId : int {
    Println = 0,
    Print,
    Len,
    Cap,
    Make,
    New,
    Append,
    Copy,
    Delete,
    Close,
    Panic,
    Recover,
    // Sentinel
    Count
};

/// Initialize the universe scope with all predeclared Go identifiers.
/// Returns the universe scope. All types/symbols are arena-allocated.
[[nodiscard]] Scope* init_universe(ArenaAllocator& arena);

/// Get a cached basic type (singleton).
[[nodiscard]] Type* basic_type(BasicKind kind);

/// Get the predeclared 'error' interface type.
[[nodiscard]] Type* error_type();

} // namespace sema
} // namespace golangc
