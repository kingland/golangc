#include "sema/scope.hpp"

// Scope implementation is mostly inline in the header.
// This file exists for the library target and future non-inline implementations.

namespace golangc {
namespace sema {

// Currently all Scope methods are inline in scope.hpp.
// This translation unit ensures the library has at least one .cpp file
// and provides a place for future non-inline implementations.

} // namespace sema
} // namespace golangc
