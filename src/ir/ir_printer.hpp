#pragma once

#include "ir/ir.hpp"

#include <string>

namespace golangc {
namespace ir {

/// Prints IR in a human-readable LLVM-IR-style text format.
class IRPrinter {
public:
    /// Print an entire module.
    [[nodiscard]] std::string print(const Module& module);

    /// Print a single function.
    [[nodiscard]] std::string print_function(const Function& func);

    /// Print a single instruction.
    [[nodiscard]] std::string print_instruction(const Instruction& inst);

private:
    std::string format_value(const Value* val);
    std::string format_type(const IRType* type);
};

} // namespace ir
} // namespace golangc
