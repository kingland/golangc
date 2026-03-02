#pragma once

#include "ir/ir.hpp"

#include <memory>
#include <string_view>
#include <vector>

namespace golangc {
namespace ir {

// ============================================================================
// IR Pass Infrastructure
// ============================================================================

/// Base class for all IR transformation passes.
struct IRPass {
    virtual ~IRPass() = default;
    /// Human-readable pass name.
    [[nodiscard]] virtual std::string_view name() const = 0;
    /// Run pass over module. Returns true if any modification was made.
    virtual bool run(Module& module) = 0;
};

/// Runs a sequence of passes to a fixed point.
class PassManager {
    std::vector<std::unique_ptr<IRPass>> passes_;

public:
    void add(std::unique_ptr<IRPass> pass);

    /// Run all passes repeatedly until no pass modifies, capped at max_iters.
    void run(Module& module, int max_iters = 10);

    /// Convenience: Mem2Reg → ConstFold → ConstProp → DCE, to fixed point.
    static void run_default(Module& module);
};

} // namespace ir
} // namespace golangc
