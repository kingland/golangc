#pragma once

#include "ir/ir_pass.hpp"
#include "ir/ir.hpp"

namespace golangc {
namespace ir {

// ============================================================================
// Pass 1: Mem2Reg — promote simple alloca/store/load to direct values
// ============================================================================

class Mem2RegPass : public IRPass {
public:
    [[nodiscard]] std::string_view name() const override { return "mem2reg"; }
    bool run(Module& module) override;

private:
    bool run_function(Function& fn);
};

// ============================================================================
// Pass 2: Constant Folding — evaluate constant expressions at compile time
// ============================================================================

class ConstFoldPass : public IRPass {
public:
    [[nodiscard]] std::string_view name() const override { return "constfold"; }
    bool run(Module& module) override;

private:
    bool run_function(Function& fn);
};

// ============================================================================
// Pass 3: Constant Propagation — substitute constants into uses
// ============================================================================

class ConstPropPass : public IRPass {
public:
    [[nodiscard]] std::string_view name() const override { return "constprop"; }
    bool run(Module& module) override;

private:
    bool run_function(Function& fn);
};

// ============================================================================
// Pass 4: Dead Code Elimination — remove unused instructions + unreachable blocks
// ============================================================================

class DCEPass : public IRPass {
public:
    [[nodiscard]] std::string_view name() const override { return "dce"; }
    bool run(Module& module) override;

private:
    bool run_function(Function& fn);
    bool remove_unreachable_blocks(Function& fn);
};

} // namespace ir
} // namespace golangc
