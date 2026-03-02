#include "ir/ir_passes.hpp"

#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstdint>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace golangc {
namespace ir {

// ============================================================================
// PassManager
// ============================================================================

void PassManager::add(std::unique_ptr<IRPass> pass) {
    passes_.push_back(std::move(pass));
}

void PassManager::run(Module& module, int max_iters) {
    for (int iter = 0; iter < max_iters; ++iter) {
        bool modified = false;
        for (auto& pass : passes_) {
            if (pass->run(module))
                modified = true;
        }
        if (!modified)
            break;
    }
}

void PassManager::run_default(Module& module) {
    PassManager pm;
    pm.add(std::make_unique<Mem2RegPass>());
    pm.add(std::make_unique<ConstFoldPass>());
    pm.add(std::make_unique<ConstPropPass>());
    pm.add(std::make_unique<DCEPass>());
    pm.run(module);
}

// ============================================================================
// Shared helper: replace all uses of old_v with new_v inside a function
// ============================================================================

static void replace_value_uses(Function& fn, Value* old_v, Value* new_v) {
    for (auto& block : fn.blocks) {
        for (auto& inst : block->instructions) {
            for (auto& op : inst->operands) {
                if (op == old_v)
                    op = new_v;
            }
        }
    }
}

// ============================================================================
// Helper: is this a "pure" instruction (safe to remove if unused)?
// ============================================================================

static bool is_pure(Opcode op) {
    switch (op) {
    case Opcode::ConstInt:
    case Opcode::ConstFloat:
    case Opcode::ConstBool:
    case Opcode::ConstString:
    case Opcode::ConstNil:
    case Opcode::Add:
    case Opcode::Sub:
    case Opcode::Mul:
    case Opcode::Div:
    case Opcode::Rem:
    case Opcode::FAdd:
    case Opcode::FSub:
    case Opcode::FMul:
    case Opcode::FDiv:
    case Opcode::And:
    case Opcode::Or:
    case Opcode::Xor:
    case Opcode::Shl:
    case Opcode::Shr:
    case Opcode::AndNot:
    case Opcode::Neg:
    case Opcode::FNeg:
    case Opcode::BitNot:
    case Opcode::LogNot:
    case Opcode::Eq:
    case Opcode::Ne:
    case Opcode::Lt:
    case Opcode::Le:
    case Opcode::Gt:
    case Opcode::Ge:
    case Opcode::FEq:
    case Opcode::FNe:
    case Opcode::FLt:
    case Opcode::FLe:
    case Opcode::FGt:
    case Opcode::FGe:
    case Opcode::ZExt:
    case Opcode::SExt:
    case Opcode::Trunc:
    case Opcode::FPExt:
    case Opcode::FPTrunc:
    case Opcode::SIToFP:
    case Opcode::FPToSI:
    case Opcode::Bitcast:
    case Opcode::ExtractValue:
    case Opcode::InsertValue:
    case Opcode::Load:
    case Opcode::GetPtr:
    case Opcode::Alloca:
    case Opcode::StringLen:
    case Opcode::StringIndex:
    case Opcode::SliceLen:
    case Opcode::SliceCap:
    case Opcode::InterfaceData:
    case Opcode::InterfaceType:
    case Opcode::ClosureEnv:
        return true;
    default:
        return false;
    }
}

// ============================================================================
// Helper: is an Instruction a known-constant?
// ============================================================================

static bool is_const_inst(const Instruction* inst) {
    return inst->opcode == Opcode::ConstInt ||
           inst->opcode == Opcode::ConstFloat ||
           inst->opcode == Opcode::ConstBool;
}

// ============================================================================
// Pass 1: Mem2Reg
// ============================================================================

// Returns true if an alloca is "promotable":
// - alloc_type is scalar (I1/I8/I16/I32/I64/F32/F64/Ptr)
// - All uses are Load or Store only (no address-escapes via GetPtr or Call)
// - Exactly one Store in the whole function
static bool is_promotable_alloca(Instruction* alloca_inst, Function& fn) {
    if (!alloca_inst->alloc_type)
        return false;

    // Only promote scalar types
    IRTypeKind k = alloca_inst->alloc_type->kind;
    bool scalar = (k == IRTypeKind::I1 || k == IRTypeKind::I8 ||
                   k == IRTypeKind::I16 || k == IRTypeKind::I32 ||
                   k == IRTypeKind::I64 || k == IRTypeKind::F32 ||
                   k == IRTypeKind::F64 || k == IRTypeKind::Ptr);
    if (!scalar)
        return false;

    int store_count = 0;
    for (auto& block : fn.blocks) {
        for (auto& inst : block->instructions) {
            // Check if this instruction uses alloca_inst as an operand
            for (size_t i = 0; i < inst->operands.size(); ++i) {
                if (inst->operands[i] == alloca_inst) {
                    if (inst->opcode == Opcode::Load) {
                        // OK — load from alloca
                    } else if (inst->opcode == Opcode::Store) {
                        // Store: operands[0]=value, operands[1]=ptr
                        // Address escapes if used as operands[0] (value being stored)
                        if (i == 0) return false;  // storing the alloca ptr somewhere
                        // i==1 means storing to alloca — that's the expected use
                        ++store_count;
                    } else {
                        // Any other use = address escape
                        return false;
                    }
                }
            }
        }
    }

    return store_count == 1;
}

bool Mem2RegPass::run_function(Function& fn) {
    bool modified = false;

    // Collect promotable allocas from the entry block
    // (allocas are placed in the entry block by IR gen)
    bool keep_going = true;
    while (keep_going) {
        keep_going = false;
        for (auto& block : fn.blocks) {
            for (auto& inst : block->instructions) {
                if (inst->opcode != Opcode::Alloca)
                    continue;
                if (!is_promotable_alloca(inst.get(), fn))
                    continue;

                // Find the single store
                Instruction* store_inst = nullptr;
                for (auto& b2 : fn.blocks) {
                    for (auto& i2 : b2->instructions) {
                        if (i2->opcode == Opcode::Store &&
                            i2->operands.size() >= 2 &&
                            i2->operands[1] == inst.get()) {
                            store_inst = i2.get();
                            break;
                        }
                    }
                    if (store_inst) break;
                }

                if (!store_inst)
                    continue;  // no store found — leave as is

                Value* stored_val = store_inst->operands[0];

                // Replace all Load(alloca) results with stored_val
                for (auto& b2 : fn.blocks) {
                    for (auto& i2 : b2->instructions) {
                        if (i2->opcode == Opcode::Load &&
                            i2->operands.size() >= 1 &&
                            i2->operands[0] == inst.get()) {
                            // Replace all uses of this load result
                            replace_value_uses(fn, i2.get(), stored_val);
                        }
                    }
                }

                // Mark store, all loads, and alloca for removal by DCE
                // We do this by replacing them with a dead ConstNil-like op.
                // Actually, we'll directly erase them here.

                // Erase all Load(alloca) instructions
                for (auto& b2 : fn.blocks) {
                    b2->instructions.erase(
                        std::remove_if(b2->instructions.begin(), b2->instructions.end(),
                            [&](const std::unique_ptr<Instruction>& i2) {
                                return i2->opcode == Opcode::Load &&
                                       i2->operands.size() >= 1 &&
                                       i2->operands[0] == inst.get();
                            }),
                        b2->instructions.end());
                }

                // Erase the Store instruction
                for (auto& b2 : fn.blocks) {
                    b2->instructions.erase(
                        std::remove_if(b2->instructions.begin(), b2->instructions.end(),
                            [&](const std::unique_ptr<Instruction>& i2) {
                                return i2.get() == store_inst;
                            }),
                        b2->instructions.end());
                }

                // Erase the Alloca instruction
                // We need a raw ptr before the unique_ptr is potentially moved
                Instruction* alloca_raw = inst.get();
                for (auto& b2 : fn.blocks) {
                    b2->instructions.erase(
                        std::remove_if(b2->instructions.begin(), b2->instructions.end(),
                            [&](const std::unique_ptr<Instruction>& i2) {
                                return i2.get() == alloca_raw;
                            }),
                        b2->instructions.end());
                }

                modified = true;
                keep_going = true;  // re-scan after mutation
                goto next_pass;  // restart outer loops
            }
        }
        next_pass:;
    }

    return modified;
}

bool Mem2RegPass::run(Module& module) {
    bool modified = false;
    for (auto& fn : module.functions) {
        if (!fn->blocks.empty())
            if (run_function(*fn))
                modified = true;
    }
    return modified;
}

// ============================================================================
// Pass 2: Constant Folding
// ============================================================================

// Create a new ConstInt instruction with the same id/type as inst
// We reuse inst's slot by changing its opcode and clearing operands.
static void fold_to_int(Instruction* inst, int64_t val) {
    inst->opcode = Opcode::ConstInt;
    inst->imm_int = val;
    inst->operands.clear();
    // type stays the same (integral)
}

static void fold_to_float(Instruction* inst, double val) {
    inst->opcode = Opcode::ConstFloat;
    inst->imm_float = val;
    inst->operands.clear();
}

bool ConstFoldPass::run_function(Function& fn) {
    bool modified = false;

    for (auto& block : fn.blocks) {
        for (auto& inst : block->instructions) {
            Opcode op = inst->opcode;

            // ---- Unary ops ----
            if (op == Opcode::Neg) {
                if (inst->operands.size() == 1) {
                    auto* src = dynamic_cast<Instruction*>(inst->operands[0]);
                    if (src && src->opcode == Opcode::ConstInt) {
                        fold_to_int(inst.get(), -src->imm_int);
                        modified = true;
                        continue;
                    }
                }
            }
            if (op == Opcode::FNeg) {
                if (inst->operands.size() == 1) {
                    auto* src = dynamic_cast<Instruction*>(inst->operands[0]);
                    if (src && src->opcode == Opcode::ConstFloat) {
                        fold_to_float(inst.get(), -src->imm_float);
                        modified = true;
                        continue;
                    }
                }
            }
            if (op == Opcode::BitNot) {
                if (inst->operands.size() == 1) {
                    auto* src = dynamic_cast<Instruction*>(inst->operands[0]);
                    if (src && src->opcode == Opcode::ConstInt) {
                        fold_to_int(inst.get(), ~src->imm_int);
                        modified = true;
                        continue;
                    }
                }
            }
            if (op == Opcode::LogNot) {
                if (inst->operands.size() == 1) {
                    auto* src = dynamic_cast<Instruction*>(inst->operands[0]);
                    if (src && (src->opcode == Opcode::ConstInt ||
                                src->opcode == Opcode::ConstBool)) {
                        fold_to_int(inst.get(), src->imm_int == 0 ? 1 : 0);
                        modified = true;
                        continue;
                    }
                }
            }

            // ---- Conversion ops with const operand ----
            if (op == Opcode::ZExt || op == Opcode::SExt || op == Opcode::Trunc) {
                if (inst->operands.size() == 1) {
                    auto* src = dynamic_cast<Instruction*>(inst->operands[0]);
                    if (src && src->opcode == Opcode::ConstInt) {
                        int64_t v = src->imm_int;
                        // Trunc: mask to target width
                        if (op == Opcode::Trunc && inst->type) {
                            if (inst->type->kind == IRTypeKind::I1)  v &= 1;
                            if (inst->type->kind == IRTypeKind::I8)  v = (int8_t)v;
                            if (inst->type->kind == IRTypeKind::I16) v = (int16_t)v;
                            if (inst->type->kind == IRTypeKind::I32) v = (int32_t)v;
                        }
                        fold_to_int(inst.get(), v);
                        modified = true;
                        continue;
                    }
                }
            }
            if (op == Opcode::SIToFP) {
                if (inst->operands.size() == 1) {
                    auto* src = dynamic_cast<Instruction*>(inst->operands[0]);
                    if (src && src->opcode == Opcode::ConstInt) {
                        fold_to_float(inst.get(), (double)src->imm_int);
                        modified = true;
                        continue;
                    }
                }
            }
            if (op == Opcode::FPToSI) {
                if (inst->operands.size() == 1) {
                    auto* src = dynamic_cast<Instruction*>(inst->operands[0]);
                    if (src && src->opcode == Opcode::ConstFloat) {
                        fold_to_int(inst.get(), (int64_t)src->imm_float);
                        modified = true;
                        continue;
                    }
                }
            }

            // ---- Binary ops ----
            if (inst->operands.size() != 2)
                continue;

            auto* lhs_inst = dynamic_cast<Instruction*>(inst->operands[0]);
            auto* rhs_inst = dynamic_cast<Instruction*>(inst->operands[1]);

            bool lhs_int = lhs_inst && lhs_inst->opcode == Opcode::ConstInt;
            bool rhs_int = rhs_inst && rhs_inst->opcode == Opcode::ConstInt;
            bool lhs_float = lhs_inst && lhs_inst->opcode == Opcode::ConstFloat;
            bool rhs_float = rhs_inst && rhs_inst->opcode == Opcode::ConstFloat;

            // --- Integer arithmetic ---
            if (lhs_int && rhs_int) {
                int64_t lv = lhs_inst->imm_int;
                int64_t rv = rhs_inst->imm_int;
                switch (op) {
                case Opcode::Add: fold_to_int(inst.get(), lv + rv); modified = true; continue;
                case Opcode::Sub: fold_to_int(inst.get(), lv - rv); modified = true; continue;
                case Opcode::Mul: fold_to_int(inst.get(), lv * rv); modified = true; continue;
                case Opcode::Div:
                    if (rv != 0) { fold_to_int(inst.get(), lv / rv); modified = true; continue; }
                    break;
                case Opcode::Rem:
                    if (rv != 0) { fold_to_int(inst.get(), lv % rv); modified = true; continue; }
                    break;
                case Opcode::And:    fold_to_int(inst.get(), lv & rv);  modified = true; continue;
                case Opcode::Or:     fold_to_int(inst.get(), lv | rv);  modified = true; continue;
                case Opcode::Xor:    fold_to_int(inst.get(), lv ^ rv);  modified = true; continue;
                case Opcode::Shl:    fold_to_int(inst.get(), (rv >= 0 && rv < 64) ? (lv << rv) : 0); modified = true; continue;
                case Opcode::Shr:    fold_to_int(inst.get(), (rv >= 0 && rv < 64) ? (lv >> rv) : 0); modified = true; continue;
                case Opcode::AndNot: fold_to_int(inst.get(), lv & ~rv); modified = true; continue;
                case Opcode::Eq:  fold_to_int(inst.get(), lv == rv ? 1 : 0); modified = true; continue;
                case Opcode::Ne:  fold_to_int(inst.get(), lv != rv ? 1 : 0); modified = true; continue;
                case Opcode::Lt:  fold_to_int(inst.get(), lv <  rv ? 1 : 0); modified = true; continue;
                case Opcode::Le:  fold_to_int(inst.get(), lv <= rv ? 1 : 0); modified = true; continue;
                case Opcode::Gt:  fold_to_int(inst.get(), lv >  rv ? 1 : 0); modified = true; continue;
                case Opcode::Ge:  fold_to_int(inst.get(), lv >= rv ? 1 : 0); modified = true; continue;
                default: break;
                }
            }

            // --- Float arithmetic ---
            if (lhs_float && rhs_float) {
                double lv = lhs_inst->imm_float;
                double rv = rhs_inst->imm_float;
                switch (op) {
                case Opcode::FAdd: fold_to_float(inst.get(), lv + rv); modified = true; continue;
                case Opcode::FSub: fold_to_float(inst.get(), lv - rv); modified = true; continue;
                case Opcode::FMul: fold_to_float(inst.get(), lv * rv); modified = true; continue;
                case Opcode::FDiv:
                    if (rv != 0.0) { fold_to_float(inst.get(), lv / rv); modified = true; continue; }
                    break;
                case Opcode::FEq:  fold_to_int(inst.get(), lv == rv ? 1 : 0); modified = true; continue;
                case Opcode::FNe:  fold_to_int(inst.get(), lv != rv ? 1 : 0); modified = true; continue;
                case Opcode::FLt:  fold_to_int(inst.get(), lv <  rv ? 1 : 0); modified = true; continue;
                case Opcode::FLe:  fold_to_int(inst.get(), lv <= rv ? 1 : 0); modified = true; continue;
                case Opcode::FGt:  fold_to_int(inst.get(), lv >  rv ? 1 : 0); modified = true; continue;
                case Opcode::FGe:  fold_to_int(inst.get(), lv >= rv ? 1 : 0); modified = true; continue;
                default: break;
                }
            }

            // --- Identity / strength reduction (one operand is a known integer constant) ---
            auto try_identity = [&]() -> bool {
                if (!lhs_int && !rhs_int) return false;
                int64_t cv = rhs_int ? rhs_inst->imm_int : lhs_inst->imm_int;
                Value* other = rhs_int ? inst->operands[0] : inst->operands[1];
                switch (op) {
                case Opcode::Add:
                    if (cv == 0) { replace_value_uses(fn, inst.get(), other); return true; }
                    break;
                case Opcode::Sub:
                    if (rhs_int && cv == 0) { replace_value_uses(fn, inst.get(), other); return true; }
                    break;
                case Opcode::Mul:
                    if (cv == 1) { replace_value_uses(fn, inst.get(), other); return true; }
                    if (cv == 0) {
                        // replace uses with the constant-zero value
                        fold_to_int(inst.get(), 0);
                        return true;
                    }
                    break;
                case Opcode::And:
                    if (cv == 0) { fold_to_int(inst.get(), 0); return true; }
                    if (cv == -1) { replace_value_uses(fn, inst.get(), other); return true; }
                    break;
                case Opcode::Or:
                    if (cv == 0) { replace_value_uses(fn, inst.get(), other); return true; }
                    break;
                case Opcode::Xor:
                    if (cv == 0) { replace_value_uses(fn, inst.get(), other); return true; }
                    break;
                case Opcode::Shl:
                case Opcode::Shr:
                    if (rhs_int && cv == 0) { replace_value_uses(fn, inst.get(), other); return true; }
                    break;
                default: break;
                }
                return false;
            };

            if (try_identity()) { modified = true; }
        }
    }

    return modified;
}

bool ConstFoldPass::run(Module& module) {
    bool modified = false;
    for (auto& fn : module.functions) {
        if (!fn->blocks.empty())
            if (run_function(*fn))
                modified = true;
    }
    return modified;
}

// ============================================================================
// Pass 3: Constant Propagation
// ============================================================================

bool ConstPropPass::run_function(Function& fn) {
    bool modified = false;

    // Build a map: value_id → Instruction* for constants
    std::unordered_map<uint32_t, Instruction*> const_map;
    for (auto& block : fn.blocks) {
        for (auto& inst : block->instructions) {
            if (is_const_inst(inst.get())) {
                const_map[inst->id] = inst.get();
            }
        }
    }

    if (const_map.empty())
        return false;

    for (auto& block : fn.blocks) {
        for (auto& inst : block->instructions) {
            // Don't propagate into stores' destination slot (it's a pointer)
            // Don't propagate into call operands (conservative)
            if (inst->opcode == Opcode::Call)
                continue;

            for (size_t i = 0; i < inst->operands.size(); ++i) {
                // For Store: skip the destination pointer (operands[1])
                if (inst->opcode == Opcode::Store && i == 1)
                    continue;

                Value* op = inst->operands[i];
                if (!op) continue;

                // Already a const inst — nothing to do
                auto* op_inst = dynamic_cast<Instruction*>(op);
                if (op_inst && is_const_inst(op_inst))
                    continue;

                // Look up by id
                auto it = const_map.find(op->id);
                if (it != const_map.end() && it->second != op_inst) {
                    inst->operands[i] = it->second;
                    modified = true;
                }
            }
        }
    }

    return modified;
}

bool ConstPropPass::run(Module& module) {
    bool modified = false;
    for (auto& fn : module.functions) {
        if (!fn->blocks.empty())
            if (run_function(*fn))
                modified = true;
    }
    return modified;
}

// ============================================================================
// Pass 4: Dead Code Elimination
// ============================================================================

bool DCEPass::remove_unreachable_blocks(Function& fn) {
    if (fn.blocks.empty())
        return false;

    bool modified = false;
    bool changed = true;
    while (changed) {
        changed = false;
        // Entry block (index 0) is always reachable
        for (size_t i = 1; i < fn.blocks.size(); ) {
            BasicBlock* bb = fn.blocks[i].get();
            if (bb->predecessors.empty()) {
                // Remove this block from its successors' predecessor lists
                for (BasicBlock* succ : bb->successors) {
                    succ->predecessors.erase(
                        std::remove(succ->predecessors.begin(),
                                    succ->predecessors.end(), bb),
                        succ->predecessors.end());
                }
                fn.blocks.erase(fn.blocks.begin() + static_cast<ptrdiff_t>(i));
                modified = true;
                changed = true;
                // don't increment i — re-check same index
            } else {
                ++i;
            }
        }
    }
    return modified;
}

bool DCEPass::run_function(Function& fn) {
    bool modified = false;

    // First: remove unreachable blocks
    if (remove_unreachable_blocks(fn))
        modified = true;

    // Then: iteratively remove dead pure instructions
    bool changed = true;
    while (changed) {
        changed = false;

        // Build the set of value IDs that are used somewhere
        std::unordered_set<uint32_t> used_ids;

        // Function parameters are always "used"
        for (auto& param : fn.params) {
            used_ids.insert(param->id);
        }

        for (auto& block : fn.blocks) {
            for (auto& inst : block->instructions) {
                for (auto* op : inst->operands) {
                    if (op) used_ids.insert(op->id);
                }
                // Block targets are not values, but keep all terminators alive
            }
        }

        // Remove pure instructions whose result is not used
        for (auto& block : fn.blocks) {
            size_t before = block->instructions.size();
            block->instructions.erase(
                std::remove_if(block->instructions.begin(), block->instructions.end(),
                    [&](const std::unique_ptr<Instruction>& inst) {
                        if (!is_pure(inst->opcode)) return false;
                        return used_ids.find(inst->id) == used_ids.end();
                    }),
                block->instructions.end());
            if (block->instructions.size() != before) {
                changed = true;
                modified = true;
            }
        }
    }

    return modified;
}

bool DCEPass::run(Module& module) {
    bool modified = false;
    for (auto& fn : module.functions) {
        if (!fn->blocks.empty())
            if (run_function(*fn))
                modified = true;
    }
    return modified;
}

} // namespace ir
} // namespace golangc
