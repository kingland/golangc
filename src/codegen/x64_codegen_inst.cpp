#include "codegen/x64_codegen.hpp"

#include <fmt/format.h>

#include <cassert>

namespace golangc {
namespace codegen {

// ============================================================================
// Main instruction dispatch
// ============================================================================

void X64CodeGenerator::emit_instruction(const ir::Instruction& inst,
                                         const ir::Function& func) {
    switch (inst.opcode) {
        case ir::Opcode::ConstInt:    emit_const_int(inst); break;
        case ir::Opcode::ConstBool:   emit_const_bool(inst); break;
        case ir::Opcode::ConstString: emit_const_string(inst); break;
        case ir::Opcode::ConstNil:
            // Store 0 (null) to temp slot
            (void)get_temp_slot(inst.id);
            emit(fmt::format("mov QWORD PTR [rbp{}], 0", get_temp_slot(inst.id)));
            break;

        case ir::Opcode::Alloca: emit_alloca(inst); break;
        case ir::Opcode::Load:   emit_load(inst); break;
        case ir::Opcode::Store:  emit_store(inst); break;
        case ir::Opcode::GetPtr: emit_getptr(inst); break;

        case ir::Opcode::Add:
        case ir::Opcode::Sub:
        case ir::Opcode::Mul:
            emit_arith(inst);
            break;
        case ir::Opcode::Div:
        case ir::Opcode::Rem:
            emit_div_rem(inst);
            break;
        case ir::Opcode::Neg:
            emit_neg(inst);
            break;

        case ir::Opcode::And:
        case ir::Opcode::Or:
        case ir::Opcode::Xor:
        case ir::Opcode::Shl:
        case ir::Opcode::Shr:
        case ir::Opcode::AndNot:
            emit_bitwise(inst);
            break;
        case ir::Opcode::BitNot:
            emit_bitnot(inst);
            break;

        case ir::Opcode::Eq: case ir::Opcode::Ne:
        case ir::Opcode::Lt: case ir::Opcode::Le:
        case ir::Opcode::Gt: case ir::Opcode::Ge:
            emit_compare(inst);
            break;

        case ir::Opcode::LogNot:
            emit_lognot(inst);
            break;

        case ir::Opcode::Br:     emit_br(inst); break;
        case ir::Opcode::CondBr: emit_condbr(inst); break;
        case ir::Opcode::Ret:    emit_ret(inst, func); break;

        case ir::Opcode::Call:    emit_call(inst); break;
        case ir::Opcode::Println: emit_println(inst); break;

        case ir::Opcode::SExt:  emit_sext(inst); break;
        case ir::Opcode::Trunc: emit_trunc(inst); break;
        case ir::Opcode::ZExt:  emit_sext(inst); break; // Same pattern for now

        // Not yet implemented — emit comment
        default:
            emit_comment(fmt::format("TODO: {}", ir::opcode_name(inst.opcode)));
            break;
    }
}

// ============================================================================
// Constants
// ============================================================================

void X64CodeGenerator::emit_const_int(const ir::Instruction& inst) {
    auto slot = get_temp_slot(inst.id);
    emit(fmt::format("mov rax, {}", inst.imm_int));
    emit(fmt::format("mov QWORD PTR [rbp{}], rax", slot));
}

void X64CodeGenerator::emit_const_bool(const ir::Instruction& inst) {
    auto slot = get_temp_slot(inst.id);
    emit(fmt::format("mov QWORD PTR [rbp{}], {}", slot, inst.imm_int ? 1 : 0));
}

void X64CodeGenerator::emit_const_string(const ir::Instruction& inst) {
    // Add string to the data section pool
    std::string label = fmt::format("__str{}", string_counter_++);
    string_pool_.push_back({label, inst.imm_string, static_cast<int64_t>(inst.imm_string.size())});

    // A string in Go is {ptr, len} — store both parts
    // The alloca for this string will be a 16-byte struct {ptr, i64}
    // For now, store ptr in one temp slot and len in another,
    // but since the IR treats strings as struct {ptr, i64}, we need to store both fields.
    auto slot = get_temp_slot(inst.id);
    // Store pointer to string data
    emit(fmt::format("lea rax, [{}]", label));
    emit(fmt::format("mov QWORD PTR [rbp{}], rax", slot));
    // Store length in the derived slot (pre-allocated by prescan_temps)
    auto len_slot = get_temp_slot(inst.id + 100000);
    emit(fmt::format("mov QWORD PTR [rbp{}], {}", len_slot,
                     static_cast<int64_t>(inst.imm_string.size())));
}

// ============================================================================
// Memory operations
// ============================================================================

void X64CodeGenerator::emit_alloca(const ir::Instruction& inst) {
    // Allocas are handled in scan_allocas; no code emitted.
    // The frame slot is already assigned.
    (void)inst;
}

void X64CodeGenerator::emit_load(const ir::Instruction& inst) {
    auto* ptr = inst.operands[0];
    auto slot = get_temp_slot(inst.id);

    // If loading a string (struct {ptr, i64}), load both fields
    if (is_string_type(inst.type)) {
        // ptr is an alloca for a string — load ptr field
        if (frame_.has_slot(ptr->id)) {
            auto ptr_off = frame_.offset_of(ptr->id);
            emit(fmt::format("mov rax, QWORD PTR [rbp{}]", ptr_off));
            emit(fmt::format("mov QWORD PTR [rbp{}], rax", slot));
            // Load length field (stored at ptr_off - 8)
            auto len_slot = get_temp_slot(inst.id + 100000);
            emit(fmt::format("mov rax, QWORD PTR [rbp{}]", ptr_off - 8));
            emit(fmt::format("mov QWORD PTR [rbp{}], rax", len_slot));
        }
        return;
    }

    // Normal scalar load: load from the address stored in the alloca slot
    if (frame_.has_slot(ptr->id)) {
        emit(fmt::format("mov rax, QWORD PTR [rbp{}]", frame_.offset_of(ptr->id)));
        emit(fmt::format("mov QWORD PTR [rbp{}], rax", slot));
    } else if (temp_slots_.count(ptr->id)) {
        emit(fmt::format("mov rax, QWORD PTR [rbp{}]", temp_slots_[ptr->id]));
        emit(fmt::format("mov QWORD PTR [rbp{}], rax", slot));
    }
}

void X64CodeGenerator::emit_store(const ir::Instruction& inst) {
    auto* val = inst.operands[0];
    auto* ptr = inst.operands[1];

    // Check if storing a string value (struct {ptr, i64})
    if (val->type && is_string_type(val->type)) {
        // val is a ConstString or Load of a string — copy both fields
        int32_t val_slot = 0;
        int32_t val_len_slot = 0;
        bool have_val = false;

        if (temp_slots_.count(val->id)) {
            val_slot = temp_slots_[val->id];
            // Length stored at val->id + 100000
            auto len_it = temp_slots_.find(val->id + 100000);
            if (len_it != temp_slots_.end()) {
                val_len_slot = len_it->second;
            } else if (frame_.has_slot(val->id + 100000)) {
                val_len_slot = frame_.offset_of(val->id + 100000);
            }
            have_val = true;
        }

        if (have_val && frame_.has_slot(ptr->id)) {
            auto ptr_off = frame_.offset_of(ptr->id);
            // Copy ptr field
            emit(fmt::format("mov rax, QWORD PTR [rbp{}]", val_slot));
            emit(fmt::format("mov QWORD PTR [rbp{}], rax", ptr_off));
            // Copy length field
            emit(fmt::format("mov rax, QWORD PTR [rbp{}]", val_len_slot));
            emit(fmt::format("mov QWORD PTR [rbp{}], rax", ptr_off - 8));
        }
        return;
    }

    // Normal scalar store
    // Load value into RAX
    load_value_to_rax(val);

    // Store to the destination alloca
    if (frame_.has_slot(ptr->id)) {
        emit(fmt::format("mov QWORD PTR [rbp{}], rax", frame_.offset_of(ptr->id)));
    } else if (temp_slots_.count(ptr->id)) {
        emit(fmt::format("mov QWORD PTR [rbp{}], rax", temp_slots_[ptr->id]));
    }
}

void X64CodeGenerator::emit_getptr(const ir::Instruction& inst) {
    auto slot = get_temp_slot(inst.id);

    if (inst.operands.size() == 1) {
        // Field access: getptr base, #field_index
        auto* base = inst.operands[0];
        if (frame_.has_slot(base->id)) {
            auto base_off = frame_.offset_of(base->id);
            int32_t field_off = static_cast<int32_t>(inst.field_index) * 8;
            emit(fmt::format("lea rax, [rbp{}]", base_off - field_off));
            emit(fmt::format("mov QWORD PTR [rbp{}], rax", slot));
        }
    } else if (inst.operands.size() >= 2) {
        // Indexed access: getptr base, index
        auto* base = inst.operands[0];
        auto* index = inst.operands[1];
        load_value_to_rax(index);
        emit("imul rax, 8");
        if (frame_.has_slot(base->id)) {
            auto base_off = frame_.offset_of(base->id);
            emit(fmt::format("lea rcx, [rbp{}]", base_off));
            emit("sub rcx, rax");
            emit(fmt::format("mov QWORD PTR [rbp{}], rcx", slot));
        }
    }
}

// ============================================================================
// Arithmetic
// ============================================================================

void X64CodeGenerator::emit_arith(const ir::Instruction& inst) {
    auto slot = get_temp_slot(inst.id);

    load_value_to_rax(inst.operands[0]);
    emit("mov r10, rax");
    load_value_to_rax(inst.operands[1]);
    emit("mov rcx, rax");
    emit("mov rax, r10");

    switch (inst.opcode) {
        case ir::Opcode::Add: emit("add rax, rcx"); break;
        case ir::Opcode::Sub: emit("sub rax, rcx"); break;
        case ir::Opcode::Mul: emit("imul rax, rcx"); break;
        default: break;
    }

    emit(fmt::format("mov QWORD PTR [rbp{}], rax", slot));
}

void X64CodeGenerator::emit_div_rem(const ir::Instruction& inst) {
    auto slot = get_temp_slot(inst.id);

    load_value_to_rax(inst.operands[0]);
    emit("mov r10, rax");
    load_value_to_rax(inst.operands[1]);
    emit("mov rcx, rax");
    emit("mov rax, r10");
    emit("cqo");       // Sign-extend RAX into RDX:RAX
    emit("idiv rcx");  // RAX = quotient, RDX = remainder

    if (inst.opcode == ir::Opcode::Div) {
        emit(fmt::format("mov QWORD PTR [rbp{}], rax", slot));
    } else {
        emit(fmt::format("mov QWORD PTR [rbp{}], rdx", slot));
    }
}

void X64CodeGenerator::emit_neg(const ir::Instruction& inst) {
    auto slot = get_temp_slot(inst.id);
    load_value_to_rax(inst.operands[0]);
    emit("neg rax");
    emit(fmt::format("mov QWORD PTR [rbp{}], rax", slot));
}

// ============================================================================
// Bitwise operations
// ============================================================================

void X64CodeGenerator::emit_bitwise(const ir::Instruction& inst) {
    auto slot = get_temp_slot(inst.id);

    load_value_to_rax(inst.operands[0]);
    emit("mov r10, rax");
    load_value_to_rax(inst.operands[1]);
    emit("mov rcx, rax");
    emit("mov rax, r10");

    switch (inst.opcode) {
        case ir::Opcode::And:    emit("and rax, rcx"); break;
        case ir::Opcode::Or:     emit("or rax, rcx"); break;
        case ir::Opcode::Xor:    emit("xor rax, rcx"); break;
        case ir::Opcode::Shl:    emit("shl rax, cl"); break;
        case ir::Opcode::Shr:    emit("sar rax, cl"); break; // Arithmetic shift right
        case ir::Opcode::AndNot: emit("not rcx"); emit("and rax, rcx"); break;
        default: break;
    }

    emit(fmt::format("mov QWORD PTR [rbp{}], rax", slot));
}

void X64CodeGenerator::emit_bitnot(const ir::Instruction& inst) {
    auto slot = get_temp_slot(inst.id);
    load_value_to_rax(inst.operands[0]);
    emit("not rax");
    emit(fmt::format("mov QWORD PTR [rbp{}], rax", slot));
}

// ============================================================================
// Comparisons
// ============================================================================

void X64CodeGenerator::emit_compare(const ir::Instruction& inst) {
    auto slot = get_temp_slot(inst.id);

    load_value_to_rax(inst.operands[0]);
    emit("mov r10, rax");
    load_value_to_rax(inst.operands[1]);
    emit("mov rcx, rax");
    emit("mov rax, r10");
    emit("cmp rax, rcx");

    // Set result byte based on condition
    const char* setcc = "sete";
    switch (inst.opcode) {
        case ir::Opcode::Eq: setcc = "sete"; break;
        case ir::Opcode::Ne: setcc = "setne"; break;
        case ir::Opcode::Lt: setcc = "setl"; break;
        case ir::Opcode::Le: setcc = "setle"; break;
        case ir::Opcode::Gt: setcc = "setg"; break;
        case ir::Opcode::Ge: setcc = "setge"; break;
        default: break;
    }

    emit(fmt::format("{} al", setcc));
    emit("movzx rax, al");
    emit(fmt::format("mov QWORD PTR [rbp{}], rax", slot));
}

void X64CodeGenerator::emit_lognot(const ir::Instruction& inst) {
    auto slot = get_temp_slot(inst.id);
    load_value_to_rax(inst.operands[0]);
    emit("test rax, rax");
    emit("sete al");
    emit("movzx rax, al");
    emit(fmt::format("mov QWORD PTR [rbp{}], rax", slot));
}

// ============================================================================
// Control flow
// ============================================================================

void X64CodeGenerator::emit_br(const ir::Instruction& inst) {
    auto* target = inst.targets[0];
    auto* parent = inst.parent;
    emit(fmt::format("jmp {}${}", masm_name(parent->parent->name), masm_name(target->label)));
}

void X64CodeGenerator::emit_condbr(const ir::Instruction& inst) {
    auto* cond = inst.operands[0];
    auto* true_bb = inst.targets[0];
    auto* false_bb = inst.targets[1];
    auto func_name = masm_name(inst.parent->parent->name);

    load_value_to_rax(cond);
    emit("test rax, rax");
    emit(fmt::format("jne {}${}", func_name, masm_name(true_bb->label)));
    emit(fmt::format("jmp {}${}", func_name, masm_name(false_bb->label)));
}

void X64CodeGenerator::emit_ret(const ir::Instruction& inst, const ir::Function& func) {
    if (!inst.operands.empty()) {
        load_value_to_rax(inst.operands[0]);
    } else {
        // Void return — zero RAX so main() returns 0 as process exit code
        emit("xor eax, eax");
    }
    emit_epilogue();
    (void)func;
}

// ============================================================================
// Function calls
// ============================================================================

void X64CodeGenerator::emit_call(const ir::Instruction& inst) {
    auto* callee = inst.operands[0];

    // Marshal arguments into registers (Windows x64 ABI)
    size_t num_args = inst.operands.size() - 1;
    for (size_t i = 0; i < num_args && i < kMaxRegArgs; ++i) {
        load_value_to_reg(inst.operands[i + 1], kArgRegs[i]);
    }

    // Stack arguments for > 4 args
    if (num_args > kMaxRegArgs) {
        for (size_t i = num_args; i > kMaxRegArgs; --i) {
            load_value_to_rax(inst.operands[i]);
            emit("push rax");
        }
    }

    // Allocate shadow space (already part of frame, but ensure alignment)
    emit(fmt::format("sub rsp, {}", kShadowSpace));

    // Call the function
    auto* func = dynamic_cast<const ir::Function*>(callee);
    if (func) {
        emit(fmt::format("call {}", masm_name(func->name)));
    } else {
        load_value_to_reg(callee, X64Reg::R10);
        emit("call r10");
    }

    emit(fmt::format("add rsp, {}", kShadowSpace));

    // Clean up stack arguments
    if (num_args > kMaxRegArgs) {
        int32_t stack_args_size = static_cast<int32_t>(num_args - kMaxRegArgs) * 8;
        emit(fmt::format("add rsp, {}", stack_args_size));
    }

    // Store result (in RAX) if the call produces a value
    if (inst.type && !inst.type->is_void()) {
        auto slot = get_temp_slot(inst.id);
        emit(fmt::format("mov QWORD PTR [rbp{}], rax", slot));
    }
}

// ============================================================================
// Println — dispatch to runtime based on argument type
// ============================================================================

void X64CodeGenerator::emit_println(const ir::Instruction& inst) {
    if (inst.operands.empty()) {
        // println() with no args — just print a newline
        // Not implementing bare newline for now
        return;
    }

    for (size_t i = 0; i < inst.operands.size(); ++i) {
        auto* arg = inst.operands[i];

        if (i > 0) {
            // Print space between arguments
            // For simplicity, we'll let runtime handle formatting
        }

        // Determine type and dispatch to appropriate runtime function
        bool handled = false;

        // Check if the argument is a ConstString
        auto* arg_inst = dynamic_cast<const ir::Instruction*>(arg);
        if (arg_inst && arg_inst->opcode == ir::Opcode::ConstString) {
            // String argument: call golangc_println_string(ptr, len)
            if (temp_slots_.count(arg->id)) {
                auto ptr_slot = temp_slots_[arg->id];
                emit(fmt::format("mov rcx, QWORD PTR [rbp{}]", ptr_slot));
                // Length is stored at id + 100000
                int32_t len_slot = 0;
                auto len_it = temp_slots_.find(arg->id + 100000);
                if (len_it != temp_slots_.end()) {
                    len_slot = len_it->second;
                } else if (frame_.has_slot(arg->id + 100000)) {
                    len_slot = frame_.offset_of(arg->id + 100000);
                }
                emit(fmt::format("mov rdx, QWORD PTR [rbp{}]", len_slot));
            }
            emit(fmt::format("sub rsp, {}", kShadowSpace));
            emit("call golangc_println_string");
            emit(fmt::format("add rsp, {}", kShadowSpace));
            handled = true;
        }

        // Check if the argument is a Load of a string type
        if (!handled && arg_inst && arg_inst->opcode == ir::Opcode::Load &&
            is_string_type(arg_inst->type)) {
            if (temp_slots_.count(arg->id)) {
                auto ptr_slot = temp_slots_[arg->id];
                emit(fmt::format("mov rcx, QWORD PTR [rbp{}]", ptr_slot));
                int32_t len_slot = 0;
                auto len_it = temp_slots_.find(arg->id + 100000);
                if (len_it != temp_slots_.end()) {
                    len_slot = len_it->second;
                } else if (frame_.has_slot(arg->id + 100000)) {
                    len_slot = frame_.offset_of(arg->id + 100000);
                }
                emit(fmt::format("mov rdx, QWORD PTR [rbp{}]", len_slot));
            }
            emit(fmt::format("sub rsp, {}", kShadowSpace));
            emit("call golangc_println_string");
            emit(fmt::format("add rsp, {}", kShadowSpace));
            handled = true;
        }

        if (!handled) {
            // Check if it's a bool type
            bool is_bool = arg->type && arg->type->kind == ir::IRTypeKind::I1;

            if (is_bool) {
                load_value_to_reg(arg, X64Reg::RCX);
                emit(fmt::format("sub rsp, {}", kShadowSpace));
                emit("call golangc_println_bool");
                emit(fmt::format("add rsp, {}", kShadowSpace));
            } else {
                // Default: integer argument
                load_value_to_reg(arg, X64Reg::RCX);
                emit(fmt::format("sub rsp, {}", kShadowSpace));
                emit("call golangc_println_int");
                emit(fmt::format("add rsp, {}", kShadowSpace));
            }
        }
    }
}

// ============================================================================
// Conversions
// ============================================================================

void X64CodeGenerator::emit_sext(const ir::Instruction& inst) {
    auto slot = get_temp_slot(inst.id);
    load_value_to_rax(inst.operands[0]);
    // For sign-extension, movsx from smaller to larger
    // Since we store everything as 64-bit on the stack, this is mostly a no-op
    emit(fmt::format("mov QWORD PTR [rbp{}], rax", slot));
}

void X64CodeGenerator::emit_trunc(const ir::Instruction& inst) {
    auto slot = get_temp_slot(inst.id);
    load_value_to_rax(inst.operands[0]);
    // Truncation: mask to target size
    if (inst.type) {
        switch (inst.type->kind) {
            case ir::IRTypeKind::I1:
                emit("and rax, 1");
                break;
            case ir::IRTypeKind::I8:
                emit("and rax, 0xFF");
                break;
            case ir::IRTypeKind::I16:
                emit("and rax, 0xFFFF");
                break;
            case ir::IRTypeKind::I32:
                emit("mov eax, eax"); // Zero-extend 32-bit
                break;
            default:
                break;
        }
    }
    emit(fmt::format("mov QWORD PTR [rbp{}], rax", slot));
}

} // namespace codegen
} // namespace golangc
