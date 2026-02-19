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
        case ir::Opcode::FPExt:    emit_fpext(inst); break;
        case ir::Opcode::FPTrunc:  emit_fptrunc(inst); break;
        case ir::Opcode::Bitcast:  emit_bitcast(inst); break;

        case ir::Opcode::InterfaceMake: emit_interface_make(inst); break;
        case ir::Opcode::InterfaceData: emit_interface_data(inst); break;

        // Aggregate operations
        case ir::Opcode::ExtractValue: emit_extract_value(inst); break;
        case ir::Opcode::InsertValue:  emit_insert_value(inst); break;

        // Panic
        case ir::Opcode::Panic: emit_panic(inst); break;

        // Defer
        case ir::Opcode::DeferCall: emit_defer_call(inst); break;

        // Float operations
        case ir::Opcode::ConstFloat: emit_const_float(inst); break;
        case ir::Opcode::FAdd:
        case ir::Opcode::FSub:
        case ir::Opcode::FMul:
        case ir::Opcode::FDiv:
            emit_float_arith(inst); break;
        case ir::Opcode::FNeg: emit_float_neg(inst); break;
        case ir::Opcode::FEq: case ir::Opcode::FNe:
        case ir::Opcode::FLt: case ir::Opcode::FLe:
        case ir::Opcode::FGt: case ir::Opcode::FGe:
            emit_float_compare(inst); break;
        case ir::Opcode::SIToFP: emit_sitofp(inst); break;
        case ir::Opcode::FPToSI: emit_fptosi(inst); break;

        // String operations
        case ir::Opcode::StringLen:    emit_string_len(inst); break;
        case ir::Opcode::StringIndex:  emit_string_index(inst); break;
        case ir::Opcode::StringConcat: emit_string_concat(inst); break;

        // Slice operations
        case ir::Opcode::SliceLen:   emit_slice_len(inst); break;
        case ir::Opcode::SliceCap:   emit_slice_cap(inst); break;
        case ir::Opcode::SliceIndex: emit_slice_index(inst); break;

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

    // A string in Go is {ptr, len} — store both QWORDs
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
    (void)inst;
}

void X64CodeGenerator::emit_load(const ir::Instruction& inst) {
    auto* ptr = inst.operands[0];
    auto slot = get_temp_slot(inst.id);

    // Determine if the source is a GetPtr (computed address in a temp slot)
    bool src_is_getptr = is_getptr(ptr);

    // Struct types: copy N QWORDs
    if (inst.type && inst.type->is_struct()) {
        int32_t nq = type_qwords(inst.type);

        if (src_is_getptr) {
            // GetPtr result is an address in a temp slot — dereference through it
            int32_t addr_slot = 0;
            if (temp_slots_.count(ptr->id))
                addr_slot = temp_slots_[ptr->id];
            else if (frame_.has_slot(ptr->id))
                addr_slot = frame_.offset_of(ptr->id);

            emit(fmt::format("mov rcx, QWORD PTR [rbp{}]", addr_slot));
            // Load N QWORDs through the pointer
            for (int32_t q = 0; q < nq; ++q) {
                emit(fmt::format("mov rax, QWORD PTR [rcx+{}]", q * 8));
                if (q == 0) {
                    emit(fmt::format("mov QWORD PTR [rbp{}], rax", slot));
                } else {
                    auto extra_slot = get_temp_slot(inst.id + static_cast<uint32_t>(q) * 100000);
                    emit(fmt::format("mov QWORD PTR [rbp{}], rax", extra_slot));
                }
            }
        } else {
            // Direct alloca: copy from frame slot
            // Fields are at ascending offsets: base, base+8, base+16, ...
            int32_t src_off = 0;
            if (frame_.has_slot(ptr->id))
                src_off = frame_.offset_of(ptr->id);
            else if (temp_slots_.count(ptr->id))
                src_off = temp_slots_[ptr->id];

            for (int32_t q = 0; q < nq; ++q) {
                emit(fmt::format("mov rax, QWORD PTR [rbp{}]", src_off + q * 8));
                if (q == 0) {
                    emit(fmt::format("mov QWORD PTR [rbp{}], rax", slot));
                } else {
                    auto extra_slot = get_temp_slot(inst.id + static_cast<uint32_t>(q) * 100000);
                    emit(fmt::format("mov QWORD PTR [rbp{}], rax", extra_slot));
                }
            }
        }
        return;
    }

    // Scalar load
    if (src_is_getptr) {
        // GetPtr result is an address in a temp slot — dereference through it
        int32_t addr_slot = 0;
        if (temp_slots_.count(ptr->id))
            addr_slot = temp_slots_[ptr->id];
        else if (frame_.has_slot(ptr->id))
            addr_slot = frame_.offset_of(ptr->id);

        emit(fmt::format("mov rcx, QWORD PTR [rbp{}]", addr_slot));
        emit("mov rax, QWORD PTR [rcx]");
        emit(fmt::format("mov QWORD PTR [rbp{}], rax", slot));
    } else if (frame_.has_slot(ptr->id)) {
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

    // Determine if destination is a GetPtr (computed address in a temp slot)
    bool dst_is_getptr = is_getptr(ptr);

    // Struct types: copy N QWORDs
    if (val->type && val->type->is_struct()) {
        int32_t nq = type_qwords(val->type);

        // Find source base slot
        int32_t val_slot = 0;
        if (temp_slots_.count(val->id))
            val_slot = temp_slots_[val->id];
        else if (frame_.has_slot(val->id))
            val_slot = frame_.offset_of(val->id);

        // Helper: get the offset for QWORD q of the source value.
        // For temp slots, extra QWORDs may be aliased at id+q*100000.
        // For frame/param slots (contiguous alloca), use val_slot + q*8.
        auto src_qword_off = [&](int32_t q) -> int32_t {
            if (q == 0) return val_slot;
            uint32_t extra_id = val->id + static_cast<uint32_t>(q) * 100000;
            auto it = temp_slots_.find(extra_id);
            if (it != temp_slots_.end()) return it->second;
            // Contiguous layout: ascending offsets from base
            return val_slot + q * 8;
        };

        if (dst_is_getptr) {
            // Destination is a computed address — store through pointer
            int32_t addr_slot = 0;
            if (temp_slots_.count(ptr->id))
                addr_slot = temp_slots_[ptr->id];
            else if (frame_.has_slot(ptr->id))
                addr_slot = frame_.offset_of(ptr->id);

            emit(fmt::format("mov rcx, QWORD PTR [rbp{}]", addr_slot));
            for (int32_t q = 0; q < nq; ++q) {
                emit(fmt::format("mov rax, QWORD PTR [rbp{}]", src_qword_off(q)));
                emit(fmt::format("mov QWORD PTR [rcx+{}], rax", q * 8));
            }
        } else {
            // Direct alloca destination: copy QWORD by QWORD
            // Fields are at ascending offsets: base, base+8, base+16, ...
            int32_t dst_off = 0;
            if (frame_.has_slot(ptr->id))
                dst_off = frame_.offset_of(ptr->id);
            else if (temp_slots_.count(ptr->id))
                dst_off = temp_slots_[ptr->id];

            for (int32_t q = 0; q < nq; ++q) {
                emit(fmt::format("mov rax, QWORD PTR [rbp{}]", src_qword_off(q)));
                emit(fmt::format("mov QWORD PTR [rbp{}], rax", dst_off + q * 8));
            }
        }
        return;
    }

    // Scalar store
    load_value_to_rax(val);

    if (dst_is_getptr) {
        // Store through computed address
        int32_t addr_slot = 0;
        if (temp_slots_.count(ptr->id))
            addr_slot = temp_slots_[ptr->id];
        else if (frame_.has_slot(ptr->id))
            addr_slot = frame_.offset_of(ptr->id);

        emit(fmt::format("mov rcx, QWORD PTR [rbp{}]", addr_slot));
        emit("mov QWORD PTR [rcx], rax");
    } else if (frame_.has_slot(ptr->id)) {
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
        int32_t base_off = 0;
        bool have_base = false;

        if (frame_.has_slot(base->id)) {
            base_off = frame_.offset_of(base->id);
            have_base = true;
        } else if (temp_slots_.count(base->id)) {
            base_off = temp_slots_[base->id];
            have_base = true;
        }

        if (have_base) {
            // If base is itself a GetPtr result (pointer), we need to
            // dereference through the pointer first, then add the field offset
            if (is_getptr(base)) {
                emit(fmt::format("mov rax, QWORD PTR [rbp{}]", base_off));
                int32_t field_off = static_cast<int32_t>(inst.field_index) * 8;
                if (field_off != 0) {
                    emit(fmt::format("add rax, {}", field_off));
                }
                emit(fmt::format("mov QWORD PTR [rbp{}], rax", slot));
            } else {
                // Struct fields are at ascending offsets from the base address:
                // field 0 at base_off, field 1 at base_off + 8, etc.
                int32_t field_off = static_cast<int32_t>(inst.field_index) * 8;
                emit(fmt::format("lea rax, [rbp{}]", base_off + field_off));
                emit(fmt::format("mov QWORD PTR [rbp{}], rax", slot));
            }
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
        auto* ret_val = inst.operands[0];

        // Check if returning a large struct via sret pointer
        if (has_sret_ && ret_val->type && is_large_struct(ret_val->type)) {
            int32_t nq = type_qwords(ret_val->type);
            // Load the sret pointer
            emit(fmt::format("mov rcx, QWORD PTR [rbp{}]", sret_slot_));

            // Copy struct value to sret pointer location
            int32_t val_slot = 0;
            if (temp_slots_.count(ret_val->id))
                val_slot = temp_slots_[ret_val->id];
            else if (frame_.has_slot(ret_val->id))
                val_slot = frame_.offset_of(ret_val->id);

            for (int32_t q = 0; q < nq; ++q) {
                int32_t src_off = val_slot;
                if (q > 0) {
                    uint32_t extra_id = ret_val->id + static_cast<uint32_t>(q) * 100000;
                    auto it = temp_slots_.find(extra_id);
                    if (it != temp_slots_.end()) src_off = it->second;
                    else src_off = val_slot + q * 8; // Contiguous layout
                }
                emit(fmt::format("mov rax, QWORD PTR [rbp{}]", src_off));
                emit(fmt::format("mov QWORD PTR [rcx+{}], rax", q * 8));
            }
            // Return the sret pointer in RAX (Windows x64 convention)
            emit(fmt::format("mov rax, QWORD PTR [rbp{}]", sret_slot_));
        } else if (ret_val->type && is_float_type(ret_val->type)) {
            // Float return in XMM0
            load_value_to_xmm(ret_val, X64Reg::XMM0);
        } else {
            load_value_to_rax(ret_val);
        }
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

    // Check if callee returns a large struct (need sret)
    bool callee_sret = false;
    int32_t sret_temp_slot = 0;
    auto* callee_func = dynamic_cast<const ir::Function*>(callee);
    if (callee_func && callee_func->return_type && is_large_struct(callee_func->return_type)) {
        callee_sret = true;
    } else if (inst.type && is_large_struct(inst.type)) {
        callee_sret = true;
    }

    // If sret, we need to allocate space and pass pointer as first arg (RCX)
    // The result temp slot already has enough space from prescan_temps
    if (callee_sret) {
        sret_temp_slot = get_temp_slot(inst.id);
        emit(fmt::format("lea rcx, [rbp{}]", sret_temp_slot));
    }

    // Marshal arguments into registers (Windows x64 ABI)
    // Float args use XMM registers at positional slots
    static constexpr X64Reg kXmmArgs[] = {X64Reg::XMM0, X64Reg::XMM1, X64Reg::XMM2, X64Reg::XMM3};
    size_t num_args = inst.operands.size() - 1;
    size_t reg_offset = callee_sret ? 1 : 0; // sret takes RCX

    for (size_t i = 0; i < num_args && (i + reg_offset) < kMaxRegArgs; ++i) {
        auto* arg = inst.operands[i + 1];

        // Large struct args: pass pointer to the value's stack slot
        if (arg->type && is_large_struct(arg->type)) {
            int32_t arg_slot = 0;
            if (temp_slots_.count(arg->id))
                arg_slot = temp_slots_[arg->id];
            else if (frame_.has_slot(arg->id))
                arg_slot = frame_.offset_of(arg->id);
            emit(fmt::format("lea {}, [rbp{}]", reg_name(kArgRegs[i + reg_offset]), arg_slot));
        } else if (arg->type && is_float_type(arg->type)) {
            // Float args go in XMM registers at positional slot
            load_value_to_xmm(arg, kXmmArgs[i + reg_offset]);
        } else {
            load_value_to_reg(arg, kArgRegs[i + reg_offset]);
        }
    }

    // Stack arguments for > 4 args (accounting for sret offset)
    if ((num_args + reg_offset) > kMaxRegArgs) {
        for (size_t i = num_args; (i + reg_offset) > kMaxRegArgs; --i) {
            load_value_to_rax(inst.operands[i]);
            emit("push rax");
        }
    }

    // Allocate shadow space
    emit(fmt::format("sub rsp, {}", kShadowSpace));

    // Call the function
    if (callee_func) {
        emit(fmt::format("call {}", masm_name(callee_func->name)));
    } else {
        load_value_to_reg(callee, X64Reg::R10);
        emit("call r10");
    }

    emit(fmt::format("add rsp, {}", kShadowSpace));

    // Clean up stack arguments
    if ((num_args + reg_offset) > kMaxRegArgs) {
        int32_t stack_args_size = static_cast<int32_t>(num_args + reg_offset - kMaxRegArgs) * 8;
        emit(fmt::format("add rsp, {}", stack_args_size));
    }

    // Store result
    if (callee_sret) {
        // Result already in the sret temp area; RAX points to it.
        // For the caller, the struct value lives at sret_temp_slot.
        // Nothing extra to do — subsequent loads will read from there.
    } else if (inst.type && is_float_type(inst.type)) {
        // Float result in XMM0
        auto slot = get_temp_slot(inst.id);
        emit(fmt::format("movsd QWORD PTR [rbp{}], xmm0", slot));
    } else if (inst.type && !inst.type->is_void()) {
        // Scalar result in RAX
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
        emit(fmt::format("sub rsp, {}", kShadowSpace));
        emit("call golangc_print_newline");
        emit(fmt::format("add rsp, {}", kShadowSpace));
        return;
    }

    bool multi_arg = inst.operands.size() > 1;

    for (size_t i = 0; i < inst.operands.size(); ++i) {
        auto* arg = inst.operands[i];

        // Print space between arguments
        if (i > 0) {
            emit(fmt::format("sub rsp, {}", kShadowSpace));
            emit("call golangc_print_space");
            emit(fmt::format("add rsp, {}", kShadowSpace));
        }

        // Determine type and dispatch to appropriate runtime function
        bool handled = false;
        auto* arg_inst = dynamic_cast<const ir::Instruction*>(arg);

        // Check if string type (ConstString or Load of string)
        bool is_string_arg = false;
        if (arg_inst && arg_inst->opcode == ir::Opcode::ConstString) {
            is_string_arg = true;
        } else if (arg_inst && arg_inst->opcode == ir::Opcode::Load &&
                   is_string_type(arg_inst->type)) {
            is_string_arg = true;
        } else if (arg->type && is_string_type(arg->type)) {
            is_string_arg = true;
        }

        if (is_string_arg) {
            // Load string ptr + len into RCX/RDX
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
            if (multi_arg) {
                emit("call golangc_print_string");
            } else {
                emit("call golangc_println_string");
            }
            emit(fmt::format("add rsp, {}", kShadowSpace));
            handled = true;
        }

        if (!handled) {
            // Check if it's a float type
            bool is_float_arg = arg->type && is_float_type(arg->type);

            if (is_float_arg) {
                // Float arg: pass in XMM0
                load_value_to_xmm(arg, X64Reg::XMM0);
                emit(fmt::format("sub rsp, {}", kShadowSpace));
                if (multi_arg)
                    emit("call golangc_print_float");
                else
                    emit("call golangc_println_float");
                emit(fmt::format("add rsp, {}", kShadowSpace));
            } else {
                // Check if it's a bool type
                bool is_bool = arg->type && arg->type->kind == ir::IRTypeKind::I1;

                if (is_bool) {
                    load_value_to_reg(arg, X64Reg::RCX);
                    emit(fmt::format("sub rsp, {}", kShadowSpace));
                    if (multi_arg)
                        emit("call golangc_print_bool");
                    else
                        emit("call golangc_println_bool");
                    emit(fmt::format("add rsp, {}", kShadowSpace));
                } else {
                    // Default: integer argument
                    load_value_to_reg(arg, X64Reg::RCX);
                    emit(fmt::format("sub rsp, {}", kShadowSpace));
                    if (multi_arg)
                        emit("call golangc_print_int");
                    else
                        emit("call golangc_println_int");
                    emit(fmt::format("add rsp, {}", kShadowSpace));
                }
            }
        }
    }

    // For multi-arg, emit final newline
    if (multi_arg) {
        emit(fmt::format("sub rsp, {}", kShadowSpace));
        emit("call golangc_print_newline");
        emit(fmt::format("add rsp, {}", kShadowSpace));
    }
}

// ============================================================================
// Conversions
// ============================================================================

void X64CodeGenerator::emit_sext(const ir::Instruction& inst) {
    auto slot = get_temp_slot(inst.id);
    load_value_to_rax(inst.operands[0]);
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

// ============================================================================
// Interface operations
// ============================================================================

void X64CodeGenerator::emit_interface_make(const ir::Instruction& inst) {
    // InterfaceMake(type_desc, data) -> {ptr, ptr}
    // type_desc = first operand (type tag/descriptor)
    // data = second operand (concrete value to box)
    auto slot = get_temp_slot(inst.id);
    auto data_slot = get_temp_slot(inst.id + 100000);

    // Store type descriptor (or tag) in first QWORD
    load_value_to_rax(inst.operands[0]);
    emit(fmt::format("mov QWORD PTR [rbp{}], rax", slot));

    // Store data pointer in second QWORD
    load_value_to_rax(inst.operands[1]);
    emit(fmt::format("mov QWORD PTR [rbp{}], rax", data_slot));
}

void X64CodeGenerator::emit_interface_data(const ir::Instruction& inst) {
    // InterfaceData(iface) -> data pointer (second QWORD of interface)
    auto* iface = inst.operands[0];
    auto slot = get_temp_slot(inst.id);

    // The interface is a 2-QWORD struct; data is the second QWORD
    auto data_it = temp_slots_.find(iface->id + 100000);
    if (data_it != temp_slots_.end()) {
        emit(fmt::format("mov rax, QWORD PTR [rbp{}]", data_it->second));
        emit(fmt::format("mov QWORD PTR [rbp{}], rax", slot));
    } else if (frame_.has_slot(iface->id + 100000)) {
        emit(fmt::format("mov rax, QWORD PTR [rbp{}]", frame_.offset_of(iface->id + 100000)));
        emit(fmt::format("mov QWORD PTR [rbp{}], rax", slot));
    }
}

// ============================================================================
// Float operations
// ============================================================================

void X64CodeGenerator::emit_const_float(const ir::Instruction& inst) {
    auto slot = get_temp_slot(inst.id);

    // Register float in pool; emit movsd from data label
    std::string label = fmt::format("__flt{}", float_counter_++);
    float_pool_.push_back({label, inst.imm_float});

    emit(fmt::format("movsd xmm0, QWORD PTR [{}]", label));
    emit(fmt::format("movsd QWORD PTR [rbp{}], xmm0", slot));
}

void X64CodeGenerator::emit_float_arith(const ir::Instruction& inst) {
    auto slot = get_temp_slot(inst.id);

    load_value_to_xmm(inst.operands[0], X64Reg::XMM0);
    load_value_to_xmm(inst.operands[1], X64Reg::XMM1);

    switch (inst.opcode) {
        case ir::Opcode::FAdd: emit("addsd xmm0, xmm1"); break;
        case ir::Opcode::FSub: emit("subsd xmm0, xmm1"); break;
        case ir::Opcode::FMul: emit("mulsd xmm0, xmm1"); break;
        case ir::Opcode::FDiv: emit("divsd xmm0, xmm1"); break;
        default: break;
    }

    emit(fmt::format("movsd QWORD PTR [rbp{}], xmm0", slot));
}

void X64CodeGenerator::emit_float_neg(const ir::Instruction& inst) {
    auto slot = get_temp_slot(inst.id);
    needs_sign_mask_ = true;

    load_value_to_xmm(inst.operands[0], X64Reg::XMM0);
    emit("xorpd xmm0, XMMWORD PTR [__f64_sign_mask]");
    emit(fmt::format("movsd QWORD PTR [rbp{}], xmm0", slot));
}

void X64CodeGenerator::emit_float_compare(const ir::Instruction& inst) {
    auto slot = get_temp_slot(inst.id);

    load_value_to_xmm(inst.operands[0], X64Reg::XMM0);
    load_value_to_xmm(inst.operands[1], X64Reg::XMM1);
    emit("ucomisd xmm0, xmm1");

    // NaN-safe comparisons
    switch (inst.opcode) {
        case ir::Opcode::FEq:
            // Equal and not unordered (NaN-safe)
            emit("sete al");
            emit("setnp cl");
            emit("and al, cl");
            break;
        case ir::Opcode::FNe:
            // Not equal or unordered (NaN-safe)
            emit("setne al");
            emit("setp cl");
            emit("or al, cl");
            break;
        case ir::Opcode::FLt:
            // Below (CF=1 for ucomisd when a < b)
            emit("setb al");
            break;
        case ir::Opcode::FLe:
            // Below or equal
            emit("setbe al");
            break;
        case ir::Opcode::FGt:
            // Above
            emit("seta al");
            break;
        case ir::Opcode::FGe:
            // Above or equal
            emit("setae al");
            break;
        default:
            break;
    }

    emit("movzx rax, al");
    emit(fmt::format("mov QWORD PTR [rbp{}], rax", slot));
}

void X64CodeGenerator::emit_sitofp(const ir::Instruction& inst) {
    auto slot = get_temp_slot(inst.id);
    load_value_to_rax(inst.operands[0]);
    emit("cvtsi2sd xmm0, rax");
    emit(fmt::format("movsd QWORD PTR [rbp{}], xmm0", slot));
}

void X64CodeGenerator::emit_fptosi(const ir::Instruction& inst) {
    auto slot = get_temp_slot(inst.id);
    load_value_to_xmm(inst.operands[0], X64Reg::XMM0);
    emit("cvttsd2si rax, xmm0");
    emit(fmt::format("mov QWORD PTR [rbp{}], rax", slot));
}

// ============================================================================
// String operations
// ============================================================================

void X64CodeGenerator::emit_string_len(const ir::Instruction& inst) {
    auto slot = get_temp_slot(inst.id);
    auto* str = inst.operands[0];

    // String is {ptr, len}; length is the second QWORD (id+100000)
    uint32_t len_id = str->id + 100000;
    auto len_it = temp_slots_.find(len_id);
    if (len_it != temp_slots_.end()) {
        emit(fmt::format("mov rax, QWORD PTR [rbp{}]", len_it->second));
    } else if (frame_.has_slot(len_id)) {
        emit(fmt::format("mov rax, QWORD PTR [rbp{}]", frame_.offset_of(len_id)));
    } else if (frame_.has_slot(str->id)) {
        // Contiguous layout: length at base + 8
        emit(fmt::format("mov rax, QWORD PTR [rbp{}]", frame_.offset_of(str->id) + 8));
    } else if (temp_slots_.count(str->id)) {
        emit(fmt::format("mov rax, QWORD PTR [rbp{}]", temp_slots_[str->id] + 8));
    }
    emit(fmt::format("mov QWORD PTR [rbp{}], rax", slot));
}

void X64CodeGenerator::emit_string_index(const ir::Instruction& inst) {
    auto slot = get_temp_slot(inst.id);
    auto* str = inst.operands[0];
    auto* index = inst.operands[1];

    // Load string pointer (first QWORD)
    if (temp_slots_.count(str->id))
        emit(fmt::format("mov rcx, QWORD PTR [rbp{}]", temp_slots_[str->id]));
    else if (frame_.has_slot(str->id))
        emit(fmt::format("mov rcx, QWORD PTR [rbp{}]", frame_.offset_of(str->id)));

    // Load index
    load_value_to_rax(index);
    // Load byte at ptr + index
    emit("movzx rax, BYTE PTR [rcx+rax]");
    emit(fmt::format("mov QWORD PTR [rbp{}], rax", slot));
}

void X64CodeGenerator::emit_string_concat(const ir::Instruction& inst) {
    auto slot = get_temp_slot(inst.id);
    auto* a = inst.operands[0];
    auto* b = inst.operands[1];

    // golangc_string_concat returns {ptr, len} = 16 bytes via sret
    // RCX = sret pointer, RDX = ptr1, R8 = len1, R9 = ptr2, stack[0] = len2

    // Sret: point to result temp slot
    emit(fmt::format("lea rcx, [rbp{}]", slot));

    // Load string a: ptr in RDX, len in R8
    if (temp_slots_.count(a->id)) {
        emit(fmt::format("mov rdx, QWORD PTR [rbp{}]", temp_slots_[a->id]));
        uint32_t a_len_id = a->id + 100000;
        auto a_len_it = temp_slots_.find(a_len_id);
        if (a_len_it != temp_slots_.end())
            emit(fmt::format("mov r8, QWORD PTR [rbp{}]", a_len_it->second));
        else
            emit(fmt::format("mov r8, QWORD PTR [rbp{}]", temp_slots_[a->id] + 8));
    } else if (frame_.has_slot(a->id)) {
        emit(fmt::format("mov rdx, QWORD PTR [rbp{}]", frame_.offset_of(a->id)));
        emit(fmt::format("mov r8, QWORD PTR [rbp{}]", frame_.offset_of(a->id) + 8));
    }

    // Load string b: ptr in R9, len on stack
    if (temp_slots_.count(b->id)) {
        emit(fmt::format("mov r9, QWORD PTR [rbp{}]", temp_slots_[b->id]));
        uint32_t b_len_id = b->id + 100000;
        auto b_len_it = temp_slots_.find(b_len_id);
        if (b_len_it != temp_slots_.end())
            emit(fmt::format("mov rax, QWORD PTR [rbp{}]", b_len_it->second));
        else
            emit(fmt::format("mov rax, QWORD PTR [rbp{}]", temp_slots_[b->id] + 8));
    } else if (frame_.has_slot(b->id)) {
        emit(fmt::format("mov r9, QWORD PTR [rbp{}]", frame_.offset_of(b->id)));
        emit(fmt::format("mov rax, QWORD PTR [rbp{}]", frame_.offset_of(b->id) + 8));
    }

    // Push len2 as 5th arg on the stack (after shadow space)
    emit("push rax");
    emit(fmt::format("sub rsp, {}", kShadowSpace));
    emit("call golangc_string_concat");
    emit(fmt::format("add rsp, {}", kShadowSpace));
    emit("add rsp, 8"); // pop the pushed arg
}

// ============================================================================
// Slice operations
// ============================================================================

void X64CodeGenerator::emit_slice_len(const ir::Instruction& inst) {
    auto slot = get_temp_slot(inst.id);
    auto* slice = inst.operands[0];

    // Slice is {ptr, len, cap}; length is the second QWORD (id+100000)
    uint32_t len_id = slice->id + 100000;
    auto len_it = temp_slots_.find(len_id);
    if (len_it != temp_slots_.end()) {
        emit(fmt::format("mov rax, QWORD PTR [rbp{}]", len_it->second));
    } else if (frame_.has_slot(len_id)) {
        emit(fmt::format("mov rax, QWORD PTR [rbp{}]", frame_.offset_of(len_id)));
    } else if (frame_.has_slot(slice->id)) {
        emit(fmt::format("mov rax, QWORD PTR [rbp{}]", frame_.offset_of(slice->id) + 8));
    } else if (temp_slots_.count(slice->id)) {
        emit(fmt::format("mov rax, QWORD PTR [rbp{}]", temp_slots_[slice->id] + 8));
    }
    emit(fmt::format("mov QWORD PTR [rbp{}], rax", slot));
}

void X64CodeGenerator::emit_slice_cap(const ir::Instruction& inst) {
    auto slot = get_temp_slot(inst.id);
    auto* slice = inst.operands[0];

    // Capacity is the third QWORD (id+200000)
    uint32_t cap_id = slice->id + 200000;
    auto cap_it = temp_slots_.find(cap_id);
    if (cap_it != temp_slots_.end()) {
        emit(fmt::format("mov rax, QWORD PTR [rbp{}]", cap_it->second));
    } else if (frame_.has_slot(cap_id)) {
        emit(fmt::format("mov rax, QWORD PTR [rbp{}]", frame_.offset_of(cap_id)));
    } else if (frame_.has_slot(slice->id)) {
        emit(fmt::format("mov rax, QWORD PTR [rbp{}]", frame_.offset_of(slice->id) + 16));
    } else if (temp_slots_.count(slice->id)) {
        emit(fmt::format("mov rax, QWORD PTR [rbp{}]", temp_slots_[slice->id] + 16));
    }
    emit(fmt::format("mov QWORD PTR [rbp{}], rax", slot));
}

void X64CodeGenerator::emit_slice_index(const ir::Instruction& inst) {
    auto slot = get_temp_slot(inst.id);
    auto* slice = inst.operands[0];
    auto* index = inst.operands[1];

    // Load slice data pointer (first QWORD)
    if (temp_slots_.count(slice->id))
        emit(fmt::format("mov rcx, QWORD PTR [rbp{}]", temp_slots_[slice->id]));
    else if (frame_.has_slot(slice->id))
        emit(fmt::format("mov rcx, QWORD PTR [rbp{}]", frame_.offset_of(slice->id)));

    // Load index and compute offset (assume 8-byte elements for now)
    load_value_to_rax(index);
    emit("imul rax, 8");
    emit("mov rax, QWORD PTR [rcx+rax]");
    emit(fmt::format("mov QWORD PTR [rbp{}], rax", slot));
}

// ============================================================================
// Panic
// ============================================================================

void X64CodeGenerator::emit_panic(const ir::Instruction& inst) {
    // Pass message string pointer (first QWORD of string) in RCX, or null
    if (!inst.operands.empty()) {
        auto* arg = inst.operands[0];
        // If arg is a string type, load the pointer (first QWORD)
        bool is_str = arg->type && is_string_type(arg->type);
        auto* arg_inst = dynamic_cast<const ir::Instruction*>(arg);
        if (arg_inst && arg_inst->opcode == ir::Opcode::ConstString) is_str = true;

        if (is_str) {
            if (temp_slots_.count(arg->id))
                emit(fmt::format("mov rcx, QWORD PTR [rbp{}]", temp_slots_[arg->id]));
            else if (frame_.has_slot(arg->id))
                emit(fmt::format("mov rcx, QWORD PTR [rbp{}]", frame_.offset_of(arg->id)));
            else
                emit("xor ecx, ecx");
        } else {
            load_value_to_reg(arg, X64Reg::RCX);
        }
    } else {
        emit("xor ecx, ecx");
    }
    emit(fmt::format("sub rsp, {}", kShadowSpace));
    emit("call golangc_panic");
    // Panic is noreturn — no epilogue after this
}

// ============================================================================
// Float width conversions
// ============================================================================

void X64CodeGenerator::emit_fpext(const ir::Instruction& inst) {
    // float32 → float64: cvtss2sd
    auto slot = get_temp_slot(inst.id);
    // Load the F32 value: we need movss (4 bytes) from the source slot
    auto* src = inst.operands[0];
    if (temp_slots_.count(src->id))
        emit(fmt::format("movss xmm0, DWORD PTR [rbp{}]", temp_slots_[src->id]));
    else if (frame_.has_slot(src->id))
        emit(fmt::format("movss xmm0, DWORD PTR [rbp{}]", frame_.offset_of(src->id)));
    emit("cvtss2sd xmm0, xmm0");
    emit(fmt::format("movsd QWORD PTR [rbp{}], xmm0", slot));
}

void X64CodeGenerator::emit_fptrunc(const ir::Instruction& inst) {
    // float64 → float32: cvtsd2ss
    auto slot = get_temp_slot(inst.id);
    load_value_to_xmm(inst.operands[0], X64Reg::XMM0);
    emit("cvtsd2ss xmm0, xmm0");
    emit(fmt::format("movss DWORD PTR [rbp{}], xmm0", slot));
}

void X64CodeGenerator::emit_bitcast(const ir::Instruction& inst) {
    // No-op: just copy the raw bits (load from source, store to dest)
    auto slot = get_temp_slot(inst.id);
    load_value_to_rax(inst.operands[0]);
    emit(fmt::format("mov QWORD PTR [rbp{}], rax", slot));
}

// ============================================================================
// Aggregate operations
// ============================================================================

void X64CodeGenerator::emit_extract_value(const ir::Instruction& inst) {
    auto slot = get_temp_slot(inst.id);
    auto* agg = inst.operands[0];
    uint32_t field = inst.field_index;

    // Find aggregate base slot
    int32_t base_off = 0;
    if (temp_slots_.count(agg->id))
        base_off = temp_slots_[agg->id];
    else if (frame_.has_slot(agg->id))
        base_off = frame_.offset_of(agg->id);

    // Try to use the pre-allocated alias slot for extra QWORDs
    if (field > 0) {
        uint32_t extra_id = agg->id + field * 100000;
        auto it = temp_slots_.find(extra_id);
        if (it != temp_slots_.end()) {
            emit(fmt::format("mov rax, QWORD PTR [rbp{}]", it->second));
        } else {
            emit(fmt::format("mov rax, QWORD PTR [rbp{}]", base_off + static_cast<int32_t>(field) * 8));
        }
    } else {
        emit(fmt::format("mov rax, QWORD PTR [rbp{}]", base_off));
    }
    emit(fmt::format("mov QWORD PTR [rbp{}], rax", slot));
}

void X64CodeGenerator::emit_insert_value(const ir::Instruction& inst) {
    // InsertValue(aggregate, value, field_index) → new aggregate with field replaced
    auto* agg = inst.operands[0];
    auto* val = inst.operands[1];
    uint32_t field = inst.field_index;

    auto slot = get_temp_slot(inst.id);

    // Determine number of QWORDs in the aggregate
    int32_t nq = 1;
    if (agg->type && agg->type->is_struct()) {
        nq = type_qwords(agg->type);
    }

    // Copy entire aggregate to result slot
    int32_t agg_off = 0;
    if (temp_slots_.count(agg->id))
        agg_off = temp_slots_[agg->id];
    else if (frame_.has_slot(agg->id))
        agg_off = frame_.offset_of(agg->id);

    for (int32_t q = 0; q < nq; ++q) {
        int32_t src_off = agg_off;
        if (q > 0) {
            uint32_t extra_id = agg->id + static_cast<uint32_t>(q) * 100000;
            auto it = temp_slots_.find(extra_id);
            if (it != temp_slots_.end()) src_off = it->second;
            else src_off = agg_off + q * 8;
        }
        int32_t dst_off = slot;
        if (q > 0) {
            uint32_t extra_id = inst.id + static_cast<uint32_t>(q) * 100000;
            auto it = temp_slots_.find(extra_id);
            if (it != temp_slots_.end()) dst_off = it->second;
            else dst_off = slot + q * 8;
        }
        emit(fmt::format("mov rax, QWORD PTR [rbp{}]", src_off));
        emit(fmt::format("mov QWORD PTR [rbp{}], rax", dst_off));
    }

    // Overwrite the specified field with the new value
    load_value_to_rax(val);
    int32_t field_off = slot;
    if (field > 0) {
        uint32_t extra_id = inst.id + field * 100000;
        auto it = temp_slots_.find(extra_id);
        if (it != temp_slots_.end()) field_off = it->second;
        else field_off = slot + static_cast<int32_t>(field) * 8;
    }
    emit(fmt::format("mov QWORD PTR [rbp{}], rax", field_off));
}

// ============================================================================
// Defer call (record for later replay at ret)
// ============================================================================

void X64CodeGenerator::emit_defer_call(const ir::Instruction& inst) {
    // Record the defer instruction for replay at each return site
    defers_.push_back(&inst);
}

} // namespace codegen
} // namespace golangc
