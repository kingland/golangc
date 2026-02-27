#include "ir/ir.hpp"

#include <fmt/format.h>

namespace golangc {
namespace ir {

// ============================================================================
// IRType string representation
// ============================================================================

[[nodiscard]] std::string ir_type_string(const IRType* t) {
    if (!t) return "void";
    switch (t->kind) {
        case IRTypeKind::Void:   return "void";
        case IRTypeKind::I1:     return "i1";
        case IRTypeKind::I8:     return "i8";
        case IRTypeKind::I16:    return "i16";
        case IRTypeKind::I32:    return "i32";
        case IRTypeKind::I64:    return "i64";
        case IRTypeKind::F32:    return "f32";
        case IRTypeKind::F64:    return "f64";
        case IRTypeKind::Ptr:    return "ptr";
        case IRTypeKind::Struct: {
            if (!t->name.empty()) return t->name;
            std::string result = "{";
            for (size_t i = 0; i < t->fields.size(); ++i) {
                if (i > 0) result += ", ";
                result += ir_type_string(t->fields[i]);
            }
            result += "}";
            return result;
        }
        case IRTypeKind::Array:
            return fmt::format("[{} x {}]", t->count, ir_type_string(t->element));
        case IRTypeKind::Func: {
            std::string result = "func(";
            for (size_t i = 0; i < t->param_types.size(); ++i) {
                if (i > 0) result += ", ";
                result += ir_type_string(t->param_types[i]);
            }
            result += ") -> ";
            result += ir_type_string(t->return_type);
            return result;
        }
    }
    return "?";
}

// ============================================================================
// Opcode names
// ============================================================================

[[nodiscard]] std::string_view opcode_name(Opcode op) {
    switch (op) {
        case Opcode::ConstInt:       return "const_int";
        case Opcode::ConstFloat:     return "const_float";
        case Opcode::ConstBool:      return "const_bool";
        case Opcode::ConstString:    return "const_string";
        case Opcode::ConstNil:       return "const_nil";
        case Opcode::Add:            return "add";
        case Opcode::Sub:            return "sub";
        case Opcode::Mul:            return "mul";
        case Opcode::Div:            return "div";
        case Opcode::Rem:            return "rem";
        case Opcode::FAdd:           return "fadd";
        case Opcode::FSub:           return "fsub";
        case Opcode::FMul:           return "fmul";
        case Opcode::FDiv:           return "fdiv";
        case Opcode::And:            return "and";
        case Opcode::Or:             return "or";
        case Opcode::Xor:            return "xor";
        case Opcode::Shl:            return "shl";
        case Opcode::Shr:            return "shr";
        case Opcode::AndNot:         return "andnot";
        case Opcode::BitNot:         return "bitnot";
        case Opcode::Eq:             return "eq";
        case Opcode::Ne:             return "ne";
        case Opcode::Lt:             return "lt";
        case Opcode::Le:             return "le";
        case Opcode::Gt:             return "gt";
        case Opcode::Ge:             return "ge";
        case Opcode::FEq:            return "feq";
        case Opcode::FNe:            return "fne";
        case Opcode::FLt:            return "flt";
        case Opcode::FLe:            return "fle";
        case Opcode::FGt:            return "fgt";
        case Opcode::FGe:            return "fge";
        case Opcode::LogNot:         return "lognot";
        case Opcode::Neg:            return "neg";
        case Opcode::FNeg:           return "fneg";
        case Opcode::Alloca:         return "alloca";
        case Opcode::Load:           return "load";
        case Opcode::Store:          return "store";
        case Opcode::GetPtr:         return "getptr";
        case Opcode::Trunc:          return "trunc";
        case Opcode::ZExt:           return "zext";
        case Opcode::SExt:           return "sext";
        case Opcode::FPExt:          return "fpext";
        case Opcode::FPTrunc:        return "fptrunc";
        case Opcode::SIToFP:         return "sitofp";
        case Opcode::FPToSI:         return "fptosi";
        case Opcode::Bitcast:        return "bitcast";
        case Opcode::ExtractValue:   return "extractvalue";
        case Opcode::InsertValue:    return "insertvalue";
        case Opcode::Br:             return "br";
        case Opcode::CondBr:         return "condbr";
        case Opcode::Ret:            return "ret";
        case Opcode::Switch:         return "switch";
        case Opcode::Phi:            return "phi";
        case Opcode::Call:           return "call";
        case Opcode::GoSpawn:        return "go";
        case Opcode::DeferCall:      return "defer";
        case Opcode::ChanMake:       return "chan_make";
        case Opcode::ChanSend:       return "chan_send";
        case Opcode::ChanRecv:       return "chan_recv";
        case Opcode::SliceMake:      return "slice_make";
        case Opcode::SliceLen:       return "slice_len";
        case Opcode::SliceCap:       return "slice_cap";
        case Opcode::SliceIndex:     return "slice_index";
        case Opcode::SliceIndexAddr: return "slice_index_addr";
        case Opcode::SliceAppend:    return "slice_append";
        case Opcode::MapMake:        return "map_make";
        case Opcode::MapGet:         return "map_get";
        case Opcode::MapSet:         return "map_set";
        case Opcode::MapLen:         return "map_len";
        case Opcode::MapDelete:      return "map_delete";
        case Opcode::MapIterMake:    return "map_iter_make";
        case Opcode::MapIterNext:    return "map_iter_next";
        case Opcode::MapIterFree:    return "map_iter_free";
        case Opcode::ClosureMake:    return "closure_make";
        case Opcode::ClosureEnv:     return "closure_env";
        case Opcode::Malloc:         return "malloc";
        case Opcode::StringLen:      return "string_len";
        case Opcode::StringIndex:    return "string_index";
        case Opcode::StringConcat:   return "string_concat";
        case Opcode::StringEq:           return "string_eq";
        case Opcode::StringDecodeRune:   return "string_decode_rune";
        case Opcode::InterfaceMake:  return "iface_make";
        case Opcode::InterfaceData:  return "iface_data";
        case Opcode::InterfaceType:  return "iface_type";
        case Opcode::Panic:          return "panic";
        case Opcode::Recover:        return "recover";
        case Opcode::Println:        return "println";
    }
    return "?";
}

// ============================================================================
// BasicBlock
// ============================================================================

[[nodiscard]] bool BasicBlock::has_terminator() const {
    if (instructions.empty()) return false;
    auto op = instructions.back()->opcode;
    return op == Opcode::Br || op == Opcode::CondBr ||
           op == Opcode::Ret || op == Opcode::Switch ||
           op == Opcode::Panic;
}

[[nodiscard]] Instruction* BasicBlock::terminator() const {
    if (!has_terminator()) return nullptr;
    return instructions.back().get();
}

// ============================================================================
// Function
// ============================================================================

BasicBlock* Function::create_block(const std::string& lbl) {
    auto block = std::make_unique<BasicBlock>(lbl);
    block->parent = this;
    auto* ptr = block.get();
    blocks.push_back(std::move(block));
    return ptr;
}

// ============================================================================
// Module
// ============================================================================

Function* Module::create_function(uint32_t id, IRType* func_type, const std::string& fname) {
    auto func = std::make_unique<Function>(id, func_type, fname);
    auto* ptr = func.get();
    functions.push_back(std::move(func));
    return ptr;
}

[[nodiscard]] Function* Module::find_function(std::string_view fname) const {
    for (const auto& f : functions) {
        if (f->name == fname) return f.get();
    }
    return nullptr;
}

} // namespace ir
} // namespace golangc
