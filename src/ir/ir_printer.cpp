#include "ir/ir_printer.hpp"

#include <fmt/format.h>

namespace golangc {
namespace ir {

std::string IRPrinter::format_value(const Value* val) {
    if (!val) return "null";

    // Functions are referenced by @name
    if (dynamic_cast<const Function*>(val)) {
        return "@" + val->name;
    }

    // Global variables are @name
    if (dynamic_cast<const GlobalVariable*>(val)) {
        return "@" + val->name;
    }

    // Regular values are %name or %id
    if (!val->name.empty()) {
        return "%" + val->name;
    }
    return fmt::format("%{}", val->id);
}

std::string IRPrinter::format_type(const IRType* type) {
    return ir_type_string(type);
}

std::string IRPrinter::print_instruction(const Instruction& inst) {
    std::string result;
    bool produces_value = true;

    // Instructions that don't produce a value
    switch (inst.opcode) {
        case Opcode::Store:
        case Opcode::Br:
        case Opcode::CondBr:
        case Opcode::Ret:
        case Opcode::Switch:
        case Opcode::ChanSend:
        case Opcode::MapSet:
        case Opcode::GoSpawn:
        case Opcode::DeferCall:
        case Opcode::Panic:
        case Opcode::Println:
            produces_value = false;
            break;
        default:
            break;
    }

    if (produces_value) {
        result += "    " + format_value(&inst) + " = ";
    } else {
        result += "    ";
    }

    switch (inst.opcode) {
        case Opcode::ConstInt:
            result += fmt::format("const_int {} : {}", inst.imm_int,
                                  format_type(inst.type));
            break;
        case Opcode::ConstFloat:
            result += fmt::format("const_float {} : {}", inst.imm_float,
                                  format_type(inst.type));
            break;
        case Opcode::ConstBool:
            result += fmt::format("const_bool {} : i1",
                                  inst.imm_int ? "true" : "false");
            break;
        case Opcode::ConstString:
            result += fmt::format("const_string \"{}\" : string", inst.imm_string);
            break;
        case Opcode::ConstNil:
            result += fmt::format("const_nil : {}", format_type(inst.type));
            break;

        case Opcode::Add: case Opcode::Sub: case Opcode::Mul:
        case Opcode::Div: case Opcode::Rem:
        case Opcode::FAdd: case Opcode::FSub: case Opcode::FMul:
        case Opcode::FDiv:
        case Opcode::And: case Opcode::Or: case Opcode::Xor:
        case Opcode::Shl: case Opcode::Shr: case Opcode::AndNot:
        case Opcode::Eq: case Opcode::Ne: case Opcode::Lt:
        case Opcode::Le: case Opcode::Gt: case Opcode::Ge:
        case Opcode::FEq: case Opcode::FNe: case Opcode::FLt:
        case Opcode::FLe: case Opcode::FGt: case Opcode::FGe:
        case Opcode::StringConcat:
            result += fmt::format("{} {}, {} : {}",
                                  opcode_name(inst.opcode),
                                  format_value(inst.operands[0]),
                                  format_value(inst.operands[1]),
                                  format_type(inst.type));
            break;

        case Opcode::Neg: case Opcode::FNeg: case Opcode::BitNot:
        case Opcode::LogNot:
            result += fmt::format("{} {} : {}",
                                  opcode_name(inst.opcode),
                                  format_value(inst.operands[0]),
                                  format_type(inst.type));
            break;

        case Opcode::Alloca:
            result += fmt::format("alloca : {}", format_type(inst.type));
            break;

        case Opcode::Load:
            result += fmt::format("load {} : {}",
                                  format_value(inst.operands[0]),
                                  format_type(inst.type));
            break;

        case Opcode::Store:
            result += fmt::format("store {}, {}",
                                  format_value(inst.operands[0]),
                                  format_value(inst.operands[1]));
            break;

        case Opcode::GetPtr:
            if (inst.operands.size() >= 2) {
                result += fmt::format("getptr {}, {} : {}",
                                      format_value(inst.operands[0]),
                                      format_value(inst.operands[1]),
                                      format_type(inst.type));
            } else if (inst.operands.size() == 1) {
                result += fmt::format("getptr {}, #{} : {}",
                                      format_value(inst.operands[0]),
                                      inst.field_index,
                                      format_type(inst.type));
            }
            break;

        case Opcode::Trunc: case Opcode::ZExt: case Opcode::SExt:
        case Opcode::FPExt: case Opcode::FPTrunc:
        case Opcode::SIToFP: case Opcode::FPToSI: case Opcode::Bitcast:
            result += fmt::format("{} {} : {}",
                                  opcode_name(inst.opcode),
                                  format_value(inst.operands[0]),
                                  format_type(inst.type));
            break;

        case Opcode::ExtractValue:
            result += fmt::format("extractvalue {}, #{} : {}",
                                  format_value(inst.operands[0]),
                                  inst.field_index,
                                  format_type(inst.type));
            break;

        case Opcode::InsertValue:
            result += fmt::format("insertvalue {}, {}, #{} : {}",
                                  format_value(inst.operands[0]),
                                  format_value(inst.operands[1]),
                                  inst.field_index,
                                  format_type(inst.type));
            break;

        case Opcode::Br:
            result += fmt::format("br {}", inst.targets[0]->label);
            break;

        case Opcode::CondBr:
            result += fmt::format("condbr {}, {}, {}",
                                  format_value(inst.operands[0]),
                                  inst.targets[0]->label,
                                  inst.targets[1]->label);
            break;

        case Opcode::Ret:
            if (inst.operands.empty()) {
                result += "ret void";
            } else {
                result += fmt::format("ret {}", format_value(inst.operands[0]));
            }
            break;

        case Opcode::Switch: {
            result += fmt::format("switch {}, default {} [",
                                  format_value(inst.operands[0]),
                                  inst.targets[0]->label);
            for (size_t i = 1; i < inst.operands.size() && i < inst.targets.size(); ++i) {
                if (i > 1) result += ", ";
                result += fmt::format("{}: {}",
                                      format_value(inst.operands[i]),
                                      inst.targets[i]->label);
            }
            result += "]";
            break;
        }

        case Opcode::Phi:
            result += "phi";
            // Phi nodes not used in alloca-based IR
            break;

        case Opcode::Call: {
            result += fmt::format("call {}(", format_value(inst.operands[0]));
            for (size_t i = 1; i < inst.operands.size(); ++i) {
                if (i > 1) result += ", ";
                result += format_value(inst.operands[i]);
            }
            result += fmt::format(") : {}", format_type(inst.type));
            break;
        }

        case Opcode::GoSpawn: {
            result += fmt::format("go {}(", format_value(inst.operands[0]));
            for (size_t i = 1; i < inst.operands.size(); ++i) {
                if (i > 1) result += ", ";
                result += format_value(inst.operands[i]);
            }
            result += ")";
            break;
        }

        case Opcode::DeferCall: {
            result += fmt::format("defer {}(", format_value(inst.operands[0]));
            for (size_t i = 1; i < inst.operands.size(); ++i) {
                if (i > 1) result += ", ";
                result += format_value(inst.operands[i]);
            }
            result += ")";
            break;
        }

        case Opcode::ChanMake:
            result += fmt::format("chan_make : {}", format_type(inst.type));
            break;

        case Opcode::ChanSend:
            result += fmt::format("chan_send {}, {}",
                                  format_value(inst.operands[0]),
                                  format_value(inst.operands[1]));
            break;

        case Opcode::ChanRecv:
            result += fmt::format("chan_recv {} : {}",
                                  format_value(inst.operands[0]),
                                  format_type(inst.type));
            break;

        case Opcode::SliceMake:
            result += fmt::format("slice_make : {}", format_type(inst.type));
            break;

        case Opcode::SliceLen:
            result += fmt::format("slice_len {} : i64",
                                  format_value(inst.operands[0]));
            break;

        case Opcode::SliceCap:
            result += fmt::format("slice_cap {} : i64",
                                  format_value(inst.operands[0]));
            break;

        case Opcode::SliceIndex:
            result += fmt::format("slice_index {}, {} : {}",
                                  format_value(inst.operands[0]),
                                  format_value(inst.operands[1]),
                                  format_type(inst.type));
            break;

        case Opcode::MapMake:
            result += fmt::format("map_make : {}", format_type(inst.type));
            break;

        case Opcode::MapGet:
            result += fmt::format("map_get {}, {} : {}",
                                  format_value(inst.operands[0]),
                                  format_value(inst.operands[1]),
                                  format_type(inst.type));
            break;

        case Opcode::MapSet:
            result += fmt::format("map_set {}, {}, {}",
                                  format_value(inst.operands[0]),
                                  format_value(inst.operands[1]),
                                  format_value(inst.operands[2]));
            break;

        case Opcode::StringLen:
            result += fmt::format("string_len {} : i64",
                                  format_value(inst.operands[0]));
            break;

        case Opcode::StringIndex:
            result += fmt::format("string_index {}, {} : i8",
                                  format_value(inst.operands[0]),
                                  format_value(inst.operands[1]));
            break;

        case Opcode::StringDecodeRune:
            result += fmt::format("string_decode_rune {}, {}, {} : i64",
                                  format_value(inst.operands[0]),
                                  format_value(inst.operands[1]),
                                  inst.operands.size() > 2 ? format_value(inst.operands[2]) : "null");
            break;

        case Opcode::InterfaceMake:
            result += fmt::format("iface_make {}, {} : interface",
                                  format_value(inst.operands[0]),
                                  format_value(inst.operands[1]));
            break;

        case Opcode::InterfaceData:
            result += fmt::format("iface_data {} : {}",
                                  format_value(inst.operands[0]),
                                  format_type(inst.type));
            break;

        case Opcode::InterfaceType:
            result += fmt::format("iface_type {} : ptr",
                                  format_value(inst.operands[0]));
            break;

        case Opcode::Panic:
            if (!inst.operands.empty()) {
                result += fmt::format("panic {}", format_value(inst.operands[0]));
            } else {
                result += "panic";
            }
            break;

        case Opcode::Recover:
            result += fmt::format("recover : {}", format_type(inst.type));
            break;

        case Opcode::Println: {
            result += "println(";
            for (size_t i = 0; i < inst.operands.size(); ++i) {
                if (i > 0) result += ", ";
                result += format_value(inst.operands[i]);
            }
            result += ")";
            break;
        }
    }

    return result;
}

std::string IRPrinter::print_function(const Function& func) {
    std::string result;

    // Function header
    result += fmt::format("func @{}(", func.name);
    for (size_t i = 0; i < func.params.size(); ++i) {
        if (i > 0) result += ", ";
        result += format_value(func.params[i].get());
        result += ": " + format_type(func.params[i]->type);
    }
    result += ")";
    if (func.return_type && !func.return_type->is_void()) {
        result += " -> " + format_type(func.return_type);
    }
    result += " {\n";

    // Basic blocks
    for (const auto& block : func.blocks) {
        result += block->label + ":\n";
        for (const auto& inst : block->instructions) {
            result += print_instruction(*inst) + "\n";
        }
    }

    result += "}\n";
    return result;
}

std::string IRPrinter::print(const Module& module) {
    std::string result;

    result += fmt::format("module \"{}\"\n\n", module.name);

    // Global variables
    for (const auto& gv : module.globals) {
        result += fmt::format("global @{} : {}\n", gv->name, format_type(gv->type));
    }
    if (!module.globals.empty()) result += "\n";

    // Functions
    for (const auto& func : module.functions) {
        result += print_function(*func);
        result += "\n";
    }

    return result;
}

} // namespace ir
} // namespace golangc
