#pragma once

#include <cstdint>
#include <memory>
#include <string>
#include <string_view>
#include <vector>

namespace golangc {
namespace ir {

// ============================================================================
// IR Type System (machine-level types, separate from sema::Type)
// ============================================================================

enum class IRTypeKind : uint8_t {
    Void,
    I1,
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,
    Ptr,
    Struct,
    Array,
    Func,
};

struct IRType {
    IRTypeKind kind;

    // For Struct types: field types
    std::vector<IRType*> fields;

    // For Array types: element type and count
    IRType* element = nullptr;
    int64_t count = 0;

    // For Func types: return type and param types
    IRType* return_type = nullptr;
    std::vector<IRType*> param_types;

    // Human-readable name (for struct types)
    std::string name;

    explicit IRType(IRTypeKind k) : kind(k) {}

    [[nodiscard]] bool is_integer() const {
        return kind == IRTypeKind::I1 || kind == IRTypeKind::I8 ||
               kind == IRTypeKind::I16 || kind == IRTypeKind::I32 ||
               kind == IRTypeKind::I64;
    }

    [[nodiscard]] bool is_float() const {
        return kind == IRTypeKind::F32 || kind == IRTypeKind::F64;
    }

    [[nodiscard]] bool is_void() const { return kind == IRTypeKind::Void; }
    [[nodiscard]] bool is_ptr() const { return kind == IRTypeKind::Ptr; }
    [[nodiscard]] bool is_struct() const { return kind == IRTypeKind::Struct; }
    [[nodiscard]] bool is_array() const { return kind == IRTypeKind::Array; }
};

/// Return a human-readable string for an IR type.
[[nodiscard]] std::string ir_type_string(const IRType* t);

// ============================================================================
// Instruction Opcodes
// ============================================================================

enum class Opcode : uint8_t {
    // Constants
    ConstInt,
    ConstFloat,
    ConstBool,
    ConstString,
    ConstNil,

    // Integer arithmetic
    Add, Sub, Mul, Div, Rem,
    // Float arithmetic
    FAdd, FSub, FMul, FDiv,

    // Bitwise
    And, Or, Xor, Shl, Shr, AndNot, BitNot,

    // Integer comparison
    Eq, Ne, Lt, Le, Gt, Ge,
    // Float comparison
    FEq, FNe, FLt, FLe, FGt, FGe,

    // Logical
    LogNot,

    // Memory
    Alloca, Load, Store, GetPtr,

    // Conversions
    Trunc, ZExt, SExt, FPExt, FPTrunc, SIToFP, FPToSI, Bitcast,

    // Aggregates
    ExtractValue, InsertValue,

    // Control flow
    Br, CondBr, Ret, Switch, Phi,

    // Calls
    Call,

    // Go-specific
    GoSpawn, DeferCall,
    ChanMake, ChanSend, ChanRecv,
    SliceMake, SliceLen, SliceCap, SliceIndex,
    MapMake, MapGet, MapSet,
    StringLen, StringIndex, StringConcat,
    InterfaceMake, InterfaceData, InterfaceType,
    Panic, Recover,
    Println,

    // Negation (int)
    Neg,
    // Float negation
    FNeg,
};

/// Return the name of an opcode.
[[nodiscard]] std::string_view opcode_name(Opcode op);

// Forward declarations
struct BasicBlock;
struct Function;
struct Module;

// ============================================================================
// Value — base for all SSA values
// ============================================================================

struct Value {
    uint32_t id = 0;
    IRType* type = nullptr;
    std::string name;

    Value() = default;
    Value(uint32_t id, IRType* type, std::string name = "")
        : id(id), type(type), name(std::move(name)) {}

    virtual ~Value() = default;
};

// ============================================================================
// Instruction — an SSA instruction producing a value
// ============================================================================

struct Instruction : Value {
    Opcode opcode;
    BasicBlock* parent = nullptr;

    // Operands (other Values)
    std::vector<Value*> operands;

    // Block targets for control flow (Br, CondBr, Switch)
    std::vector<BasicBlock*> targets;

    // Constant data
    int64_t imm_int = 0;
    double imm_float = 0.0;
    std::string imm_string;

    // For ExtractValue/InsertValue: field index
    uint32_t field_index = 0;

    Instruction() = default;
    Instruction(uint32_t id, Opcode op, IRType* type, std::string name = "")
        : Value(id, type, std::move(name)), opcode(op) {}
};

// ============================================================================
// BasicBlock
// ============================================================================

struct BasicBlock {
    std::string label;
    Function* parent = nullptr;
    std::vector<std::unique_ptr<Instruction>> instructions;
    std::vector<BasicBlock*> predecessors;
    std::vector<BasicBlock*> successors;

    explicit BasicBlock(std::string label) : label(std::move(label)) {}

    /// Check if this block has a terminator instruction.
    [[nodiscard]] bool has_terminator() const;

    /// Get the terminator instruction, or nullptr.
    [[nodiscard]] Instruction* terminator() const;
};

// ============================================================================
// Function
// ============================================================================

struct Function : Value {
    std::vector<std::unique_ptr<Value>> params;
    std::vector<std::unique_ptr<BasicBlock>> blocks;
    IRType* return_type = nullptr;
    bool is_method = false;

    Function() = default;
    Function(uint32_t id, IRType* func_type, std::string name)
        : Value(id, func_type, std::move(name)) {}

    /// Get the entry block.
    [[nodiscard]] BasicBlock* entry() const {
        return blocks.empty() ? nullptr : blocks.front().get();
    }

    /// Create a new basic block in this function.
    BasicBlock* create_block(const std::string& label);
};

// ============================================================================
// GlobalVariable
// ============================================================================

struct GlobalVariable : Value {
    bool is_const = false;
    Instruction* initializer = nullptr;

    GlobalVariable() = default;
    GlobalVariable(uint32_t id, IRType* type, std::string name, bool is_const = false)
        : Value(id, type, std::move(name)), is_const(is_const) {}
};

// ============================================================================
// Module
// ============================================================================

struct Module {
    std::string name;
    std::vector<std::unique_ptr<Function>> functions;
    std::vector<std::unique_ptr<GlobalVariable>> globals;

    // Owned IR types
    std::vector<std::unique_ptr<IRType>> types;

    explicit Module(std::string name) : name(std::move(name)) {}

    /// Create a new function in this module.
    Function* create_function(uint32_t id, IRType* func_type, const std::string& fname);

    /// Find a function by name.
    [[nodiscard]] Function* find_function(std::string_view fname) const;
};

} // namespace ir
} // namespace golangc
