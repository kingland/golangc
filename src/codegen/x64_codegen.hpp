#pragma once

#include "ir/ir.hpp"

#include <cstdint>
#include <string>
#include <unordered_map>
#include <vector>

namespace golangc {
namespace codegen {

// ============================================================================
// x86-64 Registers
// ============================================================================

enum class X64Reg : uint8_t {
    RAX, RCX, RDX, RBX, RSP, RBP, RSI, RDI,
    R8, R9, R10, R11, R12, R13, R14, R15,
    XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7,
    XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14, XMM15,
};

enum class RegSize : uint8_t {
    Byte,   // 8-bit (al, cl, ...)
    Word,   // 16-bit (ax, cx, ...)
    DWord,  // 32-bit (eax, ecx, ...)
    QWord,  // 64-bit (rax, rcx, ...)
};

/// Return the MASM register name for a given register and size.
[[nodiscard]] std::string_view reg_name(X64Reg reg, RegSize size = RegSize::QWord);

// Windows x64 ABI: first 4 integer/pointer args in RCX, RDX, R8, R9
inline constexpr X64Reg kArgRegs[] = {X64Reg::RCX, X64Reg::RDX, X64Reg::R8, X64Reg::R9};
inline constexpr int kMaxRegArgs = 4;
inline constexpr int kShadowSpace = 32; // 32-byte shadow space required by Windows x64

// ============================================================================
// Frame Layout — maps IR allocas to stack offsets
// ============================================================================

struct FrameSlot {
    int32_t offset;   // Negative offset from RBP (e.g., -8, -16, ...)
    int32_t size;     // Size in bytes
};

class FrameLayout {
public:
    /// Assign a stack slot for an IR value (alloca or spill).
    /// Returns the offset from RBP.
    int32_t allocate(uint32_t value_id, int32_t size, int32_t alignment = 8);

    /// Look up the RBP offset for an IR value.
    [[nodiscard]] int32_t offset_of(uint32_t value_id) const;

    /// Check if a value has been allocated a slot.
    [[nodiscard]] bool has_slot(uint32_t value_id) const;

    /// Get the total frame size (aligned to 16 bytes).
    [[nodiscard]] int32_t frame_size() const;

    /// Reset the layout for a new function.
    void reset();

private:
    std::unordered_map<uint32_t, FrameSlot> slots_;
    int32_t current_offset_ = 0; // Grows downward (negative from RBP)
};

// ============================================================================
// String Literal Pool
// ============================================================================

struct StringLiteral {
    std::string label;
    std::string data;
    int64_t length;
};

// ============================================================================
// Float Literal Pool
// ============================================================================

struct FloatLiteral {
    std::string label;
    double value;
};

// ============================================================================
// X64CodeGenerator — IR Module → MASM assembly text
// ============================================================================

class X64CodeGenerator {
public:
    /// Generate MASM assembly for an entire IR module.
    [[nodiscard]] std::string generate(const ir::Module& module);

    /// Generate MASM assembly for a single function (for testing).
    [[nodiscard]] std::string generate_function(const ir::Function& func);

    /// Get a MASM-safe function name (replace dots with $).
    [[nodiscard]] static std::string masm_name(std::string_view ir_name);

    /// Compute size of an IR type in bytes.
    [[nodiscard]] static int32_t type_size(const ir::IRType* type);

    /// Check if an IR type is the string struct type {ptr, i64}.
    [[nodiscard]] static bool is_string_type(const ir::IRType* type);

    /// Check if an IR type is a float type (F32 or F64).
    [[nodiscard]] static bool is_float_type(const ir::IRType* type);

    /// Check if an IR type is the slice struct type {ptr, i64, i64}.
    [[nodiscard]] static bool is_slice_type(const ir::IRType* type);

    /// Compute the number of QWORDs needed to represent a type.
    [[nodiscard]] static int32_t type_qwords(const ir::IRType* type);

    /// Check if a struct type is >8 bytes (needs pass-by-pointer in Windows x64 ABI).
    [[nodiscard]] static bool is_large_struct(const ir::IRType* type);

private:
    std::string out_;           // Assembly output buffer
    FrameLayout frame_;         // Current function's frame layout
    int string_counter_ = 0;   // Counter for string literal labels
    std::vector<StringLiteral> string_pool_; // String literals for .data section
    int float_counter_ = 0;    // Counter for float literal labels
    std::vector<FloatLiteral> float_pool_;   // Float literals for .data section
    bool needs_sign_mask_ = false; // Whether __f64_sign_mask is needed

    // Maps IR Value IDs to stack slots (for temporaries, not just allocas)
    std::unordered_map<uint32_t, int32_t> temp_slots_;

    // ---- Module structure ----
    void emit_module_header(const ir::Module& module);
    void emit_data_section();
    void emit_module_footer();

    // ---- Function generation ----
    void emit_function(const ir::Function& func);
    void emit_prologue(const ir::Function& func);
    void emit_epilogue();
    void scan_allocas(const ir::Function& func);
    void prescan_temps(const ir::Function& func);

    // ---- Block generation ----
    void emit_block(const ir::BasicBlock& block, const ir::Function& func);

    // ---- Instruction selection (x64_codegen_inst.cpp) ----
    void emit_instruction(const ir::Instruction& inst, const ir::Function& func);

    // Individual instruction emitters
    void emit_const_int(const ir::Instruction& inst);
    void emit_const_bool(const ir::Instruction& inst);
    void emit_const_string(const ir::Instruction& inst);
    void emit_alloca(const ir::Instruction& inst);
    void emit_load(const ir::Instruction& inst);
    void emit_store(const ir::Instruction& inst);
    void emit_arith(const ir::Instruction& inst);
    void emit_div_rem(const ir::Instruction& inst);
    void emit_neg(const ir::Instruction& inst);
    void emit_bitwise(const ir::Instruction& inst);
    void emit_bitnot(const ir::Instruction& inst);
    void emit_compare(const ir::Instruction& inst);
    void emit_lognot(const ir::Instruction& inst);
    void emit_br(const ir::Instruction& inst);
    void emit_condbr(const ir::Instruction& inst);
    void emit_ret(const ir::Instruction& inst, const ir::Function& func);
    void emit_call(const ir::Instruction& inst);
    void emit_println(const ir::Instruction& inst);
    void emit_getptr(const ir::Instruction& inst);
    void emit_sext(const ir::Instruction& inst);
    void emit_trunc(const ir::Instruction& inst);
    void emit_interface_make(const ir::Instruction& inst);
    void emit_interface_data(const ir::Instruction& inst);

    // Float operations
    void emit_const_float(const ir::Instruction& inst);
    void emit_float_arith(const ir::Instruction& inst);
    void emit_float_neg(const ir::Instruction& inst);
    void emit_float_compare(const ir::Instruction& inst);
    void emit_sitofp(const ir::Instruction& inst);
    void emit_fptosi(const ir::Instruction& inst);

    // String operations
    void emit_string_len(const ir::Instruction& inst);
    void emit_string_index(const ir::Instruction& inst);
    void emit_string_concat(const ir::Instruction& inst);

    // Slice operations
    void emit_slice_len(const ir::Instruction& inst);
    void emit_slice_cap(const ir::Instruction& inst);
    void emit_slice_index(const ir::Instruction& inst);

    // Panic
    void emit_panic(const ir::Instruction& inst);

    // Conversions (float width)
    void emit_fpext(const ir::Instruction& inst);
    void emit_fptrunc(const ir::Instruction& inst);
    void emit_bitcast(const ir::Instruction& inst);

    // Aggregate operations
    void emit_extract_value(const ir::Instruction& inst);
    void emit_insert_value(const ir::Instruction& inst);

    // Defer support
    void emit_defer_call(const ir::Instruction& inst);

    // Goroutine / Channel operations
    void emit_chan_make(const ir::Instruction& inst);
    void emit_chan_send(const ir::Instruction& inst);
    void emit_chan_recv(const ir::Instruction& inst);
    void emit_go_spawn(const ir::Instruction& inst);
    void emit_slice_make(const ir::Instruction& inst);

    // Map operations
    void emit_map_make(const ir::Instruction& inst);
    void emit_map_get(const ir::Instruction& inst);
    void emit_map_set(const ir::Instruction& inst);

    // ---- Helpers ----
    /// Emit a line of assembly (indented with 4 spaces).
    void emit(std::string_view line);
    /// Emit a label line.
    void emit_label(std::string_view label);
    /// Emit a comment.
    void emit_comment(std::string_view comment);
    /// Emit a blank line.
    void emit_blank();

    /// Get a temporary stack slot for an IR value's result.
    int32_t get_temp_slot(uint32_t value_id);

    /// Load an IR value into RAX. The value may be a constant, param, or alloca.
    void load_value_to_rax(const ir::Value* val);

    /// Load an IR value into a specific register.
    void load_value_to_reg(const ir::Value* val, X64Reg reg);

    /// Load a float value from a stack slot into an XMM register.
    void load_value_to_xmm(const ir::Value* val, X64Reg xmm_reg);

    /// Get the address operand string for a value on the stack.
    [[nodiscard]] std::string stack_operand(uint32_t value_id) const;

    /// Copy N QWORDs between two stack locations via RAX.
    void emit_struct_copy(int32_t dst_offset, int32_t src_offset, int32_t num_qwords);

    /// Check if an IR value is a GetPtr instruction.
    [[nodiscard]] static bool is_getptr(const ir::Value* val);

    /// Track the current function being compiled (for sret detection).
    const ir::Function* current_func_ = nullptr;

    /// Sret pointer slot offset (for functions returning large structs).
    int32_t sret_slot_ = 0;
    bool has_sret_ = false;

    /// Deferred calls collected during function emission (replayed LIFO at ret).
    std::vector<const ir::Instruction*> defers_;

    /// Get the MASM block label for a basic block within a function.
    [[nodiscard]] static std::string block_label(const ir::Function& func,
                                                  std::string_view block_name);
};

} // namespace codegen
} // namespace golangc
