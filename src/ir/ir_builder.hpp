#pragma once

#include "ir/ir.hpp"
#include "ir/ir_type_map.hpp"

#include <string>

namespace golangc {
namespace ir {

/// Fluent API for constructing IR instructions.
/// Manages insertion point and value ID generation.
class IRBuilder {
public:
    explicit IRBuilder(IRTypeMap& type_map);

    // ---- Insertion point ----
    void set_insert_block(BasicBlock* block) { insert_block_ = block; }
    [[nodiscard]] BasicBlock* insert_block() const { return insert_block_; }

    // ---- Constants ----
    Instruction* create_const_int(IRType* type, int64_t val, const std::string& name = "");
    Instruction* create_const_float(IRType* type, double val, const std::string& name = "");
    Instruction* create_const_bool(bool val, const std::string& name = "");
    Instruction* create_const_string(const std::string& val, const std::string& name = "");
    Instruction* create_const_nil(IRType* type, const std::string& name = "");

    // ---- Integer arithmetic ----
    Instruction* create_add(Value* lhs, Value* rhs, const std::string& name = "");
    Instruction* create_sub(Value* lhs, Value* rhs, const std::string& name = "");
    Instruction* create_mul(Value* lhs, Value* rhs, const std::string& name = "");
    Instruction* create_div(Value* lhs, Value* rhs, const std::string& name = "");
    Instruction* create_rem(Value* lhs, Value* rhs, const std::string& name = "");
    Instruction* create_neg(Value* operand, const std::string& name = "");

    // ---- Float arithmetic ----
    Instruction* create_fadd(Value* lhs, Value* rhs, const std::string& name = "");
    Instruction* create_fsub(Value* lhs, Value* rhs, const std::string& name = "");
    Instruction* create_fmul(Value* lhs, Value* rhs, const std::string& name = "");
    Instruction* create_fdiv(Value* lhs, Value* rhs, const std::string& name = "");
    Instruction* create_fneg(Value* operand, const std::string& name = "");

    // ---- Bitwise ----
    Instruction* create_and(Value* lhs, Value* rhs, const std::string& name = "");
    Instruction* create_or(Value* lhs, Value* rhs, const std::string& name = "");
    Instruction* create_xor(Value* lhs, Value* rhs, const std::string& name = "");
    Instruction* create_shl(Value* lhs, Value* rhs, const std::string& name = "");
    Instruction* create_shr(Value* lhs, Value* rhs, const std::string& name = "");
    Instruction* create_andnot(Value* lhs, Value* rhs, const std::string& name = "");
    Instruction* create_bitnot(Value* operand, const std::string& name = "");

    // ---- Integer comparison ----
    Instruction* create_eq(Value* lhs, Value* rhs, const std::string& name = "");
    Instruction* create_ne(Value* lhs, Value* rhs, const std::string& name = "");
    Instruction* create_lt(Value* lhs, Value* rhs, const std::string& name = "");
    Instruction* create_le(Value* lhs, Value* rhs, const std::string& name = "");
    Instruction* create_gt(Value* lhs, Value* rhs, const std::string& name = "");
    Instruction* create_ge(Value* lhs, Value* rhs, const std::string& name = "");

    // ---- Float comparison ----
    Instruction* create_feq(Value* lhs, Value* rhs, const std::string& name = "");
    Instruction* create_fne(Value* lhs, Value* rhs, const std::string& name = "");
    Instruction* create_flt(Value* lhs, Value* rhs, const std::string& name = "");
    Instruction* create_fle(Value* lhs, Value* rhs, const std::string& name = "");
    Instruction* create_fgt(Value* lhs, Value* rhs, const std::string& name = "");
    Instruction* create_fge(Value* lhs, Value* rhs, const std::string& name = "");

    // ---- Logical ----
    Instruction* create_lognot(Value* operand, const std::string& name = "");

    // ---- Memory ----
    Instruction* create_alloca(IRType* alloc_type, const std::string& name = "");
    Instruction* create_load(Value* ptr, IRType* result_type, const std::string& name = "");
    Instruction* create_store(Value* val, Value* ptr);
    Instruction* create_getptr(Value* base, Value* index, IRType* result_type,
                               const std::string& name = "");

    // ---- Conversions ----
    Instruction* create_trunc(Value* val, IRType* to, const std::string& name = "");
    Instruction* create_zext(Value* val, IRType* to, const std::string& name = "");
    Instruction* create_sext(Value* val, IRType* to, const std::string& name = "");
    Instruction* create_fpext(Value* val, IRType* to, const std::string& name = "");
    Instruction* create_fptrunc(Value* val, IRType* to, const std::string& name = "");
    Instruction* create_sitofp(Value* val, IRType* to, const std::string& name = "");
    Instruction* create_fptosi(Value* val, IRType* to, const std::string& name = "");
    Instruction* create_bitcast(Value* val, IRType* to, const std::string& name = "");

    // ---- Aggregates ----
    Instruction* create_extract_value(Value* agg, uint32_t index, IRType* result_type,
                                      const std::string& name = "");
    Instruction* create_insert_value(Value* agg, Value* val, uint32_t index,
                                     const std::string& name = "");

    // ---- Control flow ----
    Instruction* create_br(BasicBlock* target);
    Instruction* create_condbr(Value* cond, BasicBlock* true_bb, BasicBlock* false_bb);
    Instruction* create_ret(Value* val = nullptr);
    Instruction* create_switch(Value* val, BasicBlock* default_bb,
                               const std::vector<std::pair<Value*, BasicBlock*>>& cases);

    // ---- Calls ----
    Instruction* create_call(Value* callee, const std::vector<Value*>& args,
                             IRType* result_type, const std::string& name = "");

    // ---- Go-specific ----
    Instruction* create_go_spawn(Value* callee, const std::vector<Value*>& args);
    Instruction* create_defer_call(Value* callee, const std::vector<Value*>& args);
    Instruction* create_chan_make(IRType* chan_type, int64_t elem_size = 8,
                                 const std::string& name = "");
    Instruction* create_chan_send(Value* ch, Value* val);
    Instruction* create_chan_recv(Value* ch, IRType* result_type, const std::string& name = "");
    Instruction* create_println(const std::vector<Value*>& args);
    Instruction* create_panic(Value* val);

    // ---- Struct operations ----
    Instruction* create_getptr_field(Value* struct_ptr, uint32_t field_index,
                                     IRType* field_type, const std::string& name = "");

    // ---- Interface operations ----
    Instruction* create_interface_make(Value* type_desc, Value* data, const std::string& name = "");
    Instruction* create_interface_data(Value* iface, IRType* result_type,
                                       const std::string& name = "");
    Instruction* create_interface_type(Value* iface, const std::string& name = "");

    // ---- Slice operations ----
    Instruction* create_slice_make(IRType* slice_type, Value* length, Value* capacity,
                                   const std::string& name = "");
    Instruction* create_slice_len(Value* slice, const std::string& name = "");
    Instruction* create_slice_cap(Value* slice, const std::string& name = "");
    Instruction* create_slice_index(Value* slice, Value* index, IRType* elem_type,
                                    const std::string& name = "");

    // ---- String operations ----
    Instruction* create_string_len(Value* str, const std::string& name = "");
    Instruction* create_string_index(Value* str, Value* index, const std::string& name = "");
    Instruction* create_string_concat(Value* a, Value* b, const std::string& name = "");

    // ---- Map operations ----
    Instruction* create_map_make(IRType* map_type, int64_t key_size = 8, int64_t val_size = 8,
                                 const std::string& name = "");
    Instruction* create_map_get(Value* m, Value* key, IRType* val_type,
                                const std::string& name = "");
    Instruction* create_map_set(Value* m, Value* key, Value* val);
    Instruction* create_map_len(Value* m, const std::string& name = "");
    Instruction* create_map_delete(Value* m, Value* key);
    Instruction* create_map_iter_make(Value* m, const std::string& name = "");
    Instruction* create_map_iter_next(Value* iter, IRType* key_type, IRType* val_type,
                                      const std::string& name = "");
    Instruction* create_map_iter_free(Value* iter);

    // ---- Extended slice operations ----
    Instruction* create_slice_index_addr(Value* slice, Value* index, const std::string& name = "");
    Instruction* create_slice_append(Value* slice, Value* elem, IRType* elem_type,
                                     const std::string& name = "");

    // ---- ID management ----
    [[nodiscard]] uint32_t next_id() { return next_id_++; }

    // ---- Type map access ----
    [[nodiscard]] IRTypeMap& type_map() { return type_map_; }

private:
    IRTypeMap& type_map_;
    BasicBlock* insert_block_ = nullptr;
    uint32_t next_id_ = 0;

    /// Create an instruction and add it to the current block.
    Instruction* emit(Opcode op, IRType* type, const std::string& name = "");

    /// Create a binary instruction.
    Instruction* emit_binary(Opcode op, Value* lhs, Value* rhs, IRType* type,
                             const std::string& name = "");

    /// Create a unary instruction.
    Instruction* emit_unary(Opcode op, Value* operand, IRType* type,
                            const std::string& name = "");

    /// Create a conversion instruction.
    Instruction* emit_conv(Opcode op, Value* val, IRType* to, const std::string& name = "");
};

} // namespace ir
} // namespace golangc
