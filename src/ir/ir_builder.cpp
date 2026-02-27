#include "ir/ir_builder.hpp"
#include "ir/ir_type_map.hpp"

#include <cassert>

namespace golangc {
namespace ir {

IRBuilder::IRBuilder(IRTypeMap& type_map) : type_map_(type_map) {}

// ============================================================================
// Internal helpers
// ============================================================================

Instruction* IRBuilder::emit(Opcode op, IRType* type, const std::string& name) {
    assert(insert_block_ && "No insertion point set");
    auto inst = std::make_unique<Instruction>(next_id(), op, type, name);
    inst->parent = insert_block_;
    auto* ptr = inst.get();
    insert_block_->instructions.push_back(std::move(inst));
    return ptr;
}

Instruction* IRBuilder::emit_binary(Opcode op, Value* lhs, Value* rhs, IRType* type,
                                     const std::string& name) {
    auto* inst = emit(op, type, name);
    inst->operands = {lhs, rhs};
    return inst;
}

Instruction* IRBuilder::emit_unary(Opcode op, Value* operand, IRType* type,
                                    const std::string& name) {
    auto* inst = emit(op, type, name);
    inst->operands = {operand};
    return inst;
}

Instruction* IRBuilder::emit_conv(Opcode op, Value* val, IRType* to, const std::string& name) {
    auto* inst = emit(op, to, name);
    inst->operands = {val};
    return inst;
}

// ============================================================================
// Constants
// ============================================================================

Instruction* IRBuilder::create_const_int(IRType* type, int64_t val, const std::string& name) {
    auto* inst = emit(Opcode::ConstInt, type, name);
    inst->imm_int = val;
    return inst;
}

Instruction* IRBuilder::create_const_float(IRType* type, double val, const std::string& name) {
    auto* inst = emit(Opcode::ConstFloat, type, name);
    inst->imm_float = val;
    return inst;
}

Instruction* IRBuilder::create_const_bool(bool val, const std::string& name) {
    auto* inst = emit(Opcode::ConstBool, type_map_.i1_type(), name);
    inst->imm_int = val ? 1 : 0;
    return inst;
}

Instruction* IRBuilder::create_const_string(const std::string& val, const std::string& name) {
    auto* inst = emit(Opcode::ConstString, type_map_.string_type(), name);
    inst->imm_string = val;
    return inst;
}

Instruction* IRBuilder::create_const_nil(IRType* type, const std::string& name) {
    return emit(Opcode::ConstNil, type, name);
}

// ============================================================================
// Integer arithmetic
// ============================================================================

Instruction* IRBuilder::create_add(Value* lhs, Value* rhs, const std::string& name) {
    return emit_binary(Opcode::Add, lhs, rhs, lhs->type, name);
}

Instruction* IRBuilder::create_sub(Value* lhs, Value* rhs, const std::string& name) {
    return emit_binary(Opcode::Sub, lhs, rhs, lhs->type, name);
}

Instruction* IRBuilder::create_mul(Value* lhs, Value* rhs, const std::string& name) {
    return emit_binary(Opcode::Mul, lhs, rhs, lhs->type, name);
}

Instruction* IRBuilder::create_div(Value* lhs, Value* rhs, const std::string& name) {
    return emit_binary(Opcode::Div, lhs, rhs, lhs->type, name);
}

Instruction* IRBuilder::create_rem(Value* lhs, Value* rhs, const std::string& name) {
    return emit_binary(Opcode::Rem, lhs, rhs, lhs->type, name);
}

Instruction* IRBuilder::create_neg(Value* operand, const std::string& name) {
    return emit_unary(Opcode::Neg, operand, operand->type, name);
}

// ============================================================================
// Float arithmetic
// ============================================================================

Instruction* IRBuilder::create_fadd(Value* lhs, Value* rhs, const std::string& name) {
    return emit_binary(Opcode::FAdd, lhs, rhs, lhs->type, name);
}

Instruction* IRBuilder::create_fsub(Value* lhs, Value* rhs, const std::string& name) {
    return emit_binary(Opcode::FSub, lhs, rhs, lhs->type, name);
}

Instruction* IRBuilder::create_fmul(Value* lhs, Value* rhs, const std::string& name) {
    return emit_binary(Opcode::FMul, lhs, rhs, lhs->type, name);
}

Instruction* IRBuilder::create_fdiv(Value* lhs, Value* rhs, const std::string& name) {
    return emit_binary(Opcode::FDiv, lhs, rhs, lhs->type, name);
}

Instruction* IRBuilder::create_fneg(Value* operand, const std::string& name) {
    return emit_unary(Opcode::FNeg, operand, operand->type, name);
}

// ============================================================================
// Bitwise
// ============================================================================

Instruction* IRBuilder::create_and(Value* lhs, Value* rhs, const std::string& name) {
    return emit_binary(Opcode::And, lhs, rhs, lhs->type, name);
}

Instruction* IRBuilder::create_or(Value* lhs, Value* rhs, const std::string& name) {
    return emit_binary(Opcode::Or, lhs, rhs, lhs->type, name);
}

Instruction* IRBuilder::create_xor(Value* lhs, Value* rhs, const std::string& name) {
    return emit_binary(Opcode::Xor, lhs, rhs, lhs->type, name);
}

Instruction* IRBuilder::create_shl(Value* lhs, Value* rhs, const std::string& name) {
    return emit_binary(Opcode::Shl, lhs, rhs, lhs->type, name);
}

Instruction* IRBuilder::create_shr(Value* lhs, Value* rhs, const std::string& name) {
    return emit_binary(Opcode::Shr, lhs, rhs, lhs->type, name);
}

Instruction* IRBuilder::create_andnot(Value* lhs, Value* rhs, const std::string& name) {
    return emit_binary(Opcode::AndNot, lhs, rhs, lhs->type, name);
}

Instruction* IRBuilder::create_bitnot(Value* operand, const std::string& name) {
    return emit_unary(Opcode::BitNot, operand, operand->type, name);
}

// ============================================================================
// Integer comparison
// ============================================================================

Instruction* IRBuilder::create_eq(Value* lhs, Value* rhs, const std::string& name) {
    return emit_binary(Opcode::Eq, lhs, rhs, type_map_.i1_type(), name);
}

Instruction* IRBuilder::create_ne(Value* lhs, Value* rhs, const std::string& name) {
    return emit_binary(Opcode::Ne, lhs, rhs, type_map_.i1_type(), name);
}

Instruction* IRBuilder::create_lt(Value* lhs, Value* rhs, const std::string& name) {
    return emit_binary(Opcode::Lt, lhs, rhs, type_map_.i1_type(), name);
}

Instruction* IRBuilder::create_le(Value* lhs, Value* rhs, const std::string& name) {
    return emit_binary(Opcode::Le, lhs, rhs, type_map_.i1_type(), name);
}

Instruction* IRBuilder::create_gt(Value* lhs, Value* rhs, const std::string& name) {
    return emit_binary(Opcode::Gt, lhs, rhs, type_map_.i1_type(), name);
}

Instruction* IRBuilder::create_ge(Value* lhs, Value* rhs, const std::string& name) {
    return emit_binary(Opcode::Ge, lhs, rhs, type_map_.i1_type(), name);
}

// ============================================================================
// Float comparison
// ============================================================================

Instruction* IRBuilder::create_feq(Value* lhs, Value* rhs, const std::string& name) {
    return emit_binary(Opcode::FEq, lhs, rhs, type_map_.i1_type(), name);
}

Instruction* IRBuilder::create_fne(Value* lhs, Value* rhs, const std::string& name) {
    return emit_binary(Opcode::FNe, lhs, rhs, type_map_.i1_type(), name);
}

Instruction* IRBuilder::create_flt(Value* lhs, Value* rhs, const std::string& name) {
    return emit_binary(Opcode::FLt, lhs, rhs, type_map_.i1_type(), name);
}

Instruction* IRBuilder::create_fle(Value* lhs, Value* rhs, const std::string& name) {
    return emit_binary(Opcode::FLe, lhs, rhs, type_map_.i1_type(), name);
}

Instruction* IRBuilder::create_fgt(Value* lhs, Value* rhs, const std::string& name) {
    return emit_binary(Opcode::FGt, lhs, rhs, type_map_.i1_type(), name);
}

Instruction* IRBuilder::create_fge(Value* lhs, Value* rhs, const std::string& name) {
    return emit_binary(Opcode::FGe, lhs, rhs, type_map_.i1_type(), name);
}

// ============================================================================
// Logical
// ============================================================================

Instruction* IRBuilder::create_lognot(Value* operand, const std::string& name) {
    return emit_unary(Opcode::LogNot, operand, type_map_.i1_type(), name);
}

// ============================================================================
// Memory
// ============================================================================

Instruction* IRBuilder::create_alloca(IRType* alloc_type, const std::string& name) {
    auto* inst = emit(Opcode::Alloca, type_map_.ptr_type(), name);
    inst->alloc_type = alloc_type;
    return inst;
}

Instruction* IRBuilder::create_load(Value* ptr, IRType* result_type, const std::string& name) {
    return emit_unary(Opcode::Load, ptr, result_type, name);
}

Instruction* IRBuilder::create_store(Value* val, Value* ptr) {
    auto* inst = emit(Opcode::Store, type_map_.void_type());
    inst->operands = {val, ptr};
    return inst;
}

Instruction* IRBuilder::create_getptr(Value* base, Value* index, IRType* result_type,
                                       const std::string& name) {
    auto* inst = emit(Opcode::GetPtr, result_type, name);
    inst->operands = {base, index};
    return inst;
}

// ============================================================================
// Conversions
// ============================================================================

Instruction* IRBuilder::create_trunc(Value* val, IRType* to, const std::string& name) {
    return emit_conv(Opcode::Trunc, val, to, name);
}

Instruction* IRBuilder::create_zext(Value* val, IRType* to, const std::string& name) {
    return emit_conv(Opcode::ZExt, val, to, name);
}

Instruction* IRBuilder::create_sext(Value* val, IRType* to, const std::string& name) {
    return emit_conv(Opcode::SExt, val, to, name);
}

Instruction* IRBuilder::create_fpext(Value* val, IRType* to, const std::string& name) {
    return emit_conv(Opcode::FPExt, val, to, name);
}

Instruction* IRBuilder::create_fptrunc(Value* val, IRType* to, const std::string& name) {
    return emit_conv(Opcode::FPTrunc, val, to, name);
}

Instruction* IRBuilder::create_sitofp(Value* val, IRType* to, const std::string& name) {
    return emit_conv(Opcode::SIToFP, val, to, name);
}

Instruction* IRBuilder::create_fptosi(Value* val, IRType* to, const std::string& name) {
    return emit_conv(Opcode::FPToSI, val, to, name);
}

Instruction* IRBuilder::create_bitcast(Value* val, IRType* to, const std::string& name) {
    return emit_conv(Opcode::Bitcast, val, to, name);
}

// ============================================================================
// Aggregates
// ============================================================================

Instruction* IRBuilder::create_extract_value(Value* agg, uint32_t index, IRType* result_type,
                                              const std::string& name) {
    auto* inst = emit(Opcode::ExtractValue, result_type, name);
    inst->operands = {agg};
    inst->field_index = index;
    return inst;
}

Instruction* IRBuilder::create_insert_value(Value* agg, Value* val, uint32_t index,
                                             const std::string& name) {
    auto* inst = emit(Opcode::InsertValue, agg->type, name);
    inst->operands = {agg, val};
    inst->field_index = index;
    return inst;
}

// ============================================================================
// Control flow
// ============================================================================

Instruction* IRBuilder::create_br(BasicBlock* target) {
    auto* inst = emit(Opcode::Br, type_map_.void_type());
    inst->targets = {target};
    // Update CFG edges
    insert_block_->successors.push_back(target);
    target->predecessors.push_back(insert_block_);
    return inst;
}

Instruction* IRBuilder::create_condbr(Value* cond, BasicBlock* true_bb, BasicBlock* false_bb) {
    auto* inst = emit(Opcode::CondBr, type_map_.void_type());
    inst->operands = {cond};
    inst->targets = {true_bb, false_bb};
    // Update CFG edges
    insert_block_->successors.push_back(true_bb);
    insert_block_->successors.push_back(false_bb);
    true_bb->predecessors.push_back(insert_block_);
    false_bb->predecessors.push_back(insert_block_);
    return inst;
}

Instruction* IRBuilder::create_ret(Value* val) {
    auto* inst = emit(Opcode::Ret, type_map_.void_type());
    if (val) {
        inst->operands = {val};
    }
    return inst;
}

Instruction* IRBuilder::create_switch(Value* val, BasicBlock* default_bb,
                                       const std::vector<std::pair<Value*, BasicBlock*>>& cases) {
    auto* inst = emit(Opcode::Switch, type_map_.void_type());
    inst->operands.push_back(val);
    inst->targets.push_back(default_bb);
    insert_block_->successors.push_back(default_bb);
    default_bb->predecessors.push_back(insert_block_);
    for (const auto& [case_val, case_bb] : cases) {
        inst->operands.push_back(case_val);
        inst->targets.push_back(case_bb);
        insert_block_->successors.push_back(case_bb);
        case_bb->predecessors.push_back(insert_block_);
    }
    return inst;
}

// ============================================================================
// Calls
// ============================================================================

Instruction* IRBuilder::create_call(Value* callee, const std::vector<Value*>& args,
                                     IRType* result_type, const std::string& name) {
    auto* inst = emit(Opcode::Call, result_type, name);
    inst->operands.push_back(callee);
    for (auto* arg : args) {
        inst->operands.push_back(arg);
    }
    return inst;
}

// ============================================================================
// Go-specific
// ============================================================================

Instruction* IRBuilder::create_go_spawn(Value* callee, const std::vector<Value*>& args) {
    auto* inst = emit(Opcode::GoSpawn, type_map_.void_type());
    inst->operands.push_back(callee);
    for (auto* arg : args) {
        inst->operands.push_back(arg);
    }
    return inst;
}

Instruction* IRBuilder::create_defer_call(Value* callee, const std::vector<Value*>& args) {
    auto* inst = emit(Opcode::DeferCall, type_map_.void_type());
    inst->operands.push_back(callee);
    for (auto* arg : args) {
        inst->operands.push_back(arg);
    }
    return inst;
}

Instruction* IRBuilder::create_chan_make(IRType* chan_type, int64_t elem_size,
                                         int64_t buf_cap, const std::string& name) {
    auto* inst = emit(Opcode::ChanMake, chan_type, name);
    inst->imm_int    = elem_size;
    inst->field_index = static_cast<uint32_t>(buf_cap); // reuse field_index for buf_cap
    return inst;
}

Instruction* IRBuilder::create_chan_send(Value* ch, Value* val) {
    auto* inst = emit(Opcode::ChanSend, type_map_.void_type());
    inst->operands = {ch, val};
    return inst;
}

Instruction* IRBuilder::create_chan_recv(Value* ch, IRType* result_type, const std::string& name) {
    auto* inst = emit(Opcode::ChanRecv, result_type, name);
    inst->operands = {ch};
    return inst;
}

Instruction* IRBuilder::create_println(const std::vector<Value*>& args) {
    auto* inst = emit(Opcode::Println, type_map_.void_type());
    inst->operands = args;
    return inst;
}

Instruction* IRBuilder::create_panic(Value* val) {
    auto* inst = emit(Opcode::Panic, type_map_.void_type());
    if (val) inst->operands = {val};
    return inst;
}

// ============================================================================
// Struct operations
// ============================================================================

Instruction* IRBuilder::create_getptr_field(Value* struct_ptr, uint32_t field_index,
                                             IRType* field_type, const std::string& name) {
    auto* inst = emit(Opcode::GetPtr, type_map_.ptr_type(), name);
    inst->operands = {struct_ptr};
    inst->field_index = field_index;
    (void)field_type; // Tracked by caller
    return inst;
}

// ============================================================================
// Interface operations
// ============================================================================

Instruction* IRBuilder::create_interface_make(Value* type_desc, Value* data,
                                               const std::string& name) {
    auto* inst = emit(Opcode::InterfaceMake, type_map_.interface_type(), name);
    inst->operands = {type_desc, data};
    return inst;
}

Instruction* IRBuilder::create_interface_data(Value* iface, IRType* result_type,
                                               const std::string& name) {
    auto* inst = emit(Opcode::InterfaceData, result_type, name);
    inst->operands = {iface};
    return inst;
}

Instruction* IRBuilder::create_interface_type(Value* iface, const std::string& name) {
    auto* inst = emit(Opcode::InterfaceType, type_map_.ptr_type(), name);
    inst->operands = {iface};
    return inst;
}

// ============================================================================
// Slice operations
// ============================================================================

Instruction* IRBuilder::create_slice_make(IRType* slice_type, Value* length, Value* capacity,
                                           const std::string& name) {
    auto* inst = emit(Opcode::SliceMake, slice_type, name);
    inst->operands = {length, capacity};
    return inst;
}

Instruction* IRBuilder::create_slice_len(Value* slice, const std::string& name) {
    return emit_unary(Opcode::SliceLen, slice, type_map_.i64_type(), name);
}

Instruction* IRBuilder::create_slice_cap(Value* slice, const std::string& name) {
    return emit_unary(Opcode::SliceCap, slice, type_map_.i64_type(), name);
}

Instruction* IRBuilder::create_slice_index(Value* slice, Value* index, IRType* elem_type,
                                            const std::string& name) {
    auto* inst = emit(Opcode::SliceIndex, elem_type, name);
    inst->operands = {slice, index};
    return inst;
}

// ============================================================================
// String operations
// ============================================================================

Instruction* IRBuilder::create_string_len(Value* str, const std::string& name) {
    return emit_unary(Opcode::StringLen, str, type_map_.i64_type(), name);
}

Instruction* IRBuilder::create_string_index(Value* str, Value* index, const std::string& name) {
    auto* inst = emit(Opcode::StringIndex, type_map_.i8_type(), name);
    inst->operands = {str, index};
    return inst;
}

Instruction* IRBuilder::create_string_concat(Value* a, Value* b, const std::string& name) {
    return emit_binary(Opcode::StringConcat, a, b, type_map_.string_type(), name);
}

Instruction* IRBuilder::create_string_eq(Value* lhs, Value* rhs, const std::string& name) {
    auto* inst = emit(Opcode::StringEq, type_map_.i1_type(), name);
    inst->operands = {lhs, rhs};
    return inst;
}

Instruction* IRBuilder::create_string_decode_rune(Value* str, Value* idx, Value* rune_alloca,
                                                   const std::string& name) {
    auto* inst = emit(Opcode::StringDecodeRune, type_map_.i64_type(), name);
    inst->operands = {str, idx, rune_alloca};
    return inst;
}

// ============================================================================
// Map operations
// ============================================================================

Instruction* IRBuilder::create_map_make(IRType* map_type, int64_t key_size, int64_t val_size,
                                         const std::string& name) {
    auto* inst = emit(Opcode::MapMake, map_type, name);
    inst->imm_int = key_size;
    inst->field_index = static_cast<uint32_t>(val_size); // reuse field_index for val_size
    return inst;
}

Instruction* IRBuilder::create_map_get(Value* m, Value* key, IRType* val_type,
                                        const std::string& name) {
    auto* inst = emit(Opcode::MapGet, val_type, name);
    inst->operands = {m, key};
    return inst;
}

Instruction* IRBuilder::create_map_set(Value* m, Value* key, Value* val) {
    auto* inst = emit(Opcode::MapSet, type_map_.void_type());
    inst->operands = {m, key, val};
    return inst;
}

Instruction* IRBuilder::create_map_len(Value* m, const std::string& name) {
    return emit_unary(Opcode::MapLen, m, type_map_.i64_type(), name);
}

Instruction* IRBuilder::create_map_delete(Value* m, Value* key) {
    auto* inst = emit(Opcode::MapDelete, type_map_.void_type());
    inst->operands = {m, key};
    return inst;
}

Instruction* IRBuilder::create_map_iter_make(Value* m, const std::string& name) {
    auto* inst = emit(Opcode::MapIterMake, type_map_.ptr_type(), name);
    inst->operands = {m};
    return inst;
}

Instruction* IRBuilder::create_map_iter_next(Value* iter, IRType* key_type, IRType* val_type,
                                              const std::string& name) {
    // Returns a tuple: {ok (i64), key (key_type), val (val_type)}
    // We model it as a ptr type; the codegen will write key/val into pre-allocated slots
    (void)key_type;
    (void)val_type;
    auto* inst = emit(Opcode::MapIterNext, type_map_.i64_type(), name);
    inst->operands = {iter};
    return inst;
}

Instruction* IRBuilder::create_map_iter_free(Value* iter) {
    auto* inst = emit(Opcode::MapIterFree, type_map_.void_type());
    inst->operands = {iter};
    return inst;
}

// ============================================================================
// Closures
// ============================================================================

Instruction* IRBuilder::create_closure_make(Value* func_ptr, Value* env_ptr,
                                             const std::string& name) {
    auto* inst = emit(Opcode::ClosureMake, type_map_.ptr_type(), name);
    // operands[0] = func_ptr, operands[1] = env_ptr (ConstNil if non-capturing)
    if (!env_ptr) {
        auto* nil = emit(Opcode::ConstNil, type_map_.ptr_type(), "env.nil");
        env_ptr = nil;
    }
    inst->operands = {func_ptr, env_ptr};
    return inst;
}

Instruction* IRBuilder::create_closure_env(Value* closure_val, const std::string& name) {
    auto* inst = emit(Opcode::ClosureEnv, type_map_.ptr_type(), name);
    inst->operands = {closure_val};
    return inst;
}

// ============================================================================
// Heap allocation
// ============================================================================

Instruction* IRBuilder::create_malloc(Value* size, const std::string& name) {
    auto* inst = emit(Opcode::Malloc, type_map_.ptr_type(), name);
    inst->operands = {size};
    return inst;
}

// ============================================================================
// Extended slice operations
// ============================================================================

Instruction* IRBuilder::create_slice_index_addr(Value* slice, Value* index,
                                                  const std::string& name) {
    auto* inst = emit(Opcode::SliceIndexAddr, type_map_.ptr_type(), name);
    inst->operands = {slice, index};
    return inst;
}

Instruction* IRBuilder::create_slice_append(Value* slice, Value* elem, IRType* elem_type,
                                             const std::string& name) {
    // imm_int stores the elem size in bytes (used by codegen to decide copy size)
    auto* inst = emit(Opcode::SliceAppend, slice->type ? slice->type : type_map_.slice_type(), name);
    inst->operands = {slice, elem};
    // Compute element size
    int64_t elem_sz = 8;
    if (elem_type) elem_sz = IRTypeMap::type_size(elem_type);
    inst->imm_int = elem_sz;
    return inst;
}

} // namespace ir
} // namespace golangc
