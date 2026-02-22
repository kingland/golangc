#include "ir/ir.hpp"
#include "ir/ir_builder.hpp"
#include "ir/ir_printer.hpp"
#include "ir/ir_type_map.hpp"
#include "sema/types.hpp"

#include <gtest/gtest.h>

using namespace golangc;
using namespace golangc::ir;

// ============================================================================
// IRType tests
// ============================================================================

TEST(IRTypeTest, PrimitiveTypes) {
    IRType void_t(IRTypeKind::Void);
    EXPECT_TRUE(void_t.is_void());
    EXPECT_FALSE(void_t.is_integer());
    EXPECT_FALSE(void_t.is_float());

    IRType i1_t(IRTypeKind::I1);
    EXPECT_TRUE(i1_t.is_integer());
    EXPECT_FALSE(i1_t.is_float());

    IRType i64_t(IRTypeKind::I64);
    EXPECT_TRUE(i64_t.is_integer());

    IRType f32_t(IRTypeKind::F32);
    EXPECT_TRUE(f32_t.is_float());
    EXPECT_FALSE(f32_t.is_integer());

    IRType f64_t(IRTypeKind::F64);
    EXPECT_TRUE(f64_t.is_float());

    IRType ptr_t(IRTypeKind::Ptr);
    EXPECT_TRUE(ptr_t.is_ptr());
    EXPECT_FALSE(ptr_t.is_integer());
}

TEST(IRTypeTest, StructType) {
    IRType s(IRTypeKind::Struct);
    EXPECT_TRUE(s.is_struct());
    EXPECT_FALSE(s.is_integer());
}

TEST(IRTypeTest, ArrayType) {
    IRType a(IRTypeKind::Array);
    EXPECT_TRUE(a.is_array());
}

TEST(IRTypeTest, TypeString) {
    IRType i64_t(IRTypeKind::I64);
    EXPECT_EQ(ir_type_string(&i64_t), "i64");

    IRType f64_t(IRTypeKind::F64);
    EXPECT_EQ(ir_type_string(&f64_t), "f64");

    IRType ptr_t(IRTypeKind::Ptr);
    EXPECT_EQ(ir_type_string(&ptr_t), "ptr");

    IRType void_t(IRTypeKind::Void);
    EXPECT_EQ(ir_type_string(&void_t), "void");

    EXPECT_EQ(ir_type_string(nullptr), "void");
}

TEST(IRTypeTest, StructTypeString) {
    IRType i64_t(IRTypeKind::I64);
    IRType s(IRTypeKind::Struct);
    s.fields = {&i64_t, &i64_t};
    EXPECT_EQ(ir_type_string(&s), "{i64, i64}");

    IRType named_s(IRTypeKind::Struct);
    named_s.name = "Point";
    EXPECT_EQ(ir_type_string(&named_s), "Point");
}

TEST(IRTypeTest, ArrayTypeString) {
    IRType i64_t(IRTypeKind::I64);
    IRType a(IRTypeKind::Array);
    a.element = &i64_t;
    a.count = 10;
    EXPECT_EQ(ir_type_string(&a), "[10 x i64]");
}

// ============================================================================
// Opcode tests
// ============================================================================

TEST(OpcodeTest, OpcodeNames) {
    EXPECT_EQ(opcode_name(Opcode::Add), "add");
    EXPECT_EQ(opcode_name(Opcode::Sub), "sub");
    EXPECT_EQ(opcode_name(Opcode::Mul), "mul");
    EXPECT_EQ(opcode_name(Opcode::FAdd), "fadd");
    EXPECT_EQ(opcode_name(Opcode::Load), "load");
    EXPECT_EQ(opcode_name(Opcode::Store), "store");
    EXPECT_EQ(opcode_name(Opcode::Alloca), "alloca");
    EXPECT_EQ(opcode_name(Opcode::Br), "br");
    EXPECT_EQ(opcode_name(Opcode::CondBr), "condbr");
    EXPECT_EQ(opcode_name(Opcode::Ret), "ret");
    EXPECT_EQ(opcode_name(Opcode::Call), "call");
    EXPECT_EQ(opcode_name(Opcode::ConstInt), "const_int");
    EXPECT_EQ(opcode_name(Opcode::ChanSend), "chan_send");
    EXPECT_EQ(opcode_name(Opcode::GoSpawn), "go");
    EXPECT_EQ(opcode_name(Opcode::Println), "println");
}

// ============================================================================
// BasicBlock tests
// ============================================================================

TEST(BasicBlockTest, EmptyBlock) {
    BasicBlock bb("entry");
    EXPECT_EQ(bb.label, "entry");
    EXPECT_FALSE(bb.has_terminator());
    EXPECT_EQ(bb.terminator(), nullptr);
    EXPECT_TRUE(bb.instructions.empty());
}

TEST(BasicBlockTest, TerminatorDetection) {
    BasicBlock bb("test");

    // Add a non-terminator instruction
    auto inst1 = std::make_unique<Instruction>(0, Opcode::ConstInt, nullptr);
    bb.instructions.push_back(std::move(inst1));
    EXPECT_FALSE(bb.has_terminator());

    // Add a terminator
    auto inst2 = std::make_unique<Instruction>(1, Opcode::Ret, nullptr);
    bb.instructions.push_back(std::move(inst2));
    EXPECT_TRUE(bb.has_terminator());
    EXPECT_NE(bb.terminator(), nullptr);
    EXPECT_EQ(bb.terminator()->opcode, Opcode::Ret);
}

// ============================================================================
// Function tests
// ============================================================================

TEST(FunctionTest, CreateBlock) {
    Function func(0, nullptr, "test_func");
    EXPECT_TRUE(func.blocks.empty());
    EXPECT_EQ(func.entry(), nullptr);

    auto* entry = func.create_block("entry");
    EXPECT_NE(entry, nullptr);
    EXPECT_EQ(entry->label, "entry");
    EXPECT_EQ(entry->parent, &func);
    EXPECT_EQ(func.entry(), entry);
    EXPECT_EQ(func.blocks.size(), 1u);

    auto* bb2 = func.create_block("bb2");
    EXPECT_EQ(func.blocks.size(), 2u);
    EXPECT_EQ(func.entry(), entry);  // Entry is still the first block
    EXPECT_EQ(bb2->parent, &func);
}

// ============================================================================
// Module tests
// ============================================================================

TEST(ModuleTest, CreateFunction) {
    Module mod("test");
    EXPECT_EQ(mod.name, "test");
    EXPECT_TRUE(mod.functions.empty());

    auto* func = mod.create_function(0, nullptr, "main");
    EXPECT_NE(func, nullptr);
    EXPECT_EQ(func->name, "main");
    EXPECT_EQ(mod.functions.size(), 1u);

    auto* found = mod.find_function("main");
    EXPECT_EQ(found, func);

    EXPECT_EQ(mod.find_function("nonexistent"), nullptr);
}

// ============================================================================
// IRTypeMap tests
// ============================================================================

TEST(IRTypeMapTest, PrimitiveSingletons) {
    IRTypeMap tm;
    EXPECT_NE(tm.void_type(), nullptr);
    EXPECT_EQ(tm.void_type()->kind, IRTypeKind::Void);
    EXPECT_EQ(tm.i1_type()->kind, IRTypeKind::I1);
    EXPECT_EQ(tm.i8_type()->kind, IRTypeKind::I8);
    EXPECT_EQ(tm.i16_type()->kind, IRTypeKind::I16);
    EXPECT_EQ(tm.i32_type()->kind, IRTypeKind::I32);
    EXPECT_EQ(tm.i64_type()->kind, IRTypeKind::I64);
    EXPECT_EQ(tm.f32_type()->kind, IRTypeKind::F32);
    EXPECT_EQ(tm.f64_type()->kind, IRTypeKind::F64);
    EXPECT_EQ(tm.ptr_type()->kind, IRTypeKind::Ptr);

    // Singletons should be the same pointer
    EXPECT_EQ(tm.i64_type(), tm.i64_type());
}

TEST(IRTypeMapTest, CompositeLayouts) {
    IRTypeMap tm;

    // String: {ptr, i64}
    auto* str = tm.string_type();
    EXPECT_EQ(str->kind, IRTypeKind::Struct);
    EXPECT_EQ(str->fields.size(), 2u);
    EXPECT_EQ(str->fields[0], tm.ptr_type());
    EXPECT_EQ(str->fields[1], tm.i64_type());

    // Slice: {ptr, i64, i64}
    auto* sl = tm.slice_type();
    EXPECT_EQ(sl->kind, IRTypeKind::Struct);
    EXPECT_EQ(sl->fields.size(), 3u);

    // Interface: {ptr, ptr}
    auto* iface = tm.interface_type();
    EXPECT_EQ(iface->kind, IRTypeKind::Struct);
    EXPECT_EQ(iface->fields.size(), 2u);
}

TEST(IRTypeMapTest, MapBasicTypes) {
    IRTypeMap tm;

    sema::Type bool_type;
    bool_type.kind = sema::TypeKind::Basic;
    bool_type.basic = sema::BasicKind::Bool;
    EXPECT_EQ(tm.map_type(&bool_type), tm.i1_type());

    sema::Type int_type;
    int_type.kind = sema::TypeKind::Basic;
    int_type.basic = sema::BasicKind::Int;
    EXPECT_EQ(tm.map_type(&int_type), tm.i64_type());

    sema::Type f32_type;
    f32_type.kind = sema::TypeKind::Basic;
    f32_type.basic = sema::BasicKind::Float32;
    EXPECT_EQ(tm.map_type(&f32_type), tm.f32_type());

    sema::Type str_type;
    str_type.kind = sema::TypeKind::Basic;
    str_type.basic = sema::BasicKind::String;
    EXPECT_EQ(tm.map_type(&str_type), tm.string_type());
}

TEST(IRTypeMapTest, MapPointerType) {
    IRTypeMap tm;

    sema::Type int_type;
    int_type.kind = sema::TypeKind::Basic;
    int_type.basic = sema::BasicKind::Int;

    sema::Type ptr_type;
    ptr_type.kind = sema::TypeKind::Pointer;
    ptr_type.pointer.base = &int_type;
    EXPECT_EQ(tm.map_type(&ptr_type), tm.ptr_type());
}

TEST(IRTypeMapTest, MapSliceType) {
    IRTypeMap tm;

    sema::Type int_type;
    int_type.kind = sema::TypeKind::Basic;
    int_type.basic = sema::BasicKind::Int;

    sema::Type slice_type;
    slice_type.kind = sema::TypeKind::Slice;
    slice_type.slice.element = &int_type;
    EXPECT_EQ(tm.map_type(&slice_type), tm.slice_type());
}

TEST(IRTypeMapTest, MapChanType) {
    IRTypeMap tm;

    sema::Type int_type;
    int_type.kind = sema::TypeKind::Basic;
    int_type.basic = sema::BasicKind::Int;

    sema::Type chan_type;
    chan_type.kind = sema::TypeKind::Chan;
    chan_type.chan.element = &int_type;
    chan_type.chan.dir = sema::ChanDir::SendRecv;
    EXPECT_EQ(tm.map_type(&chan_type), tm.ptr_type());
}

TEST(IRTypeMapTest, MapMapType) {
    IRTypeMap tm;

    sema::Type str_type;
    str_type.kind = sema::TypeKind::Basic;
    str_type.basic = sema::BasicKind::String;

    sema::Type int_type;
    int_type.kind = sema::TypeKind::Basic;
    int_type.basic = sema::BasicKind::Int;

    sema::Type map_type;
    map_type.kind = sema::TypeKind::Map;
    map_type.map.key = &str_type;
    map_type.map.value = &int_type;
    EXPECT_EQ(tm.map_type(&map_type), tm.ptr_type());
}

TEST(IRTypeMapTest, MapArrayType) {
    IRTypeMap tm;

    sema::Type int_type;
    int_type.kind = sema::TypeKind::Basic;
    int_type.basic = sema::BasicKind::Int;

    sema::Type arr_type;
    arr_type.kind = sema::TypeKind::Array;
    arr_type.array.element = &int_type;
    arr_type.array.length = 5;

    auto* result = tm.map_type(&arr_type);
    EXPECT_EQ(result->kind, IRTypeKind::Array);
    EXPECT_EQ(result->element, tm.i64_type());
    EXPECT_EQ(result->count, 5);
}

TEST(IRTypeMapTest, MapInterfaceType) {
    IRTypeMap tm;

    sema::InterfaceType iface_data;
    sema::Type iface_type;
    iface_type.kind = sema::TypeKind::Interface;
    iface_type.interface_ = &iface_data;
    EXPECT_EQ(tm.map_type(&iface_type), tm.interface_type());
}

TEST(IRTypeMapTest, NullType) {
    IRTypeMap tm;
    EXPECT_EQ(tm.map_type(nullptr), tm.void_type());
}

TEST(IRTypeMapTest, CachingWorks) {
    IRTypeMap tm;

    sema::Type int_type;
    int_type.kind = sema::TypeKind::Basic;
    int_type.basic = sema::BasicKind::Int;

    auto* first = tm.map_type(&int_type);
    auto* second = tm.map_type(&int_type);
    EXPECT_EQ(first, second);
}

// ============================================================================
// IRBuilder tests
// ============================================================================

TEST(IRBuilderTest, Constants) {
    IRTypeMap tm;
    IRBuilder builder(tm);

    Function func(0, nullptr, "test");
    auto* bb = func.create_block("entry");
    builder.set_insert_block(bb);

    auto* ci = builder.create_const_int(tm.i64_type(), 42);
    EXPECT_EQ(ci->opcode, Opcode::ConstInt);
    EXPECT_EQ(ci->imm_int, 42);
    EXPECT_EQ(ci->type, tm.i64_type());

    auto* cf = builder.create_const_float(tm.f64_type(), 3.14);
    EXPECT_EQ(cf->opcode, Opcode::ConstFloat);
    EXPECT_DOUBLE_EQ(cf->imm_float, 3.14);

    auto* cb = builder.create_const_bool(true);
    EXPECT_EQ(cb->opcode, Opcode::ConstBool);
    EXPECT_EQ(cb->imm_int, 1);

    auto* cs = builder.create_const_string("hello");
    EXPECT_EQ(cs->opcode, Opcode::ConstString);
    EXPECT_EQ(cs->imm_string, "hello");

    auto* cn = builder.create_const_nil(tm.ptr_type());
    EXPECT_EQ(cn->opcode, Opcode::ConstNil);
}

TEST(IRBuilderTest, Arithmetic) {
    IRTypeMap tm;
    IRBuilder builder(tm);

    Function func(0, nullptr, "test");
    auto* bb = func.create_block("entry");
    builder.set_insert_block(bb);

    auto* a = builder.create_const_int(tm.i64_type(), 10);
    auto* b = builder.create_const_int(tm.i64_type(), 20);

    auto* add = builder.create_add(a, b);
    EXPECT_EQ(add->opcode, Opcode::Add);
    EXPECT_EQ(add->operands.size(), 2u);
    EXPECT_EQ(add->operands[0], a);
    EXPECT_EQ(add->operands[1], b);
    EXPECT_EQ(add->type, tm.i64_type());

    auto* sub = builder.create_sub(a, b);
    EXPECT_EQ(sub->opcode, Opcode::Sub);

    auto* mul = builder.create_mul(a, b);
    EXPECT_EQ(mul->opcode, Opcode::Mul);

    auto* div = builder.create_div(a, b);
    EXPECT_EQ(div->opcode, Opcode::Div);

    auto* rem = builder.create_rem(a, b);
    EXPECT_EQ(rem->opcode, Opcode::Rem);
}

TEST(IRBuilderTest, FloatArithmetic) {
    IRTypeMap tm;
    IRBuilder builder(tm);

    Function func(0, nullptr, "test");
    auto* bb = func.create_block("entry");
    builder.set_insert_block(bb);

    auto* a = builder.create_const_float(tm.f64_type(), 1.0);
    auto* b = builder.create_const_float(tm.f64_type(), 2.0);

    EXPECT_EQ(builder.create_fadd(a, b)->opcode, Opcode::FAdd);
    EXPECT_EQ(builder.create_fsub(a, b)->opcode, Opcode::FSub);
    EXPECT_EQ(builder.create_fmul(a, b)->opcode, Opcode::FMul);
    EXPECT_EQ(builder.create_fdiv(a, b)->opcode, Opcode::FDiv);
}

TEST(IRBuilderTest, Comparisons) {
    IRTypeMap tm;
    IRBuilder builder(tm);

    Function func(0, nullptr, "test");
    auto* bb = func.create_block("entry");
    builder.set_insert_block(bb);

    auto* a = builder.create_const_int(tm.i64_type(), 1);
    auto* b = builder.create_const_int(tm.i64_type(), 2);

    auto* eq = builder.create_eq(a, b);
    EXPECT_EQ(eq->opcode, Opcode::Eq);
    EXPECT_EQ(eq->type, tm.i1_type());

    EXPECT_EQ(builder.create_ne(a, b)->type, tm.i1_type());
    EXPECT_EQ(builder.create_lt(a, b)->type, tm.i1_type());
    EXPECT_EQ(builder.create_le(a, b)->type, tm.i1_type());
    EXPECT_EQ(builder.create_gt(a, b)->type, tm.i1_type());
    EXPECT_EQ(builder.create_ge(a, b)->type, tm.i1_type());
}

TEST(IRBuilderTest, MemoryOps) {
    IRTypeMap tm;
    IRBuilder builder(tm);

    Function func(0, nullptr, "test");
    auto* bb = func.create_block("entry");
    builder.set_insert_block(bb);

    auto* alloca = builder.create_alloca(tm.i64_type(), "x.addr");
    EXPECT_EQ(alloca->opcode, Opcode::Alloca);
    EXPECT_EQ(alloca->type, tm.ptr_type());
    EXPECT_EQ(alloca->name, "x.addr");

    auto* val = builder.create_const_int(tm.i64_type(), 42);
    auto* store = builder.create_store(val, alloca);
    EXPECT_EQ(store->opcode, Opcode::Store);
    EXPECT_EQ(store->operands[0], val);
    EXPECT_EQ(store->operands[1], alloca);

    auto* load = builder.create_load(alloca, tm.i64_type(), "x");
    EXPECT_EQ(load->opcode, Opcode::Load);
    EXPECT_EQ(load->type, tm.i64_type());
}

TEST(IRBuilderTest, ControlFlow) {
    IRTypeMap tm;
    IRBuilder builder(tm);

    Function func(0, nullptr, "test");
    auto* entry = func.create_block("entry");
    auto* then_bb = func.create_block("then");
    auto* else_bb = func.create_block("else");
    auto* merge_bb = func.create_block("merge");

    builder.set_insert_block(entry);
    auto* cond = builder.create_const_bool(true);
    auto* br = builder.create_condbr(cond, then_bb, else_bb);
    EXPECT_EQ(br->opcode, Opcode::CondBr);
    EXPECT_EQ(br->targets.size(), 2u);
    EXPECT_EQ(br->targets[0], then_bb);
    EXPECT_EQ(br->targets[1], else_bb);

    // Check CFG edges
    EXPECT_EQ(entry->successors.size(), 2u);
    EXPECT_EQ(then_bb->predecessors.size(), 1u);
    EXPECT_EQ(else_bb->predecessors.size(), 1u);

    builder.set_insert_block(then_bb);
    builder.create_br(merge_bb);
    EXPECT_TRUE(then_bb->has_terminator());

    builder.set_insert_block(else_bb);
    builder.create_br(merge_bb);

    builder.set_insert_block(merge_bb);
    auto* ret = builder.create_ret();
    EXPECT_EQ(ret->opcode, Opcode::Ret);
    EXPECT_TRUE(ret->operands.empty());
}

TEST(IRBuilderTest, ReturnValue) {
    IRTypeMap tm;
    IRBuilder builder(tm);

    Function func(0, nullptr, "test");
    auto* bb = func.create_block("entry");
    builder.set_insert_block(bb);

    auto* val = builder.create_const_int(tm.i64_type(), 42);
    auto* ret = builder.create_ret(val);
    EXPECT_EQ(ret->opcode, Opcode::Ret);
    EXPECT_EQ(ret->operands.size(), 1u);
    EXPECT_EQ(ret->operands[0], val);
}

TEST(IRBuilderTest, CallInstruction) {
    IRTypeMap tm;
    IRBuilder builder(tm);

    Function callee(0, nullptr, "callee");
    Function caller(1, nullptr, "caller");
    auto* bb = caller.create_block("entry");
    builder.set_insert_block(bb);

    auto* arg = builder.create_const_int(tm.i64_type(), 42);
    auto* call = builder.create_call(&callee, {arg}, tm.i64_type(), "result");
    EXPECT_EQ(call->opcode, Opcode::Call);
    EXPECT_EQ(call->operands.size(), 2u);
    EXPECT_EQ(call->operands[0], &callee);
    EXPECT_EQ(call->operands[1], arg);
    EXPECT_EQ(call->type, tm.i64_type());
}

TEST(IRBuilderTest, GoSpecific) {
    IRTypeMap tm;
    IRBuilder builder(tm);

    Function func(0, nullptr, "test");
    auto* bb = func.create_block("entry");
    builder.set_insert_block(bb);

    auto* ch = builder.create_chan_make(tm.ptr_type(), 8, "ch");
    EXPECT_EQ(ch->opcode, Opcode::ChanMake);

    auto* val = builder.create_const_int(tm.i64_type(), 42);
    auto* send = builder.create_chan_send(ch, val);
    EXPECT_EQ(send->opcode, Opcode::ChanSend);

    auto* recv = builder.create_chan_recv(ch, tm.i64_type(), "recv");
    EXPECT_EQ(recv->opcode, Opcode::ChanRecv);

    auto* println = builder.create_println({val});
    EXPECT_EQ(println->opcode, Opcode::Println);
}

// ============================================================================
// IRPrinter tests
// ============================================================================

TEST(IRPrinterTest, PrintModule) {
    Module mod("test");
    IRPrinter printer;
    auto output = printer.print(mod);
    EXPECT_NE(output.find("module \"test\""), std::string::npos);
}

TEST(IRPrinterTest, PrintFunction) {
    IRTypeMap tm;
    IRBuilder builder(tm);

    Function func(0, nullptr, "main");
    func.return_type = tm.void_type();
    auto* bb = func.create_block("entry");
    builder.set_insert_block(bb);
    builder.create_ret();

    IRPrinter printer;
    auto output = printer.print_function(func);
    EXPECT_NE(output.find("func @main()"), std::string::npos);
    EXPECT_NE(output.find("entry:"), std::string::npos);
    EXPECT_NE(output.find("ret void"), std::string::npos);
}

TEST(IRPrinterTest, PrintInstructions) {
    IRTypeMap tm;
    IRBuilder builder(tm);

    Function func(0, nullptr, "test");
    auto* bb = func.create_block("entry");
    builder.set_insert_block(bb);

    auto* val = builder.create_const_int(tm.i64_type(), 42, "x");
    IRPrinter printer;
    auto output = printer.print_instruction(*val);
    EXPECT_NE(output.find("const_int 42"), std::string::npos);
    EXPECT_NE(output.find("i64"), std::string::npos);
}
