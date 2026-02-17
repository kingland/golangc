#include "sema/types.hpp"
#include "sema/universe.hpp"
#include "common/arena_allocator.hpp"

#include <gtest/gtest.h>

using namespace golangc;
using namespace golangc::sema;

// ============================================================================
// Basic type construction and formatting
// ============================================================================

TEST(TypesTest, BasicTypeString) {
    EXPECT_EQ(type_string(basic_type(BasicKind::Bool)), "bool");
    EXPECT_EQ(type_string(basic_type(BasicKind::Int)), "int");
    EXPECT_EQ(type_string(basic_type(BasicKind::Int8)), "int8");
    EXPECT_EQ(type_string(basic_type(BasicKind::Int16)), "int16");
    EXPECT_EQ(type_string(basic_type(BasicKind::Int32)), "int32");
    EXPECT_EQ(type_string(basic_type(BasicKind::Int64)), "int64");
    EXPECT_EQ(type_string(basic_type(BasicKind::Uint)), "uint");
    EXPECT_EQ(type_string(basic_type(BasicKind::Uint8)), "uint8");
    EXPECT_EQ(type_string(basic_type(BasicKind::Float32)), "float32");
    EXPECT_EQ(type_string(basic_type(BasicKind::Float64)), "float64");
    EXPECT_EQ(type_string(basic_type(BasicKind::String)), "string");
    EXPECT_EQ(type_string(basic_type(BasicKind::Complex128)), "complex128");
}

TEST(TypesTest, UntypedTypeString) {
    EXPECT_EQ(type_string(basic_type(BasicKind::UntypedBool)), "untyped bool");
    EXPECT_EQ(type_string(basic_type(BasicKind::UntypedInt)), "untyped int");
    EXPECT_EQ(type_string(basic_type(BasicKind::UntypedFloat)), "untyped float");
    EXPECT_EQ(type_string(basic_type(BasicKind::UntypedString)), "untyped string");
    EXPECT_EQ(type_string(basic_type(BasicKind::UntypedNil)), "untyped nil");
}

TEST(TypesTest, NullTypeString) {
    EXPECT_EQ(type_string(nullptr), "<nil>");
}

TEST(TypesTest, PointerTypeString) {
    ArenaAllocator arena;
    auto* t = arena.create<Type>();
    t->kind = TypeKind::Pointer;
    t->pointer.base = basic_type(BasicKind::Int);
    EXPECT_EQ(type_string(t), "*int");
}

TEST(TypesTest, SliceTypeString) {
    ArenaAllocator arena;
    auto* t = arena.create<Type>();
    t->kind = TypeKind::Slice;
    t->slice.element = basic_type(BasicKind::String);
    EXPECT_EQ(type_string(t), "[]string");
}

TEST(TypesTest, ArrayTypeString) {
    ArenaAllocator arena;
    auto* t = arena.create<Type>();
    t->kind = TypeKind::Array;
    t->array.element = basic_type(BasicKind::Int);
    t->array.length = 10;
    EXPECT_EQ(type_string(t), "[10]int");
}

TEST(TypesTest, MapTypeString) {
    ArenaAllocator arena;
    auto* t = arena.create<Type>();
    t->kind = TypeKind::Map;
    t->map.key = basic_type(BasicKind::String);
    t->map.value = basic_type(BasicKind::Int);
    EXPECT_EQ(type_string(t), "map[string]int");
}

TEST(TypesTest, ChanTypeString) {
    ArenaAllocator arena;

    auto* t1 = arena.create<Type>();
    t1->kind = TypeKind::Chan;
    t1->chan.element = basic_type(BasicKind::Int);
    t1->chan.dir = ChanDir::SendRecv;
    EXPECT_EQ(type_string(t1), "chan int");

    auto* t2 = arena.create<Type>();
    t2->kind = TypeKind::Chan;
    t2->chan.element = basic_type(BasicKind::Int);
    t2->chan.dir = ChanDir::SendOnly;
    EXPECT_EQ(type_string(t2), "chan<- int");

    auto* t3 = arena.create<Type>();
    t3->kind = TypeKind::Chan;
    t3->chan.element = basic_type(BasicKind::Int);
    t3->chan.dir = ChanDir::RecvOnly;
    EXPECT_EQ(type_string(t3), "<-chan int");
}

TEST(TypesTest, FuncTypeString) {
    ArenaAllocator arena;
    auto* ft = arena.create<FuncType>();
    ft->params.push_back(FuncParam{"x", basic_type(BasicKind::Int)});
    ft->results.push_back(FuncParam{"", basic_type(BasicKind::Bool)});

    auto* t = arena.create<Type>();
    t->kind = TypeKind::Func;
    t->func = ft;
    EXPECT_EQ(type_string(t), "func(int) bool");
}

TEST(TypesTest, FuncMultipleResultsString) {
    ArenaAllocator arena;
    auto* ft = arena.create<FuncType>();
    ft->params.push_back(FuncParam{"", basic_type(BasicKind::String)});
    ft->results.push_back(FuncParam{"", basic_type(BasicKind::Int)});
    ft->results.push_back(FuncParam{"", basic_type(BasicKind::Bool)});

    auto* t = arena.create<Type>();
    t->kind = TypeKind::Func;
    t->func = ft;
    EXPECT_EQ(type_string(t), "func(string) (int, bool)");
}

TEST(TypesTest, EmptyFuncTypeString) {
    ArenaAllocator arena;
    auto* ft = arena.create<FuncType>();
    auto* t = arena.create<Type>();
    t->kind = TypeKind::Func;
    t->func = ft;
    EXPECT_EQ(type_string(t), "func()");
}

TEST(TypesTest, StructTypeString) {
    ArenaAllocator arena;
    auto* st = arena.create<StructType>();
    st->fields.push_back(StructField{"X", basic_type(BasicKind::Int)});
    st->fields.push_back(StructField{"Y", basic_type(BasicKind::Int)});

    auto* t = arena.create<Type>();
    t->kind = TypeKind::Struct;
    t->struct_ = st;
    EXPECT_EQ(type_string(t), "struct{X int; Y int}");
}

TEST(TypesTest, EmptyStructTypeString) {
    ArenaAllocator arena;
    auto* st = arena.create<StructType>();
    auto* t = arena.create<Type>();
    t->kind = TypeKind::Struct;
    t->struct_ = st;
    EXPECT_EQ(type_string(t), "struct{}");
}

TEST(TypesTest, NamedTypeString) {
    ArenaAllocator arena;
    auto* nt = arena.create<NamedType>();
    nt->name = "Point";
    nt->underlying = basic_type(BasicKind::Int);

    auto* t = arena.create<Type>();
    t->kind = TypeKind::Named;
    t->named = nt;
    EXPECT_EQ(type_string(t), "Point");
}

// ============================================================================
// Type identity
// ============================================================================

TEST(TypesTest, BasicTypesIdentical) {
    EXPECT_TRUE(identical(basic_type(BasicKind::Int), basic_type(BasicKind::Int)));
    EXPECT_FALSE(identical(basic_type(BasicKind::Int), basic_type(BasicKind::Int32)));
    EXPECT_FALSE(identical(basic_type(BasicKind::String), basic_type(BasicKind::Bool)));
}

TEST(TypesTest, SamePointerIdentical) {
    auto* t = basic_type(BasicKind::Int);
    EXPECT_TRUE(identical(t, t));
}

TEST(TypesTest, NullIdentical) {
    EXPECT_FALSE(identical(nullptr, basic_type(BasicKind::Int)));
    EXPECT_FALSE(identical(basic_type(BasicKind::Int), nullptr));
    EXPECT_TRUE(identical(nullptr, nullptr));
}

TEST(TypesTest, StructuralPointerIdentity) {
    ArenaAllocator arena;
    auto* t1 = arena.create<Type>();
    t1->kind = TypeKind::Pointer;
    t1->pointer.base = basic_type(BasicKind::Int);

    auto* t2 = arena.create<Type>();
    t2->kind = TypeKind::Pointer;
    t2->pointer.base = basic_type(BasicKind::Int);

    EXPECT_TRUE(identical(t1, t2));
}

TEST(TypesTest, DifferentPointerNotIdentical) {
    ArenaAllocator arena;
    auto* t1 = arena.create<Type>();
    t1->kind = TypeKind::Pointer;
    t1->pointer.base = basic_type(BasicKind::Int);

    auto* t2 = arena.create<Type>();
    t2->kind = TypeKind::Pointer;
    t2->pointer.base = basic_type(BasicKind::String);

    EXPECT_FALSE(identical(t1, t2));
}

TEST(TypesTest, SliceIdentity) {
    ArenaAllocator arena;
    auto* t1 = arena.create<Type>();
    t1->kind = TypeKind::Slice;
    t1->slice.element = basic_type(BasicKind::Int);

    auto* t2 = arena.create<Type>();
    t2->kind = TypeKind::Slice;
    t2->slice.element = basic_type(BasicKind::Int);

    EXPECT_TRUE(identical(t1, t2));
}

TEST(TypesTest, ArrayIdentity) {
    ArenaAllocator arena;
    auto* t1 = arena.create<Type>();
    t1->kind = TypeKind::Array;
    t1->array.element = basic_type(BasicKind::Int);
    t1->array.length = 5;

    auto* t2 = arena.create<Type>();
    t2->kind = TypeKind::Array;
    t2->array.element = basic_type(BasicKind::Int);
    t2->array.length = 5;

    auto* t3 = arena.create<Type>();
    t3->kind = TypeKind::Array;
    t3->array.element = basic_type(BasicKind::Int);
    t3->array.length = 10;

    EXPECT_TRUE(identical(t1, t2));
    EXPECT_FALSE(identical(t1, t3));
}

TEST(TypesTest, NamedTypeIdentityIsPointerBased) {
    ArenaAllocator arena;
    auto* nt1 = arena.create<NamedType>();
    nt1->name = "Foo";

    auto* t1 = arena.create<Type>();
    t1->kind = TypeKind::Named;
    t1->named = nt1;

    auto* t2 = arena.create<Type>();
    t2->kind = TypeKind::Named;
    t2->named = nt1; // Same named type

    auto* nt2 = arena.create<NamedType>();
    nt2->name = "Foo";
    auto* t3 = arena.create<Type>();
    t3->kind = TypeKind::Named;
    t3->named = nt2; // Different named type, same name

    EXPECT_TRUE(identical(t1, t2));
    EXPECT_FALSE(identical(t1, t3)); // Different named type objects
}

// ============================================================================
// Underlying
// ============================================================================

TEST(TypesTest, UnderlyingBasic) {
    auto* t = basic_type(BasicKind::Int);
    EXPECT_EQ(underlying(t), t);
}

TEST(TypesTest, UnderlyingNamed) {
    ArenaAllocator arena;
    auto* nt = arena.create<NamedType>();
    nt->name = "MyInt";
    nt->underlying = basic_type(BasicKind::Int);

    auto* t = arena.create<Type>();
    t->kind = TypeKind::Named;
    t->named = nt;

    EXPECT_EQ(underlying(t), basic_type(BasicKind::Int));
}

// ============================================================================
// Default type
// ============================================================================

TEST(TypesTest, DefaultTypeUntyped) {
    EXPECT_EQ(default_type(basic_type(BasicKind::UntypedBool))->basic, BasicKind::Bool);
    EXPECT_EQ(default_type(basic_type(BasicKind::UntypedInt))->basic, BasicKind::Int);
    EXPECT_EQ(default_type(basic_type(BasicKind::UntypedFloat))->basic, BasicKind::Float64);
    EXPECT_EQ(default_type(basic_type(BasicKind::UntypedString))->basic, BasicKind::String);
    EXPECT_EQ(default_type(basic_type(BasicKind::UntypedRune))->basic, BasicKind::Int32);
    EXPECT_EQ(default_type(basic_type(BasicKind::UntypedComplex))->basic, BasicKind::Complex128);
}

TEST(TypesTest, DefaultTypeTyped) {
    // Typed types should return themselves
    EXPECT_EQ(default_type(basic_type(BasicKind::Int))->basic, BasicKind::Int);
    EXPECT_EQ(default_type(basic_type(BasicKind::String))->basic, BasicKind::String);
}

// ============================================================================
// Type predicates
// ============================================================================

TEST(TypesTest, IsUntyped) {
    EXPECT_TRUE(is_untyped(basic_type(BasicKind::UntypedBool)));
    EXPECT_TRUE(is_untyped(basic_type(BasicKind::UntypedInt)));
    EXPECT_TRUE(is_untyped(basic_type(BasicKind::UntypedNil)));
    EXPECT_FALSE(is_untyped(basic_type(BasicKind::Int)));
    EXPECT_FALSE(is_untyped(basic_type(BasicKind::String)));
    EXPECT_FALSE(is_untyped(nullptr));
}

TEST(TypesTest, IsNumeric) {
    EXPECT_TRUE(is_numeric(basic_type(BasicKind::Int)));
    EXPECT_TRUE(is_numeric(basic_type(BasicKind::Float64)));
    EXPECT_TRUE(is_numeric(basic_type(BasicKind::UntypedInt)));
    EXPECT_FALSE(is_numeric(basic_type(BasicKind::Bool)));
    EXPECT_FALSE(is_numeric(basic_type(BasicKind::String)));
}

TEST(TypesTest, IsInteger) {
    EXPECT_TRUE(is_integer(basic_type(BasicKind::Int)));
    EXPECT_TRUE(is_integer(basic_type(BasicKind::Int64)));
    EXPECT_TRUE(is_integer(basic_type(BasicKind::Uint8)));
    EXPECT_TRUE(is_integer(basic_type(BasicKind::UntypedInt)));
    EXPECT_FALSE(is_integer(basic_type(BasicKind::Float64)));
    EXPECT_FALSE(is_integer(basic_type(BasicKind::String)));
}

TEST(TypesTest, IsBoolean) {
    EXPECT_TRUE(is_boolean(basic_type(BasicKind::Bool)));
    EXPECT_TRUE(is_boolean(basic_type(BasicKind::UntypedBool)));
    EXPECT_FALSE(is_boolean(basic_type(BasicKind::Int)));
}

TEST(TypesTest, IsString) {
    EXPECT_TRUE(is_string(basic_type(BasicKind::String)));
    EXPECT_TRUE(is_string(basic_type(BasicKind::UntypedString)));
    EXPECT_FALSE(is_string(basic_type(BasicKind::Int)));
}

TEST(TypesTest, IsOrdered) {
    EXPECT_TRUE(is_ordered(basic_type(BasicKind::Int)));
    EXPECT_TRUE(is_ordered(basic_type(BasicKind::Float64)));
    EXPECT_TRUE(is_ordered(basic_type(BasicKind::String)));
    EXPECT_FALSE(is_ordered(basic_type(BasicKind::Bool)));
}

TEST(TypesTest, IsComparable) {
    EXPECT_TRUE(is_comparable(basic_type(BasicKind::Int)));
    EXPECT_TRUE(is_comparable(basic_type(BasicKind::String)));
    EXPECT_TRUE(is_comparable(basic_type(BasicKind::Bool)));

    // Slices are not comparable
    ArenaAllocator arena;
    auto* slice = arena.create<Type>();
    slice->kind = TypeKind::Slice;
    slice->slice.element = basic_type(BasicKind::Int);
    EXPECT_FALSE(is_comparable(slice));

    // Pointers are comparable
    auto* ptr = arena.create<Type>();
    ptr->kind = TypeKind::Pointer;
    ptr->pointer.base = basic_type(BasicKind::Int);
    EXPECT_TRUE(is_comparable(ptr));
}

// ============================================================================
// BasicInfo
// ============================================================================

TEST(TypesTest, BasicInfo) {
    const auto& info = basic_info(BasicKind::Int);
    EXPECT_EQ(info.name, "int");
    EXPECT_TRUE(info.is_numeric);
    EXPECT_TRUE(info.is_integer);
    EXPECT_FALSE(info.is_unsigned);
    EXPECT_FALSE(info.is_float);
    EXPECT_FALSE(info.is_untyped);

    const auto& finfo = basic_info(BasicKind::Float64);
    EXPECT_EQ(finfo.name, "float64");
    EXPECT_TRUE(finfo.is_numeric);
    EXPECT_FALSE(finfo.is_integer);
    EXPECT_TRUE(finfo.is_float);

    const auto& uinfo = basic_info(BasicKind::UntypedInt);
    EXPECT_TRUE(uinfo.is_untyped);
    EXPECT_TRUE(uinfo.is_numeric);
}
