#include "sema/scope.hpp"
#include "sema/constant.hpp"
#include "sema/types.hpp"
#include "sema/universe.hpp"
#include "common/arena_allocator.hpp"

#include <gtest/gtest.h>

using namespace golangc;
using namespace golangc::sema;

// ============================================================================
// Basic scope operations
// ============================================================================

TEST(ScopeTest, CreateScope) {
    ArenaAllocator arena;
    auto* scope = arena.create<Scope>(ScopeKind::Package, nullptr);
    EXPECT_EQ(scope->kind(), ScopeKind::Package);
    EXPECT_EQ(scope->parent(), nullptr);
}

TEST(ScopeTest, InsertAndLookup) {
    ArenaAllocator arena;
    auto* scope = arena.create<Scope>(ScopeKind::Package, nullptr);

    auto* sym = arena.create<Symbol>();
    sym->kind = SymbolKind::Var;
    sym->name = "x";
    sym->type = basic_type(BasicKind::Int);

    auto* conflict = scope->insert(sym);
    EXPECT_EQ(conflict, nullptr); // No conflict

    auto* found = scope->lookup_local("x");
    EXPECT_NE(found, nullptr);
    EXPECT_EQ(found->name, "x");
    EXPECT_EQ(found->kind, SymbolKind::Var);
}

TEST(ScopeTest, LookupNotFound) {
    ArenaAllocator arena;
    auto* scope = arena.create<Scope>(ScopeKind::Package, nullptr);

    auto* found = scope->lookup_local("x");
    EXPECT_EQ(found, nullptr);

    auto* found2 = scope->lookup("x");
    EXPECT_EQ(found2, nullptr);
}

TEST(ScopeTest, DuplicateInsert) {
    ArenaAllocator arena;
    auto* scope = arena.create<Scope>(ScopeKind::Package, nullptr);

    auto* sym1 = arena.create<Symbol>();
    sym1->kind = SymbolKind::Var;
    sym1->name = "x";
    sym1->type = basic_type(BasicKind::Int);

    auto* sym2 = arena.create<Symbol>();
    sym2->kind = SymbolKind::Var;
    sym2->name = "x";
    sym2->type = basic_type(BasicKind::String);

    EXPECT_EQ(scope->insert(sym1), nullptr);   // Success
    EXPECT_EQ(scope->insert(sym2), sym1);       // Conflict, returns existing
}

TEST(ScopeTest, ParentChainLookup) {
    ArenaAllocator arena;
    auto* parent = arena.create<Scope>(ScopeKind::Package, nullptr);
    auto* child = arena.create<Scope>(ScopeKind::Block, parent);

    auto* sym = arena.create<Symbol>();
    sym->kind = SymbolKind::Var;
    sym->name = "x";
    sym->type = basic_type(BasicKind::Int);
    (void)parent->insert(sym);

    // Should not be found locally in child
    EXPECT_EQ(child->lookup_local("x"), nullptr);

    // Should be found through parent chain
    auto* found = child->lookup("x");
    EXPECT_NE(found, nullptr);
    EXPECT_EQ(found->name, "x");
}

TEST(ScopeTest, Shadowing) {
    ArenaAllocator arena;
    auto* parent = arena.create<Scope>(ScopeKind::Package, nullptr);
    auto* child = arena.create<Scope>(ScopeKind::Block, parent);

    auto* sym_parent = arena.create<Symbol>();
    sym_parent->kind = SymbolKind::Var;
    sym_parent->name = "x";
    sym_parent->type = basic_type(BasicKind::Int);
    (void)parent->insert(sym_parent);

    auto* sym_child = arena.create<Symbol>();
    sym_child->kind = SymbolKind::Var;
    sym_child->name = "x";
    sym_child->type = basic_type(BasicKind::String);
    (void)child->insert(sym_child);

    // Child should find its own 'x'
    auto* found = child->lookup("x");
    EXPECT_NE(found, nullptr);
    EXPECT_EQ(found->type->basic, BasicKind::String);

    // Parent should find its own 'x'
    auto* pfound = parent->lookup("x");
    EXPECT_NE(pfound, nullptr);
    EXPECT_EQ(pfound->type->basic, BasicKind::Int);
}

TEST(ScopeTest, ChildRegistration) {
    ArenaAllocator arena;
    auto* parent = arena.create<Scope>(ScopeKind::Package, nullptr);
    auto* child1 = arena.create<Scope>(ScopeKind::Block, parent);
    auto* child2 = arena.create<Scope>(ScopeKind::Block, parent);
    (void)child1;
    (void)child2;

    EXPECT_EQ(parent->children().size(), 2u);
}

TEST(ScopeTest, DeepNesting) {
    ArenaAllocator arena;
    auto* s1 = arena.create<Scope>(ScopeKind::Universe, nullptr);
    auto* s2 = arena.create<Scope>(ScopeKind::Package, s1);
    auto* s3 = arena.create<Scope>(ScopeKind::Function, s2);
    auto* s4 = arena.create<Scope>(ScopeKind::Block, s3);

    auto* sym = arena.create<Symbol>();
    sym->kind = SymbolKind::Type;
    sym->name = "int";
    sym->type = basic_type(BasicKind::Int);
    (void)s1->insert(sym);

    // Should be found from deeply nested scope
    auto* found = s4->lookup("int");
    EXPECT_NE(found, nullptr);
    EXPECT_EQ(found->name, "int");
}

// ============================================================================
// Universe scope
// ============================================================================

TEST(ScopeTest, UniverseScopeHasTypes) {
    ArenaAllocator arena;
    auto* universe = init_universe(arena);

    EXPECT_NE(universe->lookup("int"), nullptr);
    EXPECT_NE(universe->lookup("string"), nullptr);
    EXPECT_NE(universe->lookup("bool"), nullptr);
    EXPECT_NE(universe->lookup("float64"), nullptr);
    EXPECT_NE(universe->lookup("byte"), nullptr);
    EXPECT_NE(universe->lookup("rune"), nullptr);
}

TEST(ScopeTest, UniverseScopeHasConstants) {
    ArenaAllocator arena;
    auto* universe = init_universe(arena);

    auto* true_sym = universe->lookup("true");
    EXPECT_NE(true_sym, nullptr);
    EXPECT_EQ(true_sym->kind, SymbolKind::Const);
    EXPECT_NE(true_sym->const_val, nullptr);
    EXPECT_TRUE(true_sym->const_val->as_bool());

    auto* false_sym = universe->lookup("false");
    EXPECT_NE(false_sym, nullptr);
    EXPECT_EQ(false_sym->kind, SymbolKind::Const);
    EXPECT_FALSE(false_sym->const_val->as_bool());
}

TEST(ScopeTest, UniverseScopeHasNil) {
    ArenaAllocator arena;
    auto* universe = init_universe(arena);

    auto* nil_sym = universe->lookup("nil");
    EXPECT_NE(nil_sym, nullptr);
    EXPECT_EQ(nil_sym->kind, SymbolKind::Nil);
}

TEST(ScopeTest, UniverseScopeHasBuiltins) {
    ArenaAllocator arena;
    auto* universe = init_universe(arena);

    auto* println_sym = universe->lookup("println");
    EXPECT_NE(println_sym, nullptr);
    EXPECT_EQ(println_sym->kind, SymbolKind::Builtin);

    EXPECT_NE(universe->lookup("len"), nullptr);
    EXPECT_NE(universe->lookup("cap"), nullptr);
    EXPECT_NE(universe->lookup("make"), nullptr);
    EXPECT_NE(universe->lookup("new"), nullptr);
    EXPECT_NE(universe->lookup("append"), nullptr);
    EXPECT_NE(universe->lookup("panic"), nullptr);
    EXPECT_NE(universe->lookup("close"), nullptr);
}

TEST(ScopeTest, UniverseScopeHasError) {
    ArenaAllocator arena;
    auto* universe = init_universe(arena);

    auto* error_sym = universe->lookup("error");
    EXPECT_NE(error_sym, nullptr);
    EXPECT_EQ(error_sym->kind, SymbolKind::Type);
}

TEST(ScopeTest, SymbolUsedFlag) {
    ArenaAllocator arena;
    auto* sym = arena.create<Symbol>();
    sym->kind = SymbolKind::Var;
    sym->name = "x";
    sym->used = false;

    EXPECT_FALSE(sym->used);
    sym->used = true;
    EXPECT_TRUE(sym->used);
}
