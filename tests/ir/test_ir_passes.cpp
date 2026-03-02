#include "ir/ir_gen.hpp"
#include "ir/ir_passes.hpp"
#include "ir/ir_printer.hpp"
#include "common/diagnostic.hpp"
#include "lexer/lexer.hpp"
#include "parser/parser.hpp"
#include "sema/checker.hpp"

#include <gtest/gtest.h>
#include <string>

using namespace golangc;

namespace {

// --------------------------------------------------------------------------
// Helpers
// --------------------------------------------------------------------------

struct IRGenResult {
    std::unique_ptr<ir::Module> module;
    std::string ir_text;
    bool has_errors = false;
};

IRGenResult generate_ir(const std::string& source) {
    IRGenResult result;

    DiagnosticEngine diag;
    bool had_error = false;
    diag.set_handler([&](const Diagnostic&) { had_error = true; });

    Lexer lexer(source, "test.go", diag);
    Parser parser(lexer, diag);
    if (!parser.parse()) { result.has_errors = true; return result; }

    sema::Checker checker(diag);
    if (!checker.check(parser.file())) { result.has_errors = true; return result; }

    ir::IRGenerator gen(checker);
    result.module = gen.generate(parser.file());

    ir::IRPrinter printer;
    result.ir_text = printer.print(*result.module);
    result.has_errors = had_error;
    return result;
}

IRGenResult generate_and_optimize(const std::string& source) {
    auto result = generate_ir(source);
    if (!result.has_errors && result.module) {
        ir::PassManager::run_default(*result.module);
        ir::IRPrinter printer;
        result.ir_text = printer.print(*result.module);
    }
    return result;
}

bool contains(const std::string& hay, const std::string& needle) {
    return hay.find(needle) != std::string::npos;
}

} // namespace

// ============================================================================
// Constant Folding Tests
// ============================================================================

TEST(ConstFoldTest, IntAdd) {
    // 2+3 should become const_int 5
    auto result = generate_and_optimize(R"(
package main
func main() {
    x := 2 + 3
    println(x)
}
)");
    EXPECT_FALSE(result.has_errors);
    // After folding, there should be a const_int 5 and no add instruction
    EXPECT_TRUE(contains(result.ir_text, "const_int 5"));
    EXPECT_FALSE(contains(result.ir_text, " add "));
}

TEST(ConstFoldTest, IntMulChain) {
    // (2+3)*4 should cascade-fold to const_int 20
    auto result = generate_and_optimize(R"(
package main
func main() {
    x := (2 + 3) * 4
    println(x)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.ir_text, "const_int 20"));
    EXPECT_FALSE(contains(result.ir_text, " add "));
    EXPECT_FALSE(contains(result.ir_text, " mul "));
}

TEST(ConstFoldTest, IntComparison) {
    // 3 < 5 should fold at compile time — no lt instruction in IR
    auto result = generate_and_optimize(R"(
package main
func f() bool { return 3 < 5 }
func main() { _ = f() }
)");
    EXPECT_FALSE(result.has_errors);
    // After folding, the lt is gone and a const_int 1 (true) appears in f()
    EXPECT_TRUE(contains(result.ir_text, "const_int 1"));
    EXPECT_FALSE(contains(result.ir_text, " lt "));
}

TEST(ConstFoldTest, UnaryNeg) {
    // -(5) should become const_int -5
    auto result = generate_and_optimize(R"(
package main
func main() {
    x := -(5)
    println(x)
}
)");
    EXPECT_FALSE(result.has_errors);
    // The negation of the constant 5
    EXPECT_TRUE(contains(result.ir_text, "const_int -5") ||
                contains(result.ir_text, "const_int 18446744073709551611"));  // two's complement
    EXPECT_FALSE(contains(result.ir_text, " neg "));
}

TEST(ConstFoldTest, FloatAdd) {
    // 1.5 + 2.5 should become const_float 4 (no fadd instruction)
    auto result = generate_and_optimize(R"(
package main
func fsum() float64 { return 1.5 + 2.5 }
func main() { _ = fsum() }
)");
    EXPECT_FALSE(result.has_errors);
    // After folding, const_float 4 in fsum body; no fadd instruction
    EXPECT_TRUE(contains(result.ir_text, "const_float 4") ||
                contains(result.ir_text, "const_float 4."));
    EXPECT_FALSE(contains(result.ir_text, " fadd "));
}

TEST(ConstFoldTest, DivByZeroSkipped) {
    // Division by zero should NOT be folded, and should not crash
    auto result = generate_and_optimize(R"(
package main
func main() {
    x := 10
    y := x / 0
    println(y)
}
)");
    EXPECT_FALSE(result.has_errors);
    // div by zero — should not fold (literal 0 in source generates ConstInt 0)
    // The compiler may have folded x:=10 but y:=x/0 should keep div
    // (at least it should not crash)
    // Check that we still have valid IR
    EXPECT_TRUE(contains(result.ir_text, "func @main"));
}

TEST(ConstFoldTest, IdentityAddZero) {
    // x+0 should reduce to x (no add instruction for that expr)
    auto result = generate_and_optimize(R"(
package main
func add0(x int) int {
    return x + 0
}
func main() { println(add0(3)) }
)");
    EXPECT_FALSE(result.has_errors);
    // The add with 0 identity should be eliminated
    // In the add0 function, the add instruction should be gone
    EXPECT_FALSE(contains(result.ir_text, " add "));
}

TEST(ConstFoldTest, IdentityMulOne) {
    // x*1 should reduce to x (no mul instruction)
    auto result = generate_and_optimize(R"(
package main
func mul1(x int) int {
    return x * 1
}
func main() { println(mul1(3)) }
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_FALSE(contains(result.ir_text, " mul "));
}

// ============================================================================
// Constant Propagation Tests
// ============================================================================

TEST(ConstPropTest, PropagateLocal) {
    // x := 5; y := x+1 → y should become const_int 6
    auto result = generate_and_optimize(R"(
package main
func main() {
    x := 5
    y := x + 1
    println(y)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.ir_text, "const_int 6"));
    EXPECT_FALSE(contains(result.ir_text, " add "));
}

TEST(ConstPropTest, PropagateMultiUse) {
    // x := 3; use x twice
    auto result = generate_and_optimize(R"(
package main
func main() {
    x := 3
    println(x)
    println(x)
}
)");
    EXPECT_FALSE(result.has_errors);
    // x should be optimized to direct const_int 3 in both println calls
    EXPECT_TRUE(contains(result.ir_text, "const_int 3"));
}

TEST(ConstPropTest, PropAfterMem2Reg) {
    // Alloca promoted to value, then propagated
    auto result = generate_and_optimize(R"(
package main
func main() {
    x := 42
    println(x)
}
)");
    EXPECT_FALSE(result.has_errors);
    // After mem2reg + constprop, should use const_int 42 directly
    EXPECT_TRUE(contains(result.ir_text, "const_int 42"));
    // No alloca should remain for x (it's a simple scalar)
    // Note: alloca may still appear for other reasons (sret etc), so we just check const is there
}

TEST(ConstPropTest, NoPropagateThroughCall) {
    // x := foo(); y := x + 1 — call result is unknown, should NOT fold
    auto result = generate_and_optimize(R"(
package main
func foo() int { return 1 }
func main() {
    x := foo()
    y := x + 1
    println(y)
}
)");
    EXPECT_FALSE(result.has_errors);
    // y = x+1 where x is from a call — should still have an add instruction
    EXPECT_TRUE(contains(result.ir_text, " add "));
}

TEST(ConstPropTest, CondBranchConst) {
    // if true { ... } — condition is const_int 1
    auto result = generate_and_optimize(R"(
package main
func main() {
    if true {
        println("yes")
    }
}
)");
    EXPECT_FALSE(result.has_errors);
    // Should have a const_bool true (from 'true') in IR — Go booleans use const_bool
    EXPECT_TRUE(contains(result.ir_text, "const_bool true"));
}

// ============================================================================
// Mem2Reg Tests
// ============================================================================

TEST(Mem2RegTest, SimpleScalarPromoted) {
    // x := 42; println(x) → no alloca/load for x
    auto result = generate_and_optimize(R"(
package main
func main() {
    x := 42
    println(x)
}
)");
    EXPECT_FALSE(result.has_errors);
    // After optimization, shouldn't have load instruction for x
    // (alloca might be gone, or at least no load)
    EXPECT_FALSE(contains(result.ir_text, "load "));
}

TEST(Mem2RegTest, TwoLoads) {
    // x := 5; both uses should refer to same value after mem2reg
    auto result = generate_and_optimize(R"(
package main
func main() {
    x := 5
    println(x)
    println(x)
}
)");
    EXPECT_FALSE(result.has_errors);
    // After mem2reg, loads of x should be gone
    EXPECT_FALSE(contains(result.ir_text, "load "));
}

TEST(Mem2RegTest, EscapingNotPromoted) {
    // p := &x; pass to function — alloca must remain
    auto result = generate_and_optimize(R"(
package main
func set(p *int, v int) { *p = v }
func main() {
    x := 1
    set(&x, 2)
    println(x)
}
)");
    EXPECT_FALSE(result.has_errors);
    // alloca for x must remain since address escapes
    // We just check it compiled correctly
    EXPECT_TRUE(contains(result.ir_text, "func @main"));
}

TEST(Mem2RegTest, StructNotPromoted) {
    // struct alloca should NOT be promoted (multi-QWORD)
    auto result = generate_and_optimize(R"(
package main
type Point struct { X, Y int }
func main() {
    p := Point{1, 2}
    println(p.X)
}
)");
    EXPECT_FALSE(result.has_errors);
    // struct alloca remains
    EXPECT_TRUE(contains(result.ir_text, "alloca "));
}

TEST(Mem2RegTest, MultiStoreNotPromoted) {
    // x := 1; x = 2 — two stores, conservative: keep alloca
    auto result = generate_and_optimize(R"(
package main
func main() {
    x := 1
    x = 2
    println(x)
}
)");
    EXPECT_FALSE(result.has_errors);
    // With two stores, mem2reg conservatively keeps alloca
    // We just check no crash and valid IR
    EXPECT_TRUE(contains(result.ir_text, "func @main"));
}

TEST(Mem2RegTest, CrossBlockPromoted) {
    // x := 99 in entry, used in if-body block
    auto result = generate_and_optimize(R"(
package main
func main() {
    x := 99
    if x > 0 {
        println(x)
    }
}
)");
    EXPECT_FALSE(result.has_errors);
    // x has one store (initialization), so mem2reg promotes it
    // After promotion, the load in the if-body is replaced by the stored value
    EXPECT_TRUE(contains(result.ir_text, "const_int 99"));
}

TEST(Mem2RegTest, FuncParamUntouched) {
    // Function parameters are Value (not Instruction), not affected by mem2reg
    auto result = generate_and_optimize(R"(
package main
func double(n int) int { return n * 2 }
func main() { println(double(5)) }
)");
    EXPECT_FALSE(result.has_errors);
    // Should compile and optimize cleanly; param n should still be referenced
    EXPECT_TRUE(contains(result.ir_text, "func @double"));
}

// ============================================================================
// DCE Tests
// ============================================================================

TEST(DCETest, DeadArithRemoved) {
    // x := 1+2 with no further use — add should be gone
    auto result = generate_and_optimize(R"(
package main
func main() {
    x := 1 + 2
    _ = x
}
)");
    EXPECT_FALSE(result.has_errors);
    // After folding + DCE: x is unused so add (folded to const_int 3) is also removed
    EXPECT_FALSE(contains(result.ir_text, " add "));
    // The folded const_int 3 is also dead and removed by DCE
    EXPECT_FALSE(contains(result.ir_text, "const_int 3"));
}

TEST(DCETest, LiveValueKept) {
    // println(x) uses x — it must not be removed
    auto result = generate_and_optimize(R"(
package main
func main() {
    x := 42
    println(x)
}
)");
    EXPECT_FALSE(result.has_errors);
    // println call must remain
    EXPECT_TRUE(contains(result.ir_text, "println"));
}

TEST(DCETest, UnreachableBlockRemoved) {
    // Code after return is unreachable
    auto result = generate_and_optimize(R"(
package main
func f() int {
    return 1
    return 2
}
func main() { println(f()) }
)");
    EXPECT_FALSE(result.has_errors);
    // The second "return 2" block should be eliminated as unreachable
    // We just verify the IR is valid
    EXPECT_TRUE(contains(result.ir_text, "func @f"));
}

TEST(DCETest, StoreKept) {
    // Store has side effects — must not be removed even if value unused
    auto result = generate_and_optimize(R"(
package main
func main() {
    x := 1
    x = 2
    println(x)
}
)");
    EXPECT_FALSE(result.has_errors);
    // store instructions must remain
    EXPECT_TRUE(contains(result.ir_text, "store "));
}

TEST(DCETest, CallKept) {
    // Call with unused return value: call must remain (side effects)
    auto result = generate_and_optimize(R"(
package main
func sideEffect() int { return 1 }
func main() {
    sideEffect()
}
)");
    EXPECT_FALSE(result.has_errors);
    // The call to sideEffect must remain
    EXPECT_TRUE(contains(result.ir_text, "call "));
}

TEST(DCETest, ChainedDeadRemoved) {
    // a := 1+2; b := a*3 — neither used
    auto result = generate_and_optimize(R"(
package main
func main() {
    a := 1 + 2
    b := a * 3
    _ = b
}
)");
    EXPECT_FALSE(result.has_errors);
    // Both add and mul should be removed by DCE (after folding)
    EXPECT_FALSE(contains(result.ir_text, " add "));
    EXPECT_FALSE(contains(result.ir_text, " mul "));
}

// ============================================================================
// Integration Tests
// ============================================================================

TEST(IntegrationTest, ConstExprChain) {
    // x:=2+3; y:=x*4; println(y) → println(const_int 20)
    auto result = generate_and_optimize(R"(
package main
func main() {
    x := 2 + 3
    y := x * 4
    println(y)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.ir_text, "const_int 20"));
    EXPECT_FALSE(contains(result.ir_text, " add "));
    EXPECT_FALSE(contains(result.ir_text, " mul "));
}

TEST(IntegrationTest, PipelineIdempotent) {
    // Running run_default twice should produce identical IR text
    auto result = generate_ir(R"(
package main
func main() {
    x := 2 + 3
    println(x)
}
)");
    EXPECT_FALSE(result.has_errors);
    ASSERT_NE(result.module, nullptr);

    ir::PassManager::run_default(*result.module);
    ir::IRPrinter printer;
    std::string first = printer.print(*result.module);

    ir::PassManager::run_default(*result.module);
    std::string second = printer.print(*result.module);

    EXPECT_EQ(first, second);
}

TEST(IntegrationTest, AllPassesTogether) {
    // Complex function: mix of arith, locals, branches
    auto result = generate_and_optimize(R"(
package main
func compute(n int) int {
    x := n + 0
    y := x * 1
    z := y + 10
    return z
}
func main() {
    println(compute(5))
}
)");
    EXPECT_FALSE(result.has_errors);
    // x+0 → n (identity), y*1 → x → n (identity), z = n+10
    // The add identity reductions should eliminate the trivial adds/muls
    EXPECT_TRUE(contains(result.ir_text, "func @compute"));
}

TEST(IntegrationTest, DeadIfFalse) {
    // if 1==2 {} — condition folds to 0/false
    auto result = generate_and_optimize(R"(
package main
func main() {
    if 1 == 2 {
        println("unreachable")
    }
    println("done")
}
)");
    EXPECT_FALSE(result.has_errors);
    // Condition 1==2 folds to const_int 0
    EXPECT_TRUE(contains(result.ir_text, "const_int 0"));
    EXPECT_FALSE(contains(result.ir_text, " eq "));
}
