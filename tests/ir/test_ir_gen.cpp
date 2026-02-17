#include "ir/ir_gen.hpp"
#include "ir/ir_printer.hpp"
#include "common/diagnostic.hpp"
#include "lexer/lexer.hpp"
#include "parser/parser.hpp"
#include "sema/checker.hpp"

#include <gtest/gtest.h>
#include <string>

using namespace golangc;

namespace {

/// Helper: parse + check + generate IR for Go source, return module + printed IR.
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
    if (!parser.parse()) {
        result.has_errors = true;
        return result;
    }

    sema::Checker checker(diag);
    if (!checker.check(parser.file())) {
        result.has_errors = true;
        return result;
    }

    ir::IRGenerator gen(checker);
    result.module = gen.generate(parser.file());

    ir::IRPrinter printer;
    result.ir_text = printer.print(*result.module);
    result.has_errors = had_error;
    return result;
}

} // namespace

// ============================================================================
// Basic IR generation tests
// ============================================================================

TEST(IRGenTest, EmptyMain) {
    auto result = generate_ir(R"(
package main
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_NE(result.module, nullptr);
    EXPECT_NE(result.ir_text.find("module \"main\""), std::string::npos);
    EXPECT_NE(result.ir_text.find("func @main()"), std::string::npos);
    EXPECT_NE(result.ir_text.find("ret void"), std::string::npos);
}

TEST(IRGenTest, HelloWorld) {
    auto result = generate_ir(R"(
package main
func main() {
    println("Hello, World!")
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_NE(result.ir_text.find("func @main()"), std::string::npos);
    EXPECT_NE(result.ir_text.find("println"), std::string::npos);
    EXPECT_NE(result.ir_text.find("Hello, World!"), std::string::npos);
}

TEST(IRGenTest, IntConstant) {
    auto result = generate_ir(R"(
package main
func main() {
    x := 42
    println(x)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_NE(result.ir_text.find("const_int 42"), std::string::npos);
    EXPECT_NE(result.ir_text.find("alloca"), std::string::npos);
    EXPECT_NE(result.ir_text.find("store"), std::string::npos);
    EXPECT_NE(result.ir_text.find("load"), std::string::npos);
}

TEST(IRGenTest, IntArithmetic) {
    auto result = generate_ir(R"(
package main
func add(a int, b int) int {
    return a + b
}
func main() {
    println(add(1, 2))
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_NE(result.ir_text.find("func @add("), std::string::npos);
    EXPECT_NE(result.ir_text.find("add "), std::string::npos);
    EXPECT_NE(result.ir_text.find("ret"), std::string::npos);
}

TEST(IRGenTest, Comparison) {
    auto result = generate_ir(R"(
package main
func check(n int) bool {
    return n > 0
}
func main() {
    println(check(5))
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_NE(result.ir_text.find("gt "), std::string::npos);
}

TEST(IRGenTest, IfStatement) {
    auto result = generate_ir(R"(
package main
func abs(n int) int {
    if n < 0 {
        return -n
    }
    return n
}
func main() {
    println(abs(-5))
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_NE(result.ir_text.find("condbr"), std::string::npos);
    EXPECT_NE(result.ir_text.find("if.then"), std::string::npos);
    EXPECT_NE(result.ir_text.find("if.merge"), std::string::npos);
}

TEST(IRGenTest, IfElse) {
    auto result = generate_ir(R"(
package main
func max(a int, b int) int {
    if a > b {
        return a
    } else {
        return b
    }
}
func main() {
    println(max(3, 5))
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_NE(result.ir_text.find("if.then"), std::string::npos);
    EXPECT_NE(result.ir_text.find("if.else"), std::string::npos);
}

TEST(IRGenTest, ForLoop) {
    auto result = generate_ir(R"(
package main
func sum(n int) int {
    s := 0
    for i := 0; i < n; i++ {
        s = s + i
    }
    return s
}
func main() {
    println(sum(10))
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_NE(result.ir_text.find("for.cond"), std::string::npos);
    EXPECT_NE(result.ir_text.find("for.body"), std::string::npos);
    EXPECT_NE(result.ir_text.find("for.post"), std::string::npos);
    EXPECT_NE(result.ir_text.find("for.done"), std::string::npos);
}

TEST(IRGenTest, InfiniteFor) {
    auto result = generate_ir(R"(
package main
func main() {
    for {
        break
    }
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_NE(result.ir_text.find("for.cond"), std::string::npos);
    // break should generate a br to for.done
    EXPECT_NE(result.ir_text.find("for.done"), std::string::npos);
}

TEST(IRGenTest, Fibonacci) {
    auto result = generate_ir(R"(
package main
func fibonacci(n int) int {
    if n <= 1 {
        return n
    }
    return fibonacci(n-1) + fibonacci(n-2)
}
func main() {
    println(fibonacci(10))
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_NE(result.ir_text.find("func @fibonacci("), std::string::npos);
    EXPECT_NE(result.ir_text.find("call @fibonacci("), std::string::npos);
    EXPECT_NE(result.ir_text.find("le "), std::string::npos);
    EXPECT_NE(result.ir_text.find("condbr"), std::string::npos);
}

TEST(IRGenTest, MultipleParams) {
    auto result = generate_ir(R"(
package main
func multiply(a int, b int) int {
    return a * b
}
func main() {
    println(multiply(3, 4))
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_NE(result.ir_text.find("func @multiply("), std::string::npos);
    EXPECT_NE(result.ir_text.find("mul "), std::string::npos);
}

TEST(IRGenTest, MultipleVarDecl) {
    auto result = generate_ir(R"(
package main
func main() {
    x := 1
    y := 2
    z := x + y
    println(z)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_NE(result.ir_text.find("x.addr"), std::string::npos);
    EXPECT_NE(result.ir_text.find("y.addr"), std::string::npos);
    EXPECT_NE(result.ir_text.find("z.addr"), std::string::npos);
}

TEST(IRGenTest, Assignment) {
    auto result = generate_ir(R"(
package main
func main() {
    x := 1
    x = 2
    println(x)
}
)");
    EXPECT_FALSE(result.has_errors);
    // Should have two stores to x.addr
    auto count = 0u;
    auto pos = result.ir_text.find("store");
    while (pos != std::string::npos) {
        ++count;
        pos = result.ir_text.find("store", pos + 1);
    }
    EXPECT_GE(count, 2u);
}

TEST(IRGenTest, IncDec) {
    auto result = generate_ir(R"(
package main
func main() {
    x := 0
    x++
    x--
    println(x)
}
)");
    EXPECT_FALSE(result.has_errors);
    // Should see add and sub for inc/dec
    EXPECT_NE(result.ir_text.find("add "), std::string::npos);
    EXPECT_NE(result.ir_text.find("sub "), std::string::npos);
}

TEST(IRGenTest, StringLiteral) {
    auto result = generate_ir(R"(
package main
func main() {
    s := "hello"
    println(s)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_NE(result.ir_text.find("const_string \"hello\""), std::string::npos);
}

TEST(IRGenTest, BoolExpressions) {
    auto result = generate_ir(R"(
package main
func main() {
    x := true
    y := !x
    println(y)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_NE(result.ir_text.find("const_bool true"), std::string::npos);
    EXPECT_NE(result.ir_text.find("lognot"), std::string::npos);
}

// ============================================================================
// Struct and method tests
// ============================================================================

TEST(IRGenTest, StructLiteral) {
    auto result = generate_ir(R"(
package main
type Point struct {
    X, Y int
}
func main() {
    p := Point{1, 2}
    println(p.X, p.Y)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_NE(result.ir_text.find("getptr"), std::string::npos);
}

TEST(IRGenTest, StructMethod) {
    auto result = generate_ir(R"(
package main
type Point struct {
    X, Y int
}
func (p Point) Add(other Point) Point {
    return Point{p.X + other.X, p.Y + other.Y}
}
func main() {
    p1 := Point{1, 2}
    p2 := Point{3, 4}
    p3 := p1.Add(p2)
    println(p3.X, p3.Y)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_NE(result.ir_text.find("func @Point.Add("), std::string::npos);
    EXPECT_NE(result.ir_text.find("call @Point.Add("), std::string::npos);
}

// ============================================================================
// Interface tests
// ============================================================================

TEST(IRGenTest, InterfaceCall) {
    auto result = generate_ir(R"(
package main
type Stringer interface {
    String() string
}
type MyInt int
func (m MyInt) String() string {
    return "MyInt"
}
func Print(s Stringer) {
    println(s.String())
}
func main() {
    var x MyInt = 42
    Print(x)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_NE(result.ir_text.find("func @MyInt.String("), std::string::npos);
    EXPECT_NE(result.ir_text.find("func @Print("), std::string::npos);
}

// ============================================================================
// Channel and goroutine tests
// ============================================================================

TEST(IRGenTest, GoroutineAndChannel) {
    auto result = generate_ir(R"(
package main
func worker(ch chan int) {
    ch <- 42
}
func main() {
    ch := make(chan int)
    go worker(ch)
    println(<-ch)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_NE(result.ir_text.find("chan_make"), std::string::npos);
    EXPECT_NE(result.ir_text.find("chan_send"), std::string::npos);
    EXPECT_NE(result.ir_text.find("chan_recv"), std::string::npos);
    EXPECT_NE(result.ir_text.find("go @worker("), std::string::npos);
}

// ============================================================================
// Sample programs (end-to-end smoke tests)
// ============================================================================

TEST(IRGenTest, SampleHello) {
    auto result = generate_ir(R"(
package main
func main() {
    println("Hello, World!")
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_NE(result.module, nullptr);
    EXPECT_GE(result.module->functions.size(), 1u);
}

TEST(IRGenTest, SampleFibonacci) {
    auto result = generate_ir(R"(
package main
func fibonacci(n int) int {
    if n <= 1 {
        return n
    }
    return fibonacci(n-1) + fibonacci(n-2)
}
func main() {
    println(fibonacci(10))
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_NE(result.module, nullptr);
    EXPECT_GE(result.module->functions.size(), 2u);

    auto* fib = result.module->find_function("fibonacci");
    EXPECT_NE(fib, nullptr);
    if (fib) {
        EXPECT_GE(fib->blocks.size(), 2u);
    }
}

TEST(IRGenTest, SampleStructs) {
    auto result = generate_ir(R"(
package main
type Point struct {
    X, Y int
}
func (p Point) Add(other Point) Point {
    return Point{p.X + other.X, p.Y + other.Y}
}
func main() {
    p1 := Point{1, 2}
    p2 := Point{3, 4}
    p3 := p1.Add(p2)
    println(p3.X, p3.Y)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_NE(result.module, nullptr);

    auto* add_method = result.module->find_function("Point.Add");
    EXPECT_NE(add_method, nullptr);
}

TEST(IRGenTest, SampleInterfaces) {
    auto result = generate_ir(R"(
package main
type Stringer interface {
    String() string
}
type MyInt int
func (m MyInt) String() string {
    return "MyInt"
}
func Print(s Stringer) {
    println(s.String())
}
func main() {
    var x MyInt = 42
    Print(x)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_NE(result.module, nullptr);
}

TEST(IRGenTest, SampleGoroutines) {
    auto result = generate_ir(R"(
package main
func worker(ch chan int) {
    ch <- 42
}
func main() {
    ch := make(chan int)
    go worker(ch)
    println(<-ch)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_NE(result.module, nullptr);
}

// ============================================================================
// Switch statement test
// ============================================================================

TEST(IRGenTest, SwitchStatement) {
    auto result = generate_ir(R"(
package main
func classify(n int) string {
    switch {
    case n < 0:
        return "negative"
    case n == 0:
        return "zero"
    default:
        return "positive"
    }
}
func main() {
    println(classify(-1))
    println(classify(0))
    println(classify(1))
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_NE(result.ir_text.find("switch.case"), std::string::npos);
}

// ============================================================================
// Break/continue test
// ============================================================================

TEST(IRGenTest, BreakContinue) {
    auto result = generate_ir(R"(
package main
func main() {
    for i := 0; i < 10; i++ {
        if i == 5 {
            break
        }
        if i == 3 {
            continue
        }
        println(i)
    }
}
)");
    EXPECT_FALSE(result.has_errors);
    // break generates br to for.done, continue generates br to for.post
    EXPECT_NE(result.ir_text.find("for.done"), std::string::npos);
    EXPECT_NE(result.ir_text.find("for.post"), std::string::npos);
}

// ============================================================================
// Var declaration
// ============================================================================

TEST(IRGenTest, VarDeclaration) {
    // var decl with initializer uses :=
    auto result = generate_ir(R"(
package main
func main() {
    var x int = 10
    println(x)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_NE(result.ir_text.find("alloca"), std::string::npos);
    EXPECT_NE(result.ir_text.find("println"), std::string::npos);
}

// ============================================================================
// Defer
// ============================================================================

TEST(IRGenTest, DeferStatement) {
    auto result = generate_ir(R"(
package main
func cleanup() {
    println("cleanup")
}
func main() {
    defer cleanup()
    println("main")
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_NE(result.ir_text.find("defer @cleanup()"), std::string::npos);
}

// ============================================================================
// Multiple return values (basic)
// ============================================================================

TEST(IRGenTest, ReturnVoid) {
    auto result = generate_ir(R"(
package main
func doNothing() {
    return
}
func main() {
    doNothing()
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_NE(result.ir_text.find("ret void"), std::string::npos);
}

// ============================================================================
// Compound assignment
// ============================================================================

TEST(IRGenTest, CompoundAssignment) {
    auto result = generate_ir(R"(
package main
func main() {
    x := 10
    x += 5
    x -= 3
    x *= 2
    println(x)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_NE(result.ir_text.find("add "), std::string::npos);
    EXPECT_NE(result.ir_text.find("sub "), std::string::npos);
    EXPECT_NE(result.ir_text.find("mul "), std::string::npos);
}

// ============================================================================
// Function with no body (forward ref)
// ============================================================================

TEST(IRGenTest, MultipleFunctions) {
    auto result = generate_ir(R"(
package main
func a() int { return 1 }
func b() int { return a() + 1 }
func main() {
    println(b())
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_NE(result.ir_text.find("func @a()"), std::string::npos);
    EXPECT_NE(result.ir_text.find("func @b()"), std::string::npos);
    EXPECT_NE(result.ir_text.find("call @a()"), std::string::npos);
}

// ============================================================================
// IR structure validation
// ============================================================================

TEST(IRGenTest, ModuleHasCorrectName) {
    auto result = generate_ir(R"(
package main
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_EQ(result.module->name, "main");
}

TEST(IRGenTest, FunctionHasEntryBlock) {
    auto result = generate_ir(R"(
package main
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    auto* main_func = result.module->find_function("main");
    EXPECT_NE(main_func, nullptr);
    if (main_func) {
        EXPECT_NE(main_func->entry(), nullptr);
        EXPECT_EQ(main_func->entry()->label, "entry");
    }
}

TEST(IRGenTest, AllBlocksTerminated) {
    auto result = generate_ir(R"(
package main
func fibonacci(n int) int {
    if n <= 1 {
        return n
    }
    return fibonacci(n-1) + fibonacci(n-2)
}
func main() {
    println(fibonacci(10))
}
)");
    EXPECT_FALSE(result.has_errors);
    for (const auto& func : result.module->functions) {
        for (const auto& block : func->blocks) {
            EXPECT_TRUE(block->has_terminator())
                << "Block '" << block->label << "' in function '"
                << func->name << "' has no terminator";
        }
    }
}

// ============================================================================
// Unary expressions
// ============================================================================

TEST(IRGenTest, UnaryMinus) {
    auto result = generate_ir(R"(
package main
func main() {
    x := -42
    println(x)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_NE(result.ir_text.find("neg "), std::string::npos);
}

// ============================================================================
// Len builtin
// ============================================================================

TEST(IRGenTest, LenArray) {
    auto result = generate_ir(R"(
package main
func main() {
    var a [5]int
    println(len(a))
}
)");
    // len of array is a constant
    EXPECT_FALSE(result.has_errors);
    EXPECT_NE(result.ir_text.find("const_int 5"), std::string::npos);
}
