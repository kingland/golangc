#include "sema/checker.hpp"
#include "sema/constant.hpp"
#include "sema/types.hpp"
#include "sema/universe.hpp"
#include "common/diagnostic.hpp"
#include "lexer/lexer.hpp"
#include "parser/parser.hpp"

#include <gtest/gtest.h>
#include <string>
#include <string_view>

using namespace golangc;
using namespace golangc::sema;

// ============================================================================
// Helper: parse and check a Go source string
// ============================================================================

namespace {

struct CheckResult {
    bool success = false;
    uint32_t error_count = 0;
    std::vector<std::string> errors;
    DiagnosticEngine diag;
};

CheckResult check_source(std::string_view source) {
    CheckResult result;

    // Capture errors
    result.diag.set_handler([&](const Diagnostic& d) {
        if (d.severity == DiagnosticSeverity::Error) {
            result.errors.push_back(d.message);
        }
    });

    // We need the source to outlive the lexer
    std::string src(source);
    Lexer lexer(src, "test.go", result.diag);
    Parser parser(lexer, result.diag);

    if (!parser.parse()) {
        result.error_count = result.diag.error_count();
        return result;
    }

    Checker checker(result.diag);
    result.success = checker.check(parser.file());
    result.error_count = result.diag.error_count();
    return result;
}

bool check_ok(std::string_view source) {
    auto result = check_source(source);
    return result.success;
}

bool check_fails(std::string_view source) {
    auto result = check_source(source);
    return !result.success;
}

bool has_error(std::string_view source, std::string_view error_substring) {
    auto result = check_source(source);
    for (const auto& err : result.errors) {
        if (err.find(error_substring) != std::string::npos) {
            return true;
        }
    }
    return false;
}

} // namespace

// ============================================================================
// Constant evaluation
// ============================================================================

TEST(ConstantTest, IntArithmetic) {
    ConstValue a(int64_t(10));
    ConstValue b(int64_t(3));

    auto sum = const_add(a, b);
    EXPECT_TRUE(sum.is_int());
    EXPECT_EQ(sum.as_int(), 13);

    auto diff = const_sub(a, b);
    EXPECT_EQ(diff.as_int(), 7);

    auto prod = const_mul(a, b);
    EXPECT_EQ(prod.as_int(), 30);

    auto quot = const_div(a, b);
    EXPECT_EQ(quot.as_int(), 3); // integer division

    auto rem = const_mod(a, b);
    EXPECT_EQ(rem.as_int(), 1);
}

TEST(ConstantTest, FloatArithmetic) {
    ConstValue a(3.14);
    ConstValue b(2.0);

    auto sum = const_add(a, b);
    EXPECT_TRUE(sum.is_float());
    EXPECT_DOUBLE_EQ(sum.as_float(), 5.14);

    auto diff = const_sub(a, b);
    EXPECT_DOUBLE_EQ(diff.as_float(), 1.14);
}

TEST(ConstantTest, StringConcat) {
    ConstValue a(std::string("hello"));
    ConstValue b(std::string(" world"));

    auto sum = const_add(a, b);
    EXPECT_TRUE(sum.is_string());
    EXPECT_EQ(sum.as_string(), "hello world");
}

TEST(ConstantTest, Negation) {
    ConstValue a(int64_t(42));
    auto neg = const_neg(a);
    EXPECT_EQ(neg.as_int(), -42);

    ConstValue b(3.14);
    auto negf = const_neg(b);
    EXPECT_DOUBLE_EQ(negf.as_float(), -3.14);
}

TEST(ConstantTest, BoolNot) {
    ConstValue t(true);
    auto r = const_not(t);
    EXPECT_TRUE(r.is_bool());
    EXPECT_FALSE(r.as_bool());

    ConstValue f(false);
    auto r2 = const_not(f);
    EXPECT_TRUE(r2.as_bool());
}

TEST(ConstantTest, Comparison) {
    ConstValue a(int64_t(5));
    ConstValue b(int64_t(10));

    EXPECT_TRUE(const_lt(a, b).as_bool());
    EXPECT_FALSE(const_lt(b, a).as_bool());
    EXPECT_TRUE(const_le(a, b).as_bool());
    EXPECT_TRUE(const_eq(a, a).as_bool());
    EXPECT_TRUE(const_neq(a, b).as_bool());
    EXPECT_TRUE(const_gt(b, a).as_bool());
    EXPECT_TRUE(const_ge(b, a).as_bool());
}

TEST(ConstantTest, DivisionByZero) {
    ConstValue a(int64_t(10));
    ConstValue zero(int64_t(0));

    auto r = const_div(a, zero);
    EXPECT_FALSE(r.is_valid());

    auto r2 = const_mod(a, zero);
    EXPECT_FALSE(r2.is_valid());
}

TEST(ConstantTest, ToString) {
    EXPECT_EQ(ConstValue(int64_t(42)).to_string(), "42");
    EXPECT_EQ(ConstValue(true).to_string(), "true");
    EXPECT_EQ(ConstValue(false).to_string(), "false");
    EXPECT_EQ(ConstValue(std::string("hello")).to_string(), "\"hello\"");
}

TEST(ConstantTest, Conversions) {
    ConstValue i(int64_t(42));
    double d = 0;
    EXPECT_TRUE(i.to_float(d));
    EXPECT_DOUBLE_EQ(d, 42.0);

    ConstValue f(42.0);
    int64_t n = 0;
    EXPECT_TRUE(f.to_int(n));
    EXPECT_EQ(n, 42);

    ConstValue f2(42.5);
    EXPECT_FALSE(f2.to_int(n)); // Not a whole number
}

// ============================================================================
// Sample program tests — should all pass type checking
// ============================================================================

TEST(SemaTest, HelloWorld) {
    EXPECT_TRUE(check_ok(R"(
package main

func main() {
    println("Hello, World!")
}
)"));
}

TEST(SemaTest, Fibonacci) {
    EXPECT_TRUE(check_ok(R"(
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
)"));
}

TEST(SemaTest, Structs) {
    EXPECT_TRUE(check_ok(R"(
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
)"));
}

TEST(SemaTest, Interfaces) {
    EXPECT_TRUE(check_ok(R"(
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
)"));
}

TEST(SemaTest, Goroutines) {
    EXPECT_TRUE(check_ok(R"(
package main

func worker(ch chan int) {
    ch <- 42
}

func main() {
    ch := make(chan int)
    go worker(ch)
    println(<-ch)
}
)"));
}

// ============================================================================
// Name resolution
// ============================================================================

TEST(SemaTest, UndefinedVariable) {
    EXPECT_TRUE(has_error(R"(
package main
func main() {
    println(x)
}
)", "undefined: x"));
}

TEST(SemaTest, UndefinedFunction) {
    EXPECT_TRUE(has_error(R"(
package main
func main() {
    foo()
}
)", "undefined: foo"));
}

TEST(SemaTest, ForwardReference) {
    // Functions should be visible before their declaration (two-pass)
    EXPECT_TRUE(check_ok(R"(
package main
func main() {
    foo()
}
func foo() {
    println("hello")
}
)"));
}

TEST(SemaTest, TypeForwardReference) {
    EXPECT_TRUE(check_ok(R"(
package main
func usePoint() Point {
    return Point{1, 2}
}
type Point struct {
    X, Y int
}
func main() {
    p := usePoint()
    println(p.X)
}
)"));
}

// ============================================================================
// Type checking errors
// ============================================================================

TEST(SemaTest, WrongReturnType) {
    EXPECT_TRUE(has_error(R"(
package main
func foo() int {
    return "hello"
}
func main() {
    println(foo())
}
)", "cannot use"));
}

TEST(SemaTest, WrongArgCount) {
    EXPECT_TRUE(has_error(R"(
package main
func foo(x int) {
}
func main() {
    foo(1, 2)
}
)", "wrong number of arguments"));
}

TEST(SemaTest, TooFewArgs) {
    EXPECT_TRUE(has_error(R"(
package main
func foo(x int, y int) {
}
func main() {
    foo(1)
}
)", "wrong number of arguments"));
}

TEST(SemaTest, WrongReturnCount) {
    EXPECT_TRUE(has_error(R"(
package main
func foo() int {
    return 1, 2
}
func main() {
    println(foo())
}
)", "wrong number of return values"));
}

TEST(SemaTest, MissingReturn) {
    EXPECT_TRUE(has_error(R"(
package main
func foo() int {
    return
}
func main() {
    println(foo())
}
)", "not enough return values"));
}

TEST(SemaTest, ArithOnString) {
    EXPECT_TRUE(has_error(R"(
package main
func main() {
    x := "hello"
    y := x - 1
    println(y)
}
)", "invalid operation"));
}

TEST(SemaTest, InvalidDeref) {
    EXPECT_TRUE(has_error(R"(
package main
func main() {
    x := 42
    y := *x
    println(y)
}
)", "cannot dereference"));
}

// ============================================================================
// Short variable declarations
// ============================================================================

TEST(SemaTest, ShortVarDecl) {
    EXPECT_TRUE(check_ok(R"(
package main
func main() {
    x := 42
    println(x)
}
)"));
}

TEST(SemaTest, ShortVarDeclMultiple) {
    EXPECT_TRUE(check_ok(R"(
package main
func main() {
    x, y := 1, 2
    println(x, y)
}
)"));
}

TEST(SemaTest, ShortVarDeclNoNew) {
    EXPECT_TRUE(has_error(R"(
package main
func main() {
    x := 1
    x := 2
    println(x)
}
)", "no new variables on left side of :="));
}

// ============================================================================
// Unused variables
// ============================================================================

TEST(SemaTest, UnusedVar) {
    EXPECT_TRUE(has_error(R"(
package main
func main() {
    x := 42
}
)", "declared and not used"));
}

TEST(SemaTest, BlankIdentIgnored) {
    // _ is always okay
    EXPECT_TRUE(check_ok(R"(
package main
func main() {
    _ = 42
}
)"));
}

// ============================================================================
// Variable declarations
// ============================================================================

TEST(SemaTest, VarDeclWithType) {
    EXPECT_TRUE(check_ok(R"(
package main
func main() {
    var x int = 42
    println(x)
}
)"));
}

TEST(SemaTest, VarDeclInferred) {
    EXPECT_TRUE(check_ok(R"(
package main
func main() {
    var x = 42
    println(x)
}
)"));
}

// ============================================================================
// Control flow
// ============================================================================

TEST(SemaTest, IfStatement) {
    EXPECT_TRUE(check_ok(R"(
package main
func main() {
    x := 10
    if x > 5 {
        println("big")
    } else {
        println("small")
    }
}
)"));
}

TEST(SemaTest, ForLoop) {
    EXPECT_TRUE(check_ok(R"(
package main
func main() {
    for i := 0; i < 10; i++ {
        println(i)
    }
}
)"));
}

TEST(SemaTest, SwitchStatement) {
    EXPECT_TRUE(check_ok(R"(
package main
func main() {
    x := 5
    switch x {
    case 1:
        println("one")
    case 5:
        println("five")
    default:
        println("other")
    }
}
)"));
}

// ============================================================================
// Struct operations
// ============================================================================

TEST(SemaTest, StructFieldAccess) {
    EXPECT_TRUE(check_ok(R"(
package main
type Point struct {
    X int
    Y int
}
func main() {
    p := Point{1, 2}
    println(p.X)
}
)"));
}

TEST(SemaTest, StructUnknownField) {
    EXPECT_TRUE(has_error(R"(
package main
type Point struct {
    X int
    Y int
}
func main() {
    p := Point{1, 2}
    println(p.Z)
}
)", "no field or method 'Z'"));
}

TEST(SemaTest, StructLitWithFieldNames) {
    EXPECT_TRUE(check_ok(R"(
package main
type Point struct {
    X int
    Y int
}
func main() {
    p := Point{X: 1, Y: 2}
    println(p.X, p.Y)
}
)"));
}

TEST(SemaTest, StructLitUnknownFieldName) {
    EXPECT_TRUE(has_error(R"(
package main
type Point struct {
    X int
    Y int
}
func main() {
    p := Point{Z: 1}
    println(p.X)
}
)", "unknown field 'Z'"));
}

// ============================================================================
// Method calls
// ============================================================================

TEST(SemaTest, MethodCall) {
    EXPECT_TRUE(check_ok(R"(
package main
type MyInt int
func (m MyInt) Double() int {
    return 0
}
func main() {
    var x MyInt = 5
    println(x.Double())
}
)"));
}

// ============================================================================
// Channels
// ============================================================================

TEST(SemaTest, ChannelSendRecv) {
    EXPECT_TRUE(check_ok(R"(
package main
func main() {
    ch := make(chan int)
    go func() {
        ch <- 42
    }()
    x := <-ch
    println(x)
}
)"));
}

// ============================================================================
// Type declarations
// ============================================================================

TEST(SemaTest, TypeDecl) {
    EXPECT_TRUE(check_ok(R"(
package main
type MyInt int
func main() {
    var x MyInt = 42
    println(x)
}
)"));
}

// ============================================================================
// Assignments
// ============================================================================

TEST(SemaTest, AssignCompatible) {
    EXPECT_TRUE(check_ok(R"(
package main
func main() {
    var x int
    x = 42
    println(x)
}
)"));
}

TEST(SemaTest, MultipleAssignment) {
    EXPECT_TRUE(check_ok(R"(
package main
func main() {
    x, y := 1, 2
    x, y = y, x
    println(x, y)
}
)"));
}

// ============================================================================
// Increment / Decrement
// ============================================================================

TEST(SemaTest, IncDec) {
    EXPECT_TRUE(check_ok(R"(
package main
func main() {
    x := 0
    x++
    x--
    println(x)
}
)"));
}

// ============================================================================
// Go / Defer
// ============================================================================

TEST(SemaTest, GoStatement) {
    EXPECT_TRUE(check_ok(R"(
package main
func foo() {}
func main() {
    go foo()
}
)"));
}

TEST(SemaTest, DeferStatement) {
    EXPECT_TRUE(check_ok(R"(
package main
func foo() {}
func main() {
    defer foo()
}
)"));
}

// ============================================================================
// Empty function
// ============================================================================

TEST(SemaTest, EmptyFunc) {
    EXPECT_TRUE(check_ok(R"(
package main
func main() {
}
)"));
}

// ============================================================================
// Multiple return values
// ============================================================================

TEST(SemaTest, MultipleReturnValues) {
    EXPECT_TRUE(check_ok(R"(
package main
func swap(a, b int) (int, int) {
    return b, a
}
func main() {
    x, y := swap(1, 2)
    println(x, y)
}
)"));
}

// ============================================================================
// Boolean conditions
// ============================================================================

TEST(SemaTest, NonBoolCondition) {
    EXPECT_TRUE(has_error(R"(
package main
func main() {
    if 42 {
        println("oops")
    }
}
)", "non-boolean condition"));
}

// ============================================================================
// Send to non-channel
// ============================================================================

TEST(SemaTest, SendToNonChannel) {
    EXPECT_TRUE(has_error(R"(
package main
func main() {
    x := 42
    x <- 1
}
)", "cannot send to non-channel"));
}

// ============================================================================
// Index non-indexable
// ============================================================================

TEST(SemaTest, IndexNonIndexable) {
    EXPECT_TRUE(has_error(R"(
package main
func main() {
    x := 42
    y := x[0]
    println(y)
}
)", "cannot index"));
}

// ============================================================================
// Redeclaration
// ============================================================================

TEST(SemaTest, Redeclaration) {
    EXPECT_TRUE(has_error(R"(
package main
func foo() {}
func foo() {}
func main() {}
)", "redeclared"));
}

// ============================================================================
// Const declaration
// ============================================================================

TEST(SemaTest, ConstDecl) {
    EXPECT_TRUE(check_ok(R"(
package main
const x = 42
func main() {
    println(x)
}
)"));
}

// ============================================================================
// String concatenation
// ============================================================================

TEST(SemaTest, StringConcat) {
    EXPECT_TRUE(check_ok(R"(
package main
func main() {
    x := "hello" + " " + "world"
    println(x)
}
)"));
}

// ============================================================================
// IncDec on non-numeric
// ============================================================================

TEST(SemaTest, IncDecNonNumeric) {
    EXPECT_TRUE(has_error(R"(
package main
func main() {
    x := "hello"
    x++
    println(x)
}
)", "non-numeric"));
}

// ============================================================================
// Call non-function
// ============================================================================

TEST(SemaTest, CallNonFunction) {
    EXPECT_TRUE(has_error(R"(
package main
func main() {
    x := 42
    x()
}
)", "cannot call non-function"));
}

// ============================================================================
// Builtin len
// ============================================================================

TEST(SemaTest, BuiltinLen) {
    EXPECT_TRUE(check_ok(R"(
package main
func main() {
    s := "hello"
    println(len(s))
}
)"));
}

// ============================================================================
// Pointer operations
// ============================================================================

TEST(SemaTest, AddressOf) {
    EXPECT_TRUE(check_ok(R"(
package main
func main() {
    x := 42
    p := &x
    println(*p)
}
)"));
}

// ============================================================================
// Named type with methods and interface satisfaction
// ============================================================================

TEST(SemaTest, InterfaceSatisfaction) {
    EXPECT_TRUE(check_ok(R"(
package main

type Stringer interface {
    String() string
}

type Name string

func (n Name) String() string {
    return "name"
}

func printIt(s Stringer) {
    println(s.String())
}

func main() {
    var n Name = "Alice"
    printIt(n)
}
)"));
}

// ============================================================================
// For range
// ============================================================================

TEST(SemaTest, ForRange) {
    // Range over a literal is hard because arrays require proper composite lit types.
    // Test with a simpler pattern using a string.
    EXPECT_TRUE(check_ok(R"(
package main
func main() {
    s := "hello"
    for i, c := range s {
        println(i, c)
    }
}
)"));
}

// ============================================================================
// Blank assignment
// ============================================================================

TEST(SemaTest, BlankAssign) {
    EXPECT_TRUE(check_ok(R"(
package main
func main() {
    _ = 42
    _ = "hello"
}
)"));
}
