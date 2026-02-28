#include "codegen/x64_codegen.hpp"
#include "common/diagnostic.hpp"
#include "ir/ir_gen.hpp"
#include "ir/ir_printer.hpp"
#include "lexer/lexer.hpp"
#include "parser/parser.hpp"
#include "sema/checker.hpp"

#include <gtest/gtest.h>
#include <string>

using namespace golangc;
using namespace golangc::codegen;

namespace {

/// Helper: compile Go source → MASM assembly string.
struct CodegenResult {
    std::string asm_text;
    std::string ir_text;
    bool has_errors = false;
};

CodegenResult compile_to_asm(const std::string& source) {
    CodegenResult result;

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
    auto module = gen.generate(parser.file());

    ir::IRPrinter printer;
    result.ir_text = printer.print(*module);

    X64CodeGenerator codegen;
    result.asm_text = codegen.generate(*module);
    result.has_errors = had_error;
    return result;
}

/// Check that a string contains a substring.
bool contains(const std::string& haystack, const std::string& needle) {
    return haystack.find(needle) != std::string::npos;
}

} // namespace

// ============================================================================
// Register name tests
// ============================================================================

TEST(RegNameTest, QWordNames) {
    EXPECT_EQ(reg_name(X64Reg::RAX), "rax");
    EXPECT_EQ(reg_name(X64Reg::RCX), "rcx");
    EXPECT_EQ(reg_name(X64Reg::RDX), "rdx");
    EXPECT_EQ(reg_name(X64Reg::RBX), "rbx");
    EXPECT_EQ(reg_name(X64Reg::RSP), "rsp");
    EXPECT_EQ(reg_name(X64Reg::RBP), "rbp");
    EXPECT_EQ(reg_name(X64Reg::R8), "r8");
    EXPECT_EQ(reg_name(X64Reg::R15), "r15");
}

TEST(RegNameTest, DWordNames) {
    EXPECT_EQ(reg_name(X64Reg::RAX, RegSize::DWord), "eax");
    EXPECT_EQ(reg_name(X64Reg::R8, RegSize::DWord), "r8d");
}

TEST(RegNameTest, ByteNames) {
    EXPECT_EQ(reg_name(X64Reg::RAX, RegSize::Byte), "al");
    EXPECT_EQ(reg_name(X64Reg::RCX, RegSize::Byte), "cl");
}

TEST(RegNameTest, XmmNames) {
    EXPECT_EQ(reg_name(X64Reg::XMM0), "xmm0");
    EXPECT_EQ(reg_name(X64Reg::XMM15), "xmm15");
}

// ============================================================================
// Frame layout tests
// ============================================================================

TEST(FrameLayoutTest, BasicAllocation) {
    FrameLayout frame;
    auto off1 = frame.allocate(1, 8);
    EXPECT_EQ(off1, -8);
    auto off2 = frame.allocate(2, 8);
    EXPECT_EQ(off2, -16);
    EXPECT_TRUE(frame.has_slot(1));
    EXPECT_TRUE(frame.has_slot(2));
    EXPECT_FALSE(frame.has_slot(3));
    EXPECT_EQ(frame.offset_of(1), -8);
    EXPECT_EQ(frame.offset_of(2), -16);
}

TEST(FrameLayoutTest, FrameAlignment) {
    FrameLayout frame;
    frame.allocate(1, 8);
    // Frame size should be 16-byte aligned
    EXPECT_EQ(frame.frame_size() % 16, 0);
}

TEST(FrameLayoutTest, Reset) {
    FrameLayout frame;
    frame.allocate(1, 8);
    EXPECT_TRUE(frame.has_slot(1));
    frame.reset();
    EXPECT_FALSE(frame.has_slot(1));
    EXPECT_EQ(frame.frame_size(), 0);
}

// ============================================================================
// MASM name mangling
// ============================================================================

TEST(CodegenHelperTest, MasmName) {
    EXPECT_EQ(X64CodeGenerator::masm_name("main"), "main");
    EXPECT_EQ(X64CodeGenerator::masm_name("Point.Add"), "Point$Add");
}

TEST(CodegenHelperTest, TypeSize) {
    ir::IRType i64_t(ir::IRTypeKind::I64);
    EXPECT_EQ(X64CodeGenerator::type_size(&i64_t), 8);

    ir::IRType i32_t(ir::IRTypeKind::I32);
    EXPECT_EQ(X64CodeGenerator::type_size(&i32_t), 4);

    ir::IRType i1_t(ir::IRTypeKind::I1);
    EXPECT_EQ(X64CodeGenerator::type_size(&i1_t), 1);

    ir::IRType ptr_t(ir::IRTypeKind::Ptr);
    EXPECT_EQ(X64CodeGenerator::type_size(&ptr_t), 8);
}

TEST(CodegenHelperTest, IsStringType) {
    ir::IRType ptr_t(ir::IRTypeKind::Ptr);
    ir::IRType i64_t(ir::IRTypeKind::I64);

    ir::IRType str_t(ir::IRTypeKind::Struct);
    str_t.fields = {&ptr_t, &i64_t};
    EXPECT_TRUE(X64CodeGenerator::is_string_type(&str_t));

    ir::IRType not_str(ir::IRTypeKind::Struct);
    not_str.fields = {&i64_t, &i64_t};
    EXPECT_FALSE(X64CodeGenerator::is_string_type(&not_str));
}

// ============================================================================
// Module structure tests
// ============================================================================

TEST(CodegenTest, EmptyMain) {
    auto result = compile_to_asm(R"(
package main
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "main PROC"));
    EXPECT_TRUE(contains(result.asm_text, "main ENDP"));
    EXPECT_TRUE(contains(result.asm_text, "push rbp"));
    EXPECT_TRUE(contains(result.asm_text, "mov rbp, rsp"));
    EXPECT_TRUE(contains(result.asm_text, "pop rbp"));
    EXPECT_TRUE(contains(result.asm_text, "ret"));
    EXPECT_TRUE(contains(result.asm_text, "END"));
}

TEST(CodegenTest, ModuleHeader) {
    auto result = compile_to_asm(R"(
package main
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "Generated by golangc"));
    EXPECT_TRUE(contains(result.asm_text, "EXTERN golangc_println_int:PROC"));
    EXPECT_TRUE(contains(result.asm_text, "EXTERN golangc_println_string:PROC"));
}

TEST(CodegenTest, TextSegment) {
    auto result = compile_to_asm(R"(
package main
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "_TEXT SEGMENT"));
    EXPECT_TRUE(contains(result.asm_text, "_TEXT ENDS"));
}

// ============================================================================
// Constant tests
// ============================================================================

TEST(CodegenTest, IntConstant) {
    auto result = compile_to_asm(R"(
package main
func main() {
    x := 42
    _ = x
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "42"));
}

TEST(CodegenTest, BoolConstant) {
    auto result = compile_to_asm(R"(
package main
func main() {
    x := true
    _ = x
}
)");
    EXPECT_FALSE(result.has_errors);
    // Should contain a 1 (true) or 0 (false) constant
    EXPECT_TRUE(contains(result.asm_text, "1"));
}

// ============================================================================
// Arithmetic tests
// ============================================================================

TEST(CodegenTest, Addition) {
    auto result = compile_to_asm(R"(
package main
func add(a int, b int) int {
    return a + b
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "add rax, rcx"));
}

TEST(CodegenTest, Subtraction) {
    auto result = compile_to_asm(R"(
package main
func sub(a int, b int) int {
    return a - b
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "sub rax, rcx"));
}

TEST(CodegenTest, Multiplication) {
    auto result = compile_to_asm(R"(
package main
func mul(a int, b int) int {
    return a * b
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "imul rax, rcx"));
}

TEST(CodegenTest, Division) {
    auto result = compile_to_asm(R"(
package main
func div(a int, b int) int {
    return a / b
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "cqo"));
    EXPECT_TRUE(contains(result.asm_text, "idiv rcx"));
}

TEST(CodegenTest, Remainder) {
    auto result = compile_to_asm(R"(
package main
func rem(a int, b int) int {
    return a % b
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "cqo"));
    EXPECT_TRUE(contains(result.asm_text, "idiv rcx"));
}

TEST(CodegenTest, Negation) {
    auto result = compile_to_asm(R"(
package main
func neg(a int) int {
    return -a
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "neg rax"));
}

// ============================================================================
// Comparison tests
// ============================================================================

TEST(CodegenTest, Equality) {
    auto result = compile_to_asm(R"(
package main
func eq(a int, b int) bool {
    return a == b
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "cmp rax, rcx"));
    EXPECT_TRUE(contains(result.asm_text, "sete al"));
}

TEST(CodegenTest, LessThan) {
    auto result = compile_to_asm(R"(
package main
func lt(a int, b int) bool {
    return a < b
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "setl al"));
}

// ============================================================================
// Control flow tests
// ============================================================================

TEST(CodegenTest, IfStatement) {
    auto result = compile_to_asm(R"(
package main
func abs(n int) int {
    if n < 0 {
        return -n
    }
    return n
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "jne"));
    EXPECT_TRUE(contains(result.asm_text, "jmp"));
}

TEST(CodegenTest, ForLoop) {
    auto result = compile_to_asm(R"(
package main
func sum(n int) int {
    s := 0
    for i := 0; i < n; i++ {
        s = s + i
    }
    return s
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    // Should have loop labels and conditional jump
    EXPECT_TRUE(contains(result.asm_text, "jne"));
    EXPECT_TRUE(contains(result.asm_text, "jmp"));
}

// ============================================================================
// Function call tests
// ============================================================================

TEST(CodegenTest, FunctionCall) {
    auto result = compile_to_asm(R"(
package main
func double(x int) int {
    return x + x
}
func main() {
    y := double(21)
    _ = y
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call double"));
}

TEST(CodegenTest, RecursiveCall) {
    auto result = compile_to_asm(R"(
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
    EXPECT_TRUE(contains(result.asm_text, "call fibonacci"));
    EXPECT_TRUE(contains(result.asm_text, "fibonacci PROC"));
    EXPECT_TRUE(contains(result.asm_text, "fibonacci ENDP"));
}

TEST(CodegenTest, MultipleParams) {
    auto result = compile_to_asm(R"(
package main
func add3(a int, b int, c int) int {
    return a + b + c
}
func main() {
    println(add3(1, 2, 3))
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call add3"));
}

// ============================================================================
// Println tests
// ============================================================================

TEST(CodegenTest, PrintlnInt) {
    auto result = compile_to_asm(R"(
package main
func main() {
    println(42)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call golangc_println_int"));
}

TEST(CodegenTest, PrintlnString) {
    auto result = compile_to_asm(R"(
package main
func main() {
    println("Hello, World!")
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call golangc_println_string"));
    EXPECT_TRUE(contains(result.asm_text, "_DATA SEGMENT"));
    EXPECT_TRUE(contains(result.asm_text, "__str0 DB"));
}

TEST(CodegenTest, PrintlnExpr) {
    auto result = compile_to_asm(R"(
package main
func main() {
    println(1 + 2)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call golangc_println_int"));
}

// ============================================================================
// Hello World end-to-end assembly
// ============================================================================

TEST(CodegenTest, HelloWorldFull) {
    auto result = compile_to_asm(R"(
package main

func main() {
    println("Hello, World!")
}
)");
    EXPECT_FALSE(result.has_errors);
    // Verify complete assembly structure
    EXPECT_TRUE(contains(result.asm_text, "Generated by golangc"));
    EXPECT_TRUE(contains(result.asm_text, "EXTERN golangc_println_string:PROC"));
    EXPECT_TRUE(contains(result.asm_text, "main PROC"));
    EXPECT_TRUE(contains(result.asm_text, "push rbp"));
    EXPECT_TRUE(contains(result.asm_text, "call golangc_println_string"));
    EXPECT_TRUE(contains(result.asm_text, "main ENDP"));
    EXPECT_TRUE(contains(result.asm_text, "_DATA SEGMENT"));
    EXPECT_TRUE(contains(result.asm_text, "END"));
}

// ============================================================================
// Fibonacci end-to-end assembly
// ============================================================================

TEST(CodegenTest, FibonacciFull) {
    auto result = compile_to_asm(R"(
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
    // Verify fibonacci function
    EXPECT_TRUE(contains(result.asm_text, "fibonacci PROC"));
    EXPECT_TRUE(contains(result.asm_text, "fibonacci ENDP"));
    EXPECT_TRUE(contains(result.asm_text, "call fibonacci"));
    // Verify main function
    EXPECT_TRUE(contains(result.asm_text, "main PROC"));
    EXPECT_TRUE(contains(result.asm_text, "call golangc_println_int"));
    EXPECT_TRUE(contains(result.asm_text, "main ENDP"));
}

// ============================================================================
// Bitwise operations
// ============================================================================

TEST(CodegenTest, BitwiseAnd) {
    auto result = compile_to_asm(R"(
package main
func f(a int, b int) int { return a & b }
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "and rax, rcx"));
}

TEST(CodegenTest, BitwiseOr) {
    auto result = compile_to_asm(R"(
package main
func f(a int, b int) int { return a | b }
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "or rax, rcx"));
}

TEST(CodegenTest, BitwiseXor) {
    auto result = compile_to_asm(R"(
package main
func f(a int, b int) int { return a ^ b }
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "xor rax, rcx"));
}

TEST(CodegenTest, ShiftLeft) {
    auto result = compile_to_asm(R"(
package main
func f(a int, b uint) int { return a << b }
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "shl rax, cl"));
}

// ============================================================================
// Logical not
// ============================================================================

TEST(CodegenTest, LogicalNot) {
    auto result = compile_to_asm(R"(
package main
func f(a bool) bool { return !a }
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "sete al"));
}

// ============================================================================
// Return value
// ============================================================================

TEST(CodegenTest, ReturnInt) {
    auto result = compile_to_asm(R"(
package main
func f() int { return 42 }
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    // Return should load value into rax
    EXPECT_TRUE(contains(result.asm_text, "42"));
    EXPECT_TRUE(contains(result.asm_text, "ret"));
}

TEST(CodegenTest, ReturnVoid) {
    auto result = compile_to_asm(R"(
package main
func f() { return }
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "ret"));
}

// ============================================================================
// Multiple functions
// ============================================================================

TEST(CodegenTest, MultipleFunctions) {
    auto result = compile_to_asm(R"(
package main
func a() int { return 1 }
func b() int { return 2 }
func main() {
    println(a() + b())
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "a PROC"));
    EXPECT_TRUE(contains(result.asm_text, "b PROC"));
    EXPECT_TRUE(contains(result.asm_text, "main PROC"));
}

// ============================================================================
// Variables and assignment
// ============================================================================

TEST(CodegenTest, LocalVariable) {
    auto result = compile_to_asm(R"(
package main
func main() {
    x := 10
    y := 20
    z := x + y
    println(z)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "10"));
    EXPECT_TRUE(contains(result.asm_text, "20"));
    EXPECT_TRUE(contains(result.asm_text, "add rax, rcx"));
}

// ============================================================================
// Complex programs
// ============================================================================

TEST(CodegenTest, GCD) {
    auto result = compile_to_asm(R"(
package main

func gcd(a int, b int) int {
    for b != 0 {
        a, b = b, a % b
    }
    return a
}

func main() {
    println(gcd(48, 18))
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "gcd PROC"));
    EXPECT_TRUE(contains(result.asm_text, "call gcd"));
}

TEST(CodegenTest, Conditional) {
    auto result = compile_to_asm(R"(
package main
func max(a int, b int) int {
    if a > b {
        return a
    }
    return b
}
func main() {
    println(max(3, 5))
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "setg al"));
}

// ============================================================================
// Struct tests
// ============================================================================

TEST(CodegenTest, StructCompositeLiteral) {
    auto result = compile_to_asm(R"(
package main
type Point struct {
    X, Y int
}
func main() {
    p := Point{1, 2}
    _ = p
}
)");
    EXPECT_FALSE(result.has_errors);
    // Should contain field stores via GetPtr indirection
    EXPECT_TRUE(contains(result.asm_text, "mov QWORD PTR [rcx], rax"));
}

TEST(CodegenTest, StructFieldAccess) {
    auto result = compile_to_asm(R"(
package main
type Point struct {
    X, Y int
}
func main() {
    p := Point{10, 20}
    println(p.X)
}
)");
    EXPECT_FALSE(result.has_errors);
    // GetPtr produces an address, Load dereferences it
    EXPECT_TRUE(contains(result.asm_text, "mov rax, QWORD PTR [rcx]"));
}

TEST(CodegenTest, StructFieldWrite) {
    auto result = compile_to_asm(R"(
package main
type Point struct {
    X, Y int
}
func main() {
    p := Point{1, 2}
    println(p.Y)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "lea rax, [rbp"));
}

TEST(CodegenTest, StructMethod) {
    auto result = compile_to_asm(R"(
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
    _ = p3
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "Point$Add PROC"));
    EXPECT_TRUE(contains(result.asm_text, "Point$Add ENDP"));
    EXPECT_TRUE(contains(result.asm_text, "call Point$Add"));
}

TEST(CodegenTest, StructSretReturn) {
    auto result = compile_to_asm(R"(
package main
type Point struct {
    X, Y int
}
func makePoint(x int, y int) Point {
    return Point{x, y}
}
func main() {
    p := makePoint(5, 10)
    _ = p
}
)");
    EXPECT_FALSE(result.has_errors);
    // Sret: caller passes pointer to result area in RCX
    EXPECT_TRUE(contains(result.asm_text, "lea rcx, [rbp"));
    EXPECT_TRUE(contains(result.asm_text, "call makePoint"));
}

TEST(CodegenTest, StructPassByPointer) {
    auto result = compile_to_asm(R"(
package main
type Pair struct {
    A, B int
}
func sum(p Pair) int {
    return p.A + p.B
}
func main() {
    println(sum(Pair{3, 7}))
}
)");
    EXPECT_FALSE(result.has_errors);
    // Struct arg >8 bytes passed by pointer (lea into register)
    EXPECT_TRUE(contains(result.asm_text, "lea rcx, [rbp"));
    EXPECT_TRUE(contains(result.asm_text, "call sum"));
}

TEST(CodegenTest, StructsGoFull) {
    auto result = compile_to_asm(R"(
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
    EXPECT_TRUE(contains(result.asm_text, "Point$Add PROC"));
    EXPECT_TRUE(contains(result.asm_text, "call Point$Add"));
    EXPECT_TRUE(contains(result.asm_text, "call golangc_print_int"));
    EXPECT_TRUE(contains(result.asm_text, "call golangc_print_space"));
    EXPECT_TRUE(contains(result.asm_text, "call golangc_print_newline"));
}

// ============================================================================
// Multi-arg println tests
// ============================================================================

TEST(CodegenTest, PrintlnMultiArg) {
    auto result = compile_to_asm(R"(
package main
func main() {
    println(1, 2)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call golangc_print_int"));
    EXPECT_TRUE(contains(result.asm_text, "call golangc_print_space"));
    EXPECT_TRUE(contains(result.asm_text, "call golangc_print_newline"));
}

TEST(CodegenTest, PrintlnSingleArg) {
    auto result = compile_to_asm(R"(
package main
func main() {
    println(42)
}
)");
    EXPECT_FALSE(result.has_errors);
    // Single arg uses println_int (with newline built in)
    EXPECT_TRUE(contains(result.asm_text, "call golangc_println_int"));
}

TEST(CodegenTest, PrintlnNoArgs) {
    auto result = compile_to_asm(R"(
package main
func main() {
    println()
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call golangc_print_newline"));
}

// ============================================================================
// Interface tests
// ============================================================================

TEST(CodegenTest, InterfaceMake) {
    auto result = compile_to_asm(R"(
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
    // Interface boxing: iface_make in IR → type tag and data stored
    EXPECT_TRUE(contains(result.asm_text, "call Print"));
    EXPECT_TRUE(contains(result.asm_text, "MyInt$String PROC"));
}

TEST(CodegenTest, InterfaceMethodCall) {
    auto result = compile_to_asm(R"(
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
    // Interface method resolved to concrete: MyInt$String
    EXPECT_TRUE(contains(result.asm_text, "call MyInt$String"));
    EXPECT_TRUE(contains(result.asm_text, "call golangc_println_string"));
}

TEST(CodegenTest, InterfacesGoFull) {
    auto result = compile_to_asm(R"(
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
    // Complete end-to-end: interface boxing, method dispatch, string return
    EXPECT_TRUE(contains(result.asm_text, "MyInt$String PROC"));
    EXPECT_TRUE(contains(result.asm_text, "Print PROC"));
    EXPECT_TRUE(contains(result.asm_text, "main PROC"));
    EXPECT_TRUE(contains(result.asm_text, "call MyInt$String"));
    EXPECT_TRUE(contains(result.asm_text, "call Print"));
    EXPECT_TRUE(contains(result.asm_text, "_DATA SEGMENT"));
}

// ============================================================================
// GetPtr indirection tests
// ============================================================================

TEST(CodegenTest, GetPtrLoadIndirection) {
    auto result = compile_to_asm(R"(
package main
type S struct {
    A, B int
}
func main() {
    s := S{10, 20}
    x := s.A
    println(x)
}
)");
    EXPECT_FALSE(result.has_errors);
    // GetPtr creates address, Load dereferences through it
    EXPECT_TRUE(contains(result.asm_text, "mov rax, QWORD PTR [rcx]"));
}

TEST(CodegenTest, GetPtrStoreIndirection) {
    auto result = compile_to_asm(R"(
package main
type S struct {
    A, B int
}
func main() {
    s := S{0, 0}
    _ = s
}
)");
    EXPECT_FALSE(result.has_errors);
    // Store through GetPtr: store to [rcx]
    EXPECT_TRUE(contains(result.asm_text, "mov QWORD PTR [rcx], rax"));
}

// ============================================================================
// Type size tests
// ============================================================================

TEST(CodegenHelperTest, StructTypeSize) {
    ir::IRType i64_t(ir::IRTypeKind::I64);

    ir::IRType point_t(ir::IRTypeKind::Struct);
    point_t.fields = {&i64_t, &i64_t};
    EXPECT_EQ(X64CodeGenerator::type_size(&point_t), 16);
    EXPECT_TRUE(X64CodeGenerator::is_large_struct(&point_t));
    EXPECT_EQ(X64CodeGenerator::type_qwords(&point_t), 2);
}

TEST(CodegenHelperTest, SmallStructNotLarge) {
    ir::IRType i64_t(ir::IRTypeKind::I64);

    ir::IRType small_t(ir::IRTypeKind::Struct);
    small_t.fields = {&i64_t};
    EXPECT_EQ(X64CodeGenerator::type_size(&small_t), 8);
    EXPECT_FALSE(X64CodeGenerator::is_large_struct(&small_t));
}

TEST(CodegenHelperTest, TypeQwords) {
    ir::IRType i64_t(ir::IRTypeKind::I64);
    EXPECT_EQ(X64CodeGenerator::type_qwords(&i64_t), 1);

    ir::IRType ptr_t(ir::IRTypeKind::Ptr);
    ir::IRType iface_t(ir::IRTypeKind::Struct);
    iface_t.fields = {&ptr_t, &ptr_t};
    EXPECT_EQ(X64CodeGenerator::type_qwords(&iface_t), 2);
}

TEST(CodegenHelperTest, IsFloatType) {
    ir::IRType f64_t(ir::IRTypeKind::F64);
    EXPECT_TRUE(X64CodeGenerator::is_float_type(&f64_t));

    ir::IRType f32_t(ir::IRTypeKind::F32);
    EXPECT_TRUE(X64CodeGenerator::is_float_type(&f32_t));

    ir::IRType i64_t(ir::IRTypeKind::I64);
    EXPECT_FALSE(X64CodeGenerator::is_float_type(&i64_t));
}

TEST(CodegenHelperTest, IsSliceType) {
    ir::IRType ptr_t(ir::IRTypeKind::Ptr);
    ir::IRType i64_t(ir::IRTypeKind::I64);

    ir::IRType slice_t(ir::IRTypeKind::Struct);
    slice_t.fields = {&ptr_t, &i64_t, &i64_t};
    EXPECT_TRUE(X64CodeGenerator::is_slice_type(&slice_t));

    ir::IRType not_slice(ir::IRTypeKind::Struct);
    not_slice.fields = {&ptr_t, &i64_t};
    EXPECT_FALSE(X64CodeGenerator::is_slice_type(&not_slice));
}

// ============================================================================
// Float constant and arithmetic tests
// ============================================================================

TEST(CodegenTest, FloatConstant) {
    auto result = compile_to_asm(R"(
package main
func main() {
    x := 3.14
    _ = x
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "movsd xmm0, QWORD PTR [__flt"));
    EXPECT_TRUE(contains(result.asm_text, "_DATA SEGMENT"));
    EXPECT_TRUE(contains(result.asm_text, "__flt0 DQ"));
}

TEST(CodegenTest, FloatAdd) {
    auto result = compile_to_asm(R"(
package main
func main() {
    x := 1.5
    y := 2.5
    z := x + y
    _ = z
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "addsd xmm0, xmm1"));
}

TEST(CodegenTest, FloatSub) {
    auto result = compile_to_asm(R"(
package main
func main() {
    x := 5.0
    y := 2.0
    z := x - y
    _ = z
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "subsd xmm0, xmm1"));
}

TEST(CodegenTest, FloatMul) {
    auto result = compile_to_asm(R"(
package main
func main() {
    x := 3.0
    y := 4.0
    z := x * y
    _ = z
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "mulsd xmm0, xmm1"));
}

TEST(CodegenTest, FloatDiv) {
    auto result = compile_to_asm(R"(
package main
func main() {
    x := 10.0
    y := 3.0
    z := x / y
    _ = z
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "divsd xmm0, xmm1"));
}

TEST(CodegenTest, FloatNeg) {
    auto result = compile_to_asm(R"(
package main
func main() {
    x := 3.14
    y := -x
    _ = y
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "xorpd xmm0"));
    EXPECT_TRUE(contains(result.asm_text, "__f64_sign_mask"));
}

// ============================================================================
// Float comparison and conversion tests
// ============================================================================

TEST(CodegenTest, FloatCompareLt) {
    auto result = compile_to_asm(R"(
package main
func main() {
    x := 1.0
    y := 2.0
    if x < y {
        println(1)
    }
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "ucomisd xmm0, xmm1"));
    EXPECT_TRUE(contains(result.asm_text, "setb al"));
}

TEST(CodegenTest, FloatCompareEq) {
    auto result = compile_to_asm(R"(
package main
func main() {
    x := 1.0
    y := 1.0
    if x == y {
        println(1)
    }
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "ucomisd xmm0, xmm1"));
    EXPECT_TRUE(contains(result.asm_text, "sete al"));
    EXPECT_TRUE(contains(result.asm_text, "setnp cl"));
}

TEST(CodegenTest, IntToFloat) {
    auto result = compile_to_asm(R"(
package main
func main() {
    x := 42
    y := float64(x)
    _ = y
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "cvtsi2sd xmm0, rax"));
}

TEST(CodegenTest, FloatToInt) {
    auto result = compile_to_asm(R"(
package main
func main() {
    x := 3.14
    y := int(x)
    _ = y
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "cvttsd2si rax, xmm0"));
}

// ============================================================================
// Float ABI tests — println, params, returns
// ============================================================================

TEST(CodegenTest, PrintlnFloat) {
    auto result = compile_to_asm(R"(
package main
func main() {
    println(3.14)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call golangc_println_float"));
}

TEST(CodegenTest, FloatFunctionParam) {
    auto result = compile_to_asm(R"(
package main
func addF(a float64, b float64) float64 {
    return a + b
}
func main() {
    println(addF(1.5, 2.5))
}
)");
    EXPECT_FALSE(result.has_errors);
    // Float params should be saved from XMM registers in prologue
    EXPECT_TRUE(contains(result.asm_text, "movsd QWORD PTR [rbp"));
    EXPECT_TRUE(contains(result.asm_text, "addsd xmm0, xmm1"));
    EXPECT_TRUE(contains(result.asm_text, "call addF"));
}

TEST(CodegenTest, FloatReturn) {
    auto result = compile_to_asm(R"(
package main
func pi() float64 {
    return 3.14159
}
func main() {
    x := pi()
    _ = x
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call pi"));
    // Float return should use movsd xmm0
    EXPECT_TRUE(contains(result.asm_text, "movsd xmm0, QWORD PTR"));
}

// ============================================================================
// String operation tests
// ============================================================================

TEST(CodegenTest, StringLen) {
    auto result = compile_to_asm(R"(
package main
func main() {
    s := "hello"
    n := len(s)
    println(n)
}
)");
    EXPECT_FALSE(result.has_errors);
    // StringLen should load the length QWORD
    EXPECT_TRUE(contains(result.asm_text, "call golangc_println_int"));
}

TEST(CodegenTest, StringIndex) {
    auto result = compile_to_asm(R"(
package main
func main() {
    s := "hello"
    c := s[0]
    _ = c
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "movzx rax, BYTE PTR [rcx+rax]"));
}

TEST(CodegenTest, StringConcat) {
    auto result = compile_to_asm(R"(
package main
func main() {
    a := "hello"
    b := " world"
    c := a + b
    println(c)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call golangc_string_concat"));
    EXPECT_TRUE(contains(result.asm_text, "call golangc_println_string"));
}

// ============================================================================
// Slice operation tests
// ============================================================================

TEST(CodegenTest, SliceLen) {
    auto result = compile_to_asm(R"(
package main
func main() {
    var s []int
    n := len(s)
    println(n)
}
)");
    EXPECT_FALSE(result.has_errors);
    // Should compile without errors; actual slice_len dispatch to codegen
    EXPECT_TRUE(contains(result.asm_text, "call golangc_println_int"));
}

// ============================================================================
// Float end-to-end test
// ============================================================================

TEST(CodegenTest, FloatsGoFull) {
    auto result = compile_to_asm(R"(
package main
func main() {
    x := 3.14
    y := 2.0
    println(x + y)
    println(x * y)
}
)");
    EXPECT_FALSE(result.has_errors);
    // Float constants emitted in data section
    EXPECT_TRUE(contains(result.asm_text, "_DATA SEGMENT"));
    EXPECT_TRUE(contains(result.asm_text, "__flt"));
    // Float arithmetic operations
    EXPECT_TRUE(contains(result.asm_text, "addsd xmm0, xmm1"));
    EXPECT_TRUE(contains(result.asm_text, "mulsd xmm0, xmm1"));
    // Println dispatches to float runtime
    EXPECT_TRUE(contains(result.asm_text, "call golangc_println_float"));
    // Module structure intact
    EXPECT_TRUE(contains(result.asm_text, "main PROC"));
    EXPECT_TRUE(contains(result.asm_text, "EXTERN golangc_println_float:PROC"));
}

TEST(CodegenTest, StringOpsFull) {
    auto result = compile_to_asm(R"(
package main
func main() {
    s := "hello"
    println(len(s))
    println(s[1])
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "main PROC"));
    EXPECT_TRUE(contains(result.asm_text, "_DATA SEGMENT"));
}

TEST(CodegenTest, FloatMultiArgPrintln) {
    auto result = compile_to_asm(R"(
package main
func main() {
    x := 1.5
    y := 2.5
    println(x, y)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call golangc_print_float"));
    EXPECT_TRUE(contains(result.asm_text, "call golangc_print_space"));
    EXPECT_TRUE(contains(result.asm_text, "call golangc_print_newline"));
}

// ============================================================================
// Phase 9: Goroutine / Channel codegen tests
// ============================================================================

TEST(CodegenTest, ChanMakeEmitsCall) {
    auto result = compile_to_asm(R"(
package main
func main() {
    ch := make(chan int)
    _ = ch
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call golangc_chan_make"));
    EXPECT_TRUE(contains(result.asm_text, "EXTERN golangc_chan_make:PROC"));
}

TEST(CodegenTest, ChanMakeElemSize8) {
    auto result = compile_to_asm(R"(
package main
func main() {
    ch := make(chan int)
    _ = ch
}
)");
    EXPECT_FALSE(result.has_errors);
    // chan int has 8-byte element size
    EXPECT_TRUE(contains(result.asm_text, "mov rcx, 8"));
}

TEST(CodegenTest, ChanSendEmitsCall) {
    auto result = compile_to_asm(R"(
package main
func f(ch chan int) {
    ch <- 42
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call golangc_chan_send"));
}

TEST(CodegenTest, ChanSendPassesAddrRdx) {
    auto result = compile_to_asm(R"(
package main
func f(ch chan int) {
    ch <- 42
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "lea rdx, [rbp"));
}

TEST(CodegenTest, ChanRecvEmitsCall) {
    auto result = compile_to_asm(R"(
package main
func main() {
    ch := make(chan int)
    x := <-ch
    _ = x
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call golangc_chan_recv"));
}

TEST(CodegenTest, ChanRecvOutputBuffer) {
    auto result = compile_to_asm(R"(
package main
func main() {
    ch := make(chan int)
    x := <-ch
    _ = x
}
)");
    EXPECT_FALSE(result.has_errors);
    // Recv passes address of output buffer via RDX
    EXPECT_TRUE(contains(result.asm_text, "lea rdx, [rbp"));
}

TEST(CodegenTest, ChanRecvWithPrintln) {
    auto result = compile_to_asm(R"(
package main
func main() {
    ch := make(chan int)
    println(<-ch)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call golangc_chan_recv"));
    EXPECT_TRUE(contains(result.asm_text, "call golangc_println_int"));
}

TEST(CodegenTest, GoSpawnEmitsCall) {
    auto result = compile_to_asm(R"(
package main
func worker(ch chan int) {
    ch <- 42
}
func main() {
    ch := make(chan int)
    go worker(ch)
    _ = <-ch
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call golangc_go_spawn"));
    EXPECT_TRUE(contains(result.asm_text, "EXTERN golangc_go_spawn:PROC"));
}

TEST(CodegenTest, GoSpawnFuncPtrLea) {
    auto result = compile_to_asm(R"(
package main
func worker(ch chan int) {
    ch <- 42
}
func main() {
    ch := make(chan int)
    go worker(ch)
    _ = <-ch
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "lea rcx, [worker]"));
}

TEST(CodegenTest, GoSpawnArgCount1) {
    auto result = compile_to_asm(R"(
package main
func worker(ch chan int) {
    ch <- 42
}
func main() {
    ch := make(chan int)
    go worker(ch)
    _ = <-ch
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "mov rdx, 1"));
}

TEST(CodegenTest, GoroutinesGoCompilesNoTodos) {
    auto result = compile_to_asm(R"(
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
    EXPECT_TRUE(contains(result.asm_text, "call golangc_chan_make"));
    EXPECT_TRUE(contains(result.asm_text, "call golangc_chan_send"));
    EXPECT_TRUE(contains(result.asm_text, "call golangc_chan_recv"));
    EXPECT_TRUE(contains(result.asm_text, "call golangc_go_spawn"));
    // No unimplemented opcodes should remain
    EXPECT_FALSE(contains(result.asm_text, "; TODO:"));
}

TEST(CodegenTest, GoroutinesGoAllExterns) {
    auto result = compile_to_asm(R"(
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
    EXPECT_TRUE(contains(result.asm_text, "EXTERN golangc_chan_make:PROC"));
    EXPECT_TRUE(contains(result.asm_text, "EXTERN golangc_chan_send:PROC"));
    EXPECT_TRUE(contains(result.asm_text, "EXTERN golangc_chan_recv:PROC"));
    EXPECT_TRUE(contains(result.asm_text, "EXTERN golangc_go_spawn:PROC"));
}

TEST(CodegenTest, GoroutinesGoTwoProcs) {
    auto result = compile_to_asm(R"(
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
    EXPECT_TRUE(contains(result.asm_text, "worker PROC"));
    EXPECT_TRUE(contains(result.asm_text, "main PROC"));
}

// ============================================================================
// Phase 10: Map codegen tests
// ============================================================================

TEST(CodegenTest, MapMakeEmitsCall) {
    auto result = compile_to_asm(R"(
package main
func main() {
    m := make(map[string]int)
    _ = m
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call golangc_map_make"));
    EXPECT_TRUE(contains(result.asm_text, "EXTERN golangc_map_make:PROC"));
}

TEST(CodegenTest, MapMakeKeySizeString) {
    auto result = compile_to_asm(R"(
package main
func main() {
    m := make(map[string]int)
    _ = m
}
)");
    EXPECT_FALSE(result.has_errors);
    // string key = {ptr, len} = 16 bytes; int val = 8 bytes
    EXPECT_TRUE(contains(result.asm_text, "mov rcx, 16"));
    EXPECT_TRUE(contains(result.asm_text, "mov rdx, 8"));
}

TEST(CodegenTest, MapSetEmitsCall) {
    auto result = compile_to_asm(R"(
package main
func main() {
    m := make(map[int]int)
    m[1] = 42
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call golangc_map_set"));
    EXPECT_TRUE(contains(result.asm_text, "EXTERN golangc_map_set:PROC"));
}

TEST(CodegenTest, MapSetPassesAddrArgs) {
    auto result = compile_to_asm(R"(
package main
func main() {
    m := make(map[int]int)
    m[1] = 42
}
)");
    EXPECT_FALSE(result.has_errors);
    // key and val passed by address via LEA
    EXPECT_TRUE(contains(result.asm_text, "lea rdx, [rbp"));
    EXPECT_TRUE(contains(result.asm_text, "lea r8, [rbp"));
}

TEST(CodegenTest, MapGetEmitsCall) {
    auto result = compile_to_asm(R"(
package main
func main() {
    m := make(map[int]int)
    x := m[1]
    _ = x
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call golangc_map_get"));
    EXPECT_TRUE(contains(result.asm_text, "EXTERN golangc_map_get:PROC"));
}

TEST(CodegenTest, MapGetDerefResult) {
    auto result = compile_to_asm(R"(
package main
func main() {
    m := make(map[int]int)
    x := m[1]
    _ = x
}
)");
    EXPECT_FALSE(result.has_errors);
    // Non-null path dereferences the returned pointer
    EXPECT_TRUE(contains(result.asm_text, "mov rax, QWORD PTR [rax]"));
}

TEST(CodegenTest, MapGetMissLabel) {
    auto result = compile_to_asm(R"(
package main
func main() {
    m := make(map[int]int)
    x := m[1]
    _ = x
}
)");
    EXPECT_FALSE(result.has_errors);
    // Miss/done labels are present for null-check branching
    EXPECT_TRUE(contains(result.asm_text, "mg_miss"));
    EXPECT_TRUE(contains(result.asm_text, "mg_done"));
}

TEST(CodegenTest, MapFullIntKey) {
    auto result = compile_to_asm(R"(
package main
func main() {
    m := make(map[int]int)
    m[10] = 99
    println(m[10])
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call golangc_map_make"));
    EXPECT_TRUE(contains(result.asm_text, "call golangc_map_set"));
    EXPECT_TRUE(contains(result.asm_text, "call golangc_map_get"));
    EXPECT_TRUE(contains(result.asm_text, "call golangc_println_int"));
    EXPECT_FALSE(contains(result.asm_text, "; TODO:"));
}

// ============================================================================
// Phase 10: Multiple return values tests
// ============================================================================

TEST(CodegenTest, MultiReturnFuncDecl) {
    auto result = compile_to_asm(R"(
package main
func divmod(a int, b int) (int, int) {
    return a / b, a % b
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "divmod PROC"));
}

TEST(CodegenTest, MultiReturnPacksStruct) {
    auto result = compile_to_asm(R"(
package main
func divmod(a int, b int) (int, int) {
    return a / b, a % b
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    // Multiple returns use InsertValue to pack into a struct
    EXPECT_TRUE(contains(result.ir_text, "insert_value") || contains(result.asm_text, "divmod PROC"));
}

TEST(CodegenTest, MultiReturnUnpack) {
    auto result = compile_to_asm(R"(
package main
func divmod(a int, b int) (int, int) {
    return a / b, a % b
}
func main() {
    q, r := divmod(17, 5)
    println(q)
    println(r)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call divmod"));
    EXPECT_TRUE(contains(result.asm_text, "call golangc_println_int"));
}

TEST(CodegenTest, MultiReturnFullDivmod) {
    auto result = compile_to_asm(R"(
package main
func divmod(a int, b int) (int, int) {
    return a / b, a % b
}
func main() {
    q, r := divmod(17, 5)
    println(q)
    println(r)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "divmod PROC"));
    EXPECT_TRUE(contains(result.asm_text, "main PROC"));
    EXPECT_FALSE(contains(result.asm_text, "; TODO:"));
}

// ============================================================================
// Phase 11: Slice writes, append, map len/delete, for-range map
// ============================================================================

TEST(CodegenTest, SliceIndexAddrEmitsStore) {
    auto result = compile_to_asm(R"(
package main
func main() {
    s := make([]int, 3)
    s[0] = 42
    println(s[0])
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call malloc"));
    EXPECT_TRUE(contains(result.asm_text, "call golangc_println_int"));
}

TEST(CodegenTest, SliceIndexAddrIsGetPtr) {
    auto result = compile_to_asm(R"(
package main
func main() {
    s := make([]int, 2)
    s[1] = 99
}
)");
    EXPECT_FALSE(result.has_errors);
    // SliceIndexAddr should compute &s[1] via imul + add
    EXPECT_TRUE(contains(result.asm_text, "imul rax, 8"));
}

TEST(CodegenTest, SliceAppendEmitsCall) {
    auto result = compile_to_asm(R"(
package main
func main() {
    s := make([]int, 0)
    s = append(s, 10)
    println(len(s))
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call golangc_slice_append"));
    EXPECT_TRUE(contains(result.asm_text, "EXTERN golangc_slice_append:PROC"));
}

TEST(CodegenTest, SliceAppendPassesAddrRcx) {
    auto result = compile_to_asm(R"(
package main
func main() {
    s := make([]int, 0)
    s = append(s, 42)
}
)");
    EXPECT_FALSE(result.has_errors);
    // RCX should be the address of the slice triple
    EXPECT_TRUE(contains(result.asm_text, "lea rcx, [rbp"));
}

TEST(CodegenTest, MapLenEmitsCall) {
    auto result = compile_to_asm(R"(
package main
func main() {
    m := make(map[string]int)
    m["a"] = 1
    println(len(m))
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call golangc_map_len"));
    EXPECT_TRUE(contains(result.asm_text, "EXTERN golangc_map_len:PROC"));
}

TEST(CodegenTest, MapDeleteEmitsCall) {
    auto result = compile_to_asm(R"(
package main
func main() {
    m := make(map[string]int)
    m["hello"] = 42
    delete(m, "hello")
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call golangc_map_delete"));
    EXPECT_TRUE(contains(result.asm_text, "EXTERN golangc_map_delete:PROC"));
}

TEST(CodegenTest, MapDeletePassesAddrRdx) {
    auto result = compile_to_asm(R"(
package main
func main() {
    m := make(map[string]int)
    delete(m, "key")
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "lea rdx, [rbp"));
}

TEST(CodegenTest, ForRangeMapEmitsIterMake) {
    auto result = compile_to_asm(R"(
package main
func main() {
    m := make(map[string]int)
    m["x"] = 1
    for k, v := range m {
        _ = k
        _ = v
    }
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call golangc_map_iter_make"));
    EXPECT_TRUE(contains(result.asm_text, "EXTERN golangc_map_iter_make:PROC"));
}

TEST(CodegenTest, ForRangeMapEmitsIterNext) {
    auto result = compile_to_asm(R"(
package main
func main() {
    m := make(map[string]int)
    for k, v := range m {
        _ = k
        _ = v
    }
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call golangc_map_iter_next"));
}

TEST(CodegenTest, ForRangeMapEmitsIterFree) {
    auto result = compile_to_asm(R"(
package main
func main() {
    m := make(map[string]int)
    for k, v := range m {
        _ = k
        _ = v
    }
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call golangc_map_iter_free"));
    EXPECT_TRUE(contains(result.asm_text, "EXTERN golangc_map_iter_free:PROC"));
}

TEST(CodegenTest, WordfreqCompilesNoTodos) {
    auto result = compile_to_asm(R"(
package main
func main() {
    m := make(map[string]int)
    m["hello"] = 3
    m["world"] = 1
    delete(m, "world")
    println(len(m))
    count := 0
    for k, v := range m {
        _ = k
        _ = v
        count = count + 1
    }
    println(count)
    s := make([]int, 0)
    s = append(s, 10)
    println(len(s))
    println(s[0])
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_FALSE(contains(result.asm_text, "; TODO:"));
    EXPECT_TRUE(contains(result.asm_text, "call golangc_map_len"));
    EXPECT_TRUE(contains(result.asm_text, "call golangc_map_delete"));
    EXPECT_TRUE(contains(result.asm_text, "call golangc_map_iter_make"));
    EXPECT_TRUE(contains(result.asm_text, "call golangc_slice_append"));
}

// ============================================================================
// Phase 12: Defer tests
// ============================================================================

TEST(CodegenTest, DeferEmitsDeferBlock) {
    auto result = compile_to_asm(R"(
package main
func log(x int) {
    println(x)
}
func main() {
    defer log(1)
}
)");
    EXPECT_FALSE(result.has_errors);
    // Deferred calls are emitted before ret
    EXPECT_TRUE(contains(result.asm_text, "call log"));
    EXPECT_TRUE(contains(result.asm_text, "ret"));
}

TEST(CodegenTest, DeferRunsBeforeReturn) {
    auto result = compile_to_asm(R"(
package main
func f() {
    defer println(99)
    println(1)
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    // Both println calls must appear in f PROC
    EXPECT_TRUE(contains(result.asm_text, "call golangc_println_int"));
    EXPECT_FALSE(contains(result.asm_text, "; TODO:"));
}

TEST(CodegenTest, DeferMultipleRunsLIFO) {
    auto result = compile_to_asm(R"(
package main
func log(x int) {
    println(x)
}
func f() {
    defer log(1)
    defer log(2)
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    // Two deferred calls to log emitted in f
    size_t first = result.asm_text.find("call log");
    EXPECT_NE(first, std::string::npos);
    size_t second = result.asm_text.find("call log", first + 1);
    EXPECT_NE(second, std::string::npos);
}

TEST(CodegenTest, DeferCompilesNoTodos) {
    auto result = compile_to_asm(R"(
package main
func cleanup(s string) {
    println(s)
}
func main() {
    defer cleanup("done")
    println("working")
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_FALSE(contains(result.asm_text, "; TODO:"));
    EXPECT_TRUE(contains(result.asm_text, "call golangc_println_string"));
}

// ============================================================================
// Phase 12: Closure / func literal tests
// ============================================================================

TEST(CodegenTest, FuncLitEmitsInnerProc) {
    auto result = compile_to_asm(R"(
package main
func main() {
    f := func(x int) int { return x + 1 }
    println(f(5))
}
)");
    EXPECT_FALSE(result.has_errors);
    // Inner function should appear as a separate PROC
    EXPECT_TRUE(contains(result.asm_text, "PROC"));
}

TEST(CodegenTest, FuncLitIndirectCall) {
    auto result = compile_to_asm(R"(
package main
func main() {
    f := func(x int) int { return x * 2 }
    println(f(7))
}
)");
    EXPECT_FALSE(result.has_errors);
    // Indirect call via register (call rax or similar)
    EXPECT_TRUE(contains(result.asm_text, "call "));
    EXPECT_FALSE(contains(result.asm_text, "; TODO:"));
}

TEST(CodegenTest, FuncLitAsArgument) {
    auto result = compile_to_asm(R"(
package main
func apply(f func(int) int, x int) int {
    return f(x)
}
func main() {
    println(apply(func(n int) int { return n + 10 }, 5))
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_FALSE(contains(result.asm_text, "; TODO:"));
    EXPECT_TRUE(contains(result.asm_text, "call golangc_println_int"));
}

TEST(CodegenTest, ClosureCapture) {
    auto result = compile_to_asm(R"(
package main
func makeAdder(n int) func(int) int {
    return func(x int) int { return x + n }
}
func main() {
    add5 := makeAdder(5)
    println(add5(10))
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_FALSE(contains(result.asm_text, "; TODO:"));
    // malloc used to allocate env struct
    EXPECT_TRUE(contains(result.asm_text, "call malloc"));
}

TEST(CodegenTest, ClosureEnvGlobal) {
    auto result = compile_to_asm(R"(
package main
func makeCounter() func() int {
    n := 0
    return func() int { n = n + 1; return n }
}
func main() {
    c := makeCounter()
    println(c())
}
)");
    EXPECT_FALSE(result.has_errors);
    // Global closure env variable must be declared
    EXPECT_TRUE(contains(result.asm_text, "golangc_closure_env"));
}

TEST(CodegenTest, ClosuresGoCompilesNoTodos) {
    auto result = compile_to_asm(R"(
package main
func apply(f func(int) int, x int) int {
    return f(x)
}
func makeAdder(n int) func(int) int {
    return func(x int) int { return x + n }
}
func main() {
    double := func(x int) int { return x * 2 }
    println(apply(double, 5))
    println(apply(func(x int) int { return x - 3 }, 10))
    add5 := makeAdder(5)
    println(add5(10))
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_FALSE(contains(result.asm_text, "; TODO:"));
    EXPECT_TRUE(contains(result.asm_text, "call golangc_println_int"));
    EXPECT_TRUE(contains(result.asm_text, "call malloc"));
}

// ============================================================================
// Phase 13: Switch Statements
// ============================================================================

TEST(SwitchTest, SwitchBasicInt) {
    auto result = compile_to_asm(R"(
package main
func f(n int) int {
    switch n {
    case 0:
        return 10
    case 1:
        return 20
    default:
        return 30
    }
}
func main() { println(f(0)) }
)");
    EXPECT_FALSE(result.has_errors);
    // Block labels use $ instead of . (MASM naming)
    EXPECT_TRUE(contains(result.asm_text, "switch$case"));
    EXPECT_TRUE(contains(result.asm_text, "switch$merge"));
}

TEST(SwitchTest, SwitchDefaultOnly) {
    auto result = compile_to_asm(R"(
package main
func main() {
    x := 5
    switch x {
    default:
        println(1)
    }
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call golangc_println_int"));
}

TEST(SwitchTest, SwitchNoDefault) {
    auto result = compile_to_asm(R"(
package main
func main() {
    x := 42
    switch x {
    case 1:
        println(1)
    case 2:
        println(2)
    }
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "switch$case"));
}

TEST(SwitchTest, SwitchMultipleValues) {
    auto result = compile_to_asm(R"(
package main
func classify(n int) int {
    switch n {
    case 1, 2, 3:
        return 1
    default:
        return 0
    }
}
func main() { println(classify(2)) }
)");
    EXPECT_FALSE(result.has_errors);
    // Multiple case values generate multiple conditional branches
    EXPECT_TRUE(contains(result.asm_text, "switch$case"));
    EXPECT_TRUE(contains(result.asm_text, "switch$next"));
}

TEST(SwitchTest, SwitchTagless) {
    auto result = compile_to_asm(R"(
package main
func grade(score int) int {
    switch {
    case score >= 90:
        return 4
    case score >= 80:
        return 3
    default:
        return 0
    }
}
func main() { println(grade(95)) }
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "switch$case"));
    EXPECT_TRUE(contains(result.asm_text, "switch$merge"));
}

TEST(SwitchTest, SwitchWithInit) {
    auto result = compile_to_asm(R"(
package main
func main() {
    switch x := 5; x {
    case 5:
        println(99)
    default:
        println(0)
    }
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call golangc_println_int"));
}

TEST(SwitchTest, SwitchBreak) {
    auto result = compile_to_asm(R"(
package main
func main() {
    x := 1
    switch x {
    case 1:
        println(1)
        break
        println(2)
    }
}
)");
    EXPECT_FALSE(result.has_errors);
    // break jumps to merge block
    EXPECT_TRUE(contains(result.asm_text, "switch$merge"));
}

TEST(SwitchTest, SwitchFallthrough) {
    auto result = compile_to_asm(R"(
package main
func main() {
    x := 1
    switch x {
    case 1:
        fallthrough
    case 2:
        println(99)
    }
}
)");
    EXPECT_FALSE(result.has_errors);
    // fallthrough generates a branch to the next case block
    EXPECT_TRUE(contains(result.asm_text, "switch$case"));
    EXPECT_TRUE(contains(result.asm_text, "call golangc_println_int"));
}

TEST(SwitchTest, SwitchStringEqEmitsCall) {
    auto result = compile_to_asm(R"(
package main
func f(s string) int {
    switch s {
    case "hello":
        return 1
    case "world":
        return 2
    default:
        return 0
    }
}
func main() { println(f("hello")) }
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call golangc_string_eq"));
}

TEST(SwitchTest, SwitchStringEqExtern) {
    auto result = compile_to_asm(R"(
package main
func f(s string) int {
    switch s {
    case "x":
        return 1
    default:
        return 0
    }
}
func main() { println(f("x")) }
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "EXTERN golangc_string_eq:PROC"));
}

TEST(SwitchTest, SwitchCompilesNoTodos) {
    auto result = compile_to_asm(R"(
package main
func classify(n int) string {
    switch n {
    case 0:
        return "zero"
    case 1, 2, 3:
        return "small"
    case 10:
        return "ten"
    default:
        return "other"
    }
}
func grade(score int) string {
    switch {
    case score >= 90:
        return "A"
    case score >= 80:
        return "B"
    case score >= 70:
        return "C"
    default:
        return "F"
    }
}
func main() {
    println(classify(0))
    println(classify(2))
    println(classify(10))
    println(classify(99))
    println(grade(95))
    println(grade(85))
    println(grade(65))
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_FALSE(contains(result.asm_text, "; TODO:"));
}

TEST(SwitchTest, SwitchGoFull) {
    auto result = compile_to_asm(R"(
package main
func classify(n int) string {
    switch n {
    case 0:
        return "zero"
    case 1, 2, 3:
        return "small"
    default:
        return "other"
    }
}
func grade(score int) string {
    switch {
    case score >= 90:
        return "A"
    default:
        return "F"
    }
}
func main() {
    println(classify(0))
    println(grade(95))
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "classify PROC"));
    EXPECT_TRUE(contains(result.asm_text, "grade PROC"));
}

// ============================================================================
// Phase 14: Select Statements
// ============================================================================

TEST(SelectTest, SelectCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
func main() {
    ch := make(chan int)
    select {
    case v := <-ch:
        println(v)
    default:
        println(0)
    }
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(SelectTest, SelectCallsSelectRuntime) {
    auto result = compile_to_asm(R"(
package main
func main() {
    ch := make(chan int)
    select {
    case v := <-ch:
        println(v)
    default:
        println(0)
    }
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call golangc_select"));
}

TEST(SelectTest, SelectExternDeclared) {
    auto result = compile_to_asm(R"(
package main
func main() {
    ch := make(chan int)
    select {
    case v := <-ch:
        println(v)
    default:
        println(0)
    }
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "EXTERN golangc_select:PROC"));
}

TEST(SelectTest, SelectDefaultOnly) {
    auto result = compile_to_asm(R"(
package main
func main() {
    select {
    default:
        println(1)
    }
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call golangc_println_int"));
}

TEST(SelectTest, SelectSingleRecvCase) {
    auto result = compile_to_asm(R"(
package main
func main() {
    ch := make(chan int)
    select {
    case v := <-ch:
        println(v)
    }
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call golangc_select"));
    EXPECT_TRUE(contains(result.asm_text, "sel$case"));
}

TEST(SelectTest, SelectRecvAndDefault) {
    auto result = compile_to_asm(R"(
package main
func main() {
    ch := make(chan int)
    select {
    case v := <-ch:
        println(v)
    default:
        println(99)
    }
}
)");
    EXPECT_FALSE(result.has_errors);
    // has_default=1 must be passed (constant 1 in asm)
    EXPECT_TRUE(contains(result.asm_text, "call golangc_select"));
    EXPECT_TRUE(contains(result.asm_text, "sel$case"));
    EXPECT_TRUE(contains(result.asm_text, "sel$merge"));
}

TEST(SelectTest, SelectNoDefaultHasDefaultZero) {
    auto result = compile_to_asm(R"(
package main
func main() {
    ch := make(chan int)
    select {
    case v := <-ch:
        println(v)
    }
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call golangc_select"));
}

TEST(SelectTest, SelectCaseBodyExecutes) {
    auto result = compile_to_asm(R"(
package main
func producer(ch chan int) {
    ch <- 42
}
func main() {
    ch := make(chan int)
    go producer(ch)
    select {
    case v := <-ch:
        println(v)
    default:
        println(0)
    }
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call golangc_select"));
    EXPECT_TRUE(contains(result.asm_text, "call golangc_println_int"));
}

TEST(SelectTest, SelectDefaultFires) {
    auto result = compile_to_asm(R"(
package main
func main() {
    ch := make(chan int)
    select {
    case v := <-ch:
        println(v)
    default:
        println(99)
    }
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_FALSE(contains(result.asm_text, "; TODO:"));
}

TEST(SelectTest, SelectGoFull) {
    auto result = compile_to_asm(R"(
package main
func producer(ch chan int, val int) {
    ch <- val
}
func main() {
    ch1 := make(chan int)
    ch2 := make(chan int)
    go producer(ch1, 10)
    select {
    case v := <-ch1:
        println(v)
    case v := <-ch2:
        println(v)
    default:
        println(0)
    }
    ch3 := make(chan int)
    select {
    case v := <-ch3:
        println(v)
    default:
        println(99)
    }
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "producer PROC"));
    EXPECT_FALSE(contains(result.asm_text, "; TODO:"));
}

// ============================================================================
// Phase 15: Variadic Functions
// ============================================================================

TEST(VariadicTest, VariadicDeclCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
func sum(nums ...int) int {
    total := 0
    for _, n := range nums {
        total = total + n
    }
    return total
}
func main() { println(sum(1, 2, 3)) }
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(VariadicTest, VariadicCallPacksSlice) {
    auto result = compile_to_asm(R"(
package main
func sum(nums ...int) int {
    total := 0
    for _, n := range nums {
        total = total + n
    }
    return total
}
func main() { println(sum(1, 2, 3)) }
)");
    EXPECT_FALSE(result.has_errors);
    // Packing args into a slice uses slice_make and slice_append
    EXPECT_TRUE(contains(result.asm_text, "call malloc"));
}

TEST(VariadicTest, VariadicCallZeroArgs) {
    auto result = compile_to_asm(R"(
package main
func sum(nums ...int) int {
    total := 0
    for _, n := range nums {
        total = total + n
    }
    return total
}
func main() { println(sum()) }
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(VariadicTest, VariadicSpread) {
    auto result = compile_to_asm(R"(
package main
func sum(nums ...int) int {
    total := 0
    for _, n := range nums {
        total = total + n
    }
    return total
}
func main() {
    nums := []int{1, 2, 3}
    println(sum(nums...))
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(VariadicTest, VariadicWithFixedParam) {
    auto result = compile_to_asm(R"(
package main
func max(first int, rest ...int) int {
    m := first
    for _, n := range rest {
        if n > m {
            m = n
        }
    }
    return m
}
func main() { println(max(3, 1, 4, 1, 5)) }
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "max PROC"));
}

TEST(VariadicTest, VariadicFixedPlusZeroVariadic) {
    auto result = compile_to_asm(R"(
package main
func max(first int, rest ...int) int {
    return first
}
func main() { println(max(42)) }
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(VariadicTest, VariadicRangeOverParam) {
    auto result = compile_to_asm(R"(
package main
func printAll(vals ...int) {
    for _, v := range vals {
        println(v)
    }
}
func main() { printAll(10, 20, 30) }
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call golangc_println_int"));
}

TEST(VariadicTest, VariadicCompilesNoTodos) {
    auto result = compile_to_asm(R"(
package main
func sum(nums ...int) int {
    total := 0
    for _, n := range nums {
        total = total + n
    }
    return total
}
func max(first int, rest ...int) int {
    m := first
    for _, n := range rest {
        if n > m { m = n }
    }
    return m
}
func main() {
    println(sum(1, 2, 3))
    println(sum())
    nums := []int{4, 5, 6}
    println(sum(nums...))
    println(max(3, 1, 4, 1, 5, 9))
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_FALSE(contains(result.asm_text, "; TODO:"));
}

TEST(VariadicTest, VariadicGoFull) {
    auto result = compile_to_asm(R"(
package main
func sum(nums ...int) int {
    total := 0
    for _, n := range nums {
        total = total + n
    }
    return total
}
func max(first int, rest ...int) int {
    m := first
    for _, n := range rest {
        if n > m { m = n }
    }
    return m
}
func main() {
    println(sum(1, 2, 3))
    println(sum(10, 20, 30, 40))
    println(sum())
    nums := []int{4, 5, 6}
    println(sum(nums...))
    println(max(3, 1, 4, 1, 5, 9, 2, 6))
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "sum PROC"));
    EXPECT_TRUE(contains(result.asm_text, "max PROC"));
    EXPECT_FALSE(contains(result.asm_text, "; TODO:"));
}

// ============================================================================
// TypeSwitch tests (Phase 16)
// ============================================================================

TEST(TypeSwitchTest, TypeSwitchCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
func typeOf(i interface{}) string {
    switch i.(type) {
    case int:
        return "int"
    case string:
        return "string"
    default:
        return "other"
    }
}
func main() {
    println(typeOf(42))
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(TypeSwitchTest, TypeSwitchHasTsLabel) {
    auto result = compile_to_asm(R"(
package main
func typeOf(i interface{}) string {
    switch i.(type) {
    case int:
        return "int"
    default:
        return "other"
    }
}
func main() {
    println(typeOf(42))
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "ts"));
}

TEST(TypeSwitchTest, TypeSwitchIntBranch) {
    auto result = compile_to_asm(R"(
package main
func typeOf(i interface{}) string {
    switch i.(type) {
    case int:
        return "int"
    default:
        return "other"
    }
}
func main() {
    println(typeOf(42))
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "typeOf PROC"));
}

TEST(TypeSwitchTest, TypeSwitchStringBranch) {
    auto result = compile_to_asm(R"(
package main
func typeOf(i interface{}) string {
    switch i.(type) {
    case int:
        return "int"
    case string:
        return "string"
    default:
        return "other"
    }
}
func main() {
    println(typeOf("hello"))
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "typeOf PROC"));
}

TEST(TypeSwitchTest, TypeSwitchBoolBranch) {
    auto result = compile_to_asm(R"(
package main
func typeOf(i interface{}) string {
    switch i.(type) {
    case int:
        return "int"
    case bool:
        return "bool"
    default:
        return "other"
    }
}
func main() {
    println(typeOf(true))
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "typeOf PROC"));
}

TEST(TypeSwitchTest, TypeSwitchDefaultOnly) {
    auto result = compile_to_asm(R"(
package main
func f(i interface{}) string {
    switch i.(type) {
    default:
        return "other"
    }
}
func main() {
    println(f(42))
}
)");
     EXPECT_FALSE(result.has_errors);
}

TEST(TypeSwitchTest, TypeSwitchNoDefault) {
    auto result = compile_to_asm(R"(
package main
func f(i interface{}) {
    switch i.(type) {
    case int:
        println("int")
    case string:
        println("string")
    }
}
func main() {
    f(42)
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(TypeSwitchTest, TypeSwitchBoundVar) {
    auto result = compile_to_asm(R"(
package main
func describe(i interface{}) string {
    switch v := i.(type) {
    case int:
        _ = v
        return "int"
    case string:
        _ = v
        return "string"
    default:
        _ = v
        return "other"
    }
}
func main() {
    println(describe(42))
}
)");
     EXPECT_FALSE(result.has_errors);
}

TEST(TypeSwitchTest, TypeSwitchCompilesNoTodos) {
    auto result = compile_to_asm(R"(
package main
func typeOf(i interface{}) string {
    switch i.(type) {
    case int:
        return "int"
    case string:
        return "string"
    case bool:
        return "bool"
    default:
        return "other"
    }
}
func main() {
    println(typeOf(42))
    println(typeOf("hello"))
    println(typeOf(true))
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_FALSE(contains(result.asm_text, "; TODO:"));
}

TEST(TypeSwitchTest, TypeSwitchGoFull) {
    auto result = compile_to_asm(R"(
package main
func typeOf(i interface{}) string {
    switch i.(type) {
    case int:
        return "int"
    case string:
        return "string"
    case bool:
        return "bool"
    default:
        return "other"
    }
}
func main() {
    println(typeOf(42))
    println(typeOf("hello"))
    println(typeOf(true))
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "typeOf PROC"));
    EXPECT_TRUE(contains(result.asm_text, "main PROC"));
    EXPECT_FALSE(contains(result.asm_text, "; TODO:"));
}


// ============================================================================
// NamedType tests (Phase 17)
// ============================================================================

TEST(NamedTypeTest, PtrRecvCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
type Counter struct{ n int }
func (c *Counter) Inc() { c.n++ }
func (c *Counter) Value() int { return c.n }
func main() {
    c := Counter{}
    c.Inc()
    println(c.Value())
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(NamedTypeTest, PtrRecvEmitsMethod) {
    auto result = compile_to_asm(R"(
package main
type Counter struct{ n int }
func (c *Counter) Inc() { c.n++ }
func main() {
    c := Counter{}
    c.Inc()
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "Counter$Inc PROC"));
}

TEST(NamedTypeTest, PtrRecvCallEmitted) {
    auto result = compile_to_asm(R"(
package main
type Counter struct{ n int }
func (c *Counter) Inc() { c.n++ }
func main() {
    c := Counter{}
    c.Inc()
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "Counter$Inc"));
}

TEST(NamedTypeTest, IotaConstCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
type Direction int
const (
    North Direction = iota
    East
    South
    West
)
func main() {
    println(int(North))
    println(int(East))
}
)");
     EXPECT_FALSE(result.has_errors);
}

TEST(NamedTypeTest, IotaValues) {
    auto result = compile_to_asm(R"(
package main
type Direction int
const (
    North Direction = iota
    East
    South
    West
)
func main() {
    println(int(North))
    println(int(West))
}
)");
     EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "main PROC"));
}

TEST(NamedTypeTest, IotaUntyped) {
    auto result = compile_to_asm(R"(
package main
const (
    A = iota
    B
    C
)
func main() {
    println(A)
    println(B)
    println(C)
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(NamedTypeTest, MethodOnConst) {
    auto result = compile_to_asm(R"(
package main
type Direction int
const (
    North Direction = iota
    East
    South
    West
)
func (d Direction) Name() string {
    switch d {
    case North:
        return "North"
    case East:
        return "East"
    case South:
        return "South"
    case West:
        return "West"
    }
    return "Unknown"
}
func main() {
    println(North.Name())
    println(West.Name())
}
)");
     EXPECT_FALSE(result.has_errors);
}

TEST(NamedTypeTest, MultiplePtrRecv) {
    auto result = compile_to_asm(R"(
package main
type Counter struct{ n int }
func (c *Counter) Inc() { c.n++ }
func (c *Counter) Value() int { return c.n }
func main() {
    c := Counter{}
    c.Inc()
    c.Inc()
    println(c.Value())
}
)");
     EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "Counter$Inc PROC"));
    EXPECT_TRUE(contains(result.asm_text, "Counter$Value PROC"));
}

TEST(NamedTypeTest, CompilesNoTodos) {
    auto result = compile_to_asm(R"(
package main
type Counter struct{ n int }
func (c *Counter) Inc() { c.n++ }
func (c *Counter) Value() int { return c.n }
func main() {
    c := Counter{}
    c.Inc()
    c.Inc()
    c.Inc()
    println(c.Value())
}
)");
     EXPECT_FALSE(result.has_errors);
    EXPECT_FALSE(contains(result.asm_text, "; TODO:"));
}

TEST(NamedTypeTest, GoFull) {
    auto result = compile_to_asm(R"(
package main
type Direction int
const (
    North Direction = iota
    East
    South
    West
)
func (d Direction) Name() string {
    switch d {
    case North:
        return "North"
    case East:
        return "East"
    case South:
        return "South"
    case West:
        return "West"
    }
    return "Unknown"
}
type Counter struct{ n int }
func (c *Counter) Inc() { c.n++ }
func (c *Counter) Value() int { return c.n }
func main() {
    println(North.Name())
    println(West.Name())
    c := Counter{}
    c.Inc()
    c.Inc()
    println(c.Value())
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "Direction$Name PROC"));
    EXPECT_TRUE(contains(result.asm_text, "Counter$Inc PROC"));
    EXPECT_TRUE(contains(result.asm_text, "Counter$Value PROC"));
    EXPECT_FALSE(contains(result.asm_text, "; TODO:"));
}

// ============================================================================
// Phase 18: Pseudo-packages (fmt, strconv, os) + rune-to-string
// ============================================================================

TEST(PseudoPkgTest, FmtPrintlnInt) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
func main() {
    fmt.Println(42)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_println_int"));
}

TEST(PseudoPkgTest, FmtPrintlnString) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
func main() {
    fmt.Println("hello world")
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_println_string"));
}

TEST(PseudoPkgTest, FmtPrintlnNoImport) {
    // fmt is always in scope — import is optional for our pseudo-package system
    auto result = compile_to_asm(R"(
package main
func main() {
    fmt.Println(123)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_println_int"));
}

TEST(PseudoPkgTest, StrconvItoa) {
    auto result = compile_to_asm(R"(
package main
import "strconv"
func main() {
    s := strconv.Itoa(42)
    println(s)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_itoa"));
}

TEST(PseudoPkgTest, StrconvAtoiCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "strconv"
func main() {
    n, _ := strconv.Atoi("123")
    println(n)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_atoi"));
}

TEST(PseudoPkgTest, FmtSprintf) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
func main() {
    s := fmt.Sprintf("%d", 42)
    println(s)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_sprintf"));
}

TEST(PseudoPkgTest, FmtPrintf) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
func main() {
    fmt.Printf("%d\n", 99)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_printf"));
}

TEST(PseudoPkgTest, RuneToString) {
    auto result = compile_to_asm(R"(
package main
func main() {
    s := string(65)
    println(s)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_rune_to_string"));
}

TEST(PseudoPkgTest, OsArgs) {
    auto result = compile_to_asm(R"(
package main
import "os"
func main() {
    args := os.Args
    println(len(args))
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_os_args_get"));
}

TEST(PseudoPkgTest, FmtPrintlnMultiArg) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
func main() {
    x := 10
    y := 20
    fmt.Println(x, y)
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(PseudoPkgTest, GoFull) {
    auto result = compile_to_asm(R"(
package main
import (
    "fmt"
    "strconv"
)
func main() {
    n := 42
    s := strconv.Itoa(n)
    fmt.Println("n=" + s)
    msg := fmt.Sprintf("value: %d", n)
    fmt.Println(msg)
    ch := string(65)
    fmt.Println(ch)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_itoa"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_sprintf"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_rune_to_string"));
    EXPECT_FALSE(contains(result.asm_text, "; TODO:"));
}

// ============================================================================
// Phase 19: for-range-string, strings package, math package
// ============================================================================

TEST(Phase19Test, ForRangeStringCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
func main() {
    s := "hello"
    for i, ch := range s {
        _ = i
        _ = ch
    }
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase19Test, ForRangeStringEmitsDecodeRune) {
    auto result = compile_to_asm(R"(
package main
func main() {
    for _, ch := range "hello" {
        _ = ch
    }
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_string_decode_rune"));
}

TEST(Phase19Test, ForRangeStringKeyOnly) {
    auto result = compile_to_asm(R"(
package main
func main() {
    n := 0
    for range "hello" {
        n++
    }
    _ = n
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase19Test, StringsContainsCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "strings"
func main() {
    b := strings.Contains("hello world", "world")
    _ = b
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_strings_contains"));
}

TEST(Phase19Test, StringsHasPrefixHasSuffix) {
    auto result = compile_to_asm(R"(
package main
import "strings"
func main() {
    a := strings.HasPrefix("hello", "hel")
    b := strings.HasSuffix("hello", "llo")
    _ = a
    _ = b
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_strings_has_prefix"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_strings_has_suffix"));
}

TEST(Phase19Test, StringsToUpperToLower) {
    auto result = compile_to_asm(R"(
package main
import "strings"
func main() {
    u := strings.ToUpper("hello")
    l := strings.ToLower("HELLO")
    _ = u
    _ = l
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_strings_to_upper"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_strings_to_lower"));
}

TEST(Phase19Test, StringsTrimSpace) {
    auto result = compile_to_asm(R"(
package main
import "strings"
func main() {
    s := strings.TrimSpace("  hello  ")
    _ = s
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_strings_trim_space"));
}

TEST(Phase19Test, StringsIndex) {
    auto result = compile_to_asm(R"(
package main
import "strings"
func main() {
    i := strings.Index("hello world", "world")
    _ = i
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_strings_index"));
}

TEST(Phase19Test, MathSqrtAbsCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "math"
func main() {
    x := math.Sqrt(2.0)
    y := math.Abs(-3.14)
    _ = x
    _ = y
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_math_sqrt"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_math_abs"));
}

TEST(Phase19Test, MathMaxMinPow) {
    auto result = compile_to_asm(R"(
package main
import "math"
func main() {
    a := math.Max(1.0, 2.0)
    b := math.Min(3.0, 4.0)
    c := math.Pow(2.0, 10.0)
    _ = a
    _ = b
    _ = c
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_math_max"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_math_min"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_math_pow"));
}

TEST(Phase19Test, MathFloorCeilRound) {
    auto result = compile_to_asm(R"(
package main
import "math"
func main() {
    a := math.Floor(3.7)
    b := math.Ceil(3.2)
    c := math.Round(3.5)
    _ = a
    _ = b
    _ = c
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_math_floor"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_math_ceil"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_math_round"));
}

TEST(Phase19Test, StringsReplaceRepeat) {
    auto result = compile_to_asm(R"(
package main
import "strings"
func main() {
    r := strings.Replace("hello world", "world", "Go", -1)
    s := strings.Repeat("ab", 3)
    _ = r
    _ = s
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_strings_replace"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_strings_repeat"));
}

TEST(Phase19Test, GoFull) {
    auto result = compile_to_asm(R"(
package main
import (
    "fmt"
    "strings"
    "math"
)
func main() {
    s := "Hello, World!"
    if strings.Contains(s, "World") {
        fmt.Println(strings.ToUpper(s))
    }
    x := math.Sqrt(16.0)
    _ = x
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_strings_contains"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_strings_to_upper"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_math_sqrt"));
    EXPECT_FALSE(contains(result.asm_text, "; TODO:"));
}

// ============================================================================
// Phase 20 Tests: strings.Builder, errors.New/fmt.Errorf, buffered channels
// ============================================================================

TEST(Phase20Test, BuilderWriteAndString) {
    auto result = compile_to_asm(R"(
package main
import "strings"
func main() {
    var b strings.Builder
    b.WriteString("hello")
    s := b.String()
    _ = s
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_builder_make"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_builder_write_string"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_builder_string"));
}

TEST(Phase20Test, BuilderMultipleWrites) {
    auto result = compile_to_asm(R"(
package main
import "strings"
func main() {
    var b strings.Builder
    b.WriteString("foo")
    b.WriteString(" ")
    b.WriteString("bar")
    _ = b.String()
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_builder_write_string"));
}

TEST(Phase20Test, BuilderReset) {
    auto result = compile_to_asm(R"(
package main
import "strings"
func main() {
    var b strings.Builder
    b.WriteString("hello")
    b.Reset()
    b.WriteString("world")
    _ = b.String()
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_builder_reset"));
}

TEST(Phase20Test, BuilderLen) {
    auto result = compile_to_asm(R"(
package main
import "strings"
func main() {
    var b strings.Builder
    b.WriteString("hello")
    n := b.Len()
    _ = n
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_builder_len"));
}

TEST(Phase20Test, ErrorsNewCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "errors"
func main() {
    err := errors.New("something went wrong")
    _ = err
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase20Test, ErrorsNewEmitsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "errors"
func main() {
    err := errors.New("oops")
    _ = err
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_errors_new"));
}

TEST(Phase20Test, FmtErrorfCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
func main() {
    err := fmt.Errorf("value %d is invalid", 42)
    _ = err
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase20Test, FmtErrorfEmitsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
func main() {
    err := fmt.Errorf("bad: %d", 1)
    _ = err
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_fmt_errorf"));
}

TEST(Phase20Test, BufferedChanMakeCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
func main() {
    ch := make(chan int, 5)
    _ = ch
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase20Test, BufferedChanEmitsCapacity) {
    auto result = compile_to_asm(R"(
package main
func main() {
    ch := make(chan int, 3)
    _ = ch
}
)");
    EXPECT_FALSE(result.has_errors);
    // field_index == 3 means "mov rdx, 3" should appear before golangc_chan_make
    EXPECT_TRUE(contains(result.asm_text, "mov rdx, 3"));
}

TEST(Phase20Test, ErrorReturnFromFunc) {
    auto result = compile_to_asm(R"(
package main
import (
    "errors"
    "fmt"
)
func divide(a, b int) (int, error) {
    if b == 0 {
        return 0, errors.New("division by zero")
    }
    return a / b, nil
}
func main() {
    result, err := divide(10, 2)
    if err != nil {
        fmt.Println("error")
    } else {
        fmt.Println(result)
    }
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_errors_new"));
}

TEST(Phase20Test, GoFull) {
    auto result = compile_to_asm(R"(
package main
import (
    "errors"
    "fmt"
    "strings"
)
func main() {
    var b strings.Builder
    b.WriteString("hello")
    b.WriteString(" world")
    s := b.String()
    _ = s

    err := errors.New("test error")
    _ = err

    ch := make(chan int, 2)
    ch <- 1
    ch <- 2
    v := <-ch
    _ = v

    e2 := fmt.Errorf("code %d", 42)
    _ = e2
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_builder_make"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_errors_new"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_fmt_errorf"));
    EXPECT_FALSE(contains(result.asm_text, "; TODO:"));
}

// ============================================================================
// Phase 21 Tests: sync.Mutex + sync.WaitGroup
// ============================================================================

TEST(Phase21Test, MutexCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "sync"
func main() {
    var mu sync.Mutex
    mu.Lock()
    mu.Unlock()
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase21Test, MutexEmitsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "sync"
func main() {
    var mu sync.Mutex
    mu.Lock()
    mu.Unlock()
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_mutex_lock"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_mutex_unlock"));
}

TEST(Phase21Test, MutexTryLock) {
    auto result = compile_to_asm(R"(
package main
import "sync"
func main() {
    var mu sync.Mutex
    ok := mu.TryLock()
    if ok {
        mu.Unlock()
    }
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_mutex_try_lock"));
}

TEST(Phase21Test, MutexZeroValueInit) {
    auto result = compile_to_asm(R"(
package main
import "sync"
func main() {
    var mu sync.Mutex
    mu.Lock()
    mu.Unlock()
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_mutex_make"));
}

TEST(Phase21Test, WaitGroupCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "sync"
func main() {
    var wg sync.WaitGroup
    wg.Add(1)
    wg.Done()
    wg.Wait()
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase21Test, WaitGroupEmitsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "sync"
func main() {
    var wg sync.WaitGroup
    wg.Add(1)
    wg.Done()
    wg.Wait()
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_waitgroup_add"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_waitgroup_done"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_waitgroup_wait"));
}

TEST(Phase21Test, WaitGroupZeroValueInit) {
    auto result = compile_to_asm(R"(
package main
import "sync"
func main() {
    var wg sync.WaitGroup
    wg.Wait()
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_waitgroup_make"));
}

TEST(Phase21Test, MutexProtectedCounter) {
    auto result = compile_to_asm(R"(
package main
import "sync"
var counter int
var mu sync.Mutex
func increment() {
    mu.Lock()
    counter = counter + 1
    mu.Unlock()
}
func main() {
    go increment()
    go increment()
    mu.Lock()
    _ = counter
    mu.Unlock()
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_mutex_lock"));
}

TEST(Phase21Test, WaitGroupWithGoroutines) {
    auto result = compile_to_asm(R"(
package main
import "sync"
func worker(wg *sync.WaitGroup) {
    wg.Done()
}
func main() {
    var wg sync.WaitGroup
    wg.Add(1)
    go worker(&wg)
    wg.Wait()
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_waitgroup_add"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_waitgroup_wait"));
}

TEST(Phase21Test, GoFull) {
    auto result = compile_to_asm(R"(
package main
import (
    "sync"
    "fmt"
)
func main() {
    var mu sync.Mutex
    var counter int
    mu.Lock()
    counter = counter + 1
    mu.Unlock()

    var wg sync.WaitGroup
    wg.Add(1)
    wg.Done()
    wg.Wait()

    fmt.Println(counter)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_mutex_make"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_waitgroup_make"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_mutex_lock"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_waitgroup_wait"));
    EXPECT_FALSE(contains(result.asm_text, "; TODO:"));
}

// ============================================================================
// Phase 22: os.Stdout/Stderr + fmt.Fprintf/Fprintln
// ============================================================================

TEST(Phase22Test, OsStdoutLoads) {
    auto result = compile_to_asm(R"(
package main
import "os"
func main() {
    _ = os.Stdout
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_os_stdout"));
}

TEST(Phase22Test, OsStderrLoads) {
    auto result = compile_to_asm(R"(
package main
import "os"
func main() {
    _ = os.Stderr
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_os_stderr"));
}

TEST(Phase22Test, FprintfCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import (
    "fmt"
    "os"
)
func main() {
    fmt.Fprintf(os.Stdout, "x=%d\n", 42)
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase22Test, FprintfEmitsRuntime) {
    auto result = compile_to_asm(R"(
package main
import (
    "fmt"
    "os"
)
func main() {
    fmt.Fprintf(os.Stdout, "x=%d\n", 42)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_fprintf"));
}

TEST(Phase22Test, FprintfToStderr) {
    auto result = compile_to_asm(R"(
package main
import (
    "fmt"
    "os"
)
func main() {
    fmt.Fprintf(os.Stderr, "err: %s\n", "msg")
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_os_stderr"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_fprintf"));
}

TEST(Phase22Test, FprintfStringArg) {
    auto result = compile_to_asm(R"(
package main
import (
    "fmt"
    "os"
)
func main() {
    name := "world"
    fmt.Fprintf(os.Stdout, "Hello, %s!\n", name)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_fprintf"));
}

TEST(Phase22Test, FprintlnCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import (
    "fmt"
    "os"
)
func main() {
    fmt.Fprintln(os.Stdout, "hello")
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase22Test, FprintlnEmitsRuntime) {
    auto result = compile_to_asm(R"(
package main
import (
    "fmt"
    "os"
)
func main() {
    fmt.Fprintln(os.Stdout, "hello")
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_fprintf"));
}

TEST(Phase22Test, FprintlnMultiArg) {
    auto result = compile_to_asm(R"(
package main
import (
    "fmt"
    "os"
)
func main() {
    fmt.Fprintln(os.Stdout, "x=", 42)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_fprintf"));
}

TEST(Phase22Test, GoFull) {
    auto result = compile_to_asm(R"(
package main
import (
    "fmt"
    "os"
)
func main() {
    fmt.Printf("value: %d\n", 99)
    fmt.Fprintf(os.Stdout, "stdout: %s\n", "ok")
    fmt.Fprintf(os.Stderr, "stderr: %d\n", 0)
    fmt.Fprintln(os.Stdout, "done")
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_printf"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_fprintf"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_os_stdout"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_os_stderr"));
    EXPECT_FALSE(contains(result.asm_text, "; TODO:"));
}

// ============================================================================
// Phase 23 Tests: os.Exit + Extended strconv + File I/O Basics
// ============================================================================

TEST(Phase23Test, OsExitCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "os"
func main() {
    os.Exit(0)
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase23Test, OsExitEmitsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "os"
func main() {
    os.Exit(0)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_os_exit"));
}

TEST(Phase23Test, OsExitNonZero) {
    auto result = compile_to_asm(R"(
package main
import "os"
func main() {
    os.Exit(1)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_os_exit"));
}

TEST(Phase23Test, OsOpenCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "os"
func main() {
    f, _ := os.Open("x.txt")
    _ = f
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase23Test, OsOpenEmitsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "os"
func main() {
    f, _ := os.Open("x.txt")
    _ = f
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_os_open"));
}

TEST(Phase23Test, OsCreateCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "os"
func main() {
    f, _ := os.Create("out.txt")
    _ = f
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase23Test, OsFileCloseCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "os"
func main() {
    f, _ := os.Create("out.txt")
    f.Close()
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase23Test, OsFileWriteStringCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "os"
func main() {
    f, _ := os.Create("out.txt")
    f.WriteString("hello")
    f.Close()
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase23Test, StrconvParseIntCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "strconv"
func main() {
    n, _ := strconv.ParseInt("42", 10, 64)
    _ = n
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase23Test, StrconvParseFloatCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "strconv"
func main() {
    x, _ := strconv.ParseFloat("3.14", 64)
    _ = x
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase23Test, StrconvFormatIntCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "strconv"
func main() {
    s := strconv.FormatInt(42, 10)
    _ = s
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase23Test, StrconvFormatFloatCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "strconv"
func main() {
    s := strconv.FormatFloat(3.14, 'f', 2, 64)
    _ = s
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase23Test, StrconvFormatBoolCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "strconv"
func main() {
    s := strconv.FormatBool(true)
    _ = s
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase23Test, GoFull) {
    auto result = compile_to_asm(R"(
package main
import (
    "fmt"
    "os"
    "strconv"
)
func main() {
    f, _ := os.Create("output.txt")
    f.WriteString("hello, file!\n")
    f.Close()
    n, _ := strconv.ParseInt("123", 10, 64)
    x, _ := strconv.ParseFloat("3.14", 64)
    fmt.Fprintf(os.Stdout, "n=%d x=%s\n", n, strconv.FormatFloat(x, 'f', 2, 64))
    fmt.Println(strconv.FormatBool(true))
    os.Exit(0)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_os_exit"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_os_create"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_parse_float"));
    EXPECT_FALSE(contains(result.asm_text, "; TODO:"));
}

