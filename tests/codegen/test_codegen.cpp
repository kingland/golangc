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
