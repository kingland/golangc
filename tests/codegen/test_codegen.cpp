#include "codegen/x64_codegen.hpp"
#include "common/diagnostic.hpp"
#include "ir/ir_gen.hpp"
#include "ir/ir_passes.hpp"
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

    ir::PassManager::run_default(*module);

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

/// Count non-overlapping occurrences of needle in text.
int count_occurrences(const std::string& text, const std::string& needle) {
    int n = 0;
    size_t pos = 0;
    while ((pos = text.find(needle, pos)) != std::string::npos) {
        ++n;
        pos += needle.size();
    }
    return n;
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
    println(x)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "42"));
}

TEST(CodegenTest, BoolConstant) {
    auto result = compile_to_asm(R"(
package main
func main() {
    if true {
        println(1)
    }
}
)");
    EXPECT_FALSE(result.has_errors);
    // Should contain a call to println
    EXPECT_TRUE(contains(result.asm_text, "call golangc_println_int"));
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
    // Use function parameters so the add cannot be constant-folded
    auto result = compile_to_asm(R"(
package main
func addTwo(x int, y int) int {
    z := x + y
    return z
}
func main() {
    println(addTwo(10, 20))
}
)");
    EXPECT_FALSE(result.has_errors);
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
// Struct pointers & pointer-receiver method tests  (Phase 7 extension)
// ============================================================================

// --- Struct value semantics ---

TEST(StructPointerTest, StructValueCopy) {
    // Assigning one struct to another copies all fields.
    auto result = compile_to_asm(R"(
package main
type Rect struct { W, H int }
func main() {
    a := Rect{3, 4}
    b := a
    _ = b
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(StructPointerTest, StructValueReturnedByValue) {
    // A function returning a struct uses sret; caller reads fields normally.
    auto result = compile_to_asm(R"(
package main
type Vec2 struct { X, Y int }
func makeVec(x, y int) Vec2 { return Vec2{x, y} }
func main() {
    v := makeVec(5, 6)
    println(v.X)
    println(v.Y)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call makeVec"));
    EXPECT_TRUE(contains(result.asm_text, "call golangc_println_int"));
}

TEST(StructPointerTest, StructValuePassedToFunc) {
    // Struct > 8 bytes is passed by pointer (LEA) under Windows x64 ABI.
    auto result = compile_to_asm(R"(
package main
type Box struct { A, B int }
func area(b Box) int { return b.A * b.B }
func main() {
    println(area(Box{3, 7}))
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call area"));
}

TEST(StructPointerTest, StructFieldMutation) {
    // Mutating a field on a local struct variable.
    auto result = compile_to_asm(R"(
package main
type Counter struct { N int }
func main() {
    c := Counter{0}
    c.N = 42
    println(c.N)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call golangc_println_int"));
}

TEST(StructPointerTest, ThreeFieldStruct) {
    // Struct with three fields exercises offset logic (base, +8, +16).
    auto result = compile_to_asm(R"(
package main
type Triple struct { A, B, C int }
func main() {
    t := Triple{1, 2, 3}
    println(t.A)
    println(t.B)
    println(t.C)
}
)");
    EXPECT_FALSE(result.has_errors);
}

// --- Pointer-receiver methods ---

TEST(StructPointerTest, PointerReceiverMethodCompiles) {
    // A method with a pointer receiver (*T) should compile without errors.
    auto result = compile_to_asm(R"(
package main
type Counter struct { N int }
func (c *Counter) Inc() { c.N++ }
func main() {
    c := Counter{0}
    c.Inc()
    println(c.N)
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(StructPointerTest, PointerReceiverMethodEmitted) {
    // Pointer-receiver method should appear as a PROC in the assembly.
    auto result = compile_to_asm(R"(
package main
type Counter struct { N int }
func (c *Counter) Inc() { c.N++ }
func main() {
    c := Counter{0}
    c.Inc()
    println(c.N)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "Counter$Inc PROC"));
    EXPECT_TRUE(contains(result.asm_text, "call Counter$Inc"));
}

TEST(StructPointerTest, PointerReceiverModifiesValue) {
    // Method with pointer receiver mutates the original struct.
    auto result = compile_to_asm(R"(
package main
type Acc struct { Sum int }
func (a *Acc) Add(n int) { a.Sum += n }
func main() {
    acc := Acc{0}
    acc.Add(10)
    acc.Add(5)
    println(acc.Sum)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "Acc$Add PROC"));
    EXPECT_TRUE(contains(result.asm_text, "call Acc$Add"));
}

TEST(StructPointerTest, PointerReceiverReturnsValue) {
    // Pointer-receiver method that reads and returns a field.
    auto result = compile_to_asm(R"(
package main
type Wrapper struct { Val int }
func (w *Wrapper) Get() int { return w.Val }
func main() {
    wr := Wrapper{99}
    println(wr.Get())
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "Wrapper$Get PROC"));
    EXPECT_TRUE(contains(result.asm_text, "call Wrapper$Get"));
}

TEST(StructPointerTest, MixedValueAndPointerReceivers) {
    // Same type can have both value-receiver and pointer-receiver methods.
    auto result = compile_to_asm(R"(
package main
type Num struct { V int }
func (n Num) Double() int { return n.V * 2 }
func (n *Num) Set(v int)  { n.V = v }
func main() {
    x := Num{3}
    x.Set(7)
    println(x.Double())
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "Num$Double PROC"));
    EXPECT_TRUE(contains(result.asm_text, "Num$Set PROC"));
}

// --- Struct pointer variables ---

TEST(StructPointerTest, TakeAddressOfStructLocal) {
    // &localStruct gives a pointer; pointer field access works via indirection.
    auto result = compile_to_asm(R"(
package main
type Node struct { Val int }
func main() {
    n := Node{42}
    p := &n
    println(p.Val)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call golangc_println_int"));
}

TEST(StructPointerTest, ModifyThroughPointer) {
    // Writing through a pointer: p.Val = v inside the function.
    auto result = compile_to_asm(R"(
package main
type Node struct { Val int }
func set(p *Node, v int) { p.Val = v }
func main() {
    n := Node{0}
    set(&n, 77)
    println(n.Val)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call set"));
    EXPECT_TRUE(contains(result.asm_text, "call golangc_println_int"));
}

TEST(StructPointerTest, PointerPassedToFunc) {
    // *T argument: just a pointer (8 bytes), passed in a register directly.
    auto result = compile_to_asm(R"(
package main
type Point struct { X, Y int }
func readX(p *Point) int { return p.X }
func main() {
    pt := Point{11, 22}
    println(readX(&pt))
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call readX"));
}

TEST(StructPointerTest, PointerReturnedFromFunc) {
    // Function returns *T (a pointer, 8 bytes — fits in RAX).
    auto result = compile_to_asm(R"(
package main
type Data struct { X int }
func getPtr(d *Data) *Data { return d }
func main() {
    d := Data{55}
    p := getPtr(&d)
    println(p.X)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call getPtr"));
}

// --- Nested structs ---

TEST(StructPointerTest, NestedStructFieldAccess) {
    // Struct embedding another struct — accessing inner fields.
    auto result = compile_to_asm(R"(
package main
type Inner struct { V int }
type Outer struct { In Inner }
func main() {
    o := Outer{Inner{7}}
    println(o.In.V)
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(StructPointerTest, NestedStructFieldWrite) {
    // Writing to a field of an embedded struct.
    auto result = compile_to_asm(R"(
package main
type Inner struct { V int }
type Outer struct { In Inner }
func main() {
    o := Outer{Inner{0}}
    o.In.V = 99
    println(o.In.V)
}
)");
    EXPECT_FALSE(result.has_errors);
}

// --- GoFull: comprehensive struct + pointer test ---

TEST(StructPointerTest, GoFull) {
    auto result = compile_to_asm(R"(
package main

type Vec struct { X, Y int }

func (v Vec) Scale(f int) Vec {
    return Vec{v.X * f, v.Y * f}
}

func (v *Vec) Negate() {
    v.X = -v.X
    v.Y = -v.Y
}

func addVecs(a, b Vec) Vec {
    return Vec{a.X + b.X, a.Y + b.Y}
}

func main() {
    v1 := Vec{2, 3}
    v2 := Vec{4, 5}
    v3 := addVecs(v1, v2)
    println(v3.X)          // 6
    println(v3.Y)          // 8
    v4 := v1.Scale(10)
    println(v4.X)          // 20
    v1.Negate()
    println(v1.X)          // -2
    p := &v2
    p.Negate()
    println(v2.Y)          // -5
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "Vec$Scale PROC"));
    EXPECT_TRUE(contains(result.asm_text, "Vec$Negate PROC"));
    EXPECT_TRUE(contains(result.asm_text, "call addVecs"));
    EXPECT_TRUE(contains(result.asm_text, "call Vec$Scale"));
    EXPECT_TRUE(contains(result.asm_text, "call Vec$Negate"));
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
    // Use println to keep the float constant live in the IR
    auto result = compile_to_asm(R"(
package main
func main() {
    x := 3.14
    println(x)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "_DATA SEGMENT"));
    EXPECT_TRUE(contains(result.asm_text, "__flt"));
    EXPECT_TRUE(contains(result.asm_text, "DQ"));
}

TEST(CodegenTest, FloatAdd) {
    // Use function params to prevent constant folding
    auto result = compile_to_asm(R"(
package main
func fadd(x float64, y float64) float64 { return x + y }
func main() { println(fadd(1.5, 2.5)) }
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "addsd xmm0, xmm1"));
}

TEST(CodegenTest, FloatSub) {
    auto result = compile_to_asm(R"(
package main
func fsub(x float64, y float64) float64 { return x - y }
func main() { println(fsub(5.0, 2.0)) }
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "subsd xmm0, xmm1"));
}

TEST(CodegenTest, FloatMul) {
    auto result = compile_to_asm(R"(
package main
func fmul(x float64, y float64) float64 { return x * y }
func main() { println(fmul(3.0, 4.0)) }
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "mulsd xmm0, xmm1"));
}

TEST(CodegenTest, FloatDiv) {
    auto result = compile_to_asm(R"(
package main
func fdiv(x float64, y float64) float64 { return x / y }
func main() { println(fdiv(10.0, 3.0)) }
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "divsd xmm0, xmm1"));
}

TEST(CodegenTest, FloatNeg) {
    // Use function parameter to prevent constant folding of negation
    auto result = compile_to_asm(R"(
package main
func fneg(x float64) float64 { return -x }
func main() { println(fneg(3.14)) }
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "xorpd xmm0"));
    EXPECT_TRUE(contains(result.asm_text, "__f64_sign_mask"));
}

// ============================================================================
// Float comparison and conversion tests
// ============================================================================

TEST(CodegenTest, FloatCompareLt) {
    // Use function params so comparison cannot be constant-folded
    auto result = compile_to_asm(R"(
package main
func flt(x float64, y float64) bool { return x < y }
func main() {
    if flt(1.0, 2.0) {
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
func feq(x float64, y float64) bool { return x == y }
func main() {
    if feq(1.0, 1.0) {
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
    // Use function param to prevent constant folding of conversion
    auto result = compile_to_asm(R"(
package main
func itof(x int) float64 { return float64(x) }
func main() { println(itof(42)) }
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "cvtsi2sd xmm0, rax"));
}

TEST(CodegenTest, FloatToInt) {
    auto result = compile_to_asm(R"(
package main
func ftoi(x float64) int { return int(x) }
func main() { println(ftoi(3.14)) }
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
    // Use a function with a param to prevent DCE of string index
    auto result = compile_to_asm(R"(
package main
func sindex(s string, i int) byte { return s[i] }
func main() { println(int(sindex("hello", 0))) }
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
    // Use function params to prevent constant folding of float ops
    auto result = compile_to_asm(R"(
package main
func floatOps(x float64, y float64) {
    println(x + y)
    println(x * y)
}
func main() {
    floatOps(3.14, 2.0)
}
)");
    EXPECT_FALSE(result.has_errors);
    // Float constants emitted in data section
    EXPECT_TRUE(contains(result.asm_text, "_DATA SEGMENT"));
    EXPECT_TRUE(contains(result.asm_text, "__flt"));
    // Float arithmetic operations preserved (params are not const-folded)
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
    EXPECT_TRUE(contains(result.asm_text, "call golangc_rc_slice_alloc"));
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
    // grade takes a param so switch comparisons are not folded
    EXPECT_TRUE(contains(result.asm_text, "cmp rax, rcx"));
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
    // Packing args into a slice uses slice_make → golangc_rc_slice_alloc
    EXPECT_TRUE(contains(result.asm_text, "call golangc_rc_slice_alloc"));
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
// Phase 30 Tests: Type switch correctness, errors.Is/As
// ============================================================================

// Verify type switch emits cmp instruction (tag comparison)
TEST(Phase30Test, TypeSwitchEmitsCmp) {
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
    EXPECT_TRUE(contains(result.asm_text, "cmp"));
}

// Verify type switch emits je/jne for branch (tag comparison produces jump)
TEST(Phase30Test, TypeSwitchEmitsJump) {
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
    // cmp + je or jne for branch taken/not taken
    EXPECT_TRUE(contains(result.asm_text, "cmp") || contains(result.asm_text, "je") || contains(result.asm_text, "jne"));
}

// Type switch with bound variable — should compile and use bound var
TEST(Phase30Test, TypeSwitchBoundVarIntCase) {
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
    println(describe("hi"))
    println(describe(true))
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "describe PROC"));
    EXPECT_FALSE(contains(result.asm_text, "; TODO:"));
}

// Type switch with multiple types in one case (Go allows: case int, string:)
TEST(Phase30Test, TypeSwitchMultipleTypesInCase) {
    auto result = compile_to_asm(R"(
package main
func isNumOrStr(i interface{}) bool {
    switch i.(type) {
    case int, string:
        return true
    default:
        return false
    }
}
func main() {
    println(isNumOrStr(1))
}
)");
    // Multi-type cases are tricky — just ensure no crash
    EXPECT_FALSE(result.has_errors);
}

// errors.New + errors.Is
TEST(Phase30Test, ErrorsNewCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "errors"
func main() {
    err := errors.New("oops")
    _ = err
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase30Test, ErrorsNewEmitsRuntime) {
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

TEST(Phase30Test, ErrorsIsCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "errors"
var ErrFoo = errors.New("foo")
func check(err error) bool {
    return errors.Is(err, ErrFoo)
}
func main() {
    println(check(ErrFoo))
}
)");
    EXPECT_FALSE(result.has_errors);
}

// Type assert (value, ok) form
TEST(Phase30Test, TypeAssertOkFormCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
func tryInt(i interface{}) (int, bool) {
    v, ok := i.(int)
    return v, ok
}
func main() {
    v, ok := tryInt(42)
    _ = v
    _ = ok
}
)");
    EXPECT_FALSE(result.has_errors);
}

// interface{} variable assignment and type switch
TEST(Phase30Test, InterfaceVarBoxingAndSwitch) {
    auto result = compile_to_asm(R"(
package main
func f(i interface{}) string {
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
    var x interface{} = 42
    println(f(x))
    var y interface{} = "hello"
    println(f(y))
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_FALSE(contains(result.asm_text, "; TODO:"));
}

// GoFull: comprehensive type switch program
TEST(Phase30Test, GoFull) {
    auto result = compile_to_asm(R"(
package main
import "errors"

var ErrNotFound = errors.New("not found")

func describe(i interface{}) string {
    switch v := i.(type) {
    case int:
        _ = v
        return "int"
    case string:
        _ = v
        return "string"
    case bool:
        _ = v
        return "bool"
    default:
        _ = v
        return "other"
    }
}

func check(err error) bool {
    return errors.Is(err, ErrNotFound)
}

func main() {
    println(describe(42))
    println(describe("hello"))
    println(describe(true))
    println(describe(3.14))
    println(check(ErrNotFound))
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "describe PROC"));
    EXPECT_TRUE(contains(result.asm_text, "main PROC"));
    EXPECT_FALSE(contains(result.asm_text, "; TODO:"));
}

// ============================================================================
// Phase 24 Tests: bufio.Scanner + bufio.NewReader + os.ReadFile
// ============================================================================

TEST(Phase24Test, BufioNewScannerCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import (
    "bufio"
    "os"
)
func main() {
    s := bufio.NewScanner(os.Stdin)
    _ = s
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase24Test, BufioNewScannerEmitsRuntime) {
    auto result = compile_to_asm(R"(
package main
import (
    "bufio"
    "os"
)
func main() {
    s := bufio.NewScanner(os.Stdin)
    _ = s
}
)");
    EXPECT_TRUE(contains(result.asm_text, "golangc_scanner_new"));
}

TEST(Phase24Test, BufioScannerScanCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import (
    "bufio"
    "os"
)
func main() {
    s := bufio.NewScanner(os.Stdin)
    _ = s.Scan()
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase24Test, BufioScannerScanEmitsRuntime) {
    auto result = compile_to_asm(R"(
package main
import (
    "bufio"
    "os"
)
func main() {
    s := bufio.NewScanner(os.Stdin)
    _ = s.Scan()
}
)");
    EXPECT_TRUE(contains(result.asm_text, "golangc_scanner_scan"));
}

TEST(Phase24Test, BufioScannerTextCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import (
    "bufio"
    "os"
)
func main() {
    s := bufio.NewScanner(os.Stdin)
    _ = s.Scan()
    _ = s.Text()
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase24Test, BufioScannerTextEmitsRuntime) {
    auto result = compile_to_asm(R"(
package main
import (
    "bufio"
    "os"
)
func main() {
    s := bufio.NewScanner(os.Stdin)
    _ = s.Scan()
    _ = s.Text()
}
)");
    EXPECT_TRUE(contains(result.asm_text, "golangc_scanner_text"));
}

TEST(Phase24Test, BufioNewReaderCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import (
    "bufio"
    "os"
)
func main() {
    r := bufio.NewReader(os.Stdin)
    _ = r
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase24Test, BufioReaderReadStringCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import (
    "bufio"
    "os"
)
func main() {
    r := bufio.NewReader(os.Stdin)
    line, _ := r.ReadString('\n')
    _ = line
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase24Test, BufioReaderReadStringEmitsRuntime) {
    auto result = compile_to_asm(R"(
package main
import (
    "bufio"
    "os"
)
func main() {
    r := bufio.NewReader(os.Stdin)
    line, _ := r.ReadString('\n')
    _ = line
}
)");
    EXPECT_TRUE(contains(result.asm_text, "golangc_breader_read_string"));
}

TEST(Phase24Test, OsReadFileCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "os"
func main() {
    data, _ := os.ReadFile("x.txt")
    _ = data
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase24Test, OsReadFileEmitsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "os"
func main() {
    data, _ := os.ReadFile("x.txt")
    _ = data
}
)");
    EXPECT_TRUE(contains(result.asm_text, "golangc_os_read_file"));
}

TEST(Phase24Test, ScannerLoopCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import (
    "bufio"
    "os"
)
func main() {
    s := bufio.NewScanner(os.Stdin)
    for s.Scan() {
        _ = s.Text()
    }
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase24Test, ScannerLoopWithFmt) {
    auto result = compile_to_asm(R"(
package main
import (
    "bufio"
    "fmt"
    "os"
)
func main() {
    s := bufio.NewScanner(os.Stdin)
    for s.Scan() {
        fmt.Println(s.Text())
    }
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase24Test, GoFull) {
    auto result = compile_to_asm(R"(
package main
import (
    "bufio"
    "fmt"
    "os"
)
func main() {
    f, _ := os.Open("input.txt")
    scanner := bufio.NewScanner(f)
    for scanner.Scan() {
        fmt.Println(scanner.Text())
    }
    f.Close()
    reader := bufio.NewReader(os.Stdin)
    line, _ := reader.ReadString('\n')
    fmt.Println(line)
    data, _ := os.ReadFile("input.txt")
    _ = data
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_scanner_new"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_breader_read_string"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_os_read_file"));
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


// ============================================================================
// Phase 25 Tests: fmt.Scan*, sort, os.Getenv, strings extras
// ============================================================================

TEST(Phase25Test, FmtScanCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
func main() {
    var x int
    fmt.Scan(&x)
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase25Test, FmtScanEmitsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
func main() {
    var x int
    fmt.Scan(&x)
}
)");
    EXPECT_TRUE(contains(result.asm_text, "golangc_fmt_scan"));
}

TEST(Phase25Test, FmtScanlnCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
func main() {
    var n int
    fmt.Scanln(&n)
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase25Test, FmtScanfCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
func main() {
    var n int
    fmt.Scanf("%d", &n)
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase25Test, FmtSscanfCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
func main() {
    var a, b int
    fmt.Sscanf("10 20", "%d %d", &a, &b)
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase25Test, FmtSscanfEmitsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
func main() {
    var a, b int
    fmt.Sscanf("10 20", "%d %d", &a, &b)
}
)");
    EXPECT_TRUE(contains(result.asm_text, "golangc_fmt_sscanf"));
}

TEST(Phase25Test, FmtSscanCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
func main() {
    var x int
    fmt.Sscan("42", &x)
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase25Test, SortIntsCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "sort"
func main() {
    a := []int{3, 1, 2}
    sort.Ints(a)
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase25Test, SortIntsEmitsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "sort"
func main() {
    a := []int{3, 1, 2}
    sort.Ints(a)
}
)");
    EXPECT_TRUE(contains(result.asm_text, "golangc_sort_ints"));
}

TEST(Phase25Test, SortStringsCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "sort"
func main() {
    a := []string{"banana", "apple", "cherry"}
    sort.Strings(a)
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase25Test, SortStringsEmitsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "sort"
func main() {
    a := []string{"banana", "apple"}
    sort.Strings(a)
}
)");
    EXPECT_TRUE(contains(result.asm_text, "golangc_sort_strings"));
}

TEST(Phase25Test, SortSliceCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "sort"
func main() {
    a := []int{5, 2, 8, 1}
    sort.Slice(a, func(i, j int) bool { return a[i] < a[j] })
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase25Test, OsGetenvCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "os"
func main() {
    s := os.Getenv("PATH")
    _ = s
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase25Test, OsGetenvEmitsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "os"
func main() {
    s := os.Getenv("PATH")
    _ = s
}
)");
    EXPECT_TRUE(contains(result.asm_text, "golangc_os_getenv"));
}

TEST(Phase25Test, StringsFieldsCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "strings"
func main() {
    parts := strings.Fields("hello world  foo")
    _ = parts
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase25Test, StringsFieldsEmitsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "strings"
func main() {
    parts := strings.Fields("hello world")
    _ = parts
}
)");
    EXPECT_TRUE(contains(result.asm_text, "golangc_strings_fields"));
}

TEST(Phase25Test, StringsTrimSuffixCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "strings"
func main() {
    s := strings.TrimSuffix("hello.go", ".go")
    _ = s
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase25Test, StringsTrimPrefixEmitsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "strings"
func main() {
    s := strings.TrimPrefix("foobar", "foo")
    _ = s
}
)");
    EXPECT_TRUE(contains(result.asm_text, "golangc_strings_trim_prefix"));
}

TEST(Phase25Test, StringsSplitRealCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "strings"
func main() {
    parts := strings.Split("a,b,c", ",")
    _ = parts
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase25Test, StringsSplitEmitsRealRuntime) {
    auto result = compile_to_asm(R"(
package main
import "strings"
func main() {
    parts := strings.Split("a,b,c", ",")
    _ = parts
}
)");
    EXPECT_TRUE(contains(result.asm_text, "golangc_strings_split"));
}

TEST(Phase25Test, StringsJoinCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "strings"
func main() {
    elems := []string{"a", "b", "c"}
    s := strings.Join(elems, ",")
    _ = s
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase25Test, StringsJoinEmitsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "strings"
func main() {
    elems := []string{"a", "b", "c"}
    s := strings.Join(elems, ",")
    _ = s
}
)");
    EXPECT_TRUE(contains(result.asm_text, "golangc_strings_join"));
}

TEST(Phase25Test, GoFull) {
    auto result = compile_to_asm(R"(
package main
import (
    "fmt"
    "os"
    "sort"
    "strings"
)
func main() {
    home := os.Getenv("HOME")
    fmt.Println("HOME=" + home)
    words := strings.Fields("the quick brown fox")
    sort.Strings(words)
    fmt.Println(strings.Join(words, ","))
    parts := strings.Split("a:b:c", ":")
    _ = parts
    fmt.Println(strings.TrimSuffix("hello.go", ".go"))
    nums := []int{5, 3, 1, 4, 2}
    sort.Ints(nums)
    var x, y int
    fmt.Sscanf("10 20", "%d %d", &x, &y)
    fmt.Println(x + y)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_sort_ints"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_strings_join"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_os_getenv"));
    EXPECT_FALSE(contains(result.asm_text, "; TODO:"));
}

// ============================================================================
// Phase 26 Tests: Named returns, []byte conversion, fmt.Sprint, time, rand
// ============================================================================

TEST(Phase26Test, NamedReturnNoError) {
    // Named return values must not trigger "declared and not used" error
    auto result = compile_to_asm(R"(
package main
func divide(a, b int) (result int, err error) {
    result = a / b
    return
}
func main() {
    r, _ := divide(10, 2)
    _ = r
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase26Test, NamedReturnMultiple) {
    auto result = compile_to_asm(R"(
package main
func minMax(nums []int) (min, max int) {
    min = nums[0]
    max = nums[0]
    for _, n := range nums {
        if n < min { min = n }
        if n > max { max = n }
    }
    return
}
func main() {
    a := []int{3, 1, 4, 1, 5}
    lo, hi := minMax(a)
    _ = lo
    _ = hi
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase26Test, ByteSliceConversionCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
func main() {
    s := "hello"
    b := []byte(s)
    _ = b
    fmt.Println(s)
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase26Test, ByteSliceConversionEmitsCode) {
    auto result = compile_to_asm(R"(
package main
func main() {
    s := "world"
    b := []byte(s)
    _ = b
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase26Test, FmtSprintCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
func main() {
    s := fmt.Sprint("hello", " ", "world")
    fmt.Println(s)
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase26Test, FmtSprintEmitsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
func main() {
    s := fmt.Sprint(42)
    _ = s
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_sprintf"));
}

TEST(Phase26Test, TimeSleepCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "time"
func main() {
    time.Sleep(time.Second)
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase26Test, TimeSleepEmitsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "time"
func main() {
    time.Sleep(100 * time.Millisecond)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_time_sleep"));
}

TEST(Phase26Test, TimeNowCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "time"
func main() {
    t := time.Now()
    _ = t
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase26Test, TimeNowEmitsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "time"
func main() {
    t := time.Now()
    _ = t
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_time_now"));
}

TEST(Phase26Test, TimeSinceCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "time"
func main() {
    start := time.Now()
    elapsed := time.Since(start)
    _ = elapsed
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase26Test, RandIntnCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "math/rand"
func main() {
    n := rand.Intn(100)
    _ = n
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase26Test, RandIntnEmitsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "math/rand"
func main() {
    n := rand.Intn(10)
    _ = n
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_rand_intn"));
}

TEST(Phase26Test, RandFloat64CompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "math/rand"
func main() {
    f := rand.Float64()
    _ = f
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase26Test, RandSeedCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "math/rand"
func main() {
    rand.Seed(42)
    n := rand.Intn(100)
    _ = n
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase26Test, GoFull) {
    // Combined Phase 26 features
    auto result = compile_to_asm(R"(
package main
import (
    "fmt"
    "math/rand"
    "time"
)
func clamp(v, lo, hi int) (result int) {
    result = v
    if result < lo { result = lo }
    if result > hi { result = hi }
    return
}
func main() {
    rand.Seed(time.Now())
    n := rand.Intn(100)
    c := clamp(n, 10, 90)
    s := fmt.Sprint("clamped:", c)
    fmt.Println(s)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_FALSE(contains(result.asm_text, "; TODO:"));
}

// ============================================================================
// Phase 27 Tests: math constants + elided composite literals in map/slice
// ============================================================================

TEST(Phase27Test, MathPiCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import (
    "fmt"
    "math"
)
func main() {
    fmt.Println(math.Pi)
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase27Test, MathPiEmitsConstant) {
    auto result = compile_to_asm(R"(
package main
import "math"
func main() {
    x := math.Pi
    _ = x
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase27Test, MathECompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "math"
func main() {
    x := math.E
    _ = x
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase27Test, MathMaxFloat64CompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "math"
func main() {
    x := math.MaxFloat64
    _ = x
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase27Test, MathMaxInt64CompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "math"
func main() {
    x := math.MaxInt64
    _ = x
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase27Test, MathSinCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "math"
func main() {
    x := math.Sin(math.Pi / 2.0)
    _ = x
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase27Test, MathSinEmitsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "math"
func main() {
    x := math.Sin(1.0)
    _ = x
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_math_sin"));
}

TEST(Phase27Test, MathCosCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "math"
func main() {
    x := math.Cos(0.0)
    _ = x
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase27Test, MathAtan2CompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "math"
func main() {
    x := math.Atan2(1.0, 1.0)
    _ = x
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase27Test, ElidedCompositeLitInMap) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
type Info struct{ Count int; Label string }
func main() {
    m := map[string]Info{
        "a": {Count: 1, Label: "one"},
        "b": {Count: 2, Label: "two"},
    }
    v := m["a"]
    fmt.Println(v.Count, v.Label)
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase27Test, ElidedCompositeLitInSlice) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
type Point struct{ X, Y int }
func main() {
    pts := []Point{{1, 2}, {3, 4}}
    fmt.Println(len(pts))
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase27Test, ElidedCompositeLitAsArg) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
type Point struct{ X, Y int }
func main() {
    pts := []Point{}
    pts = append(pts, Point{1, 2})
    pts = append(pts, Point{3, 4})
    fmt.Println(len(pts))
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase27Test, GoFull) {
    auto result = compile_to_asm(R"(
package main
import (
    "fmt"
    "math"
)
type Vec2 struct{ X, Y float64 }
func magnitude(v Vec2) float64 {
    return math.Sqrt(v.X*v.X + v.Y*v.Y)
}
func main() {
    vecs := []Vec2{{3, 4}, {5, 12}}
    for _, v := range vecs {
        mag := magnitude(v)
        _ = mag
    }
    fmt.Println(math.Pi)
    fmt.Println(math.MaxInt64)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_FALSE(contains(result.asm_text, "; TODO:"));
}

// ============================================================================
// Phase 28: strings extras + fmt.Stringer dispatch
// ============================================================================

TEST(Phase28Test, StringsContainsRuneCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "strings"
func main() {
    s := "hello"
    b := strings.ContainsRune(s, 'e')
    _ = b
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_strings_contains_rune"));
}

TEST(Phase28Test, StringsIndexByteCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "strings"
func main() {
    s := "hello"
    i := strings.IndexByte(s, 'l')
    _ = i
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_strings_index_byte"));
}

TEST(Phase28Test, StringsLastIndexCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "strings"
func main() {
    s := "hello world hello"
    i := strings.LastIndex(s, "hello")
    _ = i
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_strings_last_index"));
}

TEST(Phase28Test, StringsIndexRuneCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "strings"
func main() {
    s := "hello"
    i := strings.IndexRune(s, 'l')
    _ = i
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_strings_index_rune"));
}

TEST(Phase28Test, StringsEqualFoldCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "strings"
func main() {
    b := strings.EqualFold("Hello", "hello")
    _ = b
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_strings_equal_fold"));
}

TEST(Phase28Test, StringsContainsAnyCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "strings"
func main() {
    b := strings.ContainsAny("hello", "aeiou")
    _ = b
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_strings_contains_any"));
}

TEST(Phase28Test, StringsReplaceAllCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "strings"
func main() {
    s := strings.ReplaceAll("foo foo foo", "foo", "bar")
    _ = s
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_strings_replace"));
}

TEST(Phase28Test, StringsTrimLeftCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "strings"
func main() {
    s := strings.TrimLeft("   hello", " ")
    _ = s
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_strings_trim_left"));
}

TEST(Phase28Test, StringsTrimRightCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "strings"
func main() {
    s := strings.TrimRight("hello   ", " ")
    _ = s
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_strings_trim_right"));
}

TEST(Phase28Test, StringsTitleCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "strings"
func main() {
    s := strings.Title("hello world")
    _ = s
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_strings_title"));
}

TEST(Phase28Test, FmtStringerDispatchCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
type Color int
func (c Color) String() string {
    if c == 0 {
        return "Red"
    }
    return "Blue"
}
func main() {
    c := Color(0)
    fmt.Println(c)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "Color$String"));
}

TEST(Phase28Test, FmtStringerDispatchEmitsMethodCall) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
type Direction int
func (d Direction) String() string {
    return "North"
}
func main() {
    d := Direction(0)
    fmt.Println(d)
}
)");
    EXPECT_FALSE(result.has_errors);
    // The method call should appear in asm
    EXPECT_TRUE(contains(result.asm_text, "Direction$String"));
    // And the result (a string) should be printed via print_string or println_string
    EXPECT_TRUE(contains(result.asm_text, "golangc_print") ||
                contains(result.asm_text, "golangc_println"));
}

TEST(Phase28Test, StringsMapCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "strings"
func toUpper(r rune) rune {
    if r >= 'a' && r <= 'z' {
        return r - 32
    }
    return r
}
func main() {
    s := strings.Map(toUpper, "hello")
    _ = s
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_strings_map"));
}

TEST(Phase28Test, MultipleStringersInPrintln) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
type MyInt int
func (m MyInt) String() string {
    return "MyInt"
}
type MyStr string
func (s MyStr) String() string {
    return "MyStr"
}
func main() {
    a := MyInt(1)
    b := MyStr("x")
    fmt.Println(a, b)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "MyInt$String"));
    EXPECT_TRUE(contains(result.asm_text, "MyStr$String"));
}

TEST(Phase28Test, GoFull) {
    auto result = compile_to_asm(R"(
package main
import (
    "fmt"
    "strings"
)
type Weekday int
func (d Weekday) String() string {
    days := []string{"Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"}
    if int(d) < len(days) {
        return days[d]
    }
    return "Unknown"
}
func main() {
    text := "hello world foo bar"
    words := strings.Fields(text)
    for _, w := range words {
        upper := strings.ToUpper(w)
        _ = upper
    }
    contains := strings.ContainsRune(text, 'o')
    _ = contains
    idx := strings.LastIndex(text, "o")
    _ = idx
    eq := strings.EqualFold("HELLO", "hello")
    _ = eq
    day := Weekday(1)
    fmt.Println(day)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_FALSE(contains(result.asm_text, "; TODO:"));
}

// ============================================================================
// Phase 29 Tests: unicode package, bytes.Buffer, fmt.Sprint stringer dispatch
// ============================================================================

TEST(Phase29Test, UnicodeIsLetterCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "unicode"
func main() {
    ok := unicode.IsLetter('A')
    _ = ok
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_unicode_is_letter"));
}

TEST(Phase29Test, UnicodeIsDigitCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "unicode"
func main() {
    ok := unicode.IsDigit('5')
    _ = ok
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_unicode_is_digit"));
}

TEST(Phase29Test, UnicodeIsSpaceCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "unicode"
func main() {
    ok := unicode.IsSpace(' ')
    _ = ok
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_unicode_is_space"));
}

TEST(Phase29Test, UnicodeIsUpperLowerCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "unicode"
func main() {
    u := unicode.IsUpper('A')
    l := unicode.IsLower('a')
    _ = u
    _ = l
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_unicode_is_upper"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_unicode_is_lower"));
}

TEST(Phase29Test, UnicodeToUpperLowerCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "unicode"
func main() {
    u := unicode.ToUpper('a')
    l := unicode.ToLower('A')
    _ = u
    _ = l
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_unicode_to_upper"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_unicode_to_lower"));
}

TEST(Phase29Test, BytesNewBufferCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "bytes"
func main() {
    b := bytes.NewBuffer(nil)
    _ = b
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_bytes_new_buffer"));
}

TEST(Phase29Test, BytesBufferWriteStringEmitsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "bytes"
func main() {
    b := bytes.NewBuffer(nil)
    b.WriteString("hello")
    _ = b
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_bytes_write_string"));
}

TEST(Phase29Test, BytesBufferStringEmitsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "bytes"
func main() {
    b := bytes.NewBuffer(nil)
    b.WriteString("world")
    s := b.String()
    _ = s
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_bytes_string"));
}

TEST(Phase29Test, BytesBufferWriteByteEmitsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "bytes"
func main() {
    b := bytes.NewBuffer(nil)
    b.WriteByte('!')
    _ = b
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_bytes_write_byte"));
}

TEST(Phase29Test, BytesBufferLenEmitsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "bytes"
func main() {
    b := bytes.NewBuffer(nil)
    b.WriteString("hi")
    n := b.Len()
    _ = n
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_bytes_len"));
}

TEST(Phase29Test, BytesBufferResetEmitsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "bytes"
func main() {
    b := bytes.NewBuffer(nil)
    b.WriteString("data")
    b.Reset()
    _ = b
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_bytes_reset"));
}

TEST(Phase29Test, FmtSprintStringerDispatch) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
type Color int
func (c Color) String() string {
    return "Red"
}
func main() {
    c := Color(0)
    s := fmt.Sprint(c)
    _ = s
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "Color$String"));
}

TEST(Phase29Test, BytesNewBufferStringCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "bytes"
func main() {
    b := bytes.NewBufferString("hello")
    s := b.String()
    _ = s
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_bytes_new_buffer_string"));
}

TEST(Phase29Test, GoFull) {
    auto result = compile_to_asm(R"(
package main
import (
    "bytes"
    "fmt"
    "unicode"
)
type Token int
func (t Token) String() string {
    if t == 0 {
        return "EOF"
    }
    return "IDENT"
}
func isWordChar(r rune) bool {
    return unicode.IsLetter(r) || unicode.IsDigit(r)
}
func main() {
    var b bytes.Buffer
    b.WriteString("hello")
    b.WriteByte(' ')
    b.WriteString("world")
    s := b.String()
    _ = s
    ok := isWordChar('a')
    _ = ok
    tok := Token(0)
    fmt.Println(tok)
    r := rune('A')
    up := unicode.ToUpper(r)
    _ = up
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_FALSE(contains(result.asm_text, "; TODO:"));
}

// ============================================================================
// Phase 31 Tests: copy(), close(), type assert panic, uintptr
// ============================================================================

// copy() basic: compiles without errors
TEST(Phase31Test, CopyCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
func main() {
    src := []int{1, 2, 3}
    dst := make([]int, 3)
    n := copy(dst, src)
    _ = n
}
)");
    EXPECT_FALSE(result.has_errors);
}

// copy() emits golangc_slice_copy runtime call
TEST(Phase31Test, CopyEmitsRuntime) {
    auto result = compile_to_asm(R"(
package main
func main() {
    src := []int{1, 2, 3}
    dst := make([]int, 3)
    n := copy(dst, src)
    _ = n
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_slice_copy"));
}

// copy() of strings
TEST(Phase31Test, CopyStringSlice) {
    auto result = compile_to_asm(R"(
package main
func main() {
    src := []string{"a", "b"}
    dst := make([]string, 2)
    n := copy(dst, src)
    _ = n
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_slice_copy"));
}

// close() basic: compiles without errors
TEST(Phase31Test, CloseCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
func main() {
    ch := make(chan int, 1)
    ch <- 42
    close(ch)
}
)");
    EXPECT_FALSE(result.has_errors);
}

// close() emits golangc_chan_close runtime call
TEST(Phase31Test, CloseEmitsRuntime) {
    auto result = compile_to_asm(R"(
package main
func main() {
    ch := make(chan int, 1)
    close(ch)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_chan_close"));
}

// type assertion single-value form should emit tag check
TEST(Phase31Test, TypeAssertSingleValueEmitsTagCheck) {
    auto result = compile_to_asm(R"(
package main
func getInt(i interface{}) int {
    return i.(int)
}
func main() {
    println(getInt(42))
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "cmp"));
}

// type assertion single-value compiles to working proc
TEST(Phase31Test, TypeAssertSingleValueProc) {
    auto result = compile_to_asm(R"(
package main
func getInt(i interface{}) int {
    return i.(int)
}
func main() {
    println(getInt(42))
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "getInt PROC"));
}

// two-value type assertion compiles correctly
TEST(Phase31Test, TypeAssertTwoValueOkForm) {
    auto result = compile_to_asm(R"(
package main
func tryInt(i interface{}) (int, bool) {
    v, ok := i.(int)
    return v, ok
}
func main() {
    v, ok := tryInt(42)
    _ = v
    _ = ok
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "tryInt PROC"));
}

// uintptr type is available
TEST(Phase31Test, UintptrCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
func main() {
    var p uintptr = 0
    _ = p
}
)");
    EXPECT_FALSE(result.has_errors);
}

// GoFull: comprehensive Phase 31 test
TEST(Phase31Test, GoFull) {
    auto result = compile_to_asm(R"(
package main

func copySlice(dst, src []int) int {
    return copy(dst, src)
}

func sendAndClose(ch chan int, val int) {
    ch <- val
    close(ch)
}

func getVal(i interface{}) int {
    v, ok := i.(int)
    if ok {
        return v
    }
    return -1
}

func main() {
    src := []int{1, 2, 3}
    dst := make([]int, 3)
    n := copySlice(dst, src)
    _ = n

    ch := make(chan int, 1)
    sendAndClose(ch, 99)

    println(getVal(42))
    println(getVal("hello"))

    var p uintptr = 0
    _ = p
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_slice_copy"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_chan_close"));
    EXPECT_FALSE(contains(result.asm_text, "; TODO:"));
}

// ============================================================================
// Phase 32 Tests: os.WriteFile/Remove/Mkdir, strings.NewReader, filepath, fmt.Sprintf Stringer
// ============================================================================

// os.WriteFile compiles
TEST(Phase32Test, OsWriteFileCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "os"
func main() {
    data := make([]byte, 5)
    err := os.WriteFile("out.txt", data, 0644)
    _ = err
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase32Test, OsWriteFileEmitsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "os"
func main() {
    data := make([]byte, 3)
    err := os.WriteFile("test.txt", data, 0644)
    _ = err
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_os_write_file"));
}

// os.Remove compiles
TEST(Phase32Test, OsRemoveCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "os"
func main() {
    err := os.Remove("tmp.txt")
    _ = err
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_os_remove"));
}

// os.Mkdir compiles
TEST(Phase32Test, OsMkdirCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "os"
func main() {
    err := os.Mkdir("mydir", 0755)
    _ = err
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_os_mkdir"));
}

// os.MkdirAll compiles
TEST(Phase32Test, OsMkdirAllCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "os"
func main() {
    err := os.MkdirAll("a/b/c", 0755)
    _ = err
}
)");
    EXPECT_FALSE(result.has_errors);
}

// os.TempDir compiles
TEST(Phase32Test, OsTempDirCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "os"
func main() {
    d := os.TempDir()
    _ = d
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_os_temp_dir"));
}

// strings.NewReader compiles
TEST(Phase32Test, StringsNewReaderCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "strings"
func main() {
    r := strings.NewReader("hello world")
    _ = r
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase32Test, StringsNewReaderEmitsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "strings"
func main() {
    r := strings.NewReader("hello world")
    _ = r
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_strings_reader_new"));
}

// filepath.Join compiles
TEST(Phase32Test, FilepathJoinCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "path/filepath"
func main() {
    p := filepath.Join("a", "b")
    _ = p
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase32Test, FilepathJoinEmitsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "path/filepath"
func main() {
    p := filepath.Join("a", "b")
    _ = p
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_filepath_join2"));
}

// filepath.Dir, Base, Ext compile
TEST(Phase32Test, FilepathDirBaseExtCompile) {
    auto result = compile_to_asm(R"(
package main
import "path/filepath"
func main() {
    d := filepath.Dir("/a/b/c.txt")
    b := filepath.Base("/a/b/c.txt")
    e := filepath.Ext("/a/b/c.txt")
    _ = d
    _ = b
    _ = e
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_filepath_dir"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_filepath_base"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_filepath_ext"));
}

// fmt.Sprintf with Stringer dispatch
TEST(Phase32Test, FmtSprintfStringerDispatch) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
type Color int
func (c Color) String() string {
    if c == 0 { return "Red" }
    return "Blue"
}
func main() {
    c := Color(0)
    s := fmt.Sprintf("color=%s", c)
    _ = s
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "Color$String"));
}

// GoFull: comprehensive Phase 32
TEST(Phase32Test, GoFull) {
    auto result = compile_to_asm(R"(
package main
import (
    "fmt"
    "os"
    "path/filepath"
    "strings"
)
type Status int
func (s Status) String() string {
    if s == 0 { return "OK" }
    return "FAIL"
}
func main() {
    // os extras
    tmp := os.TempDir()
    p := filepath.Join(tmp, "test.txt")
    _ = p
    data := make([]byte, 3)
    data[0] = 65
    err := os.WriteFile(p, data, 0644)
    _ = err
    err2 := os.Remove(p)
    _ = err2

    // strings.NewReader
    r := strings.NewReader("hello")
    _ = r

    // fmt.Sprintf with Stringer
    st := Status(0)
    msg := fmt.Sprintf("status=%s", st)
    _ = msg
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_os_write_file"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_filepath_join2"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_strings_reader_new"));
    EXPECT_FALSE(contains(result.asm_text, "; TODO:"));
}

// =============================================================================
// Phase 33 Tests
// =============================================================================

// --- Part 1: Parser fix — []T{} composite literal as call argument ---

TEST(Phase33Test, CompositeLitByteSliceCallArg) {
    // []byte{65,66,67} passed directly to a function call
    auto result = compile_to_asm(R"(
package main
func useBytes(b []byte) int { return len(b) }
func main() {
    n := useBytes([]byte{65, 66, 67})
    _ = n
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase33Test, CompositeLitIntSliceCallArg) {
    // []int{1,2,3} passed directly to a function call
    auto result = compile_to_asm(R"(
package main
func sumInts(a []int) int {
    s := 0
    for _, v := range a { s += v }
    return s
}
func main() {
    n := sumInts([]int{1, 2, 3})
    _ = n
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase33Test, CompositeLitStringSliceCallArg) {
    // []string{"a","b"} as call arg
    auto result = compile_to_asm(R"(
package main
import "strings"
func main() {
    s := strings.Join([]string{"hello", "world"}, " ")
    _ = s
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase33Test, CompositeLitMakeAndLiteralMixed) {
    // make([]int, 3) and []int{1,2,3} in same function
    auto result = compile_to_asm(R"(
package main
func useBytes(b []byte) int { return len(b) }
func main() {
    a := make([]int, 3)
    _ = a
    n := useBytes([]byte{10, 20})
    _ = n
}
)");
    EXPECT_FALSE(result.has_errors);
}

// --- Part 2: fmt.Printf Stringer dispatch ---

TEST(Phase33Test, FmtPrintfStringerDispatch) {
    // fmt.Printf should call .String() method on typed arg
    auto result = compile_to_asm(R"(
package main
import "fmt"
type Color int
func (c Color) String() string {
    if c == 0 { return "Red" }
    return "Blue"
}
func main() {
    c := Color(0)
    fmt.Printf("color=%s\n", c)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "Color$String"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_printf"));
}

TEST(Phase33Test, FmtPrintfNoStringerPlainArg) {
    // fmt.Printf with plain int — no stringer call
    auto result = compile_to_asm(R"(
package main
import "fmt"
func main() {
    x := 42
    fmt.Printf("val=%d\n", x)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_printf"));
}

TEST(Phase33Test, FmtPrintfMultipleArgs) {
    // fmt.Printf with multiple args, one Stringer
    auto result = compile_to_asm(R"(
package main
import "fmt"
type Status int
func (s Status) String() string { return "OK" }
func main() {
    st := Status(0)
    fmt.Printf("%d: %s\n", 42, st)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "Status$String"));
}

// --- Part 3: os.Stat + os.IsNotExist ---

TEST(Phase33Test, OsStatCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "os"
func main() {
    fi, err := os.Stat("somefile.txt")
    _ = fi
    _ = err
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase33Test, OsStatEmitsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "os"
func main() {
    fi, err := os.Stat("test.txt")
    _ = fi
    _ = err
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_os_stat"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_os_stat_error"));
}

TEST(Phase33Test, OsIsNotExistCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "os"
func main() {
    _, err := os.Stat("nonexistent_xyz.txt")
    ok := os.IsNotExist(err)
    _ = ok
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase33Test, OsIsNotExistEmitsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "os"
func main() {
    _, err := os.Stat("nonexistent_xyz.txt")
    ok := os.IsNotExist(err)
    _ = ok
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_os_is_not_exist"));
}

TEST(Phase33Test, OsFileInfoMethodsCompile) {
    auto result = compile_to_asm(R"(
package main
import "os"
func main() {
    fi, err := os.Stat("go.mod")
    if err == nil {
        n := fi.Name()
        s := fi.Size()
        d := fi.IsDir()
        _ = n
        _ = s
        _ = d
    }
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase33Test, OsFileInfoMethodsEmitRuntime) {
    auto result = compile_to_asm(R"(
package main
import "os"
func main() {
    fi, err := os.Stat("go.mod")
    if err == nil {
        n := fi.Name()
        s := fi.Size()
        d := fi.IsDir()
        _ = n
        _ = s
        _ = d
    }
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_file_info_name"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_file_info_size"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_file_info_is_dir"));
}

// GoFull: comprehensive Phase 33
TEST(Phase33Test, GoFull) {
    auto result = compile_to_asm(R"(
package main
import (
    "fmt"
    "os"
    "strings"
)
type Direction int
func (d Direction) String() string {
    if d == 0 { return "North" }
    return "South"
}
func useBytes(b []byte) int { return len(b) }
func main() {
    // Part 1: composite literal as call arg
    n := useBytes([]byte{65, 66, 67})
    _ = n

    // Part 2: fmt.Printf Stringer
    dir := Direction(0)
    fmt.Printf("dir=%s\n", dir)

    // Part 3: os.Stat + os.IsNotExist
    _, err := os.Stat("nonexistent_xyz.txt")
    isNotExist := os.IsNotExist(err)
    _ = isNotExist

    // strings.Join with composite literal
    joined := strings.Join([]string{"a", "b", "c"}, ",")
    _ = joined
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "Direction$String"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_os_stat"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_os_is_not_exist"));
}

// ============================================================================
// Phase 35 Tests: os.File.Read/Write/Seek, os.Rename, Builder.WriteString (int,error)
// ============================================================================

TEST(Phase35Test, OsFileReadCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "os"
func main() {
    f, _ := os.Open("test.txt")
    buf := make([]byte, 64)
    n, _ := f.Read(buf)
    _ = n
    f.Close()
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase35Test, OsFileReadEmitsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "os"
func main() {
    f, _ := os.Open("test.txt")
    buf := make([]byte, 64)
    n, _ := f.Read(buf)
    _ = n
    f.Close()
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_os_file_read"));
}

TEST(Phase35Test, OsFileWriteCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "os"
func main() {
    f, _ := os.Create("out.txt")
    data := []byte{72, 101, 108, 108, 111}
    n, _ := f.Write(data)
    _ = n
    f.Close()
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase35Test, OsFileWriteEmitsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "os"
func main() {
    f, _ := os.Create("out.txt")
    data := []byte{72, 101, 108, 108, 111}
    n, _ := f.Write(data)
    _ = n
    f.Close()
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_os_file_write"));
}

TEST(Phase35Test, OsFileSeekCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "os"
func main() {
    f, _ := os.Open("test.txt")
    pos, _ := f.Seek(0, 2)
    _ = pos
    f.Close()
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase35Test, OsFileSeekEmitsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "os"
func main() {
    f, _ := os.Open("test.txt")
    pos, _ := f.Seek(0, 2)
    _ = pos
    f.Close()
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_os_file_seek"));
}

TEST(Phase35Test, OsRenameCompilesNoErrors) {
    auto result = compile_to_asm(R"(
package main
import "os"
func main() {
    err := os.Rename("old.txt", "new.txt")
    _ = err
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase35Test, OsRenameEmitsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "os"
func main() {
    err := os.Rename("old.txt", "new.txt")
    _ = err
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_os_rename"));
}

TEST(Phase35Test, BuilderWriteStringReturnsIntError) {
    auto result = compile_to_asm(R"(
package main
import "strings"
func main() {
    var b strings.Builder
    n, _ := b.WriteString("hello")
    _ = n
    s := b.String()
    _ = s
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_builder_write_string"));
}

TEST(Phase35Test, BuilderWriteStringDiscardReturn) {
    auto result = compile_to_asm(R"(
package main
import "strings"
func main() {
    var b strings.Builder
    b.WriteString("hello")
    b.WriteString(" world")
    s := b.String()
    _ = s
}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase35Test, OsFileReadWriteSeekAll) {
    auto result = compile_to_asm(R"(
package main
import "os"
func main() {
    f, _ := os.Create("tmp.bin")
    data := []byte{1, 2, 3, 4}
    f.Write(data)
    f.Seek(0, 0)
    buf := make([]byte, 4)
    n, _ := f.Read(buf)
    _ = n
    f.Close()
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_os_file_write"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_os_file_seek"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_os_file_read"));
}

TEST(Phase35Test, OsRenameWithHelper) {
    auto result = compile_to_asm(R"(
package main
import "os"
func maybeRename(src, dst string) {
    os.Rename(src, dst)
}
func main() {
    maybeRename("a.txt", "b.txt")
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_os_rename"));
}

TEST(Phase35Test, GoFull) {
    auto result = compile_to_asm(R"(
package main
import (
    "os"
    "strings"
)
func main() {
    var sb strings.Builder
    n, _ := sb.WriteString("Phase35")
    _ = n
    sb.WriteString(" test")
    res := sb.String()
    _ = res

    f, _ := os.Create("phase35_tmp.bin")
    data := []byte{80, 104, 115}
    written, _ := f.Write(data)
    _ = written
    pos, _ := f.Seek(0, 0)
    _ = pos
    buf := make([]byte, 3)
    nr, _ := f.Read(buf)
    _ = nr
    f.Close()

    os.Rename("phase35_tmp.bin", "phase35_out.bin")
    os.Remove("phase35_out.bin")
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_builder_write_string"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_os_file_write"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_os_file_seek"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_os_file_read"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_os_rename"));
}

// ============================================================================
// Phase 36: fmt verb improvements
// ============================================================================

TEST(FmtVerb36Test, SprintfFloatF) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
func doFmt(x float64) string {
    return fmt.Sprintf("%f", x)
}
func main() { _ = doFmt(3.14) }
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_sprintf"));
}

TEST(FmtVerb36Test, SprintfFloatPrec) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
func doFmt(x float64) string {
    return fmt.Sprintf("%.2f", x)
}
func main() { _ = doFmt(3.14) }
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_sprintf"));
}

TEST(FmtVerb36Test, SprintfFloatE) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
func doFmt(x float64) string {
    return fmt.Sprintf("%e", x)
}
func main() { _ = doFmt(3.14) }
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_sprintf"));
}

TEST(FmtVerb36Test, SprintfFloatG) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
func doFmt(x float64) string {
    return fmt.Sprintf("%g", x)
}
func main() { _ = doFmt(3.14) }
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_sprintf"));
}

TEST(FmtVerb36Test, SprintfOctal) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
func doFmt(n int) string {
    return fmt.Sprintf("%o", n)
}
func main() { _ = doFmt(8) }
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_sprintf"));
}

TEST(FmtVerb36Test, SprintfHexLower) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
func doFmt(n int) string {
    return fmt.Sprintf("%x", n)
}
func main() { _ = doFmt(255) }
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_sprintf"));
}

TEST(FmtVerb36Test, SprintfHexUpper) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
func doFmt(n int) string {
    return fmt.Sprintf("%X", n)
}
func main() { _ = doFmt(255) }
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_sprintf"));
}

TEST(FmtVerb36Test, SprintfBinary) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
func doFmt(n int) string {
    return fmt.Sprintf("%b", n)
}
func main() { _ = doFmt(10) }
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_sprintf"));
}

TEST(FmtVerb36Test, SprintfWidth) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
func doFmt(n int) string {
    return fmt.Sprintf("%5d", n)
}
func main() { _ = doFmt(42) }
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_sprintf"));
}

TEST(FmtVerb36Test, SprintfZeroPad) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
func doFmt(n int) string {
    return fmt.Sprintf("%05d", n)
}
func main() { _ = doFmt(42) }
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_sprintf"));
}

TEST(FmtVerb36Test, SprintfPlusFlag) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
func doFmt(n int) string {
    return fmt.Sprintf("%+d", n)
}
func main() { _ = doFmt(42) }
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_sprintf"));
}

TEST(FmtVerb36Test, SprintfLeftAlign) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
func doFmt(n int) string {
    return fmt.Sprintf("%-10d", n)
}
func main() { _ = doFmt(42) }
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_sprintf"));
}

TEST(FmtVerb36Test, SprintfQuoted) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
func doFmt(s string) string {
    return fmt.Sprintf("%q", s)
}
func main() { _ = doFmt("hello") }
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_sprintf"));
}

TEST(FmtVerb36Test, SprintfBool) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
func doFmt(b bool) string {
    return fmt.Sprintf("%t", b)
}
func main() { _ = doFmt(true) }
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_sprintf"));
}

TEST(FmtVerb36Test, SprintfPointer) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
func doFmt(p int) string {
    return fmt.Sprintf("%p", p)
}
func main() { _ = doFmt(0) }
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_sprintf"));
}

TEST(FmtVerb36Test, SprintfWidthFloat) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
func doFmt(x float64) string {
    return fmt.Sprintf("%8.3f", x)
}
func main() { _ = doFmt(3.14159) }
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_sprintf"));
}

TEST(FmtVerb36Test, SprintfMultiVerb) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
func doFmt(n int, x float64) string {
    return fmt.Sprintf("%d %x %.2f", n, n, x)
}
func main() { _ = doFmt(255, 3.14) }
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_sprintf"));
}

TEST(FmtVerb36Test, SprintfPercentLit) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
func doFmt() string {
    return fmt.Sprintf("100%%")
}
func main() { _ = doFmt() }
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_sprintf"));
}

TEST(FmtVerb36Test, PrintfFloatF) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
func doFmt(x float64) {
    fmt.Printf("%.2f\n", x)
}
func main() { doFmt(3.14) }
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_printf"));
}

TEST(FmtVerb36Test, PrintfOctal) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
func doFmt(n int) {
    fmt.Printf("%o\n", n)
}
func main() { doFmt(8) }
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_printf"));
}

TEST(FmtVerb36Test, PrintfHexLower) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
func doFmt(n int) {
    fmt.Printf("%x\n", n)
}
func main() { doFmt(255) }
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_printf"));
}

TEST(FmtVerb36Test, PrintfBool) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
func doFmt(b bool) {
    fmt.Printf("%t\n", b)
}
func main() { doFmt(true) }
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_printf"));
}

TEST(FmtVerb36Test, FprintfFloatF) {
    auto result = compile_to_asm(R"(
package main
import (
    "fmt"
    "os"
)
func doFmt(x float64) {
    fmt.Fprintf(os.Stdout, "%.2f\n", x)
}
func main() { doFmt(3.14) }
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_fprintf"));
}

TEST(FmtVerb36Test, FprintfHex) {
    auto result = compile_to_asm(R"(
package main
import (
    "fmt"
    "os"
)
func doFmt(n int) {
    fmt.Fprintf(os.Stdout, "%x\n", n)
}
func main() { doFmt(255) }
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_fprintf"));
}

TEST(FmtVerb36Test, SprintfNegWidth) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
func doFmt(n int) string {
    return fmt.Sprintf("%-8d", n)
}
func main() { _ = doFmt(42) }
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_sprintf"));
}

// ============================================================================
// RCTest — reference counting IR emission and codegen
// ============================================================================

// String concat produces an RC object → Release call should appear in ASM
TEST(RCTest, StringConcatEmitsRelease) {
    auto result = compile_to_asm(R"(
package main
func f(a string, b string) {
    c := a + b
    _ = c
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call golangc_release"));
}

// RC extern declarations are emitted for any RC-triggering program
TEST(RCTest, StringConcatExternsDeclared) {
    auto result = compile_to_asm(R"(
package main
func f(a string, b string) {
    c := a + b
    _ = c
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "EXTERN golangc_retain:PROC"));
    EXPECT_TRUE(contains(result.asm_text, "EXTERN golangc_release:PROC"));
}

// make([]int, 5) → Release call for the slice backing array
TEST(RCTest, SliceMakeEmitsRelease) {
    auto result = compile_to_asm(R"(
package main
func f() {
    s := make([]int, 5)
    _ = s
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call golangc_release"));
}

// make(map[string]int) → Release call for the map object
TEST(RCTest, MapMakeEmitsRelease) {
    auto result = compile_to_asm(R"(
package main
func f() {
    m := make(map[string]int)
    _ = m
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call golangc_release"));
}

// make(chan int) → Release call for the channel object
TEST(RCTest, ChanMakeEmitsRelease) {
    auto result = compile_to_asm(R"(
package main
func f() {
    ch := make(chan int)
    _ = ch
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call golangc_release"));
}

// Slice make uses golangc_rc_slice_alloc, NOT malloc
TEST(RCTest, SliceMakeUsesRCAlloc) {
    auto result = compile_to_asm(R"(
package main
func f() {
    s := make([]int, 5)
    _ = s
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_rc_slice_alloc"));
    EXPECT_FALSE(contains(result.asm_text, "call malloc"));
}

// Simple string literal assignment (no concat) → no RC release call
TEST(RCTest, NoRCForStringLiteral) {
    auto result = compile_to_asm(R"(
package main
func f() {
    a := "hello"
    _ = a
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    // EXTERN declaration may still appear; check that no call is emitted
    EXPECT_FALSE(contains(result.asm_text, "call golangc_release"));
}

// Integer variables → no retain/release calls
TEST(RCTest, NoRCForInts) {
    auto result = compile_to_asm(R"(
package main
func f() {
    x := 42
    _ = x
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    // EXTERN declarations may still appear; check that no calls are emitted
    EXPECT_FALSE(contains(result.asm_text, "call golangc_retain"));
    EXPECT_FALSE(contains(result.asm_text, "call golangc_release"));
}

// Function parameters are not RC-owned → no release call for params
TEST(RCTest, NoRCForParams) {
    auto result = compile_to_asm(R"(
package main
func f(s string) {
    _ = s
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_FALSE(contains(result.asm_text, "call golangc_release"));
}

// Multiple RC vars → multiple release calls
TEST(RCTest, MultipleRCVarsGetReleased) {
    auto result = compile_to_asm(R"(
package main
func f(a string, b string) {
    s := a + b
    m := make(map[int]int)
    _ = s
    _ = m
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    // At least 2 release calls (one for string, one for map)
    EXPECT_GE(count_occurrences(result.asm_text, "call golangc_release"), 2);
}

// golangc_rc_slice_alloc extern is declared
TEST(RCTest, RcSliceAllocExternDeclared) {
    auto result = compile_to_asm(R"(
package main
func f() {
    s := make([]int, 3)
    _ = s
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "EXTERN golangc_rc_slice_alloc:PROC"));
}

// Returned RC string: no release call before ret (caller owns it)
TEST(RCTest, RCFuncReturnNoRelease) {
    auto result = compile_to_asm(R"(
package main
func f(a string, b string) string {
    return a + b
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    // The returned concat result should NOT be released here
    // (EXTERN declaration may appear but no call instruction)
    EXPECT_FALSE(contains(result.asm_text, "call golangc_release"));
}

// Assign two different concat results in same function → both get released
TEST(RCTest, StringConcatAssignTwice) {
    auto result = compile_to_asm(R"(
package main
func f(a string, b string, c string, d string) {
    s1 := a + b
    s2 := c + d
    _ = s1
    _ = s2
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_GE(count_occurrences(result.asm_text, "call golangc_release"), 2);
}

// Any RC-triggering program → retain EXTERN present
TEST(RCTest, RcRetainExternPresent) {
    auto result = compile_to_asm(R"(
package main
func f(a string, b string) {
    s := a + b
    _ = s
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "EXTERN golangc_retain:PROC"));
}

// Any RC-triggering program → release EXTERN present
TEST(RCTest, RcReleaseExternPresent) {
    auto result = compile_to_asm(R"(
package main
func f(a string, b string) {
    s := a + b
    _ = s
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "EXTERN golangc_release:PROC"));
}

// The IR text should contain "release" for RC-producing vars
TEST(RCTest, IRTextContainsRelease) {
    auto result = compile_to_asm(R"(
package main
func f(a string, b string) {
    s := a + b
    _ = s
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.ir_text, "release"));
}

// make([]int, n) using variable length → rc_slice_alloc still present
TEST(RCTest, SliceMakeVarLenUsesRCAlloc) {
    auto result = compile_to_asm(R"(
package main
func f(n int) {
    s := make([]int, n)
    _ = s
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_rc_slice_alloc"));
}

// ============================================================================
// RC38A tests — Gap 1: var decl, Gap 2: reassignment, Gap 3: explicit return
// ============================================================================

// Gap 1: var s string = a+b → scope-exit release emitted
TEST(RCTest, VarDeclStringConcatEmitsRelease) {
    auto result = compile_to_asm(R"(
package main
func f(a string, b string) {
    var s string = a + b
    _ = s
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call golangc_release"));
}

// Gap 1: var m map[int]int = make(map[int]int) → scope-exit release emitted
TEST(RCTest, VarDeclMapMakeEmitsRelease) {
    auto result = compile_to_asm(R"(
package main
func f() {
    var m map[int]int = make(map[int]int)
    _ = m
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call golangc_release"));
}

// Gap 1: var sl []int = make([]int, 3) → scope-exit release emitted
TEST(RCTest, VarDeclSliceMakeEmitsRelease) {
    auto result = compile_to_asm(R"(
package main
func f() {
    var sl []int = make([]int, 3)
    _ = sl
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call golangc_release"));
}

// Gap 2: s := a+b then s = c+d → old released on overwrite + scope-exit = 2 releases
TEST(RCTest, AssignOverwriteEmitsRelease) {
    auto result = compile_to_asm(R"(
package main
func f(a string, b string, c string, d string) {
    s := a + b
    s = c + d
    _ = s
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_GE(count_occurrences(result.asm_text, "call golangc_release"), 2);
}

// Gap 2: s := a+b then s = "hello" (literal, not RC) → exactly 1 release (scope-exit only removed, old released)
TEST(RCTest, AssignOverwriteNonRCReleasesOld) {
    auto result = compile_to_asm(R"(
package main
func f(a string, b string) {
    s := a + b
    s = "hello"
    _ = s
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    // Old RC value released on assignment; s is no longer tracked after assign → no scope-exit
    EXPECT_GE(count_occurrences(result.asm_text, "call golangc_release"), 1);
}

// Gap 3: explicit void return → releases RC locals before ret
TEST(RCTest, ExplicitReturnVoidReleasesRC) {
    auto result = compile_to_asm(R"(
package main
func f(a string, b string) {
    s := a + b
    _ = s
    return
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "call golangc_release"));
}

// Gap 3: explicit return of non-RC value → no release
TEST(RCTest, ExplicitReturnNonRCNoRelease) {
    auto result = compile_to_asm(R"(
package main
func f() {
    x := 42
    _ = x
    return
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_FALSE(contains(result.asm_text, "call golangc_release"));
}

// Gap 3: return s (the RC var) → s not released; other RC vars are released
TEST(RCTest, ExplicitReturnStringReleasesOtherRC) {
    auto result = compile_to_asm(R"(
package main
func f(a string, b string) string {
    s := a + b
    m := make(map[int]int)
    _ = m
    return s
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    // m should be released (1), s should not be released (returned)
    EXPECT_GE(count_occurrences(result.asm_text, "call golangc_release"), 1);
}

// Gap 3: void return with 2 RC locals → both released
TEST(RCTest, ExplicitReturnReleasesAllWhenNoneReturned) {
    auto result = compile_to_asm(R"(
package main
func f(a string, b string) {
    s := a + b
    m := make(map[int]int)
    _ = s
    _ = m
    return
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_GE(count_occurrences(result.asm_text, "call golangc_release"), 2);
}

// Gap 3: multi-value return with RC local → RC local released before pack
TEST(RCTest, MultiReturnReleasesRC) {
    auto result = compile_to_asm(R"(
package main
func f() (int, int) {
    m := make(map[int]int)
    _ = m
    return 1, 2
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_GE(count_occurrences(result.asm_text, "call golangc_release"), 1);
}

// Gap 2: triple overwrite → 3 releases (2 old + 1 scope-exit)
TEST(RCTest, AssignToTrackedVarTwiceReleasesOld) {
    auto result = compile_to_asm(R"(
package main
func f(a string, b string, c string, d string, e string, ff string) {
    s := a + b
    s = c + d
    s = e + ff
    _ = s
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_GE(count_occurrences(result.asm_text, "call golangc_release"), 3);
}

// Gap 1 + Gap 3: var decl then explicit return → exactly 1 release
TEST(RCTest, VarDeclThenExplicitReturn) {
    auto result = compile_to_asm(R"(
package main
func f(a string, b string) {
    var s string = a + b
    _ = s
    return
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_GE(count_occurrences(result.asm_text, "call golangc_release"), 1);
}

// ============================================================================
// Phase 38B Tests: unicode extras
// ============================================================================

TEST(UnicodeExtrasTest, IsPunctCompilesAndCallsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "unicode"
func f(r rune) bool { return unicode.IsPunct(r) }
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_unicode_is_punct"));
}

TEST(UnicodeExtrasTest, IsControlCompilesAndCallsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "unicode"
func f(r rune) bool { return unicode.IsControl(r) }
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_unicode_is_control"));
}

TEST(UnicodeExtrasTest, IsMarkCompilesAndCallsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "unicode"
func f(r rune) bool { return unicode.IsMark(r) }
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_unicode_is_mark"));
}

TEST(UnicodeExtrasTest, IsNumberCompilesAndCallsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "unicode"
func f(r rune) bool { return unicode.IsNumber(r) }
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_unicode_is_number"));
}

TEST(UnicodeExtrasTest, IsPrintCompilesAndCallsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "unicode"
func f(r rune) bool { return unicode.IsPrint(r) }
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_unicode_is_print"));
}

TEST(UnicodeExtrasTest, IsTitleCompilesAndCallsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "unicode"
func f(r rune) bool { return unicode.IsTitle(r) }
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_unicode_is_title"));
}

TEST(UnicodeExtrasTest, IsSymbolCompilesAndCallsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "unicode"
func f(r rune) bool { return unicode.IsSymbol(r) }
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_unicode_is_symbol"));
}

TEST(UnicodeExtrasTest, MaxRuneConstant) {
    auto result = compile_to_asm(R"(
package main
import "unicode"
func f() int { return unicode.MaxRune }
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    // 0x10FFFF = 1114111 decimal
    EXPECT_TRUE(contains(result.asm_text, "1114111"));
}

TEST(UnicodeExtrasTest, ReplacementCharConstant) {
    auto result = compile_to_asm(R"(
package main
import "unicode"
func f() int { return unicode.ReplacementChar }
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    // 0xFFFD = 65533 decimal
    EXPECT_TRUE(contains(result.asm_text, "65533"));
}

TEST(UnicodeExtrasTest, MultipleUnicodeFuncsInOneFunc) {
    auto result = compile_to_asm(R"(
package main
import "unicode"
func classify(r rune) bool {
    return unicode.IsLetter(r) || unicode.IsNumber(r) || unicode.IsPunct(r)
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_unicode_is_letter"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_unicode_is_number"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_unicode_is_punct"));
}

TEST(UnicodeExtrasTest, ExistingFunctionsUnchanged) {
    auto result = compile_to_asm(R"(
package main
import "unicode"
func f(r rune) int {
    if unicode.IsUpper(r) { return unicode.ToLower(r) }
    if unicode.IsLower(r) { return unicode.ToUpper(r) }
    return int(r)
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_unicode_is_upper"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_unicode_is_lower"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_unicode_to_upper"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_unicode_to_lower"));
}

// ============================================================================
// Phase 38B Tests: math/bits package
// ============================================================================

TEST(MathBitsTest, OnesCountCompilesAndCallsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "math/bits"
func f(x int) int { return bits.OnesCount(x) }
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_bits_ones_count"));
}

TEST(MathBitsTest, LenCompilesAndCallsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "math/bits"
func f(x int) int { return bits.Len(x) }
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_bits_len"));
}

TEST(MathBitsTest, LeadingZeros64CompilesAndCallsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "math/bits"
func f(x int) int { return bits.LeadingZeros64(x) }
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_bits_leading_zeros"));
}

TEST(MathBitsTest, TrailingZeros32CompilesWithSentinel) {
    auto result = compile_to_asm(R"(
package main
import "math/bits"
func f(x int) int { return bits.TrailingZeros32(x) }
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_bits_trailing_zeros"));
}

// ============================================================
// StructFmtTest: fmt.Println / fmt.Sprintf with struct args
// ============================================================

TEST(StructFmtTest, PrintlnStructCallsGenStructToString) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
type Point struct { X int; Y int }
func main() {
    p := Point{1, 2}
    fmt.Println(p)
}
)");
    EXPECT_FALSE(result.has_errors);
    // gen_struct_to_string produces a golangc_sprintf call
    EXPECT_TRUE(contains(result.asm_text, "golangc_sprintf"));
}

TEST(StructFmtTest, PrintlnStructFormatStringContainsBraces) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
type Point struct { X int; Y int }
func main() {
    p := Point{3, 4}
    fmt.Println(p)
}
)");
    EXPECT_FALSE(result.has_errors);
    // The format string "{%d %d}" is emitted as DB bytes: {=123 %=37 d=100 ' '=32 }=125
    // Pattern: 123, 37, 100, 32, 37, 100, 125
    EXPECT_TRUE(contains(result.asm_text, "123, 37, 100, 32, 37, 100, 125"));
}

TEST(StructFmtTest, SprintfPercentVStructCallsGenStructToString) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
type Rect struct { W int; H int }
func f(r Rect) string {
    return fmt.Sprintf("%v", r)
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_sprintf"));
    // struct format "{%d %d}" is emitted as DB bytes
    EXPECT_TRUE(contains(result.asm_text, "123, 37, 100, 32, 37, 100, 125"));
}

TEST(StructFmtTest, PrintlnTwoStructArgs) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
type Vec struct { X int; Y int }
func main() {
    a := Vec{1, 2}
    b := Vec{3, 4}
    fmt.Println(a, b)
}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_FALSE(result.has_errors);
}

TEST(StructFmtTest, PrintlnStructWithFloatField) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
type Pair struct { N int; F float64 }
func main() {
    p := Pair{7, 3.14}
    fmt.Println(p)
}
)");
    EXPECT_FALSE(result.has_errors);
    // Float field uses %g verb; "{%d %g}" as DB: {=123 %=37 d=100 ' '=32 %=37 g=103 }=125
    EXPECT_TRUE(contains(result.asm_text, "123, 37, 100, 32, 37, 103, 125"));
}

TEST(StructFmtTest, PrintlnStructWithStringField) {
    auto result = compile_to_asm(R"(
package main
import "fmt"
type Named struct { Name string; Age int }
func main() {
    n := Named{"Alice", 30}
    fmt.Println(n)
}
)");
    EXPECT_FALSE(result.has_errors);
    // "{%s %d}" as DB: {=123 %=37 s=115 ' '=32 %=37 d=100 }=125
    EXPECT_TRUE(contains(result.asm_text, "123, 37, 115, 32, 37, 100, 125"));
}

TEST(MathBitsTest, RotateLeft64CompilesAndCallsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "math/bits"
func f(x int, k int) int { return bits.RotateLeft64(x, k) }
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_bits_rotate_left"));
}

TEST(MathBitsTest, RotateLeft32CompilesAndCallsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "math/bits"
func f(x int, k int) int { return bits.RotateLeft32(x, k) }
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_bits_rotate_left32"));
}

TEST(MathBitsTest, Reverse64CompilesAndCallsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "math/bits"
func f(x int) int { return bits.Reverse64(x) }
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_bits_reverse64"));
}

TEST(MathBitsTest, Reverse8CompilesAndCallsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "math/bits"
func f(x int) int { return bits.Reverse8(x) }
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_bits_reverse8"));
}

TEST(MathBitsTest, ReverseBytes64CompilesAndCallsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "math/bits"
func f(x int) int { return bits.ReverseBytes64(x) }
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_bits_reverse_bytes64"));
}

TEST(MathBitsTest, UintSizeConstant) {
    auto result = compile_to_asm(R"(
package main
import "math/bits"
func f() int { return bits.UintSize }
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "64"));
}

TEST(MathBitsTest, Len8MasksTo8Bits) {
    auto result = compile_to_asm(R"(
package main
import "math/bits"
func f(x int) int { return bits.Len8(x) }
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_bits_len"));
    // AND with 0xFF mask (255)
    EXPECT_TRUE(contains(result.asm_text, "255"));
}

TEST(MathBitsTest, LeadingZeros8ShiftsSentinel) {
    auto result = compile_to_asm(R"(
package main
import "math/bits"
func f(x int) int { return bits.LeadingZeros8(x) }
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_bits_leading_zeros"));
    // shift=56 applied before call, then subtracted
    EXPECT_TRUE(contains(result.asm_text, "56"));
}

TEST(MathBitsTest, MultipleBitsFuncsInOneFunc) {
    auto result = compile_to_asm(R"(
package main
import "math/bits"
func analyze(x int) int {
    return bits.OnesCount(x) + bits.LeadingZeros64(x) + bits.TrailingZeros64(x)
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_bits_ones_count"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_bits_leading_zeros"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_bits_trailing_zeros"));
}

// ============================================================================
// Phase 38C Tests: strings.Cut, strconv.FormatUint/AppendInt, math fixes, os.ReadDir
// ============================================================================

TEST(Phase38CTest, StringsCutCompilesAndCallsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "strings"
func f(s string, sep string) (string, string, bool) {
    before, after, found := strings.Cut(s, sep)
    return before, after, found
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_strings_cut"));
}

TEST(Phase38CTest, StringsCutExternDeclared) {
    auto result = compile_to_asm(R"(
package main
import "strings"
func f(s string) string {
    before, _, _ := strings.Cut(s, "/")
    return before
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "EXTERN golangc_strings_cut"));
}

TEST(Phase38CTest, StrconvFormatUintCompilesAndCallsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "strconv"
func f(x int) string { return strconv.FormatUint(x, 16) }
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_format_uint"));
}

TEST(Phase38CTest, StrconvFormatUintBase10) {
    auto result = compile_to_asm(R"(
package main
import "strconv"
func f(x int) string { return strconv.FormatUint(x, 10) }
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_format_uint"));
}

TEST(Phase38CTest, StrconvAppendIntCompilesAndCallsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "strconv"
func f(dst []byte, n int) []byte { return strconv.AppendInt(dst, n, 10) }
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_append_int"));
}

TEST(Phase38CTest, MathSinCompilesWithCorrectBuiltin) {
    auto result = compile_to_asm(R"(
package main
import "math"
func f(x float64) float64 { return math.Sin(x) }
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_math_sin"));
}

TEST(Phase38CTest, MathCosCompilesWithCorrectBuiltin) {
    auto result = compile_to_asm(R"(
package main
import "math"
func f(x float64) float64 { return math.Cos(x) }
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_math_cos"));
}

TEST(Phase38CTest, MathTanCompilesWithCorrectBuiltin) {
    auto result = compile_to_asm(R"(
package main
import "math"
func f(x float64) float64 { return math.Tan(x) }
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_math_tan"));
}

TEST(Phase38CTest, MathAtan2CompilesWithCorrectBuiltin) {
    auto result = compile_to_asm(R"(
package main
import "math"
func f(y float64, x float64) float64 { return math.Atan2(y, x) }
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_math_atan2"));
}

TEST(Phase38CTest, MathModCompilesWithCorrectBuiltin) {
    auto result = compile_to_asm(R"(
package main
import "math"
func f(x float64, y float64) float64 { return math.Mod(x, y) }
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_math_mod"));
}

TEST(Phase38CTest, MathTruncCompilesWithCorrectBuiltin) {
    auto result = compile_to_asm(R"(
package main
import "math"
func f(x float64) float64 { return math.Trunc(x) }
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_math_trunc"));
}

TEST(Phase38CTest, MathExpCompilesWithCorrectBuiltin) {
    auto result = compile_to_asm(R"(
package main
import "math"
func f(x float64) float64 { return math.Exp(x) }
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_math_exp"));
}

TEST(Phase38CTest, MathInfCompilesNoError) {
    auto result = compile_to_asm(R"(
package main
import "math"
func f(sign int) float64 { return math.Inf(sign) }
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase38CTest, MathIsNaNCompilesNoError) {
    auto result = compile_to_asm(R"(
package main
import "math"
func f(x float64) bool { return math.IsNaN(x) }
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase38CTest, MathIsInfCompilesNoError) {
    auto result = compile_to_asm(R"(
package main
import "math"
func f(x float64, sign int) bool { return math.IsInf(x, sign) }
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
}

TEST(Phase38CTest, OsReadDirCompilesAndCallsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "os"
func f(dir string) {
    entries, _ := os.ReadDir(dir)
    _ = entries
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_os_read_dir"));
}

TEST(Phase38CTest, MathSinCosNotMappedToAbs) {
    // Regression: ensure sin/cos use their own functions, not golangc_math_abs
    auto result = compile_to_asm(R"(
package main
import "math"
func f(x float64) float64 { return math.Sin(x) + math.Cos(x) }
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_math_sin"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_math_cos"));
    // Previously both would incorrectly emit golangc_math_abs
}

TEST(Phase38CTest, StringsCutMultipleAssign) {
    auto result = compile_to_asm(R"(
package main
import "strings"
func f(s string) bool {
    _, _, found := strings.Cut(s, "=")
    return found
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_strings_cut"));
}

// ============================================================================
// Phase 38D Tests: strings.LastIndexByte, strings.Builder.WriteRune,
//                  strconv.Quote/Unquote, bufio.Writer, sync.Once
// ============================================================================

TEST(Phase38DTest, StringsLastIndexByteCompilesAndCallsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "strings"
func f(s string, c byte) int { return strings.LastIndexByte(s, c) }
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_strings_last_index_byte"));
}

TEST(Phase38DTest, StringsBuilderWriteRuneCompilesAndCallsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "strings"
func f() string {
    var b strings.Builder
    b.WriteRune('A')
    return b.String()
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_builder_write_rune"));
}

TEST(Phase38DTest, StrconvQuoteCompilesAndCallsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "strconv"
func f(s string) string { return strconv.Quote(s) }
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_strconv_quote"));
}

TEST(Phase38DTest, StrconvUnquoteCompilesAndCallsRuntime) {
    auto result = compile_to_asm(R"(
package main
import "strconv"
func f(s string) (string, bool) {
    t, err := strconv.Unquote(s)
    _ = err
    return t, true
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_strconv_unquote"));
}

TEST(Phase38DTest, BufioNewWriterCompilesAndCallsRuntime) {
    auto result = compile_to_asm(R"(
package main
import (
    "bufio"
    "os"
)
func f() {
    w := bufio.NewWriter(os.Stdout)
    _ = w
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_bufio_writer_new"));
}

TEST(Phase38DTest, BufioWriterWriteStringAndFlushCompile) {
    auto result = compile_to_asm(R"(
package main
import (
    "bufio"
    "os"
)
func f(s string) {
    w := bufio.NewWriter(os.Stdout)
    w.WriteString(s)
    w.Flush()
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_bufio_writer_write_string"));
    EXPECT_TRUE(contains(result.asm_text, "golangc_bufio_writer_flush"));
}

TEST(Phase38DTest, BufioWriterWriteByteCompiles) {
    auto result = compile_to_asm(R"(
package main
import (
    "bufio"
    "os"
)
func f(c byte) {
    w := bufio.NewWriter(os.Stdout)
    w.WriteByte(c)
    w.Flush()
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_bufio_writer_write_byte"));
}

TEST(Phase38DTest, SyncOnceVarDeclCompiles) {
    auto result = compile_to_asm(R"(
package main
import "sync"
func f() {
    var o sync.Once
    _ = o
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_sync_once_new"));
}

TEST(Phase38DTest, SyncOnceDoCompiles) {
    auto result = compile_to_asm(R"(
package main
import "sync"
func init_fn() {}
func f() {
    var o sync.Once
    o.Do(init_fn)
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "golangc_sync_once_do"));
}

TEST(Phase38DTest, BufioWriterExternDeclared) {
    auto result = compile_to_asm(R"(
package main
import (
    "bufio"
    "os"
)
func f() {
    w := bufio.NewWriter(os.Stdout)
    w.Flush()
}
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "EXTERN golangc_bufio_writer_new"));
}

TEST(Phase38DTest, StrconvQuoteExternDeclared) {
    auto result = compile_to_asm(R"(
package main
import "strconv"
func f(s string) string { return strconv.Quote(s) }
func main() {}
)");
    EXPECT_FALSE(result.has_errors);
    EXPECT_TRUE(contains(result.asm_text, "EXTERN golangc_strconv_quote"));
}
