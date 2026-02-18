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
