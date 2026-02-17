#include "common/diagnostic.hpp"

#include <gtest/gtest.h>

using namespace golangc;

TEST(DiagnosticEngineTest, InitialState) {
    DiagnosticEngine diag;
    EXPECT_FALSE(diag.has_errors());
    EXPECT_EQ(diag.error_count(), 0u);
    EXPECT_EQ(diag.warning_count(), 0u);
    EXPECT_TRUE(diag.diagnostics().empty());
}

TEST(DiagnosticEngineTest, ReportError) {
    DiagnosticEngine diag;
    SourceLocation loc{"test.go", {1, 1}, 0};

    diag.error(loc, "undefined variable");

    EXPECT_TRUE(diag.has_errors());
    EXPECT_EQ(diag.error_count(), 1u);
    EXPECT_EQ(diag.diagnostics().size(), 1u);
    EXPECT_EQ(diag.diagnostics()[0].message, "undefined variable");
    EXPECT_EQ(diag.diagnostics()[0].severity, DiagnosticSeverity::Error);
}

TEST(DiagnosticEngineTest, ReportWarning) {
    DiagnosticEngine diag;
    SourceLocation loc{"test.go", {1, 1}, 0};

    diag.warning(loc, "unused variable");

    EXPECT_FALSE(diag.has_errors());
    EXPECT_EQ(diag.warning_count(), 1u);
    EXPECT_EQ(diag.diagnostics().size(), 1u);
}

TEST(DiagnosticEngineTest, ReportNote) {
    DiagnosticEngine diag;
    SourceLocation loc{"test.go", {1, 1}, 0};

    diag.note(loc, "declared here");

    EXPECT_FALSE(diag.has_errors());
    EXPECT_EQ(diag.error_count(), 0u);
    EXPECT_EQ(diag.warning_count(), 0u);
    EXPECT_EQ(diag.diagnostics().size(), 1u);
}

TEST(DiagnosticEngineTest, FormattedError) {
    DiagnosticEngine diag;
    SourceLocation loc{"test.go", {10, 5}, 42};

    diag.error(loc, "undefined: {}", "foo");

    EXPECT_TRUE(diag.has_errors());
    EXPECT_EQ(diag.diagnostics()[0].message, "undefined: foo");
}

TEST(DiagnosticEngineTest, CustomHandler) {
    DiagnosticEngine diag;
    SourceLocation loc{"test.go", {1, 1}, 0};

    Diagnostic captured{};
    diag.set_handler([&captured](const Diagnostic& d) { captured = d; });

    diag.error(loc, "test error");

    EXPECT_EQ(captured.message, "test error");
    EXPECT_EQ(captured.severity, DiagnosticSeverity::Error);
}

TEST(DiagnosticEngineTest, Clear) {
    DiagnosticEngine diag;
    SourceLocation loc{"test.go", {1, 1}, 0};

    diag.error(loc, "error1");
    diag.warning(loc, "warning1");
    diag.clear();

    EXPECT_FALSE(diag.has_errors());
    EXPECT_EQ(diag.error_count(), 0u);
    EXPECT_EQ(diag.warning_count(), 0u);
    EXPECT_TRUE(diag.diagnostics().empty());
}

TEST(FormatDiagnosticTest, ErrorFormat) {
    Diagnostic diag{DiagnosticSeverity::Error, {"main.go", {10, 5}, 42}, "undefined: foo"};
    std::string result = format_diagnostic(diag);
    EXPECT_EQ(result, "main.go:10:5: error: undefined: foo");
}

TEST(FormatDiagnosticTest, WarningFormat) {
    Diagnostic diag{DiagnosticSeverity::Warning, {"main.go", {3, 1}, 10}, "unused variable"};
    std::string result = format_diagnostic(diag);
    EXPECT_EQ(result, "main.go:3:1: warning: unused variable");
}

TEST(FormatDiagnosticTest, NoteFormat) {
    Diagnostic diag{DiagnosticSeverity::Note, {"main.go", {1, 1}, 0}, "declared here"};
    std::string result = format_diagnostic(diag);
    EXPECT_EQ(result, "main.go:1:1: note: declared here");
}
