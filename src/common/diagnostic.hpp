#pragma once

#include "source_location.hpp"

#include <fmt/format.h>

#include <functional>
#include <string>
#include <string_view>
#include <vector>

namespace golangc {

/// Severity levels for diagnostics.
enum class DiagnosticSeverity : uint8_t {
    Note,
    Warning,
    Error,
};

/// A single diagnostic message with location and severity.
struct Diagnostic {
    DiagnosticSeverity severity;
    SourceLocation location;
    std::string message;
};

/// Collects and manages compiler diagnostics.
class DiagnosticEngine {
public:
    using DiagnosticHandler = std::function<void(const Diagnostic&)>;

    /// Set a custom handler for diagnostics (e.g., for testing).
    void set_handler(DiagnosticHandler handler) { handler_ = std::move(handler); }

    /// Report an error diagnostic.
    void error(SourceLocation loc, std::string_view msg) {
        emit(DiagnosticSeverity::Error, loc, std::string(msg));
    }

    /// Report a warning diagnostic.
    void warning(SourceLocation loc, std::string_view msg) {
        emit(DiagnosticSeverity::Warning, loc, std::string(msg));
    }

    /// Report a note diagnostic.
    void note(SourceLocation loc, std::string_view msg) {
        emit(DiagnosticSeverity::Note, loc, std::string(msg));
    }

    /// Report a formatted error diagnostic.
    template <typename... Args>
    void error(SourceLocation loc, fmt::format_string<Args...> fmt_str, Args&&... args) {
        emit(DiagnosticSeverity::Error, loc,
             fmt::format(fmt_str, std::forward<Args>(args)...));
    }

    /// Report a formatted warning diagnostic.
    template <typename... Args>
    void warning(SourceLocation loc, fmt::format_string<Args...> fmt_str, Args&&... args) {
        emit(DiagnosticSeverity::Warning, loc,
             fmt::format(fmt_str, std::forward<Args>(args)...));
    }

    /// Report a formatted note diagnostic.
    template <typename... Args>
    void note(SourceLocation loc, fmt::format_string<Args...> fmt_str, Args&&... args) {
        emit(DiagnosticSeverity::Note, loc,
             fmt::format(fmt_str, std::forward<Args>(args)...));
    }

    [[nodiscard]] bool has_errors() const { return error_count_ > 0; }
    [[nodiscard]] uint32_t error_count() const { return error_count_; }
    [[nodiscard]] uint32_t warning_count() const { return warning_count_; }
    [[nodiscard]] const std::vector<Diagnostic>& diagnostics() const { return diagnostics_; }

    void clear() {
        diagnostics_.clear();
        error_count_ = 0;
        warning_count_ = 0;
    }

private:
    void emit(DiagnosticSeverity severity, SourceLocation loc, std::string msg);

    std::vector<Diagnostic> diagnostics_;
    DiagnosticHandler handler_;
    uint32_t error_count_   = 0;
    uint32_t warning_count_ = 0;
};

/// Format a diagnostic for display.
[[nodiscard]] std::string format_diagnostic(const Diagnostic& diag);

} // namespace golangc
