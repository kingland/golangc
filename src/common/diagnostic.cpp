#include "common/diagnostic.hpp"

#include <fmt/format.h>

namespace golangc {

void DiagnosticEngine::emit(DiagnosticSeverity severity, SourceLocation loc, std::string msg) {
    if (severity == DiagnosticSeverity::Error) {
        ++error_count_;
    } else if (severity == DiagnosticSeverity::Warning) {
        ++warning_count_;
    }

    diagnostics_.push_back(Diagnostic{severity, loc, std::move(msg)});

    if (handler_) {
        handler_(diagnostics_.back());
    }
}

[[nodiscard]] std::string format_diagnostic(const Diagnostic& diag) {
    std::string_view severity_str;
    switch (diag.severity) {
    case DiagnosticSeverity::Note:
        severity_str = "note";
        break;
    case DiagnosticSeverity::Warning:
        severity_str = "warning";
        break;
    case DiagnosticSeverity::Error:
        severity_str = "error";
        break;
    }

    return fmt::format("{}: {}: {}", diag.location.to_string(), severity_str, diag.message);
}

} // namespace golangc
