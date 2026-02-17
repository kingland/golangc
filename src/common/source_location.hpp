#pragma once

#include <cstdint>
#include <string>
#include <string_view>

namespace golangc {

/// Represents a position in source code (1-based line and column).
struct SourcePosition {
    uint32_t line   = 1;
    uint32_t column = 1;

    [[nodiscard]] bool operator==(const SourcePosition&) const = default;
    [[nodiscard]] auto operator<=>(const SourcePosition&) const = default;
};

/// Represents a location in a source file (file + position).
struct SourceLocation {
    std::string_view filename;
    SourcePosition position;
    uint32_t offset = 0; // byte offset from start of source

    [[nodiscard]] std::string to_string() const {
        return std::string(filename) + ":" + std::to_string(position.line) + ":" +
               std::to_string(position.column);
    }
};

/// Represents a range in source code (start to end location).
struct SourceRange {
    SourceLocation start;
    SourceLocation end;

    [[nodiscard]] static SourceRange from_single(SourceLocation loc) {
        return SourceRange{loc, loc};
    }

    [[nodiscard]] static SourceRange merge(const SourceRange& a, const SourceRange& b) {
        return SourceRange{a.start, b.end};
    }
};

} // namespace golangc
