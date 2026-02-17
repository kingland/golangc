#pragma once

#include "common/diagnostic.hpp"
#include "common/source_location.hpp"
#include "lexer/token.hpp"

#include <string_view>
#include <vector>

namespace golangc {

/// Lexer for Go source code.
/// Converts source text into a stream of tokens.
/// Handles automatic semicolon insertion per the Go specification.
class Lexer {
public:
    Lexer(std::string_view source, std::string_view filename, DiagnosticEngine& diag);

    /// Get the next token from the source.
    [[nodiscard]] Token next();

    /// Tokenize the entire source and return all tokens.
    [[nodiscard]] std::vector<Token> tokenize_all();

    /// Check if we've reached the end of the source.
    [[nodiscard]] bool at_end() const;

private:
    std::string_view source_;
    std::string_view filename_;
    DiagnosticEngine& diag_;
    uint32_t offset_ = 0;
    uint32_t line_   = 1;
    uint32_t column_ = 1;

    // For automatic semicolon insertion
    TokenKind last_token_ = TokenKind::Invalid;
    bool pending_semicolon_ = false;

    [[nodiscard]] char peek() const;
    [[nodiscard]] char peek_next() const;
    [[nodiscard]] char peek_at(uint32_t ahead) const;
    char advance();
    bool match_char(char expected);
    void skip_whitespace(bool& newline_seen);
    [[nodiscard]] SourceLocation current_location() const;

    Token scan_token();
    Token make_token(TokenKind kind, uint32_t start_offset, SourceLocation loc) const;
    Token scan_identifier(SourceLocation loc);
    Token scan_number(SourceLocation loc);
    Token scan_hex_number(uint32_t start_offset, SourceLocation loc);
    Token scan_octal_number(uint32_t start_offset, SourceLocation loc);
    Token scan_binary_number(uint32_t start_offset, SourceLocation loc);
    Token scan_decimal_or_float(uint32_t start_offset, SourceLocation loc);
    Token scan_string(SourceLocation loc);
    Token scan_raw_string(SourceLocation loc);
    Token scan_rune(SourceLocation loc);
    bool skip_line_comment();
    bool skip_block_comment();

    // Escape sequence parsing helpers
    bool scan_escape_sequence();
    bool scan_hex_digits(int count);
    bool scan_octal_digits(int count);

    // Unicode helpers
    [[nodiscard]] static bool is_letter(char c);
    [[nodiscard]] static bool is_digit(char c);
    [[nodiscard]] static bool is_hex_digit(char c);
    [[nodiscard]] static bool is_octal_digit(char c);
    [[nodiscard]] static bool is_binary_digit(char c);

    // Automatic semicolon insertion: returns true if a semicolon should
    // be inserted before a newline given the last token kind.
    [[nodiscard]] static bool needs_semicolon_after(TokenKind kind);
};

} // namespace golangc
