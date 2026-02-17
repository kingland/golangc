#pragma once

#include "common/source_location.hpp"

#include <cstdint>
#include <string>
#include <string_view>

namespace golangc {

/// All token kinds in the Go language.
enum class TokenKind : uint16_t {
    // Special tokens
    Invalid,
    Eof,
    Comment,

    // Literals
    Identifier,
    IntLiteral,
    FloatLiteral,
    ImaginaryLiteral,
    RuneLiteral,
    StringLiteral,

    // Operators and delimiters
    Plus,          // +
    Minus,         // -
    Star,          // *
    Slash,         // /
    Percent,       // %
    Ampersand,     // &
    Pipe,          // |
    Caret,         // ^
    ShiftLeft,     // <<
    ShiftRight,    // >>
    AmpCaret,      // &^
    PlusAssign,    // +=
    MinusAssign,   // -=
    StarAssign,    // *=
    SlashAssign,   // /=
    PercentAssign, // %=
    AmpAssign,     // &=
    PipeAssign,    // |=
    CaretAssign,   // ^=
    ShlAssign,     // <<=
    ShrAssign,     // >>=
    AmpCaretAssign,// &^=
    LogicalAnd,    // &&
    LogicalOr,     // ||
    Arrow,         // <-
    Increment,     // ++
    Decrement,     // --
    Equal,         // ==
    Less,          // <
    Greater,       // >
    Assign,        // =
    Not,           // !
    NotEqual,      // !=
    LessEqual,     // <=
    GreaterEqual,  // >=
    ColonAssign,   // :=
    Ellipsis,      // ...
    LParen,        // (
    RParen,        // )
    LBracket,      // [
    RBracket,      // ]
    LBrace,        // {
    RBrace,        // }
    Comma,         // ,
    Dot,           // .
    Semicolon,     // ;
    Colon,         // :
    Tilde,         // ~

    // Keywords (25 Go keywords)
    KW_break,
    KW_case,
    KW_chan,
    KW_const,
    KW_continue,
    KW_default,
    KW_defer,
    KW_else,
    KW_fallthrough,
    KW_for,
    KW_func,
    KW_go,
    KW_goto,
    KW_if,
    KW_import,
    KW_interface,
    KW_map,
    KW_package,
    KW_range,
    KW_return,
    KW_select,
    KW_struct,
    KW_switch,
    KW_type,
    KW_var,
};

/// Returns the string representation of a token kind.
[[nodiscard]] std::string_view token_kind_to_string(TokenKind kind);

/// Check if a token kind is a keyword.
[[nodiscard]] bool is_keyword(TokenKind kind);

/// Check if a token kind is a literal.
[[nodiscard]] bool is_literal(TokenKind kind);

/// Check if a token kind is an operator.
[[nodiscard]] bool is_operator(TokenKind kind);

/// Look up a keyword from a string. Returns TokenKind::Identifier if not a keyword.
[[nodiscard]] TokenKind lookup_keyword(std::string_view word);

/// A single token from the lexer.
struct Token {
    TokenKind kind = TokenKind::Invalid;
    std::string_view text;
    SourceLocation location;

    [[nodiscard]] bool is(TokenKind k) const { return kind == k; }
    [[nodiscard]] bool is_not(TokenKind k) const { return kind != k; }
};

} // namespace golangc
