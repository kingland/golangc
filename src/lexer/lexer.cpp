#include "lexer/lexer.hpp"

#include <cctype>

namespace golangc {

// ============================================================================
// Token utility functions (kept from Phase 1)
// ============================================================================

std::string_view token_kind_to_string(TokenKind kind) {
    switch (kind) {
    case TokenKind::Invalid:          return "Invalid";
    case TokenKind::Eof:              return "EOF";
    case TokenKind::Comment:          return "Comment";
    case TokenKind::Identifier:       return "Identifier";
    case TokenKind::IntLiteral:       return "IntLiteral";
    case TokenKind::FloatLiteral:     return "FloatLiteral";
    case TokenKind::ImaginaryLiteral: return "ImaginaryLiteral";
    case TokenKind::RuneLiteral:      return "RuneLiteral";
    case TokenKind::StringLiteral:    return "StringLiteral";
    case TokenKind::Plus:             return "+";
    case TokenKind::Minus:            return "-";
    case TokenKind::Star:             return "*";
    case TokenKind::Slash:            return "/";
    case TokenKind::Percent:          return "%";
    case TokenKind::Ampersand:        return "&";
    case TokenKind::Pipe:             return "|";
    case TokenKind::Caret:            return "^";
    case TokenKind::ShiftLeft:        return "<<";
    case TokenKind::ShiftRight:       return ">>";
    case TokenKind::AmpCaret:         return "&^";
    case TokenKind::PlusAssign:       return "+=";
    case TokenKind::MinusAssign:      return "-=";
    case TokenKind::StarAssign:       return "*=";
    case TokenKind::SlashAssign:      return "/=";
    case TokenKind::PercentAssign:    return "%=";
    case TokenKind::AmpAssign:        return "&=";
    case TokenKind::PipeAssign:       return "|=";
    case TokenKind::CaretAssign:      return "^=";
    case TokenKind::ShlAssign:        return "<<=";
    case TokenKind::ShrAssign:        return ">>=";
    case TokenKind::AmpCaretAssign:   return "&^=";
    case TokenKind::LogicalAnd:       return "&&";
    case TokenKind::LogicalOr:        return "||";
    case TokenKind::Arrow:            return "<-";
    case TokenKind::Increment:        return "++";
    case TokenKind::Decrement:        return "--";
    case TokenKind::Equal:            return "==";
    case TokenKind::Less:             return "<";
    case TokenKind::Greater:          return ">";
    case TokenKind::Assign:           return "=";
    case TokenKind::Not:              return "!";
    case TokenKind::NotEqual:         return "!=";
    case TokenKind::LessEqual:        return "<=";
    case TokenKind::GreaterEqual:     return ">=";
    case TokenKind::ColonAssign:      return ":=";
    case TokenKind::Ellipsis:         return "...";
    case TokenKind::LParen:           return "(";
    case TokenKind::RParen:           return ")";
    case TokenKind::LBracket:         return "[";
    case TokenKind::RBracket:         return "]";
    case TokenKind::LBrace:           return "{";
    case TokenKind::RBrace:           return "}";
    case TokenKind::Comma:            return ",";
    case TokenKind::Dot:              return ".";
    case TokenKind::Semicolon:        return ";";
    case TokenKind::Colon:            return ":";
    case TokenKind::Tilde:            return "~";
    case TokenKind::KW_break:         return "break";
    case TokenKind::KW_case:          return "case";
    case TokenKind::KW_chan:           return "chan";
    case TokenKind::KW_const:         return "const";
    case TokenKind::KW_continue:      return "continue";
    case TokenKind::KW_default:       return "default";
    case TokenKind::KW_defer:         return "defer";
    case TokenKind::KW_else:          return "else";
    case TokenKind::KW_fallthrough:   return "fallthrough";
    case TokenKind::KW_for:           return "for";
    case TokenKind::KW_func:          return "func";
    case TokenKind::KW_go:            return "go";
    case TokenKind::KW_goto:          return "goto";
    case TokenKind::KW_if:            return "if";
    case TokenKind::KW_import:        return "import";
    case TokenKind::KW_interface:     return "interface";
    case TokenKind::KW_map:           return "map";
    case TokenKind::KW_package:       return "package";
    case TokenKind::KW_range:         return "range";
    case TokenKind::KW_return:        return "return";
    case TokenKind::KW_select:        return "select";
    case TokenKind::KW_struct:        return "struct";
    case TokenKind::KW_switch:        return "switch";
    case TokenKind::KW_type:          return "type";
    case TokenKind::KW_var:           return "var";
    }
    return "Unknown";
}

bool is_keyword(TokenKind kind) {
    return kind >= TokenKind::KW_break && kind <= TokenKind::KW_var;
}

bool is_literal(TokenKind kind) {
    return kind >= TokenKind::Identifier && kind <= TokenKind::StringLiteral;
}

bool is_operator(TokenKind kind) {
    return kind >= TokenKind::Plus && kind <= TokenKind::Tilde;
}

TokenKind lookup_keyword(std::string_view word) {
    // Sorted by frequency of use for slightly faster average lookup
    if (word == "return")      return TokenKind::KW_return;
    if (word == "if")          return TokenKind::KW_if;
    if (word == "func")        return TokenKind::KW_func;
    if (word == "var")         return TokenKind::KW_var;
    if (word == "for")         return TokenKind::KW_for;
    if (word == "range")       return TokenKind::KW_range;
    if (word == "type")        return TokenKind::KW_type;
    if (word == "struct")      return TokenKind::KW_struct;
    if (word == "package")     return TokenKind::KW_package;
    if (word == "import")      return TokenKind::KW_import;
    if (word == "const")       return TokenKind::KW_const;
    if (word == "else")        return TokenKind::KW_else;
    if (word == "switch")      return TokenKind::KW_switch;
    if (word == "case")        return TokenKind::KW_case;
    if (word == "default")     return TokenKind::KW_default;
    if (word == "break")       return TokenKind::KW_break;
    if (word == "continue")    return TokenKind::KW_continue;
    if (word == "defer")       return TokenKind::KW_defer;
    if (word == "go")          return TokenKind::KW_go;
    if (word == "goto")        return TokenKind::KW_goto;
    if (word == "chan")         return TokenKind::KW_chan;
    if (word == "select")      return TokenKind::KW_select;
    if (word == "interface")   return TokenKind::KW_interface;
    if (word == "map")         return TokenKind::KW_map;
    if (word == "fallthrough") return TokenKind::KW_fallthrough;
    return TokenKind::Identifier;
}

// ============================================================================
// Character classification helpers
// ============================================================================

bool Lexer::is_letter(char c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

bool Lexer::is_digit(char c) {
    return c >= '0' && c <= '9';
}

bool Lexer::is_hex_digit(char c) {
    return is_digit(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');
}

bool Lexer::is_octal_digit(char c) {
    return c >= '0' && c <= '7';
}

bool Lexer::is_binary_digit(char c) {
    return c == '0' || c == '1';
}

// ============================================================================
// Lexer core
// ============================================================================

Lexer::Lexer(std::string_view source, std::string_view filename, DiagnosticEngine& diag)
    : source_(source), filename_(filename), diag_(diag) {}

bool Lexer::at_end() const {
    return offset_ >= source_.size();
}

char Lexer::peek() const {
    if (at_end()) return '\0';
    return source_[offset_];
}

char Lexer::peek_next() const {
    if (offset_ + 1 >= source_.size()) return '\0';
    return source_[offset_ + 1];
}

char Lexer::peek_at(uint32_t ahead) const {
    if (offset_ + ahead >= source_.size()) return '\0';
    return source_[offset_ + ahead];
}

char Lexer::advance() {
    char c = source_[offset_++];
    if (c == '\n') {
        ++line_;
        column_ = 1;
    } else {
        ++column_;
    }
    return c;
}

bool Lexer::match_char(char expected) {
    if (at_end() || source_[offset_] != expected) return false;
    advance();
    return true;
}

SourceLocation Lexer::current_location() const {
    return SourceLocation{filename_, {line_, column_}, offset_};
}

Token Lexer::make_token(TokenKind kind, uint32_t start_offset, SourceLocation loc) const {
    return Token{kind, source_.substr(start_offset, offset_ - start_offset), loc};
}

// ============================================================================
// Automatic semicolon insertion
// Per Go spec: a semicolon is automatically inserted into the token stream
// immediately after a line's final token if that token is:
//   - an identifier
//   - an integer, floating-point, imaginary, rune, or string literal
//   - one of: break, continue, fallthrough, return
//   - one of: ++, --, ), ], }
// ============================================================================

bool Lexer::needs_semicolon_after(TokenKind kind) {
    switch (kind) {
    case TokenKind::Identifier:
    case TokenKind::IntLiteral:
    case TokenKind::FloatLiteral:
    case TokenKind::ImaginaryLiteral:
    case TokenKind::RuneLiteral:
    case TokenKind::StringLiteral:
    case TokenKind::KW_break:
    case TokenKind::KW_continue:
    case TokenKind::KW_fallthrough:
    case TokenKind::KW_return:
    case TokenKind::Increment:
    case TokenKind::Decrement:
    case TokenKind::RParen:
    case TokenKind::RBracket:
    case TokenKind::RBrace:
        return true;
    default:
        return false;
    }
}

// ============================================================================
// Whitespace and comment handling
// ============================================================================

void Lexer::skip_whitespace(bool& newline_seen) {
    while (!at_end()) {
        char c = peek();
        if (c == ' ' || c == '\t' || c == '\r') {
            advance();
        } else if (c == '\n') {
            newline_seen = true;
            advance();
        } else {
            break;
        }
    }
}

bool Lexer::skip_line_comment() {
    // We've already consumed "//"
    while (!at_end() && peek() != '\n') {
        advance();
    }
    // Return true if comment ended at newline (for semicolon insertion).
    // Don't consume the newline - let skip_whitespace handle it.
    return true;
}

bool Lexer::skip_block_comment() {
    // We've already consumed "/*"
    bool has_newline = false;
    while (!at_end()) {
        if (peek() == '\n') {
            has_newline = true;
        }
        if (peek() == '*' && peek_next() == '/') {
            advance(); // *
            advance(); // /
            return has_newline;
        }
        advance();
    }
    diag_.error(current_location(), "unterminated block comment");
    return has_newline;
}

// ============================================================================
// Identifier scanning
// ============================================================================

Token Lexer::scan_identifier(SourceLocation loc) {
    uint32_t start = offset_ - 1; // we already consumed the first char
    while (!at_end() && (is_letter(peek()) || is_digit(peek()))) {
        advance();
    }
    auto text = source_.substr(start, offset_ - start);
    TokenKind kind = lookup_keyword(text);
    return Token{kind, text, loc};
}

// ============================================================================
// Number scanning
// Go numeric literals:
//   decimal:   [1-9][0-9_]* or 0
//   hex:       0[xX][0-9a-fA-F_]+
//   octal:     0[oO]?[0-7_]+
//   binary:    0[bB][01_]+
//   float:     digits.digits? or digits[eE][+-]?digits or .digits
//   imaginary: any numeric literal followed by 'i'
// Underscores are permitted between digits (Go 1.13+).
// ============================================================================

Token Lexer::scan_number(SourceLocation loc) {
    uint32_t start = offset_ - 1; // we already consumed the first digit

    char first = source_[start];

    // Check for 0x, 0o, 0b prefixes
    if (first == '0' && !at_end()) {
        char next = peek();
        if (next == 'x' || next == 'X') {
            advance(); // consume x/X
            return scan_hex_number(start, loc);
        }
        if (next == 'o' || next == 'O') {
            advance(); // consume o/O
            return scan_octal_number(start, loc);
        }
        if (next == 'b' || next == 'B') {
            advance(); // consume b/B
            return scan_binary_number(start, loc);
        }
        // Legacy octal: 0[0-7_]+ (but might also be 0.xxx float or just 0)
    }

    return scan_decimal_or_float(start, loc);
}

Token Lexer::scan_hex_number(uint32_t start, SourceLocation loc) {
    // Consume hex digits and underscores
    bool has_digits = false;
    while (!at_end() && (is_hex_digit(peek()) || peek() == '_')) {
        if (peek() != '_') has_digits = true;
        advance();
    }
    if (!has_digits) {
        diag_.error(loc, "hexadecimal literal has no digits");
        return make_token(TokenKind::IntLiteral, start, loc);
    }

    // Hex float: 0x digits '.' digits? 'p' [+-]? digits
    if (!at_end() && peek() == '.') {
        advance(); // consume '.'
        while (!at_end() && (is_hex_digit(peek()) || peek() == '_')) {
            advance();
        }
        // Must have 'p' or 'P' exponent
        if (!at_end() && (peek() == 'p' || peek() == 'P')) {
            advance();
            if (!at_end() && (peek() == '+' || peek() == '-')) {
                advance();
            }
            bool has_exp_digits = false;
            while (!at_end() && (is_digit(peek()) || peek() == '_')) {
                if (peek() != '_') has_exp_digits = true;
                advance();
            }
            if (!has_exp_digits) {
                diag_.error(loc, "hexadecimal floating-point literal has no exponent digits");
            }
        } else {
            diag_.error(loc, "hexadecimal floating-point literal requires 'p' exponent");
        }
        // Check imaginary
        if (!at_end() && peek() == 'i') {
            advance();
            return make_token(TokenKind::ImaginaryLiteral, start, loc);
        }
        return make_token(TokenKind::FloatLiteral, start, loc);
    }

    // Hex int with 'p' exponent -> float
    if (!at_end() && (peek() == 'p' || peek() == 'P')) {
        advance();
        if (!at_end() && (peek() == '+' || peek() == '-')) {
            advance();
        }
        bool has_exp_digits = false;
        while (!at_end() && (is_digit(peek()) || peek() == '_')) {
            if (peek() != '_') has_exp_digits = true;
            advance();
        }
        if (!has_exp_digits) {
            diag_.error(loc, "hexadecimal floating-point literal has no exponent digits");
        }
        if (!at_end() && peek() == 'i') {
            advance();
            return make_token(TokenKind::ImaginaryLiteral, start, loc);
        }
        return make_token(TokenKind::FloatLiteral, start, loc);
    }

    // Check imaginary
    if (!at_end() && peek() == 'i') {
        advance();
        return make_token(TokenKind::ImaginaryLiteral, start, loc);
    }

    return make_token(TokenKind::IntLiteral, start, loc);
}

Token Lexer::scan_octal_number(uint32_t start, SourceLocation loc) {
    bool has_digits = false;
    while (!at_end() && (is_octal_digit(peek()) || peek() == '_')) {
        if (peek() != '_') has_digits = true;
        advance();
    }
    if (!has_digits) {
        diag_.error(loc, "octal literal has no digits");
    }
    if (!at_end() && peek() == 'i') {
        advance();
        return make_token(TokenKind::ImaginaryLiteral, start, loc);
    }
    return make_token(TokenKind::IntLiteral, start, loc);
}

Token Lexer::scan_binary_number(uint32_t start, SourceLocation loc) {
    bool has_digits = false;
    while (!at_end() && (is_binary_digit(peek()) || peek() == '_')) {
        if (peek() != '_') has_digits = true;
        advance();
    }
    if (!has_digits) {
        diag_.error(loc, "binary literal has no digits");
    }
    if (!at_end() && peek() == 'i') {
        advance();
        return make_token(TokenKind::ImaginaryLiteral, start, loc);
    }
    return make_token(TokenKind::IntLiteral, start, loc);
}

Token Lexer::scan_decimal_or_float(uint32_t start, SourceLocation loc) {
    // Consume decimal digits and underscores
    while (!at_end() && (is_digit(peek()) || peek() == '_')) {
        advance();
    }

    bool is_float = false;

    // Check for '.' -> float
    if (!at_end() && peek() == '.') {
        // But not ".." (ellipsis after number is unlikely but guard anyway)
        if (peek_next() != '.') {
            is_float = true;
            advance(); // consume '.'
            // Consume fractional digits
            while (!at_end() && (is_digit(peek()) || peek() == '_')) {
                advance();
            }
        }
    }

    // Check for exponent [eE][+-]?digits
    if (!at_end() && (peek() == 'e' || peek() == 'E')) {
        is_float = true;
        advance(); // consume e/E
        if (!at_end() && (peek() == '+' || peek() == '-')) {
            advance();
        }
        bool has_exp_digits = false;
        while (!at_end() && (is_digit(peek()) || peek() == '_')) {
            if (peek() != '_') has_exp_digits = true;
            advance();
        }
        if (!has_exp_digits) {
            diag_.error(loc, "exponent has no digits");
        }
    }

    // Check imaginary suffix
    if (!at_end() && peek() == 'i') {
        advance();
        return make_token(TokenKind::ImaginaryLiteral, start, loc);
    }

    return make_token(is_float ? TokenKind::FloatLiteral : TokenKind::IntLiteral, start, loc);
}

// ============================================================================
// String literal scanning
// ============================================================================

Token Lexer::scan_string(SourceLocation loc) {
    uint32_t start = offset_ - 1; // we already consumed the opening "

    while (!at_end()) {
        char c = peek();
        if (c == '"') {
            advance(); // consume closing "
            return make_token(TokenKind::StringLiteral, start, loc);
        }
        if (c == '\n') {
            diag_.error(current_location(), "newline in string literal");
            return make_token(TokenKind::StringLiteral, start, loc);
        }
        if (c == '\\') {
            advance(); // consume backslash
            if (!scan_escape_sequence()) {
                // Error already reported by scan_escape_sequence
            }
        } else {
            advance();
        }
    }

    diag_.error(loc, "unterminated string literal");
    return make_token(TokenKind::StringLiteral, start, loc);
}

Token Lexer::scan_raw_string(SourceLocation loc) {
    uint32_t start = offset_ - 1; // we already consumed the opening `

    while (!at_end()) {
        if (peek() == '`') {
            advance(); // consume closing `
            return make_token(TokenKind::StringLiteral, start, loc);
        }
        advance(); // raw strings accept everything including newlines
    }

    diag_.error(loc, "unterminated raw string literal");
    return make_token(TokenKind::StringLiteral, start, loc);
}

// ============================================================================
// Rune literal scanning
// ============================================================================

Token Lexer::scan_rune(SourceLocation loc) {
    uint32_t start = offset_ - 1; // we already consumed the opening '

    if (at_end() || peek() == '\'') {
        if (!at_end()) advance(); // consume closing '
        diag_.error(loc, "empty rune literal");
        return make_token(TokenKind::RuneLiteral, start, loc);
    }

    if (peek() == '\\') {
        advance(); // consume backslash
        scan_escape_sequence();
    } else {
        advance(); // consume the character
    }

    if (at_end() || peek() != '\'') {
        diag_.error(loc, "unterminated rune literal");
        // Try to recover by finding the closing quote on the same line
        while (!at_end() && peek() != '\'' && peek() != '\n') {
            advance();
        }
        if (!at_end() && peek() == '\'') {
            advance();
        }
        return make_token(TokenKind::RuneLiteral, start, loc);
    }

    advance(); // consume closing '
    return make_token(TokenKind::RuneLiteral, start, loc);
}

// ============================================================================
// Escape sequence parsing
// ============================================================================

bool Lexer::scan_escape_sequence() {
    if (at_end()) {
        diag_.error(current_location(), "unterminated escape sequence");
        return false;
    }

    char c = advance();
    switch (c) {
    case 'a': case 'b': case 'f': case 'n': case 'r': case 't': case 'v':
    case '\\': case '\'': case '"':
        return true;
    case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7':
        // Octal: already consumed first digit, need up to 2 more
        scan_octal_digits(2);
        return true;
    case 'x':
        // Hex byte: \xNN
        return scan_hex_digits(2);
    case 'u':
        // Unicode: \uNNNN
        return scan_hex_digits(4);
    case 'U':
        // Unicode: \UNNNNNNNN
        return scan_hex_digits(8);
    default:
        diag_.error(current_location(), "unknown escape sequence '\\{}'", c);
        return false;
    }
}

bool Lexer::scan_hex_digits(int count) {
    for (int i = 0; i < count; ++i) {
        if (at_end() || !is_hex_digit(peek())) {
            diag_.error(current_location(), "expected {} hex digits in escape sequence", count);
            return false;
        }
        advance();
    }
    return true;
}

bool Lexer::scan_octal_digits(int count) {
    for (int i = 0; i < count; ++i) {
        if (at_end() || !is_octal_digit(peek())) {
            break; // octal escapes can be shorter
        }
        advance();
    }
    return true;
}

// ============================================================================
// Main scanning logic
// ============================================================================

Token Lexer::next() {
    // If we have a pending semicolon from newline-based insertion, emit it
    if (pending_semicolon_) {
        pending_semicolon_ = false;
        auto loc = current_location();
        last_token_ = TokenKind::Semicolon;
        return Token{TokenKind::Semicolon, ";", loc};
    }

    // Skip whitespace and comments, tracking newlines for semicolon insertion
    while (true) {
        bool newline_seen = false;
        skip_whitespace(newline_seen);

        // Check for semicolon insertion at newline
        if (newline_seen && needs_semicolon_after(last_token_)) {
            auto loc = current_location();
            last_token_ = TokenKind::Semicolon;
            // Don't set pending_semicolon_ - we emit it now.
            // But we still need to continue scanning after this semicolon
            // on the next call to next().
            return Token{TokenKind::Semicolon, ";", loc};
        }

        // Check for comments
        if (!at_end() && peek() == '/') {
            if (peek_next() == '/') {
                // Line comment - acts like a newline for semicolon insertion
                auto comment_loc = current_location();
                advance(); // /
                advance(); // /
                skip_line_comment();
                // Line comments end at newline, so check semicolon insertion
                // The newline will be consumed by skip_whitespace on next iteration
                // But we need to handle the case where the comment triggers semicolon
                if (needs_semicolon_after(last_token_)) {
                    last_token_ = TokenKind::Semicolon;
                    return Token{TokenKind::Semicolon, ";", comment_loc};
                }
                continue; // loop back to skip more whitespace
            }
            if (peek_next() == '*') {
                auto comment_loc = current_location();
                advance(); // /
                advance(); // *
                bool had_newline = skip_block_comment();
                // Block comment with newline acts like newline for semicolon
                if (had_newline && needs_semicolon_after(last_token_)) {
                    last_token_ = TokenKind::Semicolon;
                    return Token{TokenKind::Semicolon, ";", comment_loc};
                }
                continue; // loop back to skip more whitespace
            }
        }

        break; // no more whitespace or comments
    }

    if (at_end()) {
        // EOF: insert semicolon if needed
        if (needs_semicolon_after(last_token_)) {
            last_token_ = TokenKind::Eof;
            auto loc = current_location();
            // We need to return semicolon now, EOF next time
            pending_semicolon_ = false;
            last_token_ = TokenKind::Semicolon;
            // Actually, we should return semicolon and then EOF on next call
            // Use a different approach: mark that we need to return EOF after
            return Token{TokenKind::Semicolon, ";", loc};
        }
        last_token_ = TokenKind::Eof;
        return Token{TokenKind::Eof, "", current_location()};
    }

    Token tok = scan_token();
    last_token_ = tok.kind;
    return tok;
}

Token Lexer::scan_token() {
    auto loc = current_location();
    char c = advance();

    // Identifiers and keywords
    if (is_letter(c)) {
        return scan_identifier(loc);
    }

    // Number literals
    if (is_digit(c)) {
        return scan_number(loc);
    }

    // Dot can start a float literal like .5
    if (c == '.') {
        if (!at_end() && is_digit(peek())) {
            // Float literal starting with .
            uint32_t start = offset_ - 1;
            while (!at_end() && (is_digit(peek()) || peek() == '_')) {
                advance();
            }
            // Exponent
            if (!at_end() && (peek() == 'e' || peek() == 'E')) {
                advance();
                if (!at_end() && (peek() == '+' || peek() == '-')) advance();
                bool has_digits = false;
                while (!at_end() && (is_digit(peek()) || peek() == '_')) {
                    if (peek() != '_') has_digits = true;
                    advance();
                }
                if (!has_digits) {
                    diag_.error(loc, "exponent has no digits");
                }
            }
            // Imaginary
            if (!at_end() && peek() == 'i') {
                advance();
                return make_token(TokenKind::ImaginaryLiteral, start, loc);
            }
            return make_token(TokenKind::FloatLiteral, start, loc);
        }
        // Check for ellipsis ...
        if (!at_end() && peek() == '.' && peek_next() == '.') {
            advance(); // second .
            advance(); // third .
            return make_token(TokenKind::Ellipsis, offset_ - 3, loc);
        }
        return Token{TokenKind::Dot, ".", loc};
    }

    switch (c) {
    // String literals
    case '"':  return scan_string(loc);
    case '`':  return scan_raw_string(loc);
    case '\'': return scan_rune(loc);

    // Simple single-character tokens
    case '(':  return Token{TokenKind::LParen,   "(", loc};
    case ')':  return Token{TokenKind::RParen,   ")", loc};
    case '[':  return Token{TokenKind::LBracket, "[", loc};
    case ']':  return Token{TokenKind::RBracket, "]", loc};
    case '{':  return Token{TokenKind::LBrace,   "{", loc};
    case '}':  return Token{TokenKind::RBrace,   "}", loc};
    case ',':  return Token{TokenKind::Comma,    ",", loc};
    case ';':  return Token{TokenKind::Semicolon,";", loc};
    case '~':  return Token{TokenKind::Tilde,    "~", loc};

    // Multi-character operators
    case '+':
        if (match_char('+')) return Token{TokenKind::Increment,  "++", loc};
        if (match_char('=')) return Token{TokenKind::PlusAssign, "+=", loc};
        return Token{TokenKind::Plus, "+", loc};

    case '-':
        if (match_char('-')) return Token{TokenKind::Decrement,   "--", loc};
        if (match_char('=')) return Token{TokenKind::MinusAssign, "-=", loc};
        return Token{TokenKind::Minus, "-", loc};

    case '*':
        if (match_char('=')) return Token{TokenKind::StarAssign, "*=", loc};
        return Token{TokenKind::Star, "*", loc};

    case '/':
        // Comments are handled in next() before we get here, so / here is division
        if (match_char('=')) return Token{TokenKind::SlashAssign, "/=", loc};
        return Token{TokenKind::Slash, "/", loc};

    case '%':
        if (match_char('=')) return Token{TokenKind::PercentAssign, "%=", loc};
        return Token{TokenKind::Percent, "%", loc};

    case '&':
        if (match_char('&')) return Token{TokenKind::LogicalAnd, "&&", loc};
        if (match_char('^')) {
            if (match_char('=')) return Token{TokenKind::AmpCaretAssign, "&^=", loc};
            return Token{TokenKind::AmpCaret, "&^", loc};
        }
        if (match_char('=')) return Token{TokenKind::AmpAssign, "&=", loc};
        return Token{TokenKind::Ampersand, "&", loc};

    case '|':
        if (match_char('|')) return Token{TokenKind::LogicalOr, "||", loc};
        if (match_char('=')) return Token{TokenKind::PipeAssign, "|=", loc};
        return Token{TokenKind::Pipe, "|", loc};

    case '^':
        if (match_char('=')) return Token{TokenKind::CaretAssign, "^=", loc};
        return Token{TokenKind::Caret, "^", loc};

    case '<':
        if (match_char('<')) {
            if (match_char('=')) return Token{TokenKind::ShlAssign, "<<=", loc};
            return Token{TokenKind::ShiftLeft, "<<", loc};
        }
        if (match_char('-')) return Token{TokenKind::Arrow,     "<-", loc};
        if (match_char('=')) return Token{TokenKind::LessEqual, "<=", loc};
        return Token{TokenKind::Less, "<", loc};

    case '>':
        if (match_char('>')) {
            if (match_char('=')) return Token{TokenKind::ShrAssign, ">>=", loc};
            return Token{TokenKind::ShiftRight, ">>", loc};
        }
        if (match_char('=')) return Token{TokenKind::GreaterEqual, ">=", loc};
        return Token{TokenKind::Greater, ">", loc};

    case '=':
        if (match_char('=')) return Token{TokenKind::Equal, "==", loc};
        return Token{TokenKind::Assign, "=", loc};

    case '!':
        if (match_char('=')) return Token{TokenKind::NotEqual, "!=", loc};
        return Token{TokenKind::Not, "!", loc};

    case ':':
        if (match_char('=')) return Token{TokenKind::ColonAssign, ":=", loc};
        return Token{TokenKind::Colon, ":", loc};

    default:
        diag_.error(loc, "unexpected character '{}'", c);
        return Token{TokenKind::Invalid, source_.substr(offset_ - 1, 1), loc};
    }
}

std::vector<Token> Lexer::tokenize_all() {
    std::vector<Token> tokens;
    while (true) {
        auto tok = next();
        tokens.push_back(tok);
        if (tok.kind == TokenKind::Eof) break;
    }
    return tokens;
}

} // namespace golangc
