#include "common/diagnostic.hpp"
#include "lexer/lexer.hpp"
#include "lexer/token.hpp"

#include <gtest/gtest.h>

#include <algorithm>
#include <string>
#include <vector>

using namespace golangc;

// ============================================================================
// Test helpers
// ============================================================================

// Tokenize expecting no errors. Returns all tokens excluding EOF.
// Note: this includes auto-inserted semicolons (including at EOF).
static std::vector<Token> tokenize_ok(std::string_view source) {
    DiagnosticEngine diag;
    Lexer lexer(source, "test.go", diag);
    auto tokens = lexer.tokenize_all();
    EXPECT_FALSE(diag.has_errors()) << "Unexpected errors tokenizing: " << source;
    if (!tokens.empty() && tokens.back().kind == TokenKind::Eof) {
        tokens.pop_back();
    }
    return tokens;
}

// Tokenize and strip ALL auto-inserted semicolons for tests that only
// care about the "real" tokens (identifiers, operators, etc.)
static std::vector<Token> tokenize_no_semis(std::string_view source) {
    auto tokens = tokenize_ok(source);
    tokens.erase(std::remove_if(tokens.begin(), tokens.end(),
                                [](const Token& t) { return t.kind == TokenKind::Semicolon; }),
                 tokens.end());
    return tokens;
}

// Tokenize expecting errors
static std::vector<Token> tokenize_with_errors(std::string_view source,
                                                DiagnosticEngine& diag) {
    Lexer lexer(source, "test.go", diag);
    auto tokens = lexer.tokenize_all();
    if (!tokens.empty() && tokens.back().kind == TokenKind::Eof) {
        tokens.pop_back();
    }
    return tokens;
}

// ============================================================================
// Token utility tests
// ============================================================================

TEST(TokenTest, TokenKindToString) {
    EXPECT_EQ(token_kind_to_string(TokenKind::Eof), "EOF");
    EXPECT_EQ(token_kind_to_string(TokenKind::Plus), "+");
    EXPECT_EQ(token_kind_to_string(TokenKind::KW_func), "func");
    EXPECT_EQ(token_kind_to_string(TokenKind::KW_package), "package");
    EXPECT_EQ(token_kind_to_string(TokenKind::Ellipsis), "...");
    EXPECT_EQ(token_kind_to_string(TokenKind::ColonAssign), ":=");
    EXPECT_EQ(token_kind_to_string(TokenKind::Arrow), "<-");
}

TEST(TokenTest, IsKeyword) {
    EXPECT_TRUE(is_keyword(TokenKind::KW_func));
    EXPECT_TRUE(is_keyword(TokenKind::KW_var));
    EXPECT_TRUE(is_keyword(TokenKind::KW_break));
    EXPECT_TRUE(is_keyword(TokenKind::KW_fallthrough));
    EXPECT_FALSE(is_keyword(TokenKind::Identifier));
    EXPECT_FALSE(is_keyword(TokenKind::Plus));
    EXPECT_FALSE(is_keyword(TokenKind::Eof));
}

TEST(TokenTest, IsLiteral) {
    EXPECT_TRUE(is_literal(TokenKind::Identifier));
    EXPECT_TRUE(is_literal(TokenKind::IntLiteral));
    EXPECT_TRUE(is_literal(TokenKind::FloatLiteral));
    EXPECT_TRUE(is_literal(TokenKind::ImaginaryLiteral));
    EXPECT_TRUE(is_literal(TokenKind::RuneLiteral));
    EXPECT_TRUE(is_literal(TokenKind::StringLiteral));
    EXPECT_FALSE(is_literal(TokenKind::Plus));
    EXPECT_FALSE(is_literal(TokenKind::KW_func));
}

TEST(TokenTest, IsOperator) {
    EXPECT_TRUE(is_operator(TokenKind::Plus));
    EXPECT_TRUE(is_operator(TokenKind::Minus));
    EXPECT_TRUE(is_operator(TokenKind::Arrow));
    EXPECT_TRUE(is_operator(TokenKind::Ellipsis));
    EXPECT_TRUE(is_operator(TokenKind::Tilde));
    EXPECT_FALSE(is_operator(TokenKind::Identifier));
    EXPECT_FALSE(is_operator(TokenKind::KW_func));
}

TEST(TokenTest, LookupAllKeywords) {
    EXPECT_EQ(lookup_keyword("break"), TokenKind::KW_break);
    EXPECT_EQ(lookup_keyword("case"), TokenKind::KW_case);
    EXPECT_EQ(lookup_keyword("chan"), TokenKind::KW_chan);
    EXPECT_EQ(lookup_keyword("const"), TokenKind::KW_const);
    EXPECT_EQ(lookup_keyword("continue"), TokenKind::KW_continue);
    EXPECT_EQ(lookup_keyword("default"), TokenKind::KW_default);
    EXPECT_EQ(lookup_keyword("defer"), TokenKind::KW_defer);
    EXPECT_EQ(lookup_keyword("else"), TokenKind::KW_else);
    EXPECT_EQ(lookup_keyword("fallthrough"), TokenKind::KW_fallthrough);
    EXPECT_EQ(lookup_keyword("for"), TokenKind::KW_for);
    EXPECT_EQ(lookup_keyword("func"), TokenKind::KW_func);
    EXPECT_EQ(lookup_keyword("go"), TokenKind::KW_go);
    EXPECT_EQ(lookup_keyword("goto"), TokenKind::KW_goto);
    EXPECT_EQ(lookup_keyword("if"), TokenKind::KW_if);
    EXPECT_EQ(lookup_keyword("import"), TokenKind::KW_import);
    EXPECT_EQ(lookup_keyword("interface"), TokenKind::KW_interface);
    EXPECT_EQ(lookup_keyword("map"), TokenKind::KW_map);
    EXPECT_EQ(lookup_keyword("package"), TokenKind::KW_package);
    EXPECT_EQ(lookup_keyword("range"), TokenKind::KW_range);
    EXPECT_EQ(lookup_keyword("return"), TokenKind::KW_return);
    EXPECT_EQ(lookup_keyword("select"), TokenKind::KW_select);
    EXPECT_EQ(lookup_keyword("struct"), TokenKind::KW_struct);
    EXPECT_EQ(lookup_keyword("switch"), TokenKind::KW_switch);
    EXPECT_EQ(lookup_keyword("type"), TokenKind::KW_type);
    EXPECT_EQ(lookup_keyword("var"), TokenKind::KW_var);
    EXPECT_EQ(lookup_keyword("main"), TokenKind::Identifier);
    EXPECT_EQ(lookup_keyword("Function"), TokenKind::Identifier);
    EXPECT_EQ(lookup_keyword(""), TokenKind::Identifier);
}

TEST(TokenTest, TokenIs) {
    Token tok{TokenKind::Plus, "+", {}};
    EXPECT_TRUE(tok.is(TokenKind::Plus));
    EXPECT_FALSE(tok.is(TokenKind::Minus));
    EXPECT_TRUE(tok.is_not(TokenKind::Minus));
    EXPECT_FALSE(tok.is_not(TokenKind::Plus));
}

// ============================================================================
// Basic lexer tests
// ============================================================================

TEST(LexerTest, EmptySource) {
    DiagnosticEngine diag;
    Lexer lexer("", "test.go", diag);
    auto tok = lexer.next();
    EXPECT_EQ(tok.kind, TokenKind::Eof);
    EXPECT_FALSE(diag.has_errors());
}

TEST(LexerTest, WhitespaceOnly) {
    auto tokens = tokenize_no_semis("   \t\t  \r\n  \n  ");
    EXPECT_TRUE(tokens.empty());
}

TEST(LexerTest, SourceLocations) {
    DiagnosticEngine diag;
    Lexer lexer("x + y", "test.go", diag);
    auto tok1 = lexer.next();
    EXPECT_EQ(tok1.location.position.line, 1u);
    EXPECT_EQ(tok1.location.position.column, 1u);

    auto tok2 = lexer.next(); // +
    EXPECT_EQ(tok2.location.position.line, 1u);
    EXPECT_EQ(tok2.location.position.column, 3u);

    auto tok3 = lexer.next(); // y
    EXPECT_EQ(tok3.location.position.line, 1u);
    EXPECT_EQ(tok3.location.position.column, 5u);
}

TEST(LexerTest, MultiLineLocations) {
    DiagnosticEngine diag;
    Lexer lexer("x\ny", "test.go", diag);
    auto tok1 = lexer.next(); // x
    EXPECT_EQ(tok1.location.position.line, 1u);

    auto semi = lexer.next(); // auto-inserted ;
    EXPECT_EQ(semi.kind, TokenKind::Semicolon);

    auto tok2 = lexer.next(); // y
    EXPECT_EQ(tok2.location.position.line, 2u);
    EXPECT_EQ(tok2.location.position.column, 1u);
}

// ============================================================================
// Identifier tests (use tokenize_no_semis to ignore EOF semicolons)
// ============================================================================

TEST(LexerTest, SimpleIdentifiers) {
    auto tokens = tokenize_no_semis("x y foo bar_baz _private");
    ASSERT_EQ(tokens.size(), 5u);
    for (const auto& tok : tokens) {
        EXPECT_EQ(tok.kind, TokenKind::Identifier);
    }
    EXPECT_EQ(tokens[0].text, "x");
    EXPECT_EQ(tokens[1].text, "y");
    EXPECT_EQ(tokens[2].text, "foo");
    EXPECT_EQ(tokens[3].text, "bar_baz");
    EXPECT_EQ(tokens[4].text, "_private");
}

TEST(LexerTest, IdentifiersWithDigits) {
    auto tokens = tokenize_no_semis("x1 foo2bar a123");
    ASSERT_EQ(tokens.size(), 3u);
    EXPECT_EQ(tokens[0].text, "x1");
    EXPECT_EQ(tokens[1].text, "foo2bar");
    EXPECT_EQ(tokens[2].text, "a123");
}

TEST(LexerTest, UnderscoreIdentifier) {
    auto tokens = tokenize_no_semis("_ __ _x x_");
    ASSERT_EQ(tokens.size(), 4u);
    EXPECT_EQ(tokens[0].text, "_");
    EXPECT_EQ(tokens[1].text, "__");
    EXPECT_EQ(tokens[2].text, "_x");
    EXPECT_EQ(tokens[3].text, "x_");
}

// ============================================================================
// Keyword tests
// ============================================================================

TEST(LexerTest, AllKeywords) {
    auto tokens = tokenize_no_semis(
        "break case chan const continue default defer else "
        "fallthrough for func go goto if import interface "
        "map package range return select struct switch type var");
    ASSERT_EQ(tokens.size(), 25u);
    EXPECT_EQ(tokens[0].kind, TokenKind::KW_break);
    EXPECT_EQ(tokens[1].kind, TokenKind::KW_case);
    EXPECT_EQ(tokens[2].kind, TokenKind::KW_chan);
    EXPECT_EQ(tokens[3].kind, TokenKind::KW_const);
    EXPECT_EQ(tokens[4].kind, TokenKind::KW_continue);
    EXPECT_EQ(tokens[5].kind, TokenKind::KW_default);
    EXPECT_EQ(tokens[6].kind, TokenKind::KW_defer);
    EXPECT_EQ(tokens[7].kind, TokenKind::KW_else);
    EXPECT_EQ(tokens[8].kind, TokenKind::KW_fallthrough);
    EXPECT_EQ(tokens[9].kind, TokenKind::KW_for);
    EXPECT_EQ(tokens[10].kind, TokenKind::KW_func);
    EXPECT_EQ(tokens[11].kind, TokenKind::KW_go);
    EXPECT_EQ(tokens[12].kind, TokenKind::KW_goto);
    EXPECT_EQ(tokens[13].kind, TokenKind::KW_if);
    EXPECT_EQ(tokens[14].kind, TokenKind::KW_import);
    EXPECT_EQ(tokens[15].kind, TokenKind::KW_interface);
    EXPECT_EQ(tokens[16].kind, TokenKind::KW_map);
    EXPECT_EQ(tokens[17].kind, TokenKind::KW_package);
    EXPECT_EQ(tokens[18].kind, TokenKind::KW_range);
    EXPECT_EQ(tokens[19].kind, TokenKind::KW_return);
    EXPECT_EQ(tokens[20].kind, TokenKind::KW_select);
    EXPECT_EQ(tokens[21].kind, TokenKind::KW_struct);
    EXPECT_EQ(tokens[22].kind, TokenKind::KW_switch);
    EXPECT_EQ(tokens[23].kind, TokenKind::KW_type);
    EXPECT_EQ(tokens[24].kind, TokenKind::KW_var);
}

TEST(LexerTest, KeywordLikeIdentifiers) {
    auto tokens = tokenize_no_semis("breakpoint cases vars iface");
    ASSERT_EQ(tokens.size(), 4u);
    for (const auto& tok : tokens) {
        EXPECT_EQ(tok.kind, TokenKind::Identifier);
    }
}

// ============================================================================
// Integer literal tests
// ============================================================================

TEST(LexerTest, DecimalIntegers) {
    auto tokens = tokenize_no_semis("0 1 42 1000000");
    ASSERT_EQ(tokens.size(), 4u);
    for (const auto& tok : tokens) {
        EXPECT_EQ(tok.kind, TokenKind::IntLiteral);
    }
    EXPECT_EQ(tokens[0].text, "0");
    EXPECT_EQ(tokens[1].text, "1");
    EXPECT_EQ(tokens[2].text, "42");
    EXPECT_EQ(tokens[3].text, "1000000");
}

TEST(LexerTest, DecimalIntegersWithUnderscores) {
    auto tokens = tokenize_no_semis("1_000_000 1_0");
    ASSERT_EQ(tokens.size(), 2u);
    EXPECT_EQ(tokens[0].kind, TokenKind::IntLiteral);
    EXPECT_EQ(tokens[0].text, "1_000_000");
    EXPECT_EQ(tokens[1].text, "1_0");
}

TEST(LexerTest, HexIntegers) {
    auto tokens = tokenize_no_semis("0x0 0xFF 0XDeadBeef 0x1_2_3");
    ASSERT_EQ(tokens.size(), 4u);
    for (const auto& tok : tokens) {
        EXPECT_EQ(tok.kind, TokenKind::IntLiteral);
    }
    EXPECT_EQ(tokens[0].text, "0x0");
    EXPECT_EQ(tokens[1].text, "0xFF");
    EXPECT_EQ(tokens[2].text, "0XDeadBeef");
    EXPECT_EQ(tokens[3].text, "0x1_2_3");
}

TEST(LexerTest, OctalIntegers) {
    auto tokens = tokenize_no_semis("0o0 0o777 0O17 0o7_7_7");
    ASSERT_EQ(tokens.size(), 4u);
    for (const auto& tok : tokens) {
        EXPECT_EQ(tok.kind, TokenKind::IntLiteral);
    }
    EXPECT_EQ(tokens[0].text, "0o0");
    EXPECT_EQ(tokens[1].text, "0o777");
    EXPECT_EQ(tokens[2].text, "0O17");
    EXPECT_EQ(tokens[3].text, "0o7_7_7");
}

TEST(LexerTest, BinaryIntegers) {
    auto tokens = tokenize_no_semis("0b0 0b1 0B1010 0b1_0_1_0");
    ASSERT_EQ(tokens.size(), 4u);
    for (const auto& tok : tokens) {
        EXPECT_EQ(tok.kind, TokenKind::IntLiteral);
    }
    EXPECT_EQ(tokens[0].text, "0b0");
    EXPECT_EQ(tokens[1].text, "0b1");
    EXPECT_EQ(tokens[2].text, "0B1010");
    EXPECT_EQ(tokens[3].text, "0b1_0_1_0");
}

// ============================================================================
// Float literal tests
// ============================================================================

TEST(LexerTest, FloatLiterals) {
    auto tokens = tokenize_no_semis("1.0 0.5 3.14 100.001");
    ASSERT_EQ(tokens.size(), 4u);
    for (const auto& tok : tokens) {
        EXPECT_EQ(tok.kind, TokenKind::FloatLiteral);
    }
    EXPECT_EQ(tokens[0].text, "1.0");
    EXPECT_EQ(tokens[1].text, "0.5");
    EXPECT_EQ(tokens[2].text, "3.14");
    EXPECT_EQ(tokens[3].text, "100.001");
}

TEST(LexerTest, FloatWithExponent) {
    auto tokens = tokenize_no_semis("1e10 1E10 1.5e+3 2.5e-4 1e0");
    ASSERT_EQ(tokens.size(), 5u);
    for (const auto& tok : tokens) {
        EXPECT_EQ(tok.kind, TokenKind::FloatLiteral);
    }
}

TEST(LexerTest, FloatStartingWithDot) {
    auto tokens = tokenize_no_semis(".5 .123 .0");
    ASSERT_EQ(tokens.size(), 3u);
    for (const auto& tok : tokens) {
        EXPECT_EQ(tok.kind, TokenKind::FloatLiteral);
    }
    EXPECT_EQ(tokens[0].text, ".5");
    EXPECT_EQ(tokens[1].text, ".123");
    EXPECT_EQ(tokens[2].text, ".0");
}

TEST(LexerTest, FloatWithUnderscores) {
    auto tokens = tokenize_no_semis("1_000.5 1.000_001 1_0e1_0");
    ASSERT_EQ(tokens.size(), 3u);
    for (const auto& tok : tokens) {
        EXPECT_EQ(tok.kind, TokenKind::FloatLiteral);
    }
}

// ============================================================================
// Imaginary literal tests
// ============================================================================

TEST(LexerTest, ImaginaryLiterals) {
    auto tokens = tokenize_no_semis("1i 42i 0i");
    ASSERT_EQ(tokens.size(), 3u);
    for (const auto& tok : tokens) {
        EXPECT_EQ(tok.kind, TokenKind::ImaginaryLiteral);
    }
}

TEST(LexerTest, ImaginaryFloatLiterals) {
    auto tokens = tokenize_no_semis("1.0i 0.5i .5i 1e3i");
    ASSERT_EQ(tokens.size(), 4u);
    for (const auto& tok : tokens) {
        EXPECT_EQ(tok.kind, TokenKind::ImaginaryLiteral);
    }
}

TEST(LexerTest, ImaginaryHexLiteral) {
    auto tokens = tokenize_no_semis("0xFFi");
    ASSERT_EQ(tokens.size(), 1u);
    EXPECT_EQ(tokens[0].kind, TokenKind::ImaginaryLiteral);
    EXPECT_EQ(tokens[0].text, "0xFFi");
}

// ============================================================================
// String literal tests
// ============================================================================

TEST(LexerTest, SimpleStrings) {
    auto tokens = tokenize_no_semis(R"("hello" "world" "")");
    ASSERT_EQ(tokens.size(), 3u);
    for (const auto& tok : tokens) {
        EXPECT_EQ(tok.kind, TokenKind::StringLiteral);
    }
    EXPECT_EQ(tokens[0].text, "\"hello\"");
    EXPECT_EQ(tokens[1].text, "\"world\"");
    EXPECT_EQ(tokens[2].text, "\"\"");
}

TEST(LexerTest, StringWithEscapes) {
    auto tokens = tokenize_no_semis(R"("hello\nworld" "tab\there" "quote\"inside" "back\\slash")");
    ASSERT_EQ(tokens.size(), 4u);
    for (const auto& tok : tokens) {
        EXPECT_EQ(tok.kind, TokenKind::StringLiteral);
    }
}

TEST(LexerTest, StringWithHexEscapes) {
    auto tokens = tokenize_no_semis(R"("\x41\x42\x43")");
    ASSERT_EQ(tokens.size(), 1u);
    EXPECT_EQ(tokens[0].kind, TokenKind::StringLiteral);
}

TEST(LexerTest, StringWithUnicodeEscapes) {
    auto tokens = tokenize_no_semis(R"("\u0048\u0065\u006C\u006C\u006F")");
    ASSERT_EQ(tokens.size(), 1u);
    EXPECT_EQ(tokens[0].kind, TokenKind::StringLiteral);
}

TEST(LexerTest, StringWithBigUnicodeEscape) {
    auto tokens = tokenize_no_semis(R"("\U00000048\U00000065")");
    ASSERT_EQ(tokens.size(), 1u);
    EXPECT_EQ(tokens[0].kind, TokenKind::StringLiteral);
}

TEST(LexerTest, StringWithOctalEscapes) {
    auto tokens = tokenize_no_semis(R"("\000\377\101")");
    ASSERT_EQ(tokens.size(), 1u);
    EXPECT_EQ(tokens[0].kind, TokenKind::StringLiteral);
}

TEST(LexerTest, RawStrings) {
    auto tokens = tokenize_no_semis("`hello` `raw\\nstring` ``");
    ASSERT_EQ(tokens.size(), 3u);
    for (const auto& tok : tokens) {
        EXPECT_EQ(tok.kind, TokenKind::StringLiteral);
    }
    EXPECT_EQ(tokens[0].text, "`hello`");
    EXPECT_EQ(tokens[1].text, "`raw\\nstring`");
    EXPECT_EQ(tokens[2].text, "``");
}

TEST(LexerTest, RawStringWithNewlines) {
    auto tokens = tokenize_no_semis("`line1\nline2\nline3`");
    ASSERT_EQ(tokens.size(), 1u);
    EXPECT_EQ(tokens[0].kind, TokenKind::StringLiteral);
}

// ============================================================================
// Rune literal tests
// ============================================================================

TEST(LexerTest, SimpleRunes) {
    auto tokens = tokenize_no_semis("'a' 'Z' '0' ' '");
    ASSERT_EQ(tokens.size(), 4u);
    for (const auto& tok : tokens) {
        EXPECT_EQ(tok.kind, TokenKind::RuneLiteral);
    }
    EXPECT_EQ(tokens[0].text, "'a'");
}

TEST(LexerTest, RuneEscapes) {
    auto tokens = tokenize_no_semis(R"('\n' '\t' '\\' '\'' '\a' '\b' '\f' '\r' '\v')");
    ASSERT_EQ(tokens.size(), 9u);
    for (const auto& tok : tokens) {
        EXPECT_EQ(tok.kind, TokenKind::RuneLiteral);
    }
}

TEST(LexerTest, RuneHexEscape) {
    auto tokens = tokenize_no_semis(R"('\x41')");
    ASSERT_EQ(tokens.size(), 1u);
    EXPECT_EQ(tokens[0].kind, TokenKind::RuneLiteral);
}

TEST(LexerTest, RuneUnicodeEscape) {
    auto tokens = tokenize_no_semis(R"('\u0041')");
    ASSERT_EQ(tokens.size(), 1u);
    EXPECT_EQ(tokens[0].kind, TokenKind::RuneLiteral);
}

TEST(LexerTest, RuneOctalEscape) {
    auto tokens = tokenize_no_semis(R"('\101')");
    ASSERT_EQ(tokens.size(), 1u);
    EXPECT_EQ(tokens[0].kind, TokenKind::RuneLiteral);
}

// ============================================================================
// Operator and delimiter tests
// ============================================================================

TEST(LexerTest, SingleCharOperators) {
    auto tokens = tokenize_no_semis("+ - * / % & | ^ < > = ! ~ .");
    ASSERT_EQ(tokens.size(), 14u);
    EXPECT_EQ(tokens[0].kind, TokenKind::Plus);
    EXPECT_EQ(tokens[1].kind, TokenKind::Minus);
    EXPECT_EQ(tokens[2].kind, TokenKind::Star);
    EXPECT_EQ(tokens[3].kind, TokenKind::Slash);
    EXPECT_EQ(tokens[4].kind, TokenKind::Percent);
    EXPECT_EQ(tokens[5].kind, TokenKind::Ampersand);
    EXPECT_EQ(tokens[6].kind, TokenKind::Pipe);
    EXPECT_EQ(tokens[7].kind, TokenKind::Caret);
    EXPECT_EQ(tokens[8].kind, TokenKind::Less);
    EXPECT_EQ(tokens[9].kind, TokenKind::Greater);
    EXPECT_EQ(tokens[10].kind, TokenKind::Assign);
    EXPECT_EQ(tokens[11].kind, TokenKind::Not);
    EXPECT_EQ(tokens[12].kind, TokenKind::Tilde);
    EXPECT_EQ(tokens[13].kind, TokenKind::Dot);
}

TEST(LexerTest, DoubleCharOperators) {
    auto tokens = tokenize_no_semis("++ -- == != <= >= := << >> && || <- &^");
    ASSERT_EQ(tokens.size(), 13u);
    EXPECT_EQ(tokens[0].kind, TokenKind::Increment);
    EXPECT_EQ(tokens[1].kind, TokenKind::Decrement);
    EXPECT_EQ(tokens[2].kind, TokenKind::Equal);
    EXPECT_EQ(tokens[3].kind, TokenKind::NotEqual);
    EXPECT_EQ(tokens[4].kind, TokenKind::LessEqual);
    EXPECT_EQ(tokens[5].kind, TokenKind::GreaterEqual);
    EXPECT_EQ(tokens[6].kind, TokenKind::ColonAssign);
    EXPECT_EQ(tokens[7].kind, TokenKind::ShiftLeft);
    EXPECT_EQ(tokens[8].kind, TokenKind::ShiftRight);
    EXPECT_EQ(tokens[9].kind, TokenKind::LogicalAnd);
    EXPECT_EQ(tokens[10].kind, TokenKind::LogicalOr);
    EXPECT_EQ(tokens[11].kind, TokenKind::Arrow);
    EXPECT_EQ(tokens[12].kind, TokenKind::AmpCaret);
}

TEST(LexerTest, AssignmentOperators) {
    auto tokens = tokenize_no_semis("+= -= *= /= %= &= |= ^= <<= >>= &^=");
    ASSERT_EQ(tokens.size(), 11u);
    EXPECT_EQ(tokens[0].kind, TokenKind::PlusAssign);
    EXPECT_EQ(tokens[1].kind, TokenKind::MinusAssign);
    EXPECT_EQ(tokens[2].kind, TokenKind::StarAssign);
    EXPECT_EQ(tokens[3].kind, TokenKind::SlashAssign);
    EXPECT_EQ(tokens[4].kind, TokenKind::PercentAssign);
    EXPECT_EQ(tokens[5].kind, TokenKind::AmpAssign);
    EXPECT_EQ(tokens[6].kind, TokenKind::PipeAssign);
    EXPECT_EQ(tokens[7].kind, TokenKind::CaretAssign);
    EXPECT_EQ(tokens[8].kind, TokenKind::ShlAssign);
    EXPECT_EQ(tokens[9].kind, TokenKind::ShrAssign);
    EXPECT_EQ(tokens[10].kind, TokenKind::AmpCaretAssign);
}

TEST(LexerTest, Delimiters) {
    auto tokens = tokenize_no_semis("( ) [ ] { } , ; :");
    std::vector<TokenKind> kinds;
    for (const auto& tok : tokens) {
        kinds.push_back(tok.kind);
    }
    EXPECT_NE(std::find(kinds.begin(), kinds.end(), TokenKind::LParen), kinds.end());
    EXPECT_NE(std::find(kinds.begin(), kinds.end(), TokenKind::RParen), kinds.end());
    EXPECT_NE(std::find(kinds.begin(), kinds.end(), TokenKind::LBracket), kinds.end());
    EXPECT_NE(std::find(kinds.begin(), kinds.end(), TokenKind::RBracket), kinds.end());
    EXPECT_NE(std::find(kinds.begin(), kinds.end(), TokenKind::LBrace), kinds.end());
    EXPECT_NE(std::find(kinds.begin(), kinds.end(), TokenKind::RBrace), kinds.end());
    EXPECT_NE(std::find(kinds.begin(), kinds.end(), TokenKind::Comma), kinds.end());
    EXPECT_NE(std::find(kinds.begin(), kinds.end(), TokenKind::Colon), kinds.end());
}

TEST(LexerTest, Ellipsis) {
    auto tokens = tokenize_no_semis("...");
    ASSERT_EQ(tokens.size(), 1u);
    EXPECT_EQ(tokens[0].kind, TokenKind::Ellipsis);
    EXPECT_EQ(tokens[0].text, "...");
}

TEST(LexerTest, DotVsEllipsis) {
    auto tokens = tokenize_no_semis("x.y");
    ASSERT_EQ(tokens.size(), 3u);
    EXPECT_EQ(tokens[0].kind, TokenKind::Identifier);
    EXPECT_EQ(tokens[1].kind, TokenKind::Dot);
    EXPECT_EQ(tokens[2].kind, TokenKind::Identifier);
}

// ============================================================================
// Comment tests
// ============================================================================

TEST(LexerTest, LineComments) {
    auto tokens = tokenize_ok("x // this is a comment\ny");
    // x ; y ; (semicolons after x at newline and after y at EOF)
    ASSERT_GE(tokens.size(), 3u);
    EXPECT_EQ(tokens[0].kind, TokenKind::Identifier);
    EXPECT_EQ(tokens[0].text, "x");
    EXPECT_EQ(tokens[1].kind, TokenKind::Semicolon);
    EXPECT_EQ(tokens[2].kind, TokenKind::Identifier);
    EXPECT_EQ(tokens[2].text, "y");
}

TEST(LexerTest, BlockComments) {
    auto tokens = tokenize_no_semis("x /* comment */ y");
    ASSERT_EQ(tokens.size(), 2u);
    EXPECT_EQ(tokens[0].kind, TokenKind::Identifier);
    EXPECT_EQ(tokens[0].text, "x");
    EXPECT_EQ(tokens[1].kind, TokenKind::Identifier);
    EXPECT_EQ(tokens[1].text, "y");
}

TEST(LexerTest, BlockCommentWithNewline) {
    auto tokens = tokenize_ok("x /* comment\nwith newline */ y");
    ASSERT_GE(tokens.size(), 3u);
    EXPECT_EQ(tokens[0].kind, TokenKind::Identifier);
    EXPECT_EQ(tokens[0].text, "x");
    EXPECT_EQ(tokens[1].kind, TokenKind::Semicolon);
    EXPECT_EQ(tokens[2].kind, TokenKind::Identifier);
    EXPECT_EQ(tokens[2].text, "y");
}

TEST(LexerTest, BlockCommentNoNewlineNoSemicolon) {
    auto tokens = tokenize_no_semis("x /* no newline */ + y");
    ASSERT_EQ(tokens.size(), 3u);
    EXPECT_EQ(tokens[0].kind, TokenKind::Identifier);
    EXPECT_EQ(tokens[1].kind, TokenKind::Plus);
    EXPECT_EQ(tokens[2].kind, TokenKind::Identifier);
}

TEST(LexerTest, CommentOnlySource) {
    auto tokens = tokenize_no_semis("// just a comment");
    EXPECT_TRUE(tokens.empty());
}

TEST(LexerTest, MultipleLineComments) {
    auto tokens = tokenize_no_semis("// comment1\n// comment2\nx");
    ASSERT_EQ(tokens.size(), 1u);
    EXPECT_EQ(tokens[0].kind, TokenKind::Identifier);
}

// ============================================================================
// Automatic semicolon insertion tests
// ============================================================================

TEST(LexerTest, SemicolonAfterIdentifier) {
    auto tokens = tokenize_ok("x\ny");
    // x ; y ;
    ASSERT_GE(tokens.size(), 3u);
    EXPECT_EQ(tokens[0].kind, TokenKind::Identifier);
    EXPECT_EQ(tokens[1].kind, TokenKind::Semicolon);
    EXPECT_EQ(tokens[2].kind, TokenKind::Identifier);
}

TEST(LexerTest, SemicolonAfterIntLiteral) {
    auto tokens = tokenize_ok("42\nx");
    ASSERT_GE(tokens.size(), 3u);
    EXPECT_EQ(tokens[0].kind, TokenKind::IntLiteral);
    EXPECT_EQ(tokens[1].kind, TokenKind::Semicolon);
    EXPECT_EQ(tokens[2].kind, TokenKind::Identifier);
}

TEST(LexerTest, SemicolonAfterFloatLiteral) {
    auto tokens = tokenize_ok("3.14\nx");
    ASSERT_GE(tokens.size(), 3u);
    EXPECT_EQ(tokens[0].kind, TokenKind::FloatLiteral);
    EXPECT_EQ(tokens[1].kind, TokenKind::Semicolon);
}

TEST(LexerTest, SemicolonAfterStringLiteral) {
    auto tokens = tokenize_ok("\"hello\"\nx");
    ASSERT_GE(tokens.size(), 3u);
    EXPECT_EQ(tokens[0].kind, TokenKind::StringLiteral);
    EXPECT_EQ(tokens[1].kind, TokenKind::Semicolon);
}

TEST(LexerTest, SemicolonAfterReturn) {
    auto tokens = tokenize_ok("return\n42");
    ASSERT_GE(tokens.size(), 3u);
    EXPECT_EQ(tokens[0].kind, TokenKind::KW_return);
    EXPECT_EQ(tokens[1].kind, TokenKind::Semicolon);
    EXPECT_EQ(tokens[2].kind, TokenKind::IntLiteral);
}

TEST(LexerTest, SemicolonAfterBreak) {
    auto tokens = tokenize_ok("break\nfor");
    ASSERT_GE(tokens.size(), 3u);
    EXPECT_EQ(tokens[0].kind, TokenKind::KW_break);
    EXPECT_EQ(tokens[1].kind, TokenKind::Semicolon);
    EXPECT_EQ(tokens[2].kind, TokenKind::KW_for);
}

TEST(LexerTest, SemicolonAfterContinue) {
    auto tokens = tokenize_ok("continue\nfor");
    ASSERT_GE(tokens.size(), 3u);
    EXPECT_EQ(tokens[0].kind, TokenKind::KW_continue);
    EXPECT_EQ(tokens[1].kind, TokenKind::Semicolon);
}

TEST(LexerTest, SemicolonAfterFallthrough) {
    auto tokens = tokenize_ok("fallthrough\ncase");
    ASSERT_GE(tokens.size(), 3u);
    EXPECT_EQ(tokens[0].kind, TokenKind::KW_fallthrough);
    EXPECT_EQ(tokens[1].kind, TokenKind::Semicolon);
}

TEST(LexerTest, SemicolonAfterIncrement) {
    auto tokens = tokenize_ok("x++\ny");
    ASSERT_GE(tokens.size(), 4u);
    EXPECT_EQ(tokens[0].kind, TokenKind::Identifier);
    EXPECT_EQ(tokens[1].kind, TokenKind::Increment);
    EXPECT_EQ(tokens[2].kind, TokenKind::Semicolon);
}

TEST(LexerTest, SemicolonAfterDecrement) {
    auto tokens = tokenize_ok("x--\ny");
    ASSERT_GE(tokens.size(), 4u);
    EXPECT_EQ(tokens[0].kind, TokenKind::Identifier);
    EXPECT_EQ(tokens[1].kind, TokenKind::Decrement);
    EXPECT_EQ(tokens[2].kind, TokenKind::Semicolon);
}

TEST(LexerTest, SemicolonAfterRParen) {
    auto tokens = tokenize_ok("foo()\nbar");
    ASSERT_GE(tokens.size(), 5u);
    EXPECT_EQ(tokens[2].kind, TokenKind::RParen);
    EXPECT_EQ(tokens[3].kind, TokenKind::Semicolon);
}

TEST(LexerTest, SemicolonAfterRBracket) {
    auto tokens = tokenize_ok("a[0]\nb");
    ASSERT_GE(tokens.size(), 6u);
    EXPECT_EQ(tokens[3].kind, TokenKind::RBracket);
    EXPECT_EQ(tokens[4].kind, TokenKind::Semicolon);
}

TEST(LexerTest, SemicolonAfterRBrace) {
    auto tokens = tokenize_ok("}\nfunc");
    ASSERT_GE(tokens.size(), 3u);
    EXPECT_EQ(tokens[0].kind, TokenKind::RBrace);
    EXPECT_EQ(tokens[1].kind, TokenKind::Semicolon);
}

TEST(LexerTest, NoSemicolonAfterOperators) {
    // + does not trigger semicolon insertion, but y at EOF does
    auto tokens = tokenize_ok("x +\ny");
    // x + y ;  (no semicolon after +, but semicolon after y at EOF)
    ASSERT_GE(tokens.size(), 3u);
    EXPECT_EQ(tokens[0].kind, TokenKind::Identifier);
    EXPECT_EQ(tokens[1].kind, TokenKind::Plus);
    EXPECT_EQ(tokens[2].kind, TokenKind::Identifier);
    // No semicolon between + and y
}

TEST(LexerTest, NoSemicolonAfterComma) {
    auto tokens = tokenize_ok("x,\ny");
    // x , y ;
    ASSERT_GE(tokens.size(), 3u);
    EXPECT_EQ(tokens[0].kind, TokenKind::Identifier);
    EXPECT_EQ(tokens[1].kind, TokenKind::Comma);
    EXPECT_EQ(tokens[2].kind, TokenKind::Identifier);
}

TEST(LexerTest, NoSemicolonAfterLBrace) {
    auto tokens = tokenize_ok("{\nx");
    // { x ;
    ASSERT_GE(tokens.size(), 2u);
    EXPECT_EQ(tokens[0].kind, TokenKind::LBrace);
    EXPECT_EQ(tokens[1].kind, TokenKind::Identifier);
}

TEST(LexerTest, SemicolonAtEOF) {
    auto tokens = tokenize_ok("x");
    ASSERT_EQ(tokens.size(), 2u);
    EXPECT_EQ(tokens[0].kind, TokenKind::Identifier);
    EXPECT_EQ(tokens[1].kind, TokenKind::Semicolon);
}

TEST(LexerTest, SemicolonAtEOFAfterRBrace) {
    auto tokens = tokenize_ok("}");
    ASSERT_EQ(tokens.size(), 2u);
    EXPECT_EQ(tokens[0].kind, TokenKind::RBrace);
    EXPECT_EQ(tokens[1].kind, TokenKind::Semicolon);
}

TEST(LexerTest, NoSemicolonAtEOFAfterOperator) {
    auto tokens = tokenize_ok("+");
    ASSERT_EQ(tokens.size(), 1u);
    EXPECT_EQ(tokens[0].kind, TokenKind::Plus);
}

TEST(LexerTest, SemicolonAfterLineComment) {
    auto tokens = tokenize_ok("x // comment\ny");
    ASSERT_GE(tokens.size(), 3u);
    EXPECT_EQ(tokens[0].kind, TokenKind::Identifier);
    EXPECT_EQ(tokens[0].text, "x");
    EXPECT_EQ(tokens[1].kind, TokenKind::Semicolon);
    EXPECT_EQ(tokens[2].kind, TokenKind::Identifier);
    EXPECT_EQ(tokens[2].text, "y");
}

TEST(LexerTest, ConsecutiveNewlinesOneSemicolon) {
    auto tokens = tokenize_ok("x\n\n\ny");
    // x ; y ;  (one semicolon between x and y, one at EOF)
    ASSERT_GE(tokens.size(), 3u);
    EXPECT_EQ(tokens[0].kind, TokenKind::Identifier);
    EXPECT_EQ(tokens[0].text, "x");
    EXPECT_EQ(tokens[1].kind, TokenKind::Semicolon);
    EXPECT_EQ(tokens[2].kind, TokenKind::Identifier);
    EXPECT_EQ(tokens[2].text, "y");
}

// ============================================================================
// Error handling tests
// ============================================================================

TEST(LexerTest, UnterminatedString) {
    DiagnosticEngine diag;
    (void)tokenize_with_errors("\"unterminated", diag);
    EXPECT_TRUE(diag.has_errors());
}

TEST(LexerTest, UnterminatedRawString) {
    DiagnosticEngine diag;
    (void)tokenize_with_errors("`unterminated", diag);
    EXPECT_TRUE(diag.has_errors());
}

TEST(LexerTest, UnterminatedBlockComment) {
    DiagnosticEngine diag;
    (void)tokenize_with_errors("/* unterminated", diag);
    EXPECT_TRUE(diag.has_errors());
}

TEST(LexerTest, EmptyRuneLiteral) {
    DiagnosticEngine diag;
    (void)tokenize_with_errors("''", diag);
    EXPECT_TRUE(diag.has_errors());
}

TEST(LexerTest, UnexpectedCharacter) {
    DiagnosticEngine diag;
    (void)tokenize_with_errors("@", diag);
    EXPECT_TRUE(diag.has_errors());
}

TEST(LexerTest, InvalidEscapeSequence) {
    DiagnosticEngine diag;
    (void)tokenize_with_errors("\"\\q\"", diag);
    EXPECT_TRUE(diag.has_errors());
}

// ============================================================================
// Full Go source tokenization tests
// ============================================================================

TEST(LexerTest, HelloWorldProgram) {
    auto tokens = tokenize_ok(
        "package main\n"
        "\n"
        "func main() {\n"
        "\tprintln(\"Hello, World!\")\n"
        "}\n");

    std::vector<TokenKind> expected = {
        TokenKind::KW_package, TokenKind::Identifier, TokenKind::Semicolon,
        TokenKind::KW_func, TokenKind::Identifier, TokenKind::LParen,
        TokenKind::RParen, TokenKind::LBrace,
        TokenKind::Identifier, TokenKind::LParen, TokenKind::StringLiteral,
        TokenKind::RParen, TokenKind::Semicolon,
        TokenKind::RBrace, TokenKind::Semicolon,
    };

    ASSERT_EQ(tokens.size(), expected.size())
        << "Expected " << expected.size() << " tokens, got " << tokens.size();
    for (size_t i = 0; i < expected.size(); ++i) {
        EXPECT_EQ(tokens[i].kind, expected[i])
            << "Token " << i << ": expected " << token_kind_to_string(expected[i])
            << ", got " << token_kind_to_string(tokens[i].kind)
            << " ('" << tokens[i].text << "')";
    }
}

TEST(LexerTest, FibonacciProgram) {
    auto tokens = tokenize_no_semis(
        "func fibonacci(n int) int {\n"
        "\tif n <= 1 {\n"
        "\t\treturn n\n"
        "\t}\n"
        "\treturn fibonacci(n-1) + fibonacci(n-2)\n"
        "}\n");

    EXPECT_EQ(tokens[0].kind, TokenKind::KW_func);
    EXPECT_EQ(tokens[1].kind, TokenKind::Identifier);
    EXPECT_EQ(tokens[1].text, "fibonacci");

    bool found_le = false;
    for (const auto& tok : tokens) {
        if (tok.kind == TokenKind::LessEqual) {
            found_le = true;
            break;
        }
    }
    EXPECT_TRUE(found_le);
}

TEST(LexerTest, ShortVarDecl) {
    auto tokens = tokenize_no_semis("x := 42");
    ASSERT_GE(tokens.size(), 3u);
    EXPECT_EQ(tokens[0].kind, TokenKind::Identifier);
    EXPECT_EQ(tokens[1].kind, TokenKind::ColonAssign);
    EXPECT_EQ(tokens[2].kind, TokenKind::IntLiteral);
}

TEST(LexerTest, ChannelOperations) {
    auto tokens = tokenize_no_semis("ch <- 42");
    ASSERT_GE(tokens.size(), 3u);
    EXPECT_EQ(tokens[0].kind, TokenKind::Identifier);
    EXPECT_EQ(tokens[1].kind, TokenKind::Arrow);
    EXPECT_EQ(tokens[2].kind, TokenKind::IntLiteral);
}

TEST(LexerTest, StructLiteral) {
    auto tokens = tokenize_no_semis("Point{1, 2}");
    ASSERT_GE(tokens.size(), 6u);
    EXPECT_EQ(tokens[0].kind, TokenKind::Identifier);
    EXPECT_EQ(tokens[0].text, "Point");
    EXPECT_EQ(tokens[1].kind, TokenKind::LBrace);
    EXPECT_EQ(tokens[2].kind, TokenKind::IntLiteral);
    EXPECT_EQ(tokens[3].kind, TokenKind::Comma);
    EXPECT_EQ(tokens[4].kind, TokenKind::IntLiteral);
    EXPECT_EQ(tokens[5].kind, TokenKind::RBrace);
}

TEST(LexerTest, VariadicFunction) {
    auto tokens = tokenize_no_semis("func f(args ...int)");
    bool found_ellipsis = false;
    for (const auto& tok : tokens) {
        if (tok.kind == TokenKind::Ellipsis) {
            found_ellipsis = true;
            break;
        }
    }
    EXPECT_TRUE(found_ellipsis);
}

TEST(LexerTest, MapType) {
    auto tokens = tokenize_no_semis("map[string]int");
    ASSERT_GE(tokens.size(), 5u);
    EXPECT_EQ(tokens[0].kind, TokenKind::KW_map);
    EXPECT_EQ(tokens[1].kind, TokenKind::LBracket);
    EXPECT_EQ(tokens[2].kind, TokenKind::Identifier);
    EXPECT_EQ(tokens[2].text, "string");
    EXPECT_EQ(tokens[3].kind, TokenKind::RBracket);
    EXPECT_EQ(tokens[4].kind, TokenKind::Identifier);
    EXPECT_EQ(tokens[4].text, "int");
}

TEST(LexerTest, ExplicitSemicolons) {
    auto tokens = tokenize_no_semis("x; y; z");
    ASSERT_EQ(tokens.size(), 3u);
    EXPECT_EQ(tokens[0].kind, TokenKind::Identifier);
    EXPECT_EQ(tokens[1].kind, TokenKind::Identifier);
    EXPECT_EQ(tokens[2].kind, TokenKind::Identifier);
}

TEST(LexerTest, ImportBlock) {
    auto tokens = tokenize_ok(
        "import (\n"
        "\t\"fmt\"\n"
        "\t\"os\"\n"
        ")\n");

    std::vector<TokenKind> expected = {
        TokenKind::KW_import, TokenKind::LParen,
        TokenKind::StringLiteral, TokenKind::Semicolon,
        TokenKind::StringLiteral, TokenKind::Semicolon,
        TokenKind::RParen, TokenKind::Semicolon,
    };

    ASSERT_EQ(tokens.size(), expected.size())
        << "Expected " << expected.size() << " tokens, got " << tokens.size();
    for (size_t i = 0; i < expected.size(); ++i) {
        EXPECT_EQ(tokens[i].kind, expected[i])
            << "Token " << i << ": expected " << token_kind_to_string(expected[i])
            << ", got " << token_kind_to_string(tokens[i].kind);
    }
}

TEST(LexerTest, TokenizeAll) {
    DiagnosticEngine diag;
    Lexer lexer("x + y", "test.go", diag);
    auto tokens = lexer.tokenize_all();
    // x + y ; EOF
    ASSERT_GE(tokens.size(), 4u);
    EXPECT_EQ(tokens.back().kind, TokenKind::Eof);
}
