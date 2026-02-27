#include "parser/parser.hpp"

#include <fmt/format.h>

#include <cassert>

namespace golangc {

using namespace ast;

// ============================================================================
// Constructor & main entry point
// ============================================================================

Parser::Parser(Lexer& lexer, DiagnosticEngine& diag)
    : lexer_(lexer), diag_(diag) {
    advance(); // Prime the first token
}

bool Parser::parse() {
    file_ = parse_file();
    return !diag_.has_errors();
}

// ============================================================================
// Token navigation
// ============================================================================

void Parser::advance() {
    prev_ = current_;
    current_ = lexer_.next();
}

bool Parser::expect(TokenKind kind) {
    if (current_.kind == kind) {
        advance();
        return true;
    }
    diag_.error(current_.location, "expected '{}', got '{}'",
                token_kind_to_string(kind), token_kind_to_string(current_.kind));
    return false;
}

bool Parser::expect_semicolon() {
    if (current_.kind == TokenKind::Semicolon) {
        advance();
        return true;
    }
    // Don't report error if we're at } or EOF - the semicolon is implicitly inserted
    if (current_.kind == TokenKind::RBrace || current_.kind == TokenKind::Eof) {
        return true;
    }
    diag_.error(current_.location, "expected ';', got '{}'",
                token_kind_to_string(current_.kind));
    return false;
}

bool Parser::match(TokenKind kind) {
    if (current_.kind == kind) {
        advance();
        return true;
    }
    return false;
}

Token Parser::consume(TokenKind kind) {
    Token tok = current_;
    if (!expect(kind)) {
        tok.kind = TokenKind::Invalid;
    }
    return tok;
}

Token Parser::consume() {
    Token tok = current_;
    advance();
    return tok;
}

// ============================================================================
// Error handling & recovery
// ============================================================================

void Parser::error(std::string_view msg) {
    diag_.error(current_.location, "{}", msg);
}

void Parser::error_at(SourceLocation loc, std::string_view msg) {
    diag_.error(loc, "{}", msg);
}

void Parser::error_expected(std::string_view what) {
    diag_.error(current_.location, "expected {}, got '{}'",
                what, token_kind_to_string(current_.kind));
}

void Parser::sync_to_decl() {
    while (!at_end()) {
        switch (current_.kind) {
            case TokenKind::KW_const:
            case TokenKind::KW_type:
            case TokenKind::KW_var:
            case TokenKind::KW_func:
                return;
            default:
                advance();
        }
    }
}

void Parser::sync_to_stmt() {
    while (!at_end()) {
        // Semicolons end statements
        if (current_.kind == TokenKind::Semicolon) {
            advance();
            return;
        }
        // Statement starters
        switch (current_.kind) {
            case TokenKind::KW_break:
            case TokenKind::KW_const:
            case TokenKind::KW_continue:
            case TokenKind::KW_defer:
            case TokenKind::KW_fallthrough:
            case TokenKind::KW_for:
            case TokenKind::KW_go:
            case TokenKind::KW_goto:
            case TokenKind::KW_if:
            case TokenKind::KW_return:
            case TokenKind::KW_select:
            case TokenKind::KW_switch:
            case TokenKind::KW_type:
            case TokenKind::KW_var:
            case TokenKind::RBrace:
                return;
            default:
                advance();
        }
    }
}

void Parser::skip_to(TokenKind kind) {
    while (!at_end() && current_.kind != kind) {
        advance();
    }
}

// ============================================================================
// AST node creation helpers
// ============================================================================

Expr* Parser::make_bad_expr(SourceLocation loc) {
    auto* e = alloc<Expr>();
    e->kind = ExprKind::Bad;
    e->bad.loc = loc;
    return e;
}

Stmt* Parser::make_bad_stmt(SourceLocation loc) {
    auto* s = alloc<Stmt>();
    s->kind = StmtKind::Bad;
    s->bad.loc = loc;
    return s;
}

Decl* Parser::make_bad_decl(SourceLocation loc) {
    auto* d = alloc<Decl>();
    d->kind = DeclKind::Bad;
    d->bad.loc = loc;
    return d;
}

TypeExpr* Parser::make_bad_type(SourceLocation loc) {
    auto* t = alloc<TypeExpr>();
    t->kind = TypeExprKind::Bad;
    t->bad.loc = loc;
    return t;
}

Expr* Parser::make_ident(Token tok) {
    auto* e = alloc<Expr>();
    e->kind = ExprKind::Ident;
    e->ident.loc = tok.location;
    e->ident.name = tok.text;
    return e;
}

IdentExpr* Parser::make_ident_node(Token tok) {
    auto* n = alloc<IdentExpr>();
    n->loc = tok.location;
    n->name = tok.text;
    return n;
}

Expr* Parser::make_expr(ExprKind kind) {
    auto* e = alloc<Expr>();
    e->kind = kind;
    return e;
}

Stmt* Parser::make_stmt(StmtKind kind) {
    auto* s = alloc<Stmt>();
    s->kind = kind;
    return s;
}

Decl* Parser::make_decl(DeclKind kind) {
    auto* d = alloc<Decl>();
    d->kind = kind;
    return d;
}

TypeExpr* Parser::make_type(TypeExprKind kind) {
    auto* t = alloc<TypeExpr>();
    t->kind = kind;
    return t;
}

// ============================================================================
// State query helpers
// ============================================================================

bool Parser::is_literal_type_start() const {
    switch (current_.kind) {
        case TokenKind::Identifier:
        case TokenKind::LBracket:  // [N]T or []T
        case TokenKind::KW_struct:
        case TokenKind::KW_map:
            return true;
        default:
            return false;
    }
}

bool Parser::is_stmt_start() const {
    switch (current_.kind) {
        case TokenKind::Identifier:
        case TokenKind::IntLiteral:
        case TokenKind::FloatLiteral:
        case TokenKind::ImaginaryLiteral:
        case TokenKind::RuneLiteral:
        case TokenKind::StringLiteral:
        case TokenKind::KW_break:
        case TokenKind::KW_const:
        case TokenKind::KW_continue:
        case TokenKind::KW_defer:
        case TokenKind::KW_fallthrough:
        case TokenKind::KW_for:
        case TokenKind::KW_go:
        case TokenKind::KW_goto:
        case TokenKind::KW_if:
        case TokenKind::KW_return:
        case TokenKind::KW_select:
        case TokenKind::KW_switch:
        case TokenKind::KW_type:
        case TokenKind::KW_var:
        case TokenKind::LBrace:
        case TokenKind::LParen:
        case TokenKind::LBracket:
        case TokenKind::Plus:
        case TokenKind::Minus:
        case TokenKind::Star:
        case TokenKind::Ampersand:
        case TokenKind::Caret:
        case TokenKind::Not:
        case TokenKind::Arrow:
        case TokenKind::KW_func:
        case TokenKind::KW_map:
        case TokenKind::KW_struct:
        case TokenKind::KW_chan:
        case TokenKind::KW_interface:
            return true;
        default:
            return false;
    }
}

bool Parser::is_decl_start() const {
    switch (current_.kind) {
        case TokenKind::KW_const:
        case TokenKind::KW_type:
        case TokenKind::KW_var:
        case TokenKind::KW_func:
            return true;
        default:
            return false;
    }
}

bool Parser::could_be_type() const {
    switch (current_.kind) {
        case TokenKind::Identifier:
        case TokenKind::LBracket:
        case TokenKind::KW_struct:
        case TokenKind::KW_interface:
        case TokenKind::KW_func:
        case TokenKind::KW_map:
        case TokenKind::KW_chan:
        case TokenKind::Star:
        case TokenKind::Arrow: // <-chan
        case TokenKind::LParen:
            return true;
        default:
            return false;
    }
}

// ============================================================================
// Operator precedence
// ============================================================================

int Parser::binary_prec(TokenKind kind) {
    switch (kind) {
        case TokenKind::LogicalOr:  return 1;
        case TokenKind::LogicalAnd: return 2;
        case TokenKind::Equal:
        case TokenKind::NotEqual:
        case TokenKind::Less:
        case TokenKind::LessEqual:
        case TokenKind::Greater:
        case TokenKind::GreaterEqual: return 3;
        case TokenKind::Plus:
        case TokenKind::Minus:
        case TokenKind::Pipe:
        case TokenKind::Caret:      return 4;
        case TokenKind::Star:
        case TokenKind::Slash:
        case TokenKind::Percent:
        case TokenKind::ShiftLeft:
        case TokenKind::ShiftRight:
        case TokenKind::Ampersand:
        case TokenKind::AmpCaret:   return 5;
        default:                    return 0;
    }
}

bool Parser::is_assign_op(TokenKind kind) {
    switch (kind) {
        case TokenKind::Assign:
        case TokenKind::PlusAssign:
        case TokenKind::MinusAssign:
        case TokenKind::StarAssign:
        case TokenKind::SlashAssign:
        case TokenKind::PercentAssign:
        case TokenKind::AmpAssign:
        case TokenKind::PipeAssign:
        case TokenKind::CaretAssign:
        case TokenKind::ShlAssign:
        case TokenKind::ShrAssign:
        case TokenKind::AmpCaretAssign:
            return true;
        default:
            return false;
    }
}

// ============================================================================
// Source file parsing
// ============================================================================

File* Parser::parse_file() {
    auto* f = alloc<File>();
    f->loc = current_.location;

    // Parse package clause
    f->package = parse_package_clause();
    if (!f->package) {
        return f;
    }

    // Parse import declarations
    std::vector<Decl*> imports;
    while (current_.kind == TokenKind::KW_import) {
        auto* imp = parse_import_decl();
        if (imp) {
            imports.push_back(imp);
        }
    }
    f->imports = make_list(arena_, imports);

    // Parse top-level declarations
    std::vector<Decl*> decls;
    while (!at_end()) {
        if (current_.kind == TokenKind::Semicolon) {
            advance();
            continue;
        }
        auto* d = parse_top_level_decl();
        if (d) {
            decls.push_back(d);
        } else {
            // Error recovery
            if (!at_end()) {
                sync_to_decl();
            }
        }
    }
    f->decls = make_list(arena_, decls);
    return f;
}

PackageDecl* Parser::parse_package_clause() {
    if (!expect(TokenKind::KW_package)) {
        return nullptr;
    }
    SourceLocation loc = prev_.location;

    if (current_.kind != TokenKind::Identifier) {
        error_expected("package name");
        return nullptr;
    }
    auto* name = make_ident_node(current_);
    advance();

    expect_semicolon();

    auto* pkg = alloc<PackageDecl>();
    pkg->loc = loc;
    pkg->name = name;
    return pkg;
}

// ============================================================================
// Import declarations
// ============================================================================

Decl* Parser::parse_import_decl() {
    SourceLocation loc = current_.location;
    expect(TokenKind::KW_import);

    auto* d = make_decl(DeclKind::Import);
    d->import_.loc = loc;
    d->import_.lparen = {};
    d->import_.rparen = {};

    if (match(TokenKind::LParen)) {
        d->import_.lparen = prev_.location;
        std::vector<ImportSpec*> specs;
        while (!at(TokenKind::RParen) && !at_end()) {
            auto* spec = parse_import_spec();
            if (spec) {
                specs.push_back(spec);
            }
            expect_semicolon();
        }
        d->import_.rparen = current_.location;
        expect(TokenKind::RParen);
        d->import_.specs = make_list(arena_, specs);
    } else {
        auto* spec = parse_import_spec();
        std::vector<ImportSpec*> specs;
        if (spec) {
            specs.push_back(spec);
        }
        d->import_.specs = make_list(arena_, specs);
    }

    expect_semicolon();
    return d;
}

ImportSpec* Parser::parse_import_spec() {
    auto* spec = alloc<ImportSpec>();
    spec->loc = current_.location;
    spec->name = nullptr;

    // Optional package name or "."
    if (current_.kind == TokenKind::Dot) {
        spec->name = make_ident_node(current_);
        advance();
    } else if (current_.kind == TokenKind::Identifier) {
        spec->name = make_ident_node(current_);
        advance();
    }

    // Import path (string literal)
    if (current_.kind != TokenKind::StringLiteral) {
        error_expected("import path");
        spec->path = nullptr;
        return spec;
    }
    auto* path_node = alloc<BasicLitExpr>();
    path_node->loc = current_.location;
    path_node->kind = TokenKind::StringLiteral;
    path_node->value = current_.text;
    spec->path = path_node;
    advance();

    return spec;
}

// ============================================================================
// Top-level declarations
// ============================================================================

Decl* Parser::parse_top_level_decl() {
    switch (current_.kind) {
        case TokenKind::KW_const: return parse_const_decl();
        case TokenKind::KW_type:  return parse_type_decl();
        case TokenKind::KW_var:   return parse_var_decl();
        case TokenKind::KW_func:  return parse_func_or_method_decl();
        default:
            error_expected("declaration");
            return nullptr;
    }
}

// ============================================================================
// Const declarations
// ============================================================================

Decl* Parser::parse_const_decl() {
    SourceLocation loc = current_.location;
    expect(TokenKind::KW_const);

    auto* d = make_decl(DeclKind::Const);
    d->const_.loc = loc;
    d->const_.lparen = {};
    d->const_.rparen = {};

    if (match(TokenKind::LParen)) {
        d->const_.lparen = prev_.location;
        std::vector<ValueSpec*> specs;
        while (!at(TokenKind::RParen) && !at_end()) {
            auto* spec = parse_const_spec();
            if (spec) {
                specs.push_back(spec);
            }
            expect_semicolon();
        }
        d->const_.rparen = current_.location;
        expect(TokenKind::RParen);
        d->const_.specs = make_list(arena_, specs);
    } else {
        std::vector<ValueSpec*> specs;
        auto* spec = parse_const_spec();
        if (spec) {
            specs.push_back(spec);
        }
        d->const_.specs = make_list(arena_, specs);
    }

    expect_semicolon();
    return d;
}

ValueSpec* Parser::parse_const_spec() {
    auto* spec = alloc<ValueSpec>();
    spec->loc = current_.location;
    spec->type = nullptr;

    // Parse identifier list
    auto idents = parse_ident_list();
    spec->names = idents;

    // Optional type
    if (could_be_type() && current_.kind != TokenKind::Assign) {
        spec->type = parse_type();
    }

    // Optional = ExpressionList
    if (match(TokenKind::Assign)) {
        spec->values = parse_expr_list();
    } else {
        spec->values = {};
    }

    return spec;
}

// ============================================================================
// Type declarations
// ============================================================================

Decl* Parser::parse_type_decl() {
    SourceLocation loc = current_.location;
    expect(TokenKind::KW_type);

    auto* d = make_decl(DeclKind::Type);
    d->type.loc = loc;
    d->type.lparen = {};
    d->type.rparen = {};

    if (match(TokenKind::LParen)) {
        d->type.lparen = prev_.location;
        std::vector<TypeSpec*> specs;
        while (!at(TokenKind::RParen) && !at_end()) {
            auto* spec = parse_type_spec();
            if (spec) {
                specs.push_back(spec);
            }
            expect_semicolon();
        }
        d->type.rparen = current_.location;
        expect(TokenKind::RParen);
        d->type.specs = make_list(arena_, specs);
    } else {
        std::vector<TypeSpec*> specs;
        auto* spec = parse_type_spec();
        if (spec) {
            specs.push_back(spec);
        }
        d->type.specs = make_list(arena_, specs);
    }

    expect_semicolon();
    return d;
}

TypeSpec* Parser::parse_type_spec() {
    auto* spec = alloc<TypeSpec>();
    spec->loc = current_.location;
    spec->is_alias = false;
    spec->assign_loc = {};

    if (current_.kind != TokenKind::Identifier) {
        error_expected("type name");
        return nullptr;
    }
    spec->name = make_ident_node(current_);
    advance();

    // Check for type alias: type X = Y
    if (match(TokenKind::Assign)) {
        spec->is_alias = true;
        spec->assign_loc = prev_.location;
    }

    spec->type = parse_type();
    return spec;
}

// ============================================================================
// Var declarations
// ============================================================================

Decl* Parser::parse_var_decl() {
    SourceLocation loc = current_.location;
    expect(TokenKind::KW_var);

    auto* d = make_decl(DeclKind::Var);
    d->var.loc = loc;
    d->var.lparen = {};
    d->var.rparen = {};

    if (match(TokenKind::LParen)) {
        d->var.lparen = prev_.location;
        std::vector<VarSpec*> specs;
        while (!at(TokenKind::RParen) && !at_end()) {
            auto* spec = parse_var_spec();
            if (spec) {
                specs.push_back(spec);
            }
            expect_semicolon();
        }
        d->var.rparen = current_.location;
        expect(TokenKind::RParen);
        d->var.specs = make_list(arena_, specs);
    } else {
        std::vector<VarSpec*> specs;
        auto* spec = parse_var_spec();
        if (spec) {
            specs.push_back(spec);
        }
        d->var.specs = make_list(arena_, specs);
    }

    expect_semicolon();
    return d;
}

VarSpec* Parser::parse_var_spec() {
    auto* spec = alloc<VarSpec>();
    spec->loc = current_.location;
    spec->type = nullptr;

    spec->names = parse_ident_list();

    // var x Type
    // var x Type = expr
    // var x = expr
    if (current_.kind != TokenKind::Assign && could_be_type()) {
        spec->type = parse_type();
        if (match(TokenKind::Assign)) {
            spec->values = parse_expr_list();
        } else {
            spec->values = {};
        }
    } else if (match(TokenKind::Assign)) {
        spec->values = parse_expr_list();
    } else {
        error_expected("type or '='");
        spec->values = {};
    }

    return spec;
}

// ============================================================================
// Function and method declarations
// ============================================================================

Decl* Parser::parse_func_or_method_decl() {
    SourceLocation loc = current_.location;
    expect(TokenKind::KW_func);

    auto* d = make_decl(DeclKind::Func);
    d->func.loc = loc;
    d->func.recv = nullptr;
    d->func.body = nullptr;

    // Check for method receiver: func (recv) Name(...)
    if (current_.kind == TokenKind::LParen) {
        d->func.recv = parse_parameters();
    }

    // Function name
    if (current_.kind != TokenKind::Identifier) {
        error_expected("function name");
        sync_to_decl();
        return d;
    }
    d->func.name = make_ident_node(current_);
    advance();

    // Signature
    d->func.type = parse_signature();

    // Optional body
    if (current_.kind == TokenKind::LBrace) {
        d->func.body = parse_block();
    }

    expect_semicolon();
    return d;
}

// ============================================================================
// Types
// ============================================================================

TypeExpr* Parser::parse_type() {
    auto* t = parse_type_or_nil();
    if (!t) {
        error_expected("type");
        return make_bad_type(current_.location);
    }
    return t;
}

TypeExpr* Parser::parse_type_or_nil() {
    switch (current_.kind) {
        case TokenKind::Identifier:
            return parse_type_name();
        case TokenKind::LBracket:
            return parse_array_or_slice_type();
        case TokenKind::KW_struct:
            return parse_struct_type();
        case TokenKind::KW_interface:
            return parse_interface_type();
        case TokenKind::KW_func:
            return parse_func_type();
        case TokenKind::KW_map:
            return parse_map_type();
        case TokenKind::KW_chan:
            return parse_chan_type();
        case TokenKind::Arrow:
            // <-chan
            return parse_chan_type();
        case TokenKind::Star:
            return parse_pointer_type();
        case TokenKind::LParen: {
            // Parenthesized type: (T)
            SourceLocation loc = current_.location;
            advance();
            auto* inner = parse_type();
            expect(TokenKind::RParen);
            auto* t = make_type(TypeExprKind::Paren);
            t->paren.loc = loc;
            t->paren.type = inner;
            return t;
        }
        default:
            return nullptr;
    }
}

TypeExpr* Parser::parse_type_name() {
    SourceLocation loc = current_.location;
    std::string_view name = current_.text;
    advance(); // consume identifier

    // Check for qualified name: pkg.Type
    if (current_.kind == TokenKind::Dot) {
        SourceLocation dot_loc = current_.location;
        advance();
        if (current_.kind != TokenKind::Identifier) {
            error_expected("type name after '.'");
            return make_bad_type(loc);
        }
        std::string_view type_name = current_.text;
        advance();
        auto* t = make_type(TypeExprKind::Qualified);
        t->qualified.loc = loc;
        t->qualified.package = name;
        t->qualified.name = type_name;
        t->qualified.dot_loc = dot_loc;
        return t;
    }

    auto* t = make_type(TypeExprKind::Ident);
    t->ident.loc = loc;
    t->ident.name = name;
    return t;
}

TypeExpr* Parser::parse_array_or_slice_type() {
    SourceLocation loc = current_.location;
    SourceLocation lbrack = current_.location;
    expect(TokenKind::LBracket);

    // []T = slice type
    if (current_.kind == TokenKind::RBracket) {
        SourceLocation rbrack = current_.location;
        advance();
        auto* elem = parse_type();
        auto* t = make_type(TypeExprKind::Slice);
        t->slice.loc = loc;
        t->slice.element = elem;
        t->slice.lbrack = lbrack;
        t->slice.rbrack = rbrack;
        return t;
    }

    // [...]T = array with ... length (handled in some contexts)
    // [N]T = array type
    auto* length = parse_expr();
    SourceLocation rbrack = current_.location;
    expect(TokenKind::RBracket);
    auto* elem = parse_type();

    auto* t = make_type(TypeExprKind::Array);
    t->array.loc = loc;
    t->array.length = length;
    t->array.element = elem;
    t->array.lbrack = lbrack;
    t->array.rbrack = rbrack;
    return t;
}

TypeExpr* Parser::parse_map_type() {
    SourceLocation loc = current_.location;
    expect(TokenKind::KW_map);
    expect(TokenKind::LBracket);
    auto* key = parse_type();
    expect(TokenKind::RBracket);
    auto* value = parse_type();

    auto* t = make_type(TypeExprKind::Map);
    t->map.loc = loc;
    t->map.key = key;
    t->map.value = value;
    return t;
}

TypeExpr* Parser::parse_chan_type() {
    SourceLocation loc = current_.location;
    ChanDir dir = ChanDir::SendRecv;

    if (current_.kind == TokenKind::Arrow) {
        // <-chan T
        advance();
        expect(TokenKind::KW_chan);
        dir = ChanDir::RecvOnly;
    } else {
        expect(TokenKind::KW_chan);
        if (match(TokenKind::Arrow)) {
            dir = ChanDir::SendOnly;
        }
    }

    auto* elem = parse_type();
    auto* t = make_type(TypeExprKind::Chan);
    t->chan.loc = loc;
    t->chan.dir = dir;
    t->chan.element = elem;
    return t;
}

TypeExpr* Parser::parse_struct_type() {
    SourceLocation loc = current_.location;
    expect(TokenKind::KW_struct);

    auto* fields = parse_field_list(TokenKind::LBrace, TokenKind::RBrace);

    auto* t = make_type(TypeExprKind::Struct);
    t->struct_.loc = loc;
    t->struct_.fields = fields;
    return t;
}

TypeExpr* Parser::parse_interface_type() {
    SourceLocation loc = current_.location;
    expect(TokenKind::KW_interface);
    SourceLocation lbrace = current_.location;
    expect(TokenKind::LBrace);

    std::vector<InterfaceMethod*> methods;
    while (!at(TokenKind::RBrace) && !at_end()) {
        auto* m = alloc<InterfaceMethod>();
        m->loc = current_.location;
        m->name = nullptr;
        m->signature = nullptr;
        m->embedded_type = nullptr;

        if (current_.kind == TokenKind::Identifier) {
            Token id_tok = current_;
            advance();

            if (current_.kind == TokenKind::LParen) {
                // Method: Name(params) results
                m->name = make_ident_node(id_tok);
                m->signature = parse_signature();
            } else {
                // Embedded type (possibly qualified)
                // Could be just "TypeName" or "pkg.TypeName"
                if (current_.kind == TokenKind::Dot) {
                    // qualified type
                    SourceLocation dot_loc = current_.location;
                    advance();
                    if (current_.kind != TokenKind::Identifier) {
                        error_expected("type name");
                    } else {
                        auto* qt = make_type(TypeExprKind::Qualified);
                        qt->qualified.loc = id_tok.location;
                        qt->qualified.package = id_tok.text;
                        qt->qualified.name = current_.text;
                        qt->qualified.dot_loc = dot_loc;
                        advance();
                        m->embedded_type = qt;
                    }
                } else {
                    auto* it = make_type(TypeExprKind::Ident);
                    it->ident.loc = id_tok.location;
                    it->ident.name = id_tok.text;
                    m->embedded_type = it;
                }
            }
        } else if (current_.kind == TokenKind::Tilde) {
            // Type constraint: ~T
            advance();
            m->embedded_type = parse_type();
        } else {
            error_expected("method or type name");
            advance();
        }
        methods.push_back(m);
        expect_semicolon();
    }
    SourceLocation rbrace = current_.location;
    expect(TokenKind::RBrace);

    auto* t = make_type(TypeExprKind::Interface);
    t->interface_.loc = loc;
    t->interface_.methods = make_list(arena_, methods);
    t->interface_.lbrace = lbrace;
    t->interface_.rbrace = rbrace;
    return t;
}

TypeExpr* Parser::parse_func_type() {
    SourceLocation loc = current_.location;
    expect(TokenKind::KW_func);
    auto* sig = parse_signature();

    auto* t = make_type(TypeExprKind::Func);
    t->func.loc = loc;
    t->func.params = sig->params;
    t->func.results = sig->results;
    return t;
}

TypeExpr* Parser::parse_pointer_type() {
    SourceLocation loc = current_.location;
    expect(TokenKind::Star);
    auto* base = parse_type();

    auto* t = make_type(TypeExprKind::Pointer);
    t->pointer.loc = loc;
    t->pointer.base = base;
    return t;
}

// ============================================================================
// Function signatures & fields
// ============================================================================

FuncTypeExpr* Parser::parse_signature() {
    auto* sig = alloc<FuncTypeExpr>();
    sig->loc = current_.location;
    sig->params = parse_parameters();
    sig->results = parse_results();
    return sig;
}

FieldList* Parser::parse_parameters() {
    auto* fl = alloc<FieldList>();
    fl->lparen = current_.location;
    expect(TokenKind::LParen);

    std::vector<Field*> fields;
    while (!at(TokenKind::RParen) && !at_end()) {
        auto* f = parse_param_decl();
        if (f) {
            fields.push_back(f);
        }
        if (!match(TokenKind::Comma)) {
            break;
        }
    }

    // Post-process: check if we have a mix of named and unnamed fields.
    // In Go, either ALL params are named or ALL are unnamed.
    // If any field has names and a type, the fields that have no names
    // but have an ident type might actually be names for the next field's type.
    // However, for simplicity, we handle the common cases:
    //   (int, int) => two unnamed params
    //   (a, b int) => two named params sharing type 'int'
    //   (a int, b int) => two named params
    // The tricky case is (a, b int) which our per-field parser produces:
    //   field{type=a}, field{names=[b], type=int}
    // We need to merge: field{names=[a, b], type=int}

    // Check if we need to merge: look for unnamed ident-typed fields followed
    // by a named field — these are likely multi-name parameter groups.
    bool has_named = false;
    bool has_unnamed = false;
    for (auto* f : fields) {
        if (f->names.count > 0) has_named = true;
        else has_unnamed = true;
    }

    if (has_named && has_unnamed && fields.size() >= 2) {
        // Merge unnamed ident fields into the following named field
        std::vector<Field*> merged;
        size_t i = 0;
        while (i < fields.size()) {
            if (fields[i]->names.count == 0 && fields[i]->type &&
                fields[i]->type->kind == TypeExprKind::Ident) {
                // Collect consecutive unnamed ident fields
                std::vector<IdentExpr*> names;
                while (i < fields.size() && fields[i]->names.count == 0 &&
                       fields[i]->type && fields[i]->type->kind == TypeExprKind::Ident) {
                    auto* name = make_ident_node(Token{
                        TokenKind::Identifier,
                        fields[i]->type->ident.name,
                        fields[i]->type->ident.loc});
                    names.push_back(name);
                    i++;
                }
                // The next field should be named — merge names into it
                if (i < fields.size() && fields[i]->names.count > 0) {
                    // Prepend collected names to the named field
                    for (size_t j = 0; j < fields[i]->names.count; j++) {
                        names.push_back(fields[i]->names.data[j]);
                    }
                    fields[i]->names = make_list(arena_, names);
                    fields[i]->loc = merged.empty() ? fields[0]->loc : fields[i]->loc;
                    merged.push_back(fields[i]);
                    i++;
                } else {
                    // No named field follows — these are all unnamed types after all
                    for (auto* name_ident : names) {
                        auto* uf = alloc<Field>();
                        uf->loc = name_ident->loc;
                        uf->tag = nullptr;
                        auto* t = make_type(TypeExprKind::Ident);
                        t->ident.loc = name_ident->loc;
                        t->ident.name = name_ident->name;
                        uf->type = t;
                        uf->names = {};
                        merged.push_back(uf);
                    }
                }
            } else {
                merged.push_back(fields[i]);
                i++;
            }
        }
        fields = std::move(merged);
    }

    fl->rparen = current_.location;
    expect(TokenKind::RParen);
    fl->fields = make_list(arena_, fields);
    return fl;
}

Field* Parser::parse_param_decl() {
    auto* field = alloc<Field>();
    field->loc = current_.location;
    field->tag = nullptr;
    field->type = nullptr;

    // Check for variadic: ...T
    if (current_.kind == TokenKind::Ellipsis) {
        SourceLocation eloc = current_.location;
        advance();
        auto* elem = parse_type();
        auto* t = make_type(TypeExprKind::Ellipsis);
        t->ellipsis.loc = eloc;
        t->ellipsis.element = elem;
        field->type = t;
        field->names = {};
        return field;
    }

    // Try to parse "name Type" or just "Type"
    // This is ambiguous — we need lookahead.
    // Strategy: if current is ident, consume it. Then check what follows:
    //   - If followed by a type-start token or "..." => "name Type" form
    //   - If followed by "," or ")" or other => just a type name (unnamed)

    if (current_.kind == TokenKind::Identifier) {
        Token first = current_;
        advance();

        // Check if this is "name Type" or "name ...Type"
        if (could_be_type() || current_.kind == TokenKind::Ellipsis) {
            // "name Type" form — this ident is a parameter name
            std::vector<IdentExpr*> names;
            names.push_back(make_ident_node(first));

            if (current_.kind == TokenKind::Ellipsis) {
                SourceLocation eloc = current_.location;
                advance();
                auto* elem = parse_type();
                auto* t = make_type(TypeExprKind::Ellipsis);
                t->ellipsis.loc = eloc;
                t->ellipsis.element = elem;
                field->type = t;
            } else {
                field->type = parse_type();
            }
            field->names = make_list(arena_, names);
            return field;
        }

        // Just a type name (unnamed parameter)
        // Check for pkg.Type
        if (current_.kind == TokenKind::Dot) {
            SourceLocation dot_loc = current_.location;
            advance();
            if (current_.kind == TokenKind::Identifier) {
                auto* t = make_type(TypeExprKind::Qualified);
                t->qualified.loc = first.location;
                t->qualified.package = first.text;
                t->qualified.name = current_.text;
                t->qualified.dot_loc = dot_loc;
                advance();
                field->type = t;
            } else {
                error_expected("type name after '.'");
                field->type = make_bad_type(first.location);
            }
        } else {
            auto* t = make_type(TypeExprKind::Ident);
            t->ident.loc = first.location;
            t->ident.name = first.text;
            field->type = t;
        }
        field->names = {};
        return field;
    }

    // Non-identifier type (e.g., *Type, []Type, func(...), etc.)
    field->type = parse_type();
    field->names = {};
    return field;
}

FieldList* Parser::parse_results() {
    if (current_.kind == TokenKind::LParen) {
        return parse_parameters();
    }

    // Single unnamed result type
    auto* t = parse_type_or_nil();
    if (!t) {
        return nullptr;
    }

    auto* field = alloc<Field>();
    field->loc = t->location();
    field->tag = nullptr;
    field->names = {};
    field->type = t;

    auto* fl = alloc<FieldList>();
    fl->lparen = {};
    fl->rparen = {};
    std::vector<Field*> fields;
    fields.push_back(field);
    fl->fields = make_list(arena_, fields);
    return fl;
}

FieldList* Parser::parse_field_list(TokenKind open, TokenKind close) {
    auto* fl = alloc<FieldList>();
    fl->lparen = current_.location;
    expect(open);

    std::vector<Field*> fields;
    while (!at(close) && !at_end()) {
        auto* f = parse_struct_field();
        if (f) {
            fields.push_back(f);
        }
        expect_semicolon();
    }
    fl->rparen = current_.location;
    expect(close);
    fl->fields = make_list(arena_, fields);
    return fl;
}

Field* Parser::parse_struct_field() {
    auto* field = alloc<Field>();
    field->loc = current_.location;
    field->tag = nullptr;
    field->type = nullptr;

    // Check for embedded field: *TypeName or TypeName
    if (current_.kind == TokenKind::Star) {
        // Could be *EmbeddedType
        SourceLocation star_loc = current_.location;
        advance();
        if (current_.kind == TokenKind::Identifier) {
            Token name_tok = current_;
            advance();
            // Is this "*Type" (embedded) or start of "*Type" as a type after names?
            // In struct fields, *Ident alone is an embedded pointer field
            // But we need to check if there are names before
            // For simplicity: *Ident is embedded pointer type field
            auto* base = make_type(TypeExprKind::Ident);
            base->ident.loc = name_tok.location;
            base->ident.name = name_tok.text;

            // Check for pkg.Type
            if (current_.kind == TokenKind::Dot) {
                SourceLocation dot_loc = current_.location;
                advance();
                if (current_.kind == TokenKind::Identifier) {
                    auto* qt = make_type(TypeExprKind::Qualified);
                    qt->qualified.loc = name_tok.location;
                    qt->qualified.package = name_tok.text;
                    qt->qualified.name = current_.text;
                    qt->qualified.dot_loc = dot_loc;
                    advance();
                    auto* pt = make_type(TypeExprKind::Pointer);
                    pt->pointer.loc = star_loc;
                    pt->pointer.base = qt;
                    field->type = pt;
                } else {
                    auto* pt = make_type(TypeExprKind::Pointer);
                    pt->pointer.loc = star_loc;
                    pt->pointer.base = base;
                    field->type = pt;
                }
            } else {
                auto* pt = make_type(TypeExprKind::Pointer);
                pt->pointer.loc = star_loc;
                pt->pointer.base = base;
                field->type = pt;
            }
            field->names = {};

            // Check for tag
            if (current_.kind == TokenKind::StringLiteral) {
                auto* tag = alloc<Expr>();
                tag->kind = ExprKind::BasicLit;
                tag->basic_lit.loc = current_.location;
                tag->basic_lit.kind = TokenKind::StringLiteral;
                tag->basic_lit.value = current_.text;
                advance();
                field->tag = tag;
            }
            return field;
        }
        // *non-ident - error or complex pointer type
        auto* pt = make_type(TypeExprKind::Pointer);
        pt->pointer.loc = star_loc;
        pt->pointer.base = parse_type();
        field->type = pt;
        field->names = {};
        return field;
    }

    if (current_.kind == TokenKind::Identifier) {
        Token first = current_;
        advance();

        // Check if next token is a type, comma, or another identifier
        // If it's a type-start or comma => this is "name Type" or "name, name Type"
        // If it's semicolon/rbrace/string => this is an embedded type
        if (current_.kind == TokenKind::Comma ||
            (could_be_type() && current_.kind != TokenKind::Dot &&
             current_.kind != TokenKind::Semicolon && current_.kind != TokenKind::RBrace &&
             current_.kind != TokenKind::StringLiteral)) {
            // Named field(s)
            std::vector<IdentExpr*> names;
            names.push_back(make_ident_node(first));
            while (match(TokenKind::Comma)) {
                if (current_.kind != TokenKind::Identifier) {
                    error_expected("field name");
                    break;
                }
                names.push_back(make_ident_node(current_));
                advance();
            }
            field->names = make_list(arena_, names);
            field->type = parse_type();
        } else if (current_.kind == TokenKind::Dot) {
            // Embedded qualified type: pkg.Type
            SourceLocation dot_loc = current_.location;
            advance();
            if (current_.kind == TokenKind::Identifier) {
                auto* t = make_type(TypeExprKind::Qualified);
                t->qualified.loc = first.location;
                t->qualified.package = first.text;
                t->qualified.name = current_.text;
                t->qualified.dot_loc = dot_loc;
                advance();
                field->type = t;
            } else {
                error_expected("type name");
                field->type = make_bad_type(first.location);
            }
            field->names = {};
        } else {
            // Embedded type (simple name)
            auto* t = make_type(TypeExprKind::Ident);
            t->ident.loc = first.location;
            t->ident.name = first.text;
            field->type = t;
            field->names = {};
        }

        // Check for tag
        if (current_.kind == TokenKind::StringLiteral) {
            auto* tag = alloc<Expr>();
            tag->kind = ExprKind::BasicLit;
            tag->basic_lit.loc = current_.location;
            tag->basic_lit.kind = TokenKind::StringLiteral;
            tag->basic_lit.value = current_.text;
            advance();
            field->tag = tag;
        }
        return field;
    }

    // Non-identifier (complex type)
    field->type = parse_type();
    field->names = {};
    return field;
}

// ============================================================================
// Statements
// ============================================================================

Stmt* Parser::parse_stmt() {
    switch (current_.kind) {
        case TokenKind::KW_const:
        case TokenKind::KW_type:
        case TokenKind::KW_var: {
            auto* s = make_stmt(StmtKind::Decl);
            s->decl.loc = current_.location;
            s->decl.decl = parse_top_level_decl();
            return s;
        }

        case TokenKind::LBrace:
            return parse_block();

        case TokenKind::KW_if:
            return parse_if_stmt();

        case TokenKind::KW_for:
            return parse_for_stmt();

        case TokenKind::KW_switch:
            return parse_switch_stmt();

        case TokenKind::KW_select:
            return parse_select_stmt();

        case TokenKind::KW_return:
            return parse_return_stmt();

        case TokenKind::KW_go:
            return parse_go_stmt();

        case TokenKind::KW_defer:
            return parse_defer_stmt();

        case TokenKind::KW_break:
        case TokenKind::KW_continue:
        case TokenKind::KW_goto:
            return parse_branch_stmt(current_.kind);

        case TokenKind::KW_fallthrough: {
            auto* s = make_stmt(StmtKind::Branch);
            s->branch.loc = current_.location;
            s->branch.tok = TokenKind::KW_fallthrough;
            s->branch.label = nullptr;
            advance();
            expect_semicolon();
            return s;
        }

        case TokenKind::Semicolon: {
            auto* s = make_stmt(StmtKind::Empty);
            s->empty.loc = current_.location;
            advance();
            return s;
        }

        default:
            // Simple statement (expression, send, assignment, short var decl, inc/dec)
            return parse_simple_stmt(false);
    }
}

Stmt* Parser::parse_simple_stmt(bool no_semi) {
    SourceLocation loc = current_.location;

    // Parse expression list (could be LHS of assignment or single expr)
    auto* first_expr = parse_expr();

    // Check for label: ident ":" stmt
    if (first_expr->kind == ExprKind::Ident && current_.kind == TokenKind::Colon) {
        auto* s = make_stmt(StmtKind::Label);
        s->label.loc = first_expr->ident.loc;
        s->label.label = &first_expr->ident;
        s->label.colon = current_.location;
        advance(); // consume ':'
        s->label.stmt = parse_stmt();
        return s;
    }

    // Check for send: ch <- value
    if (current_.kind == TokenKind::Arrow) {
        auto* s = make_stmt(StmtKind::Send);
        s->send.loc = loc;
        s->send.chan = first_expr;
        s->send.arrow = current_.location;
        advance();
        s->send.value = parse_expr();
        if (!no_semi) expect_semicolon();
        return s;
    }

    // Check for increment/decrement
    if (current_.kind == TokenKind::Increment || current_.kind == TokenKind::Decrement) {
        auto* s = make_stmt(StmtKind::IncDec);
        s->inc_dec.loc = loc;
        s->inc_dec.x = first_expr;
        s->inc_dec.tok = current_.kind;
        advance();
        if (!no_semi) expect_semicolon();
        return s;
    }

    // Check for short var declaration: x := ... or x, y := ...
    if (current_.kind == TokenKind::ColonAssign) {
        SourceLocation tok_loc = current_.location;
        advance();

        // Check for range clause: x := range expr
        if (current_.kind == TokenKind::KW_range) {
            advance();
            auto* s = make_stmt(StmtKind::Range);
            s->range.loc = loc;
            s->range.key = first_expr;
            s->range.value = nullptr;
            s->range.tok = TokenKind::ColonAssign;
            s->range.tok_loc = tok_loc;
            s->range.x = parse_expr();
            // no_semi is expected true here (called from for stmt)
            return s;
        }

        std::vector<Expr*> lhs;
        lhs.push_back(first_expr);

        auto rhs = parse_expr_list();

        auto* s = make_stmt(StmtKind::ShortVarDecl);
        s->short_var_decl.loc = loc;
        s->short_var_decl.lhs = make_list(arena_, lhs);
        s->short_var_decl.rhs = rhs;
        s->short_var_decl.tok_loc = tok_loc;
        if (!no_semi) expect_semicolon();
        return s;
    }

    // Check for assignment (=, +=, -=, etc.)
    if (is_assign_op(current_.kind)) {
        TokenKind op = current_.kind;
        SourceLocation tok_loc = current_.location;
        advance();

        std::vector<Expr*> lhs;
        lhs.push_back(first_expr);

        auto rhs = parse_expr_list();

        auto* s = make_stmt(StmtKind::Assign);
        s->assign.loc = loc;
        s->assign.lhs = make_list(arena_, lhs);
        s->assign.rhs = rhs;
        s->assign.tok = op;
        s->assign.tok_loc = tok_loc;
        if (!no_semi) expect_semicolon();
        return s;
    }

    // Check for comma (multi-value expressions followed by assignment or :=)
    if (current_.kind == TokenKind::Comma) {
        std::vector<Expr*> exprs;
        exprs.push_back(first_expr);
        while (match(TokenKind::Comma)) {
            exprs.push_back(parse_expr());
        }

        if (current_.kind == TokenKind::ColonAssign) {
            SourceLocation tok_loc = current_.location;
            advance();

            // Check for range clause: i, v := range expr
            if (current_.kind == TokenKind::KW_range) {
                advance();
                auto* s = make_stmt(StmtKind::Range);
                s->range.loc = loc;
                s->range.key = exprs.size() > 0 ? exprs[0] : nullptr;
                s->range.value = exprs.size() > 1 ? exprs[1] : nullptr;
                s->range.tok = TokenKind::ColonAssign;
                s->range.tok_loc = tok_loc;
                s->range.x = parse_expr();
                return s;
            }

            auto rhs = parse_expr_list();
            auto* s = make_stmt(StmtKind::ShortVarDecl);
            s->short_var_decl.loc = loc;
            s->short_var_decl.lhs = make_list(arena_, exprs);
            s->short_var_decl.rhs = rhs;
            s->short_var_decl.tok_loc = tok_loc;
            if (!no_semi) expect_semicolon();
            return s;
        }

        if (is_assign_op(current_.kind)) {
            TokenKind op = current_.kind;
            SourceLocation tok_loc = current_.location;
            advance();
            auto rhs = parse_expr_list();
            auto* s = make_stmt(StmtKind::Assign);
            s->assign.loc = loc;
            s->assign.lhs = make_list(arena_, exprs);
            s->assign.rhs = rhs;
            s->assign.tok = op;
            s->assign.tok_loc = tok_loc;
            if (!no_semi) expect_semicolon();
            return s;
        }

        // Error: comma-separated expressions without assignment
        error("expected assignment or short variable declaration");
        auto* s = make_stmt(StmtKind::Expr);
        s->expr.loc = loc;
        s->expr.x = first_expr;
        if (!no_semi) expect_semicolon();
        return s;
    }

    // Expression statement
    auto* s = make_stmt(StmtKind::Expr);
    s->expr.loc = loc;
    s->expr.x = first_expr;
    if (!no_semi) expect_semicolon();
    return s;
}

Stmt* Parser::parse_block() {
    SourceLocation lbrace = current_.location;
    expect(TokenKind::LBrace);

    std::vector<Stmt*> stmts;
    parse_stmt_list(stmts);

    SourceLocation rbrace = current_.location;
    expect(TokenKind::RBrace);

    auto* s = make_stmt(StmtKind::Block);
    s->block.loc = lbrace;
    s->block.stmts = make_list(arena_, stmts);
    s->block.lbrace = lbrace;
    s->block.rbrace = rbrace;
    return s;
}

void Parser::parse_stmt_list(std::vector<Stmt*>& stmts) {
    while (!at(TokenKind::RBrace) && !at(TokenKind::KW_case) &&
           !at(TokenKind::KW_default) && !at_end()) {
        auto* stmt = parse_stmt();
        if (stmt) {
            stmts.push_back(stmt);
        }
    }
}

Stmt* Parser::parse_if_stmt() {
    SourceLocation loc = current_.location;
    expect(TokenKind::KW_if);

    auto* s = make_stmt(StmtKind::If);
    s->if_.loc = loc;
    s->if_.init = nullptr;
    s->if_.else_body = nullptr;

    // Disable composite literals so that `if x {` is not parsed as `if x{...}`
    bool saved = allow_composite_lit_;
    allow_composite_lit_ = false;

    // Parse condition or "init; cond"
    auto* first = parse_simple_stmt(/*no_semi=*/true);

    if (current_.kind == TokenKind::LBrace) {
        // "if cond { ... }" - first is the condition as an expr stmt
        if (first->kind == StmtKind::Expr) {
            s->if_.cond = first->expr.x;
        } else {
            error("expected expression as if condition");
            s->if_.cond = make_bad_expr(loc);
        }
    } else if (match(TokenKind::Semicolon)) {
        // "if init; cond { ... }"
        s->if_.init = first;
        s->if_.cond = parse_expr();
    } else {
        error_expected("'{' or ';'");
        s->if_.cond = make_bad_expr(loc);
    }

    allow_composite_lit_ = saved;

    s->if_.body = parse_block();

    // Optional else clause
    if (match(TokenKind::KW_else)) {
        if (current_.kind == TokenKind::KW_if) {
            s->if_.else_body = parse_if_stmt();
        } else {
            s->if_.else_body = parse_block();
        }
    }

    return s;
}

Stmt* Parser::parse_for_stmt() {
    SourceLocation loc = current_.location;
    expect(TokenKind::KW_for);

    // for { ... } (infinite loop)
    if (current_.kind == TokenKind::LBrace) {
        auto* s = make_stmt(StmtKind::For);
        s->for_.loc = loc;
        s->for_.init = nullptr;
        s->for_.cond = nullptr;
        s->for_.post = nullptr;
        s->for_.body = parse_block();
        return s;
    }

    // Disable composite literals in for-header
    bool saved = allow_composite_lit_;
    allow_composite_lit_ = false;

    // Check for range clause: for k, v := range x { ... }
    // or: for range x { ... }
    if (current_.kind == TokenKind::KW_range) {
        advance();
        auto* s = make_stmt(StmtKind::Range);
        s->range.loc = loc;
        s->range.key = nullptr;
        s->range.value = nullptr;
        s->range.tok = TokenKind::Invalid;
        s->range.tok_loc = {};
        s->range.x = parse_expr();
        allow_composite_lit_ = saved;
        s->range.body = parse_block();
        return s;
    }

    // Parse first expression/statement (no semicolon consumption)
    auto* first = parse_simple_stmt(/*no_semi=*/true);

    // If parse_simple_stmt returned a Range statement, it already consumed
    // "key, value := range x" — just add the body and return
    if (first->kind == StmtKind::Range) {
        allow_composite_lit_ = saved;
        first->range.body = parse_block();
        return first;
    }

    // for cond { ... }
    if (current_.kind == TokenKind::LBrace) {
        allow_composite_lit_ = saved;
        if (first->kind == StmtKind::Expr) {
            auto* s = make_stmt(StmtKind::For);
            s->for_.loc = loc;
            s->for_.init = nullptr;
            s->for_.cond = first->expr.x;
            s->for_.post = nullptr;
            s->for_.body = parse_block();
            return s;
        }
        // Some other statement form before { - treat as no-condition loop
        auto* s = make_stmt(StmtKind::For);
        s->for_.loc = loc;
        s->for_.init = nullptr;
        s->for_.cond = nullptr;
        s->for_.post = nullptr;
        s->for_.body = parse_block();
        return s;
    }

    // for init; cond; post { ... }
    expect(TokenKind::Semicolon);
    auto* s = make_stmt(StmtKind::For);
    s->for_.loc = loc;
    s->for_.init = first;

    // Parse condition
    if (current_.kind != TokenKind::Semicolon) {
        s->for_.cond = parse_expr();
    } else {
        s->for_.cond = nullptr;
    }
    expect(TokenKind::Semicolon);

    // Parse post (no semicolon)
    if (current_.kind != TokenKind::LBrace) {
        s->for_.post = parse_simple_stmt(/*no_semi=*/true);
    } else {
        s->for_.post = nullptr;
    }

    allow_composite_lit_ = saved;
    s->for_.body = parse_block();
    return s;
}

Stmt* Parser::parse_switch_stmt() {
    SourceLocation loc = current_.location;
    expect(TokenKind::KW_switch);

    // Disable composite literals in switch header
    bool saved = allow_composite_lit_;
    allow_composite_lit_ = false;

    // Helper: detect if a stmt is a type switch guard (x.(type) or v := x.(type))
    auto is_typeswitch_guard = [](Stmt* s) -> bool {
        if (!s) return false;
        if (s->kind == StmtKind::Expr && s->expr.x &&
            s->expr.x->kind == ExprKind::TypeAssert &&
            s->expr.x->type_assert.type == nullptr)
            return true;
        if (s->kind == StmtKind::ShortVarDecl) {
            auto& svd = s->short_var_decl;
            return svd.rhs.count > 0 && svd.rhs[0] &&
                   svd.rhs[0]->kind == ExprKind::TypeAssert &&
                   svd.rhs[0]->type_assert.type == nullptr;
        }
        return false;
    };

    // Helper: build and return a TypeSwitchStmt given init and assign stmts
    auto build_type_switch = [&](Stmt* init_stmt, Stmt* assign_stmt) -> Stmt* {
        auto* ts = make_stmt(StmtKind::TypeSwitch);
        ts->type_switch.loc = loc;
        ts->type_switch.init = init_stmt;
        ts->type_switch.assign = assign_stmt;
        ts->type_switch.lbrace = current_.location;
        expect(TokenKind::LBrace);
        std::vector<CaseClause*> type_cases;
        while (!at(TokenKind::RBrace) && !at_end())
            type_cases.push_back(parse_case_clause());
        ts->type_switch.cases = make_list(arena_, type_cases);
        ts->type_switch.rbrace = current_.location;
        expect(TokenKind::RBrace);
        return ts;
    };

    // switch { ... } or switch tag { ... } or switch init; tag { ... }
    if (current_.kind != TokenKind::LBrace) {
        auto* first = parse_simple_stmt(/*no_semi=*/true);

        if (current_.kind == TokenKind::LBrace) {
            // switch tag { ... }  or  switch x.(type) { ... }
            allow_composite_lit_ = saved;
            if (is_typeswitch_guard(first)) {
                return build_type_switch(nullptr, first);
            }
            auto* s = make_stmt(StmtKind::Switch);
            s->switch_.loc = loc;
            s->switch_.init = nullptr;
            s->switch_.tag = nullptr;
            if (first->kind == StmtKind::Expr)
                s->switch_.tag = first->expr.x;
            s->switch_.lbrace = current_.location;
            expect(TokenKind::LBrace);
            std::vector<CaseClause*> cases;
            while (!at(TokenKind::RBrace) && !at_end())
                cases.push_back(parse_case_clause());
            s->switch_.cases = make_list(arena_, cases);
            s->switch_.rbrace = current_.location;
            expect(TokenKind::RBrace);
            return s;
        } else if (match(TokenKind::Semicolon)) {
            // switch init; [tag] { ... }
            // NOTE: do NOT restore allow_composite_lit_ here yet.
            // The tag expression must be parsed with composite literals
            // disabled so that  switch init; x {  does not misparse x{...}
            // as a composite literal.
            if (current_.kind == TokenKind::LBrace) {
                // switch init; { ... }  (no tag)
                allow_composite_lit_ = saved;
                auto* s = make_stmt(StmtKind::Switch);
                s->switch_.loc = loc;
                s->switch_.init = first;
                s->switch_.tag = nullptr;
                s->switch_.lbrace = current_.location;
                expect(TokenKind::LBrace);
                std::vector<CaseClause*> cases;
                while (!at(TokenKind::RBrace) && !at_end())
                    cases.push_back(parse_case_clause());
                s->switch_.cases = make_list(arena_, cases);
                s->switch_.rbrace = current_.location;
                expect(TokenKind::RBrace);
                return s;
            }
            // Parse the tag / type guard after the semicolon.
            // Keep allow_composite_lit_ = false during tag parse so an
            // identifier tag followed by '{' is not consumed as a composite
            // literal.  Restore it only after tag_stmt is parsed.
            auto* tag_stmt = parse_simple_stmt(/*no_semi=*/true);
            allow_composite_lit_ = saved;
            if (is_typeswitch_guard(tag_stmt)) {
                return build_type_switch(first, tag_stmt);
            }
            auto* s = make_stmt(StmtKind::Switch);
            s->switch_.loc = loc;
            s->switch_.init = first;
            s->switch_.tag = (tag_stmt->kind == StmtKind::Expr) ? tag_stmt->expr.x : nullptr;
            s->switch_.lbrace = current_.location;
            expect(TokenKind::LBrace);
            std::vector<CaseClause*> cases;
            while (!at(TokenKind::RBrace) && !at_end())
                cases.push_back(parse_case_clause());
            s->switch_.cases = make_list(arena_, cases);
            s->switch_.rbrace = current_.location;
            expect(TokenKind::RBrace);
            return s;
        } else {
            allow_composite_lit_ = saved;
            error_expected("'{' or ';'");
        }
    }

    allow_composite_lit_ = saved;

    auto* s = make_stmt(StmtKind::Switch);
    s->switch_.loc = loc;
    s->switch_.init = nullptr;
    s->switch_.tag = nullptr;

    s->switch_.lbrace = current_.location;
    expect(TokenKind::LBrace);

    std::vector<CaseClause*> cases;
    while (!at(TokenKind::RBrace) && !at_end()) {
        cases.push_back(parse_case_clause());
    }
    s->switch_.cases = make_list(arena_, cases);

    s->switch_.rbrace = current_.location;
    expect(TokenKind::RBrace);
    return s;
}

CaseClause* Parser::parse_case_clause() {
    auto* cc = alloc<CaseClause>();
    cc->loc = current_.location;

    if (match(TokenKind::KW_case)) {
        cc->values = parse_expr_list();
    } else if (match(TokenKind::KW_default)) {
        cc->values = {};
    } else {
        error_expected("'case' or 'default'");
        cc->values = {};
    }

    cc->colon = current_.location;
    expect(TokenKind::Colon);

    std::vector<Stmt*> body;
    parse_stmt_list(body);
    cc->body = make_list(arena_, body);
    return cc;
}

Stmt* Parser::parse_select_stmt() {
    SourceLocation loc = current_.location;
    expect(TokenKind::KW_select);

    auto* s = make_stmt(StmtKind::Select);
    s->select.loc = loc;
    s->select.lbrace = current_.location;
    expect(TokenKind::LBrace);

    std::vector<CommClause*> cases;
    while (!at(TokenKind::RBrace) && !at_end()) {
        cases.push_back(parse_comm_clause());
    }
    s->select.cases = make_list(arena_, cases);

    s->select.rbrace = current_.location;
    expect(TokenKind::RBrace);
    return s;
}

CommClause* Parser::parse_comm_clause() {
    auto* cc = alloc<CommClause>();
    cc->loc = current_.location;
    cc->comm = nullptr;

    if (match(TokenKind::KW_case)) {
        cc->comm = parse_simple_stmt(/*no_semi=*/true);
    } else if (!match(TokenKind::KW_default)) {
        error_expected("'case' or 'default'");
    }

    cc->colon = current_.location;
    expect(TokenKind::Colon);

    std::vector<Stmt*> body;
    parse_stmt_list(body);
    cc->body = make_list(arena_, body);
    return cc;
}

Stmt* Parser::parse_return_stmt() {
    SourceLocation loc = current_.location;
    expect(TokenKind::KW_return);

    auto* s = make_stmt(StmtKind::Return);
    s->return_.loc = loc;

    // Parse optional return values
    if (current_.kind != TokenKind::Semicolon && current_.kind != TokenKind::RBrace &&
        current_.kind != TokenKind::Eof) {
        s->return_.results = parse_expr_list();
    } else {
        s->return_.results = {};
    }

    expect_semicolon();
    return s;
}

Stmt* Parser::parse_go_stmt() {
    SourceLocation loc = current_.location;
    expect(TokenKind::KW_go);

    auto* s = make_stmt(StmtKind::Go);
    s->go.loc = loc;
    s->go.call = parse_expr();
    expect_semicolon();
    return s;
}

Stmt* Parser::parse_defer_stmt() {
    SourceLocation loc = current_.location;
    expect(TokenKind::KW_defer);

    auto* s = make_stmt(StmtKind::Defer);
    s->defer.loc = loc;
    s->defer.call = parse_expr();
    expect_semicolon();
    return s;
}

Stmt* Parser::parse_branch_stmt(TokenKind keyword) {
    SourceLocation loc = current_.location;
    advance(); // consume keyword

    auto* s = make_stmt(StmtKind::Branch);
    s->branch.loc = loc;
    s->branch.tok = keyword;
    s->branch.label = nullptr;

    // Optional label (for break, continue, goto)
    if (keyword != TokenKind::KW_fallthrough && current_.kind == TokenKind::Identifier) {
        s->branch.label = make_ident_node(current_);
        advance();
    }

    expect_semicolon();
    return s;
}

// ============================================================================
// Expressions
// ============================================================================

Expr* Parser::parse_expr() {
    return parse_binary_expr(1);
}

Expr* Parser::parse_binary_expr(int prec) {
    auto* left = parse_unary_expr();

    while (true) {
        int op_prec = binary_prec(current_.kind);
        if (op_prec < prec) {
            break;
        }

        TokenKind op = current_.kind;
        SourceLocation op_loc = current_.location;
        advance();

        auto* right = parse_binary_expr(op_prec + 1);

        auto* bin = make_expr(ExprKind::Binary);
        bin->binary.loc = left->location();
        bin->binary.op = op;
        bin->binary.left = left;
        bin->binary.right = right;
        bin->binary.op_loc = op_loc;
        left = bin;
    }

    return left;
}

Expr* Parser::parse_unary_expr() {
    switch (current_.kind) {
        case TokenKind::Plus:
        case TokenKind::Minus:
        case TokenKind::Not:
        case TokenKind::Caret:
        case TokenKind::Ampersand: {
            SourceLocation loc = current_.location;
            TokenKind op = current_.kind;
            advance();
            auto* x = parse_unary_expr();
            auto* e = make_expr(ExprKind::Unary);
            e->unary.loc = loc;
            e->unary.op = op;
            e->unary.x = x;
            return e;
        }
        case TokenKind::Star: {
            SourceLocation loc = current_.location;
            advance();
            auto* x = parse_unary_expr();
            auto* e = make_expr(ExprKind::Star);
            e->star.loc = loc;
            e->star.x = x;
            return e;
        }
        case TokenKind::Arrow: {
            // <-x (channel receive)
            SourceLocation loc = current_.location;
            advance();
            auto* x = parse_unary_expr();
            auto* e = make_expr(ExprKind::Unary);
            e->unary.loc = loc;
            e->unary.op = TokenKind::Arrow;
            e->unary.x = x;
            return e;
        }
        default:
            return parse_primary_expr();
    }
}

Expr* Parser::parse_primary_expr() {
    auto* x = parse_operand();

    // Parse postfix operations: selector, index, slice, type assert, call
    while (true) {
        switch (current_.kind) {
            case TokenKind::Dot:
                x = parse_selector_or_type_assert(x);
                break;
            case TokenKind::LBracket:
                x = parse_index_or_slice(x);
                break;
            case TokenKind::LParen:
                x = parse_call_expr(x);
                break;
            case TokenKind::LBrace:
                // Composite literal: Type{...}
                // Only valid if composite lits are allowed and x is type-like
                if (allow_composite_lit_ &&
                    (x->kind == ExprKind::Ident || x->kind == ExprKind::Selector)) {
                    // Convert the expression to a type for composite literal
                    TypeExpr* type = nullptr;
                    if (x->kind == ExprKind::Ident) {
                        type = make_type(TypeExprKind::Ident);
                        type->ident.loc = x->ident.loc;
                        type->ident.name = x->ident.name;
                    } else if (x->kind == ExprKind::Selector) {
                        type = make_type(TypeExprKind::Qualified);
                        type->qualified.loc = x->selector.loc;
                        type->qualified.package = x->selector.x->ident.name;
                        type->qualified.name = x->selector.sel->name;
                        type->qualified.dot_loc = {};
                    }
                    x = parse_composite_lit(type);
                } else {
                    return x;
                }
                break;
            default:
                return x;
        }
    }
}

Expr* Parser::parse_operand() {
    switch (current_.kind) {
        case TokenKind::Identifier: {
            Token tok = current_;
            advance();
            return make_ident(tok);
        }

        case TokenKind::IntLiteral:
        case TokenKind::FloatLiteral:
        case TokenKind::ImaginaryLiteral:
        case TokenKind::RuneLiteral:
        case TokenKind::StringLiteral: {
            auto* e = make_expr(ExprKind::BasicLit);
            e->basic_lit.loc = current_.location;
            e->basic_lit.kind = current_.kind;
            e->basic_lit.value = current_.text;
            advance();
            return e;
        }

        case TokenKind::LParen: {
            SourceLocation lparen = current_.location;
            advance();
            auto* x = parse_expr();
            SourceLocation rparen = current_.location;
            expect(TokenKind::RParen);
            auto* e = make_expr(ExprKind::Paren);
            e->paren.loc = lparen;
            e->paren.x = x;
            e->paren.lparen = lparen;
            e->paren.rparen = rparen;
            return e;
        }

        case TokenKind::KW_func:
            return parse_func_lit();

        // Type conversions / composite literals with type keywords
        case TokenKind::KW_map: {
            auto* type = parse_map_type();
            if (current_.kind == TokenKind::LBrace) {
                return parse_composite_lit(type);
            }
            // Map type used as conversion
            auto* e = make_ident(Token{TokenKind::Identifier, "map", type->location()});
            return e;
        }
        case TokenKind::KW_struct: {
            auto* type = parse_struct_type();
            if (current_.kind == TokenKind::LBrace) {
                return parse_composite_lit(type);
            }
            auto* e = make_bad_expr(type->location());
            return e;
        }
        case TokenKind::LBracket: {
            // [N]T{...} or []T{...} as composite literal
            auto* type = parse_array_or_slice_type();
            if (current_.kind == TokenKind::LBrace) {
                return parse_composite_lit(type);
            }
            return make_bad_expr(type->location());
        }

        default:
            error_expected("expression");
            auto* e = make_bad_expr(current_.location);
            advance();
            return e;
    }
}

Expr* Parser::parse_composite_lit(TypeExpr* type) {
    auto* e = make_expr(ExprKind::CompositeLit);
    e->composite_lit.loc = type ? type->location() : current_.location;
    e->composite_lit.type = type;
    e->composite_lit.lbrace = current_.location;
    expect(TokenKind::LBrace);

    std::vector<Expr*> elts;
    while (!at(TokenKind::RBrace) && !at_end()) {
        elts.push_back(parse_element());
        if (!match(TokenKind::Comma)) {
            break;
        }
    }
    e->composite_lit.elts = make_list(arena_, elts);
    e->composite_lit.rbrace = current_.location;
    expect(TokenKind::RBrace);
    return e;
}

Expr* Parser::parse_element() {
    auto* x = parse_expr();

    // Check for key: value
    if (current_.kind == TokenKind::Colon) {
        SourceLocation colon = current_.location;
        advance();
        auto* value = parse_expr();
        // Check if value is a composite literal without explicit type (nested { ... })
        // This is handled by parse_expr -> parse_primary_expr -> parse_operand

        auto* kv = make_expr(ExprKind::KeyValue);
        kv->key_value.loc = x->location();
        kv->key_value.key = x;
        kv->key_value.value = value;
        kv->key_value.colon = colon;
        return kv;
    }

    return x;
}

Expr* Parser::parse_literal_value() {
    return parse_composite_lit(nullptr);
}

Expr* Parser::parse_func_lit() {
    SourceLocation loc = current_.location;
    auto* type = parse_func_type();

    auto* e = make_expr(ExprKind::FuncLit);
    e->func_lit.loc = loc;
    e->func_lit.type = type;
    e->func_lit.body = parse_block();
    return e;
}

Expr* Parser::parse_selector_or_type_assert(Expr* x) {
    SourceLocation dot_loc = current_.location;
    advance(); // consume '.'

    // Type assertion: x.(type) or x.(T)
    if (current_.kind == TokenKind::LParen) {
        SourceLocation lparen = current_.location;
        advance();

        if (current_.kind == TokenKind::KW_type) {
            // x.(type) - for type switch
            advance();
            SourceLocation rparen = current_.location;
            expect(TokenKind::RParen);
            auto* e = make_expr(ExprKind::TypeAssert);
            e->type_assert.loc = x->location();
            e->type_assert.x = x;
            e->type_assert.type = nullptr; // null means type switch
            e->type_assert.lparen = lparen;
            e->type_assert.rparen = rparen;
            return e;
        }

        auto* type = parse_type();
        SourceLocation rparen = current_.location;
        expect(TokenKind::RParen);

        auto* e = make_expr(ExprKind::TypeAssert);
        e->type_assert.loc = x->location();
        e->type_assert.x = x;
        e->type_assert.type = type;
        e->type_assert.lparen = lparen;
        e->type_assert.rparen = rparen;
        return e;
    }

    // Selector: x.field
    if (current_.kind != TokenKind::Identifier) {
        error_expected("selector or type assertion");
        return make_bad_expr(dot_loc);
    }

    auto* sel = make_ident_node(current_);
    advance();

    auto* e = make_expr(ExprKind::Selector);
    e->selector.loc = x->location();
    e->selector.x = x;
    e->selector.sel = sel;
    return e;
}

Expr* Parser::parse_index_or_slice(Expr* x) {
    SourceLocation lbrack = current_.location;
    advance(); // consume '['

    // Check for empty slice: x[:]
    if (current_.kind == TokenKind::Colon) {
        advance();
        Expr* high = nullptr;
        Expr* max = nullptr;
        bool three = false;
        if (current_.kind != TokenKind::RBracket) {
            high = parse_expr();
            if (current_.kind == TokenKind::Colon) {
                three = true;
                advance();
                max = parse_expr();
            }
        }
        SourceLocation rbrack = current_.location;
        expect(TokenKind::RBracket);

        auto* e = make_expr(ExprKind::Slice);
        e->slice.loc = x->location();
        e->slice.x = x;
        e->slice.low = nullptr;
        e->slice.high = high;
        e->slice.max = max;
        e->slice.three_index = three;
        e->slice.lbrack = lbrack;
        e->slice.rbrack = rbrack;
        return e;
    }

    auto* index = parse_expr();

    // Check for slice: x[lo:hi] or x[lo:hi:max]
    if (current_.kind == TokenKind::Colon) {
        advance();
        Expr* high = nullptr;
        Expr* max = nullptr;
        bool three = false;
        if (current_.kind != TokenKind::RBracket) {
            high = parse_expr();
            if (current_.kind == TokenKind::Colon) {
                three = true;
                advance();
                max = parse_expr();
            }
        }
        SourceLocation rbrack = current_.location;
        expect(TokenKind::RBracket);

        auto* e = make_expr(ExprKind::Slice);
        e->slice.loc = x->location();
        e->slice.x = x;
        e->slice.low = index;
        e->slice.high = high;
        e->slice.max = max;
        e->slice.three_index = three;
        e->slice.lbrack = lbrack;
        e->slice.rbrack = rbrack;
        return e;
    }

    SourceLocation rbrack = current_.location;
    expect(TokenKind::RBracket);

    auto* e = make_expr(ExprKind::Index);
    e->index.loc = x->location();
    e->index.x = x;
    e->index.index = index;
    e->index.lbrack = lbrack;
    e->index.rbrack = rbrack;
    return e;
}

Expr* Parser::parse_call_expr(Expr* func) {
    SourceLocation lparen = current_.location;
    advance(); // consume '('

    auto* e = make_expr(ExprKind::Call);
    e->call.loc = func->location();
    e->call.func = func;
    e->call.has_ellipsis = false;
    e->call.lparen = lparen;

    std::vector<Expr*> args;
    while (!at(TokenKind::RParen) && !at_end()) {
        // In Go, call arguments can be types (e.g., make(chan int, 10), make([]int, 5)).
        // If we see a type keyword or '[' that can't start an expression, parse as type
        // and wrap in a CompositeLit with no elements (as a type carrier).
        bool is_type_arg = (current_.kind == TokenKind::KW_chan ||
                            current_.kind == TokenKind::KW_map ||
                            current_.kind == TokenKind::KW_interface);
        // '[' starting a call arg is either []T (slice type) or [N]T (array type)
        // when not followed by a valid expression context — treat as type carrier.
        if (!is_type_arg && current_.kind == TokenKind::LBracket) {
            // Peek: if next token is ']' it's []T (slice type), always a type arg.
            // If next is a digit or ']' it's [N]T. Use type parsing.
            is_type_arg = true;
        }
        if (is_type_arg) {
            auto* type = parse_type();
            // Wrap the type in a CompositeLit with no elements — acts as type carrier
            auto* te = make_expr(ExprKind::CompositeLit);
            te->composite_lit.loc = type->location();
            te->composite_lit.type = type;
            te->composite_lit.elts = {};
            te->composite_lit.lbrace = {};
            te->composite_lit.rbrace = {};
            args.push_back(te);
        } else {
            args.push_back(parse_expr());
        }
        if (match(TokenKind::Ellipsis)) {
            e->call.has_ellipsis = true;
        }
        if (!match(TokenKind::Comma)) {
            break;
        }
    }
    e->call.args = make_list(arena_, args);
    e->call.rparen = current_.location;
    expect(TokenKind::RParen);
    return e;
}

List<Expr*> Parser::parse_expr_list() {
    std::vector<Expr*> exprs;
    exprs.push_back(parse_expr());
    while (match(TokenKind::Comma)) {
        exprs.push_back(parse_expr());
    }
    return make_list(arena_, exprs);
}

List<IdentExpr*> Parser::parse_ident_list() {
    std::vector<IdentExpr*> idents;
    if (current_.kind != TokenKind::Identifier) {
        error_expected("identifier");
        return {};
    }
    idents.push_back(make_ident_node(current_));
    advance();
    while (match(TokenKind::Comma)) {
        if (current_.kind != TokenKind::Identifier) {
            error_expected("identifier");
            break;
        }
        idents.push_back(make_ident_node(current_));
        advance();
    }
    return make_list(arena_, idents);
}

} // namespace golangc
