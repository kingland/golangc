#pragma once

#include "ast/ast.hpp"

#include <string>

namespace golangc {
namespace ast {

/// Pretty-prints an AST as an indented tree structure.
class AstPrinter {
public:
    [[nodiscard]] std::string print(const File* file);

private:
    std::string output_;
    int indent_ = 0;

    void print_file(const File* file);
    void print_decl(const Decl* decl);
    void print_stmt(const Stmt* stmt);
    void print_expr(const Expr* expr);
    void print_type(const TypeExpr* type);
    void print_field_list(const FieldList* fl, std::string_view label);
    void print_field(const Field* f);

    void line(std::string_view text);
    void indent();
    void dedent();
};

} // namespace ast
} // namespace golangc
