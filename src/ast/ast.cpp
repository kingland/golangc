#include "ast/ast.hpp"

namespace golangc {
namespace ast {

SourceLocation Expr::location() const {
    switch (kind) {
        case ExprKind::Bad:          return bad.loc;
        case ExprKind::Ident:        return ident.loc;
        case ExprKind::BasicLit:     return basic_lit.loc;
        case ExprKind::CompositeLit: return composite_lit.loc;
        case ExprKind::FuncLit:      return func_lit.loc;
        case ExprKind::Paren:        return paren.loc;
        case ExprKind::Selector:     return selector.loc;
        case ExprKind::Index:        return index.loc;
        case ExprKind::Slice:        return slice.loc;
        case ExprKind::TypeAssert:   return type_assert.loc;
        case ExprKind::Call:         return call.loc;
        case ExprKind::Unary:        return unary.loc;
        case ExprKind::Binary:       return binary.loc;
        case ExprKind::KeyValue:     return key_value.loc;
        case ExprKind::Ellipsis:     return ellipsis.loc;
        case ExprKind::Star:         return star.loc;
    }
    return {};
}

SourceLocation TypeExpr::location() const {
    switch (kind) {
        case TypeExprKind::Bad:       return bad.loc;
        case TypeExprKind::Ident:     return ident.loc;
        case TypeExprKind::Qualified: return qualified.loc;
        case TypeExprKind::Array:     return array.loc;
        case TypeExprKind::Slice:     return slice.loc;
        case TypeExprKind::Map:       return map.loc;
        case TypeExprKind::Chan:      return chan.loc;
        case TypeExprKind::Pointer:   return pointer.loc;
        case TypeExprKind::Func:      return func.loc;
        case TypeExprKind::Struct:    return struct_.loc;
        case TypeExprKind::Interface: return interface_.loc;
        case TypeExprKind::Paren:     return paren.loc;
        case TypeExprKind::Ellipsis:  return ellipsis.loc;
    }
    return {};
}

SourceLocation Stmt::location() const {
    switch (kind) {
        case StmtKind::Bad:          return bad.loc;
        case StmtKind::Block:        return block.loc;
        case StmtKind::Expr:         return expr.loc;
        case StmtKind::Send:         return send.loc;
        case StmtKind::IncDec:       return inc_dec.loc;
        case StmtKind::Assign:       return assign.loc;
        case StmtKind::ShortVarDecl: return short_var_decl.loc;
        case StmtKind::Return:       return return_.loc;
        case StmtKind::Branch:       return branch.loc;
        case StmtKind::If:           return if_.loc;
        case StmtKind::For:          return for_.loc;
        case StmtKind::Range:        return range.loc;
        case StmtKind::Switch:       return switch_.loc;
        case StmtKind::TypeSwitch:   return type_switch.loc;
        case StmtKind::Select:       return select.loc;
        case StmtKind::CaseClause:   return case_clause.loc;
        case StmtKind::CommClause:   return comm_clause.loc;
        case StmtKind::Go:           return go.loc;
        case StmtKind::Defer:        return defer.loc;
        case StmtKind::Label:        return label.loc;
        case StmtKind::Empty:        return empty.loc;
        case StmtKind::Decl:         return decl.loc;
    }
    return {};
}

SourceLocation Decl::location() const {
    switch (kind) {
        case DeclKind::Bad:     return bad.loc;
        case DeclKind::Package: return package.loc;
        case DeclKind::Import:  return import_.loc;
        case DeclKind::Const:   return const_.loc;
        case DeclKind::Type:    return type.loc;
        case DeclKind::Var:     return var.loc;
        case DeclKind::Func:    return func.loc;
    }
    return {};
}

} // namespace ast
} // namespace golangc
