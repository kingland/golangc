#include "ast/ast_printer.hpp"

#include <fmt/format.h>

namespace golangc {
namespace ast {

std::string AstPrinter::print(const File* file) {
    output_.clear();
    indent_ = 0;
    if (file) {
        print_file(file);
    }
    return output_;
}

void AstPrinter::line(std::string_view text) {
    for (int i = 0; i < indent_; ++i) {
        output_ += "  ";
    }
    output_ += text;
    output_ += '\n';
}

void AstPrinter::indent() { ++indent_; }
void AstPrinter::dedent() { --indent_; }

void AstPrinter::print_file(const File* file) {
    line("File");
    indent();
    if (file->package) {
        line(fmt::format("PackageDecl: {}", file->package->name->name));
    }
    for (const auto* d : file->imports) {
        print_decl(d);
    }
    for (const auto* d : file->decls) {
        print_decl(d);
    }
    dedent();
}

void AstPrinter::print_decl(const Decl* decl) {
    if (!decl) { line("nil"); return; }
    switch (decl->kind) {
        case DeclKind::Bad:
            line("BadDecl");
            break;
        case DeclKind::Package:
            line(fmt::format("PackageDecl: {}", decl->package.name->name));
            break;
        case DeclKind::Import: {
            line("ImportDecl");
            indent();
            for (const auto* spec : decl->import_.specs) {
                if (spec->name) {
                    line(fmt::format("ImportSpec: {} {}", spec->name->name,
                                     spec->path ? spec->path->value : "<nil>"));
                } else {
                    line(fmt::format("ImportSpec: {}",
                                     spec->path ? spec->path->value : "<nil>"));
                }
            }
            dedent();
            break;
        }
        case DeclKind::Const: {
            line("ConstDecl");
            indent();
            for (const auto* spec : decl->const_.specs) {
                std::string names;
                for (uint32_t i = 0; i < spec->names.count; ++i) {
                    if (i > 0) names += ", ";
                    names += spec->names[i]->name;
                }
                line(fmt::format("ConstSpec: {}", names));
                indent();
                if (spec->type) {
                    line("Type:");
                    indent();
                    print_type(spec->type);
                    dedent();
                }
                for (const auto* v : spec->values) {
                    print_expr(v);
                }
                dedent();
            }
            dedent();
            break;
        }
        case DeclKind::Type: {
            line("TypeDecl");
            indent();
            for (const auto* spec : decl->type.specs) {
                line(fmt::format("TypeSpec: {}{}", spec->name->name,
                                 spec->is_alias ? " (alias)" : ""));
                indent();
                print_type(spec->type);
                dedent();
            }
            dedent();
            break;
        }
        case DeclKind::Var: {
            line("VarDecl");
            indent();
            for (const auto* spec : decl->var.specs) {
                std::string names;
                for (uint32_t i = 0; i < spec->names.count; ++i) {
                    if (i > 0) names += ", ";
                    names += spec->names[i]->name;
                }
                line(fmt::format("VarSpec: {}", names));
                indent();
                if (spec->type) {
                    line("Type:");
                    indent();
                    print_type(spec->type);
                    dedent();
                }
                for (const auto* v : spec->values) {
                    print_expr(v);
                }
                dedent();
            }
            dedent();
            break;
        }
        case DeclKind::Func: {
            std::string label = "FuncDecl";
            if (decl->func.recv) {
                label = "MethodDecl";
            }
            line(fmt::format("{}: {}", label,
                             decl->func.name ? decl->func.name->name : "<nil>"));
            indent();
            if (decl->func.recv) {
                print_field_list(decl->func.recv, "Receiver");
            }
            if (decl->func.type) {
                if (decl->func.type->params) {
                    print_field_list(decl->func.type->params, "Params");
                }
                if (decl->func.type->results) {
                    print_field_list(decl->func.type->results, "Results");
                }
            }
            if (decl->func.body) {
                print_stmt(decl->func.body);
            }
            dedent();
            break;
        }
    }
}

void AstPrinter::print_stmt(const Stmt* stmt) {
    if (!stmt) { line("nil"); return; }
    switch (stmt->kind) {
        case StmtKind::Bad:
            line("BadStmt");
            break;
        case StmtKind::Block:
            line("BlockStmt");
            indent();
            for (const auto* s : stmt->block.stmts) {
                print_stmt(s);
            }
            dedent();
            break;
        case StmtKind::Expr:
            line("ExprStmt");
            indent();
            print_expr(stmt->expr.x);
            dedent();
            break;
        case StmtKind::Send:
            line("SendStmt");
            indent();
            line("Chan:");
            indent(); print_expr(stmt->send.chan); dedent();
            line("Value:");
            indent(); print_expr(stmt->send.value); dedent();
            dedent();
            break;
        case StmtKind::IncDec:
            line(fmt::format("IncDecStmt: {}",
                             stmt->inc_dec.tok == TokenKind::Increment ? "++" : "--"));
            indent();
            print_expr(stmt->inc_dec.x);
            dedent();
            break;
        case StmtKind::Assign:
            line(fmt::format("AssignStmt: {}", token_kind_to_string(stmt->assign.tok)));
            indent();
            line("LHS:");
            indent();
            for (const auto* e : stmt->assign.lhs) print_expr(e);
            dedent();
            line("RHS:");
            indent();
            for (const auto* e : stmt->assign.rhs) print_expr(e);
            dedent();
            dedent();
            break;
        case StmtKind::ShortVarDecl:
            line("ShortVarDecl");
            indent();
            line("LHS:");
            indent();
            for (const auto* e : stmt->short_var_decl.lhs) print_expr(e);
            dedent();
            line("RHS:");
            indent();
            for (const auto* e : stmt->short_var_decl.rhs) print_expr(e);
            dedent();
            dedent();
            break;
        case StmtKind::Return:
            line("ReturnStmt");
            indent();
            for (const auto* e : stmt->return_.results) print_expr(e);
            dedent();
            break;
        case StmtKind::Branch:
            line(fmt::format("BranchStmt: {}{}",
                             token_kind_to_string(stmt->branch.tok),
                             stmt->branch.label ? fmt::format(" {}", stmt->branch.label->name) : ""));
            break;
        case StmtKind::If:
            line("IfStmt");
            indent();
            if (stmt->if_.init) {
                line("Init:");
                indent(); print_stmt(stmt->if_.init); dedent();
            }
            line("Cond:");
            indent(); print_expr(stmt->if_.cond); dedent();
            line("Body:");
            indent(); print_stmt(stmt->if_.body); dedent();
            if (stmt->if_.else_body) {
                line("Else:");
                indent(); print_stmt(stmt->if_.else_body); dedent();
            }
            dedent();
            break;
        case StmtKind::For:
            line("ForStmt");
            indent();
            if (stmt->for_.init) {
                line("Init:");
                indent(); print_stmt(stmt->for_.init); dedent();
            }
            if (stmt->for_.cond) {
                line("Cond:");
                indent(); print_expr(stmt->for_.cond); dedent();
            }
            if (stmt->for_.post) {
                line("Post:");
                indent(); print_stmt(stmt->for_.post); dedent();
            }
            line("Body:");
            indent(); print_stmt(stmt->for_.body); dedent();
            dedent();
            break;
        case StmtKind::Range:
            line("RangeStmt");
            indent();
            if (stmt->range.key) {
                line("Key:"); indent(); print_expr(stmt->range.key); dedent();
            }
            if (stmt->range.value) {
                line("Value:"); indent(); print_expr(stmt->range.value); dedent();
            }
            line("X:"); indent(); print_expr(stmt->range.x); dedent();
            line("Body:"); indent(); print_stmt(stmt->range.body); dedent();
            dedent();
            break;
        case StmtKind::Switch:
            line("SwitchStmt");
            indent();
            if (stmt->switch_.init) {
                line("Init:"); indent(); print_stmt(stmt->switch_.init); dedent();
            }
            if (stmt->switch_.tag) {
                line("Tag:"); indent(); print_expr(stmt->switch_.tag); dedent();
            }
            for (const auto* cc : stmt->switch_.cases) {
                line(cc->values.empty() ? "Default:" : "Case:");
                indent();
                for (const auto* v : cc->values) print_expr(v);
                line("Body:");
                indent();
                for (const auto* s : cc->body) print_stmt(s);
                dedent();
                dedent();
            }
            dedent();
            break;
        case StmtKind::TypeSwitch:
            line("TypeSwitchStmt");
            break;
        case StmtKind::Select:
            line("SelectStmt");
            indent();
            for (const auto* cc : stmt->select.cases) {
                line(cc->comm ? "Case:" : "Default:");
                indent();
                if (cc->comm) print_stmt(cc->comm);
                line("Body:");
                indent();
                for (const auto* s : cc->body) print_stmt(s);
                dedent();
                dedent();
            }
            dedent();
            break;
        case StmtKind::CaseClause:
            line("CaseClause");
            break;
        case StmtKind::CommClause:
            line("CommClause");
            break;
        case StmtKind::Go:
            line("GoStmt");
            indent(); print_expr(stmt->go.call); dedent();
            break;
        case StmtKind::Defer:
            line("DeferStmt");
            indent(); print_expr(stmt->defer.call); dedent();
            break;
        case StmtKind::Label:
            line(fmt::format("LabelStmt: {}", stmt->label.label->name));
            indent(); print_stmt(stmt->label.stmt); dedent();
            break;
        case StmtKind::Empty:
            line("EmptyStmt");
            break;
        case StmtKind::Decl:
            print_decl(stmt->decl.decl);
            break;
    }
}

void AstPrinter::print_expr(const Expr* expr) {
    if (!expr) { line("nil"); return; }
    switch (expr->kind) {
        case ExprKind::Bad:
            line("BadExpr");
            break;
        case ExprKind::Ident:
            line(fmt::format("Ident: {}", expr->ident.name));
            break;
        case ExprKind::BasicLit:
            line(fmt::format("BasicLit: {} {}", token_kind_to_string(expr->basic_lit.kind),
                             expr->basic_lit.value));
            break;
        case ExprKind::CompositeLit:
            line("CompositeLit");
            indent();
            if (expr->composite_lit.type) {
                line("Type:");
                indent(); print_type(expr->composite_lit.type); dedent();
            }
            for (const auto* e : expr->composite_lit.elts) {
                print_expr(e);
            }
            dedent();
            break;
        case ExprKind::FuncLit:
            line("FuncLit");
            indent();
            if (expr->func_lit.type) print_type(expr->func_lit.type);
            if (expr->func_lit.body) print_stmt(expr->func_lit.body);
            dedent();
            break;
        case ExprKind::Paren:
            line("ParenExpr");
            indent(); print_expr(expr->paren.x); dedent();
            break;
        case ExprKind::Selector:
            line(fmt::format("SelectorExpr: .{}", expr->selector.sel->name));
            indent(); print_expr(expr->selector.x); dedent();
            break;
        case ExprKind::Index:
            line("IndexExpr");
            indent();
            print_expr(expr->index.x);
            line("Index:");
            indent(); print_expr(expr->index.index); dedent();
            dedent();
            break;
        case ExprKind::Slice:
            line(fmt::format("SliceExpr{}", expr->slice.three_index ? " (3-index)" : ""));
            indent();
            print_expr(expr->slice.x);
            if (expr->slice.low) { line("Low:"); indent(); print_expr(expr->slice.low); dedent(); }
            if (expr->slice.high) { line("High:"); indent(); print_expr(expr->slice.high); dedent(); }
            if (expr->slice.max) { line("Max:"); indent(); print_expr(expr->slice.max); dedent(); }
            dedent();
            break;
        case ExprKind::TypeAssert:
            line("TypeAssertExpr");
            indent();
            print_expr(expr->type_assert.x);
            if (expr->type_assert.type) {
                line("Type:");
                indent(); print_type(expr->type_assert.type); dedent();
            } else {
                line("Type: (type switch)");
            }
            dedent();
            break;
        case ExprKind::Call:
            line("CallExpr");
            indent();
            line("Func:");
            indent(); print_expr(expr->call.func); dedent();
            if (!expr->call.args.empty()) {
                line("Args:");
                indent();
                for (const auto* a : expr->call.args) print_expr(a);
                dedent();
            }
            dedent();
            break;
        case ExprKind::Unary:
            line(fmt::format("UnaryExpr: {}", token_kind_to_string(expr->unary.op)));
            indent(); print_expr(expr->unary.x); dedent();
            break;
        case ExprKind::Binary:
            line(fmt::format("BinaryExpr: {}", token_kind_to_string(expr->binary.op)));
            indent();
            print_expr(expr->binary.left);
            print_expr(expr->binary.right);
            dedent();
            break;
        case ExprKind::KeyValue:
            line("KeyValueExpr");
            indent();
            line("Key:");
            indent(); print_expr(expr->key_value.key); dedent();
            line("Value:");
            indent(); print_expr(expr->key_value.value); dedent();
            dedent();
            break;
        case ExprKind::Ellipsis:
            line("EllipsisExpr");
            break;
        case ExprKind::Star:
            line("StarExpr");
            indent(); print_expr(expr->star.x); dedent();
            break;
    }
}

void AstPrinter::print_type(const TypeExpr* type) {
    if (!type) { line("nil"); return; }
    switch (type->kind) {
        case TypeExprKind::Bad:
            line("BadType");
            break;
        case TypeExprKind::Ident:
            line(fmt::format("IdentType: {}", type->ident.name));
            break;
        case TypeExprKind::Qualified:
            line(fmt::format("QualifiedType: {}.{}", type->qualified.package, type->qualified.name));
            break;
        case TypeExprKind::Array:
            line("ArrayType");
            indent();
            line("Length:"); indent(); print_expr(type->array.length); dedent();
            line("Element:"); indent(); print_type(type->array.element); dedent();
            dedent();
            break;
        case TypeExprKind::Slice:
            line("SliceType");
            indent();
            line("Element:"); indent(); print_type(type->slice.element); dedent();
            dedent();
            break;
        case TypeExprKind::Map:
            line("MapType");
            indent();
            line("Key:"); indent(); print_type(type->map.key); dedent();
            line("Value:"); indent(); print_type(type->map.value); dedent();
            dedent();
            break;
        case TypeExprKind::Chan: {
            const char* dir_str = "chan";
            if (type->chan.dir == ChanDir::SendOnly) dir_str = "chan<-";
            else if (type->chan.dir == ChanDir::RecvOnly) dir_str = "<-chan";
            line(fmt::format("ChanType: {}", dir_str));
            indent(); print_type(type->chan.element); dedent();
            break;
        }
        case TypeExprKind::Pointer:
            line("PointerType");
            indent(); print_type(type->pointer.base); dedent();
            break;
        case TypeExprKind::Func:
            line("FuncType");
            indent();
            if (type->func.params) print_field_list(type->func.params, "Params");
            if (type->func.results) print_field_list(type->func.results, "Results");
            dedent();
            break;
        case TypeExprKind::Struct:
            line("StructType");
            indent();
            if (type->struct_.fields) print_field_list(type->struct_.fields, "Fields");
            dedent();
            break;
        case TypeExprKind::Interface:
            line("InterfaceType");
            indent();
            for (const auto* m : type->interface_.methods) {
                if (m->name) {
                    line(fmt::format("Method: {}", m->name->name));
                } else if (m->embedded_type) {
                    line("Embedded:");
                    indent(); print_type(m->embedded_type); dedent();
                }
            }
            dedent();
            break;
        case TypeExprKind::Paren:
            line("ParenType");
            indent(); print_type(type->paren.type); dedent();
            break;
        case TypeExprKind::Ellipsis:
            line("EllipsisType");
            indent(); print_type(type->ellipsis.element); dedent();
            break;
    }
}

void AstPrinter::print_field_list(const FieldList* fl, std::string_view label) {
    if (!fl) return;
    line(fmt::format("{}:", label));
    indent();
    for (const auto* f : fl->fields) {
        print_field(f);
    }
    dedent();
}

void AstPrinter::print_field(const Field* f) {
    if (!f) { line("nil"); return; }
    std::string names;
    for (uint32_t i = 0; i < f->names.count; ++i) {
        if (i > 0) names += ", ";
        names += f->names[i]->name;
    }
    if (names.empty()) {
        line("Field:");
    } else {
        line(fmt::format("Field: {}", names));
    }
    indent();
    if (f->type) print_type(f->type);
    if (f->tag) print_expr(f->tag);
    dedent();
}

} // namespace ast
} // namespace golangc
