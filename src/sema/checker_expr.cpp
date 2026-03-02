#include "sema/checker.hpp"
#include "sema/universe.hpp"

#include <cstring>
#include <fmt/format.h>

namespace golangc {
namespace sema {

// ============================================================================
// Main expression dispatcher
// ============================================================================

ExprInfo Checker::check_expr(ast::Expr* expr) {
    if (!expr) return {};

    ExprInfo info;

    switch (expr->kind) {
        case ast::ExprKind::Bad:
            break;

        case ast::ExprKind::Ident:
            info = check_ident(expr->ident, expr->ident.loc);
            break;

        case ast::ExprKind::BasicLit:
            info = check_basic_lit(expr->basic_lit);
            break;

        case ast::ExprKind::CompositeLit:
            info = check_composite_lit(expr->composite_lit);
            break;

        case ast::ExprKind::FuncLit:
            info = check_func_lit(expr->func_lit);
            break;

        case ast::ExprKind::Paren:
            info = check_paren(expr->paren);
            break;

        case ast::ExprKind::Selector:
            info = check_selector(expr->selector);
            break;

        case ast::ExprKind::Index:
            info = check_index(expr->index);
            break;

        case ast::ExprKind::Slice:
            info = check_slice_expr(expr->slice);
            break;

        case ast::ExprKind::TypeAssert:
            info = check_type_assert(expr->type_assert);
            break;

        case ast::ExprKind::Call:
            info = check_call(expr->call);
            break;

        case ast::ExprKind::Unary:
            info = check_unary(expr->unary);
            break;

        case ast::ExprKind::Binary:
            info = check_binary(expr->binary);
            break;

        case ast::ExprKind::Star:
            info = check_star(expr->star);
            break;

        case ast::ExprKind::KeyValue:
            // Key-value is handled by composite lit checker
            info = check_expr(expr->key_value.value);
            break;

        case ast::ExprKind::Ellipsis:
            // Handled in call expression context
            break;
    }

    record_expr(expr, info);
    return info;
}

// ============================================================================
// Identifier
// ============================================================================

ExprInfo Checker::check_ident(ast::IdentExpr& expr, SourceLocation loc) {
    if (expr.name == "_") {
        ExprInfo blank;
        blank.is_lvalue = true;
        return blank;
    }

    auto* sym = lookup(expr.name);
    if (!sym) {
        diag_.error(loc, "undefined: {}", expr.name);
        return {};
    }

    sym->used = true;

    ExprInfo info;
    info.type = sym->type;
    info.symbol = sym;
    info.const_val = sym->const_val;

    // Variables and parameters are lvalues
    if (sym->kind == SymbolKind::Var) {
        info.is_lvalue = true;
    }

    return info;
}

// ============================================================================
// Basic literals
// ============================================================================

ExprInfo Checker::check_basic_lit(ast::BasicLitExpr& expr) {
    ExprInfo info;

    switch (expr.kind) {
        case TokenKind::IntLiteral:
            info.type = basic_type(BasicKind::UntypedInt);
            info.const_val = eval_const_expr(nullptr); // handled below
            break;
        case TokenKind::FloatLiteral:
            info.type = basic_type(BasicKind::UntypedFloat);
            break;
        case TokenKind::ImaginaryLiteral:
            info.type = basic_type(BasicKind::UntypedComplex);
            break;
        case TokenKind::RuneLiteral:
            info.type = basic_type(BasicKind::UntypedRune);
            break;
        case TokenKind::StringLiteral:
            info.type = basic_type(BasicKind::UntypedString);
            break;
        default:
            break;
    }

    // Evaluate constant value
    // We need to construct a temporary expr for eval_const_expr
    // But we can just inline the logic here
    if (expr.kind == TokenKind::IntLiteral) {
        try {
            int64_t val = std::stoll(std::string(expr.value));
            info.const_val = arena_.create<ConstValue>(val);
        } catch (...) {}
    } else if (expr.kind == TokenKind::FloatLiteral) {
        try {
            double val = std::stod(std::string(expr.value));
            info.const_val = arena_.create<ConstValue>(val);
        } catch (...) {}
    } else if (expr.kind == TokenKind::StringLiteral) {
        auto sv = expr.value;
        if (sv.size() >= 2) {
            sv = sv.substr(1, sv.size() - 2);
        }
        info.const_val = arena_.create<ConstValue>(std::string(sv));
    } else if (expr.kind == TokenKind::RuneLiteral) {
        auto sv = expr.value;
        if (sv.size() >= 3) {
            info.const_val = arena_.create<ConstValue>(static_cast<int64_t>(sv[1]));
        }
    }

    return info;
}

// ============================================================================
// Composite literal
// ============================================================================

ExprInfo Checker::check_composite_lit(ast::CompositeLitExpr& expr) {
    ExprInfo info;

    if (!expr.type) {
        // Type-elided composite literal inside another composite literal
        // (e.g. map[string]T{"k": {field: val}}). Check elements permissively;
        // the IR generator will infer type from surrounding context.
        for (auto* elt : expr.elts.span()) {
            if (elt) (void)check_expr(elt);
        }
        return info; // type stays nullptr; IR gen handles it
    }

    Type* lit_type = resolve_type(expr.type);
    if (!lit_type) return info;

    info.type = lit_type;

    // Type-check elements
    const Type* u = underlying(lit_type);
    if (!u) return info;

    if (u->kind == TypeKind::Struct && u->struct_) {
        // Struct literal
        const auto& fields = u->struct_->fields;
        bool has_keys = false;
        bool has_no_keys = false;

        for (auto* elt : expr.elts.span()) {
            if (!elt) continue;
            if (elt->kind == ast::ExprKind::KeyValue) {
                has_keys = true;
                auto& kv = elt->key_value;

                // Key should be a field name
                if (kv.key && kv.key->kind == ast::ExprKind::Ident) {
                    auto field_name = kv.key->ident.name;
                    bool found = false;
                    for (const auto& f : fields) {
                        if (f.name == field_name) {
                            found = true;
                            auto val_info = check_expr(kv.value);
                            if (val_info.type && !assignable_to(val_info.type, f.type)) {
                                diag_.error(kv.key->ident.loc,
                                    "cannot use {} as {} in field value",
                                    type_string(val_info.type), type_string(f.type));
                            }
                            break;
                        }
                    }
                    if (!found) {
                        diag_.error(kv.key->ident.loc,
                            "unknown field '{}' in struct literal", field_name);
                    }
                }
            } else {
                has_no_keys = true;
                (void)check_expr(elt);
            }
        }

        if (has_keys && has_no_keys) {
            diag_.error(expr.loc, "mixture of field:value and value elements in struct literal");
        }

        if (has_no_keys && expr.elts.count != static_cast<uint32_t>(fields.size())) {
            diag_.error(expr.loc, "too few/many values in struct literal");
        }
    } else {
        // Array, slice, map — check element expressions.
        // For map literals with struct/composite values, infer type for elided literals.
        Type* elem_type = nullptr;
        if (u->kind == sema::TypeKind::Map)   elem_type = u->map.value;
        if (u->kind == sema::TypeKind::Slice) elem_type = u->slice.element;
        if (u->kind == sema::TypeKind::Array) elem_type = u->array.element;

        for (auto* elt : expr.elts.span()) {
            if (!elt) continue;
            // Key-value pair: infer value type for elided composite literals
            if (elt->kind == ast::ExprKind::KeyValue) {
                auto& kv = elt->key_value;
                (void)check_expr(kv.key);
                // If the value is a type-elided composite literal, set its type
                if (kv.value && kv.value->kind == ast::ExprKind::CompositeLit &&
                    !kv.value->composite_lit.type && elem_type) {
                    // Synthesize a type for this elided composite literal
                    kv.value->composite_lit.type = nullptr; // stays nil
                    // Record expr_info with the inferred element type
                    ExprInfo elt_info;
                    elt_info.type = elem_type;
                    record_expr(kv.value, elt_info);
                    // Check its sub-elements recursively using the element type
                    const Type* elem_u = underlying(elem_type);
                    if (elem_u && elem_u->kind == TypeKind::Struct && elem_u->struct_) {
                        for (auto* sub : kv.value->composite_lit.elts.span()) {
                            if (sub) (void)check_expr(sub);
                        }
                    }
                } else {
                    (void)check_expr(kv.value);
                }
            } else if (elt->kind == ast::ExprKind::CompositeLit &&
                       !elt->composite_lit.type && elem_type) {
                // Positional element that is a type-elided composite literal
                ExprInfo elt_info;
                elt_info.type = elem_type;
                record_expr(elt, elt_info);
                for (auto* sub : elt->composite_lit.elts.span()) {
                    if (sub) (void)check_expr(sub);
                }
            } else {
                (void)check_expr(elt);
            }
        }
    }

    return info;
}

// ============================================================================
// Function literal
// ============================================================================

ExprInfo Checker::check_func_lit(ast::FuncLitExpr& expr) {
    ExprInfo info;

    Type* ft = resolve_type(expr.type);
    if (!ft) return info;
    info.type = ft;

    if (ft->kind == TypeKind::Func && ft->func && expr.body) {
        auto* prev_func = current_func_;
        current_func_ = ft->func;

        auto* scope = open_scope(ScopeKind::Function);

        // Add parameters and record AST ident → Symbol* mapping (same as check_func_decl).
        size_t param_idx = 0;
        for (const auto& param : ft->func->params) {
            if (!param.name.empty() && param.name != "_") {
                auto* psym = declare(SymbolKind::Var, param.name, param.type, expr.loc);
                // Map the AST IdentExpr* for this parameter name to the symbol,
                // so the IR generator can look it up via checker_.decl_symbol().
                if (psym && expr.type && expr.type->func.params) {
                    size_t ast_idx = 0;
                    for (auto* field : expr.type->func.params->fields) {
                        for (auto* name_node : field->names) {
                            if (ast_idx == param_idx && name_node) {
                                decl_sym_map_[name_node] = psym;
                            }
                            ++ast_idx;
                        }
                    }
                }
            }
            ++param_idx;
        }

        // Check body
        if (expr.body->kind == ast::StmtKind::Block) {
            for (auto* stmt : expr.body->block.stmts.span()) {
                check_stmt(stmt);
            }
        }

        close_scope();
        (void)scope;
        current_func_ = prev_func;
    }

    return info;
}

// ============================================================================
// Parenthesized expression
// ============================================================================

ExprInfo Checker::check_paren(ast::ParenExpr& expr) {
    return check_expr(expr.x);
}

// ============================================================================
// Pseudo-package selector (fmt.X, strconv.X, os.X)
// ============================================================================

ExprInfo Checker::check_pseudo_pkg_selector(const Symbol& pkg_sym,
                                              ast::SelectorExpr& expr) {
    ExprInfo info;
    std::string_view pkg = pkg_sym.pkg_name;
    std::string_view sel = expr.sel ? expr.sel->name : "";

    // Helper: create an arena-allocated builtin symbol for this member.
    auto make_member_builtin = [&](BuiltinId id) -> Symbol* {
        auto* sym = arena_.create<Symbol>();
        sym->kind = SymbolKind::Builtin;
        sym->builtin_id = static_cast<int>(id);
        sym->used = true;
        // Build a stable string_view by storing the name in the arena.
        std::string full = std::string(pkg) + "." + std::string(sel);
        auto* stored = arena_.allocate_array<char>(full.size() + 1);
        std::memcpy(stored, full.data(), full.size() + 1);
        sym->name = std::string_view(stored, full.size());
        return sym;
    };

    if (pkg == "fmt") {
        if (sel == "Println") {
            info.symbol = make_member_builtin(BuiltinId::FmtPrintln);
            info.type = nullptr; // void return
            return info;
        }
        if (sel == "Printf") {
            info.symbol = make_member_builtin(BuiltinId::FmtPrintf);
            info.type = nullptr;
            return info;
        }
        if (sel == "Sprintf") {
            info.symbol = make_member_builtin(BuiltinId::FmtSprintf);
            info.type = basic_type(BasicKind::String);
            return info;
        }
        if (sel == "Errorf") {
            info.symbol = make_member_builtin(BuiltinId::FmtErrorf);
            info.type = error_type();
            return info;
        }
        if (sel == "Fprintf")  { info.symbol = make_member_builtin(BuiltinId::FmtFprintf);  info.type = nullptr; return info; }
        if (sel == "Fprintln") { info.symbol = make_member_builtin(BuiltinId::FmtFprintln); info.type = nullptr; return info; }
        if (sel == "Scan") {
            info.symbol = make_member_builtin(BuiltinId::FmtScan);
            info.type = make_tuple_type({basic_type(BasicKind::Int), error_type()});
            return info;
        }
        if (sel == "Scanln") {
            info.symbol = make_member_builtin(BuiltinId::FmtScanln);
            info.type = make_tuple_type({basic_type(BasicKind::Int), error_type()});
            return info;
        }
        if (sel == "Scanf") {
            info.symbol = make_member_builtin(BuiltinId::FmtScanf);
            info.type = make_tuple_type({basic_type(BasicKind::Int), error_type()});
            return info;
        }
        if (sel == "Sscan") {
            info.symbol = make_member_builtin(BuiltinId::FmtSscan);
            info.type = make_tuple_type({basic_type(BasicKind::Int), error_type()});
            return info;
        }
        if (sel == "Sscanf") {
            info.symbol = make_member_builtin(BuiltinId::FmtSscanf);
            info.type = make_tuple_type({basic_type(BasicKind::Int), error_type()});
            return info;
        }
        if (sel == "Sprint") {
            info.symbol = make_member_builtin(BuiltinId::FmtSprint);
            info.type = basic_type(BasicKind::String);
            return info;
        }
        if (sel == "Fprint") {
            info.symbol = make_member_builtin(BuiltinId::FmtFprint);
            info.type = make_tuple_type({basic_type(BasicKind::Int), error_type()});
            return info;
        }
        if (sel == "Print") {
            info.symbol = make_member_builtin(BuiltinId::FmtPrintln); // reuse Println codegen, no trailing newline is a minor difference
            info.type = nullptr;
            return info;
        }
    }

    if (pkg == "errors") {
        auto* bool_ty = basic_type(BasicKind::Bool);
        if (sel == "New") {
            info.symbol = make_member_builtin(BuiltinId::ErrorsNew);
            info.type = error_type();
            return info;
        }
        if (sel == "Is") {
            info.symbol = make_member_builtin(BuiltinId::ErrorsIs);
            info.type = bool_ty;
            return info;
        }
        if (sel == "As") {
            info.symbol = make_member_builtin(BuiltinId::ErrorsAs);
            info.type = bool_ty;
            return info;
        }
    }

    if (pkg == "strconv") {
        if (sel == "Itoa") {
            info.symbol = make_member_builtin(BuiltinId::StrconvItoa);
            info.type = basic_type(BasicKind::String);
            return info;
        }
        if (sel == "Atoi") {
            info.symbol = make_member_builtin(BuiltinId::StrconvAtoi);
            // Returns (int, error) — represented as a Tuple type.
            info.type = make_tuple_type({basic_type(BasicKind::Int), error_type()});
            return info;
        }
        if (sel == "ParseInt") {
            info.symbol = make_member_builtin(BuiltinId::StrconvParseInt);
            info.type = make_tuple_type({basic_type(BasicKind::Int64), error_type()});
            return info;
        }
        if (sel == "ParseFloat") {
            info.symbol = make_member_builtin(BuiltinId::StrconvParseFloat);
            info.type = make_tuple_type({basic_type(BasicKind::Float64), error_type()});
            return info;
        }
        if (sel == "FormatInt") {
            info.symbol = make_member_builtin(BuiltinId::StrconvFormatInt);
            info.type = basic_type(BasicKind::String);
            return info;
        }
        if (sel == "FormatFloat") {
            info.symbol = make_member_builtin(BuiltinId::StrconvFormatFloat);
            info.type = basic_type(BasicKind::String);
            return info;
        }
        if (sel == "FormatBool") {
            info.symbol = make_member_builtin(BuiltinId::StrconvFormatBool);
            info.type = basic_type(BasicKind::String);
            return info;
        }
        if (sel == "ParseBool") {
            info.symbol = make_member_builtin(BuiltinId::StrconvParseBool);
            info.type = make_tuple_type({basic_type(BasicKind::Bool), error_type()});
            return info;
        }
    }

    if (pkg == "os") {
        if (sel == "Args") {
            info.symbol = make_member_builtin(BuiltinId::OsArgs);
            // os.Args is []string
            info.type = make_slice_type(basic_type(BasicKind::String));
            return info;
        }
        if (sel == "Stdout") { info.symbol = make_member_builtin(BuiltinId::OsStdout); info.type = os_file_ptr_type(); return info; }
        if (sel == "Stderr") { info.symbol = make_member_builtin(BuiltinId::OsStderr); info.type = os_file_ptr_type(); return info; }
        if (sel == "Stdin")  { info.symbol = make_member_builtin(BuiltinId::OsStdin);  info.type = os_file_ptr_type(); return info; }
        if (sel == "Exit") {
            info.symbol = make_member_builtin(BuiltinId::OsExit);
            info.type = nullptr;
            return info;
        }
        if (sel == "Open") {
            info.symbol = make_member_builtin(BuiltinId::OsOpen);
            info.type = make_tuple_type({os_file_ptr_type(), error_type()});
            return info;
        }
        if (sel == "Create") {
            info.symbol = make_member_builtin(BuiltinId::OsCreate);
            info.type = make_tuple_type({os_file_ptr_type(), error_type()});
            return info;
        }
        if (sel == "ReadFile") {
            info.symbol = make_member_builtin(BuiltinId::OsReadFile);
            info.type = make_tuple_type({make_slice_type(basic_type(BasicKind::Uint8)), error_type()});
            return info;
        }
        if (sel == "Getenv") {
            info.symbol = make_member_builtin(BuiltinId::OsGetenv);
            info.type = basic_type(BasicKind::String);
            return info;
        }
        if (sel == "WriteFile") {
            info.symbol = make_member_builtin(BuiltinId::OsWriteFile);
            info.type = error_type();
            return info;
        }
        if (sel == "Remove") {
            info.symbol = make_member_builtin(BuiltinId::OsRemove);
            info.type = error_type();
            return info;
        }
        if (sel == "Mkdir") {
            info.symbol = make_member_builtin(BuiltinId::OsMkdir);
            info.type = error_type();
            return info;
        }
        if (sel == "MkdirAll") {
            info.symbol = make_member_builtin(BuiltinId::OsMkdirAll);
            info.type = error_type();
            return info;
        }
        if (sel == "UserHomeDir") {
            info.symbol = make_member_builtin(BuiltinId::OsUserHomeDir);
            info.type = make_tuple_type({basic_type(BasicKind::String), error_type()});
            return info;
        }
        if (sel == "TempDir") {
            info.symbol = make_member_builtin(BuiltinId::OsTempDir);
            info.type = basic_type(BasicKind::String);
            return info;
        }
    }

    if (pkg == "strings") {
        auto* bool_type  = basic_type(BasicKind::Bool);
        auto* int_type   = basic_type(BasicKind::Int);
        auto* str_type   = basic_type(BasicKind::String);
        auto* str_slice  = make_slice_type(str_type);
        if (sel == "Contains")  { info.symbol = make_member_builtin(BuiltinId::StringsContains);  info.type = bool_type; return info; }
        if (sel == "HasPrefix") { info.symbol = make_member_builtin(BuiltinId::StringsHasPrefix);  info.type = bool_type; return info; }
        if (sel == "HasSuffix") { info.symbol = make_member_builtin(BuiltinId::StringsHasSuffix);  info.type = bool_type; return info; }
        if (sel == "Index")     { info.symbol = make_member_builtin(BuiltinId::StringsIndex);      info.type = int_type;  return info; }
        if (sel == "ToUpper")   { info.symbol = make_member_builtin(BuiltinId::StringsToUpper);   info.type = str_type;  return info; }
        if (sel == "ToLower")   { info.symbol = make_member_builtin(BuiltinId::StringsToLower);   info.type = str_type;  return info; }
        if (sel == "TrimSpace") { info.symbol = make_member_builtin(BuiltinId::StringsTrimSpace); info.type = str_type;  return info; }
        if (sel == "Repeat")    { info.symbol = make_member_builtin(BuiltinId::StringsRepeat);    info.type = str_type;  return info; }
        if (sel == "Replace")   { info.symbol = make_member_builtin(BuiltinId::StringsReplace);   info.type = str_type;  return info; }
        if (sel == "Count")     { info.symbol = make_member_builtin(BuiltinId::StringsCount);     info.type = int_type;  return info; }
        if (sel == "Trim")      { info.symbol = make_member_builtin(BuiltinId::StringsTrim);      info.type = str_type;  return info; }
        if (sel == "Split")      { info.symbol = make_member_builtin(BuiltinId::StringsSplit);      info.type = str_slice; return info; }
        if (sel == "Join")       { info.symbol = make_member_builtin(BuiltinId::StringsJoin);       info.type = str_type;  return info; }
        if (sel == "Fields")     { info.symbol = make_member_builtin(BuiltinId::StringsFields);     info.type = str_slice; return info; }
        if (sel == "TrimPrefix") { info.symbol = make_member_builtin(BuiltinId::StringsTrimPrefix); info.type = str_type;  return info; }
        if (sel == "TrimSuffix")   { info.symbol = make_member_builtin(BuiltinId::StringsTrimSuffix);   info.type = str_type;  return info; }
        if (sel == "ContainsRune") { info.symbol = make_member_builtin(BuiltinId::StringsContainsRune); info.type = bool_type; return info; }
        if (sel == "IndexByte")    { info.symbol = make_member_builtin(BuiltinId::StringsIndexByte);    info.type = int_type;  return info; }
        if (sel == "LastIndex")    { info.symbol = make_member_builtin(BuiltinId::StringsLastIndex);    info.type = int_type;  return info; }
        if (sel == "Map")          { info.symbol = make_member_builtin(BuiltinId::StringsMap);          info.type = str_type;  return info; }
        if (sel == "Title")        { info.symbol = make_member_builtin(BuiltinId::StringsTitle);        info.type = str_type;  return info; }
        if (sel == "EqualFold")    { info.symbol = make_member_builtin(BuiltinId::StringsEqualFold);    info.type = bool_type; return info; }
        if (sel == "ContainsAny")  { info.symbol = make_member_builtin(BuiltinId::StringsContainsAny);  info.type = bool_type; return info; }
        if (sel == "IndexRune")    { info.symbol = make_member_builtin(BuiltinId::StringsIndexRune);    info.type = int_type;  return info; }
        if (sel == "ReplaceAll")   { info.symbol = make_member_builtin(BuiltinId::StringsReplaceAll);   info.type = str_type;  return info; }
        if (sel == "TrimLeft")     { info.symbol = make_member_builtin(BuiltinId::StringsTrimLeft);     info.type = str_type;  return info; }
        if (sel == "TrimRight")    { info.symbol = make_member_builtin(BuiltinId::StringsTrimRight);    info.type = str_type;  return info; }
        if (sel == "NewReader") {
            info.symbol = make_member_builtin(BuiltinId::StringsNewReader);
            info.type = strings_reader_ptr_type();
            return info;
        }
    }

    if (pkg == "io") {
        if (sel == "ReadAll") {
            info.symbol = make_member_builtin(BuiltinId::IoReadAll);
            info.type = make_tuple_type({make_slice_type(basic_type(BasicKind::Uint8)), error_type()});
            return info;
        }
    }

    if (pkg == "filepath") {
        auto* str_type = basic_type(BasicKind::String);
        if (sel == "Join")  { info.symbol = make_member_builtin(BuiltinId::FilepathJoin);  info.type = str_type; return info; }
        if (sel == "Dir")   { info.symbol = make_member_builtin(BuiltinId::FilepathDir);   info.type = str_type; return info; }
        if (sel == "Base")  { info.symbol = make_member_builtin(BuiltinId::FilepathBase);  info.type = str_type; return info; }
        if (sel == "Ext")   { info.symbol = make_member_builtin(BuiltinId::FilepathExt);   info.type = str_type; return info; }
        if (sel == "Abs")   {
            info.symbol = make_member_builtin(BuiltinId::FilepathAbs);
            info.type = make_tuple_type({str_type, error_type()});
            return info;
        }
    }

    if (pkg == "math") {
        auto* f64_type = basic_type(BasicKind::Float64);
        auto* int_type = basic_type(BasicKind::Int64);
        // math constants — returned as float64/int constant values in IR
        if (sel == "Pi")         { info.symbol = make_member_builtin(BuiltinId::MathAbs); info.type = f64_type; return info; } // sentinel; IR handles by name
        if (sel == "E")          { info.symbol = make_member_builtin(BuiltinId::MathAbs); info.type = f64_type; return info; }
        if (sel == "Phi")        { info.symbol = make_member_builtin(BuiltinId::MathAbs); info.type = f64_type; return info; }
        if (sel == "Sqrt2")      { info.symbol = make_member_builtin(BuiltinId::MathAbs); info.type = f64_type; return info; }
        if (sel == "SqrtE")      { info.symbol = make_member_builtin(BuiltinId::MathAbs); info.type = f64_type; return info; }
        if (sel == "SqrtPi")     { info.symbol = make_member_builtin(BuiltinId::MathAbs); info.type = f64_type; return info; }
        if (sel == "SqrtPhi")    { info.symbol = make_member_builtin(BuiltinId::MathAbs); info.type = f64_type; return info; }
        if (sel == "Ln2")        { info.symbol = make_member_builtin(BuiltinId::MathAbs); info.type = f64_type; return info; }
        if (sel == "Log2E")      { info.symbol = make_member_builtin(BuiltinId::MathAbs); info.type = f64_type; return info; }
        if (sel == "Ln10")       { info.symbol = make_member_builtin(BuiltinId::MathAbs); info.type = f64_type; return info; }
        if (sel == "Log10E")     { info.symbol = make_member_builtin(BuiltinId::MathAbs); info.type = f64_type; return info; }
        if (sel == "MaxFloat32") { info.symbol = make_member_builtin(BuiltinId::MathAbs); info.type = f64_type; return info; }
        if (sel == "MaxFloat64") { info.symbol = make_member_builtin(BuiltinId::MathAbs); info.type = f64_type; return info; }
        if (sel == "SmallestNonzeroFloat32") { info.symbol = make_member_builtin(BuiltinId::MathAbs); info.type = f64_type; return info; }
        if (sel == "SmallestNonzeroFloat64") { info.symbol = make_member_builtin(BuiltinId::MathAbs); info.type = f64_type; return info; }
        if (sel == "MaxInt")   { info.symbol = make_member_builtin(BuiltinId::MathAbs); info.type = int_type; return info; }
        if (sel == "MinInt")   { info.symbol = make_member_builtin(BuiltinId::MathAbs); info.type = int_type; return info; }
        if (sel == "MaxInt8")  { info.symbol = make_member_builtin(BuiltinId::MathAbs); info.type = int_type; return info; }
        if (sel == "MaxInt16") { info.symbol = make_member_builtin(BuiltinId::MathAbs); info.type = int_type; return info; }
        if (sel == "MaxInt32") { info.symbol = make_member_builtin(BuiltinId::MathAbs); info.type = int_type; return info; }
        if (sel == "MaxInt64") { info.symbol = make_member_builtin(BuiltinId::MathAbs); info.type = int_type; return info; }
        if (sel == "MinInt8")  { info.symbol = make_member_builtin(BuiltinId::MathAbs); info.type = int_type; return info; }
        if (sel == "MinInt16") { info.symbol = make_member_builtin(BuiltinId::MathAbs); info.type = int_type; return info; }
        if (sel == "MinInt32") { info.symbol = make_member_builtin(BuiltinId::MathAbs); info.type = int_type; return info; }
        if (sel == "MinInt64") { info.symbol = make_member_builtin(BuiltinId::MathAbs); info.type = int_type; return info; }
        if (sel == "MaxUint8")  { info.symbol = make_member_builtin(BuiltinId::MathAbs); info.type = int_type; return info; }
        if (sel == "MaxUint16") { info.symbol = make_member_builtin(BuiltinId::MathAbs); info.type = int_type; return info; }
        if (sel == "MaxUint32") { info.symbol = make_member_builtin(BuiltinId::MathAbs); info.type = int_type; return info; }
        if (sel == "MaxUint64") { info.symbol = make_member_builtin(BuiltinId::MathAbs); info.type = int_type; return info; }
        // math functions
        if (sel == "Abs")   { info.symbol = make_member_builtin(BuiltinId::MathAbs);   info.type = f64_type; return info; }
        if (sel == "Sqrt")  { info.symbol = make_member_builtin(BuiltinId::MathSqrt);  info.type = f64_type; return info; }
        if (sel == "Floor") { info.symbol = make_member_builtin(BuiltinId::MathFloor); info.type = f64_type; return info; }
        if (sel == "Ceil")  { info.symbol = make_member_builtin(BuiltinId::MathCeil);  info.type = f64_type; return info; }
        if (sel == "Round") { info.symbol = make_member_builtin(BuiltinId::MathRound); info.type = f64_type; return info; }
        if (sel == "Max")   { info.symbol = make_member_builtin(BuiltinId::MathMax);   info.type = f64_type; return info; }
        if (sel == "Min")   { info.symbol = make_member_builtin(BuiltinId::MathMin);   info.type = f64_type; return info; }
        if (sel == "Pow")   { info.symbol = make_member_builtin(BuiltinId::MathPow);   info.type = f64_type; return info; }
        if (sel == "Log")   { info.symbol = make_member_builtin(BuiltinId::MathLog);   info.type = f64_type; return info; }
        if (sel == "Log2")  { info.symbol = make_member_builtin(BuiltinId::MathLog2);  info.type = f64_type; return info; }
        if (sel == "Log10") { info.symbol = make_member_builtin(BuiltinId::MathLog10); info.type = f64_type; return info; }
        if (sel == "Sin")   { info.symbol = make_member_builtin(BuiltinId::MathAbs);   info.type = f64_type; return info; }
        if (sel == "Cos")   { info.symbol = make_member_builtin(BuiltinId::MathAbs);   info.type = f64_type; return info; }
        if (sel == "Tan")   { info.symbol = make_member_builtin(BuiltinId::MathAbs);   info.type = f64_type; return info; }
        if (sel == "Inf")   { info.symbol = make_member_builtin(BuiltinId::MathAbs);   info.type = f64_type; return info; }
        if (sel == "IsInf") { info.symbol = make_member_builtin(BuiltinId::MathAbs);   info.type = basic_type(BasicKind::Bool); return info; }
        if (sel == "IsNaN") { info.symbol = make_member_builtin(BuiltinId::MathAbs);   info.type = basic_type(BasicKind::Bool); return info; }
        if (sel == "NaN")   { info.symbol = make_member_builtin(BuiltinId::MathAbs);   info.type = f64_type; return info; }
        if (sel == "Mod")   { info.symbol = make_member_builtin(BuiltinId::MathAbs);   info.type = f64_type; return info; }
        if (sel == "Trunc") { info.symbol = make_member_builtin(BuiltinId::MathAbs);   info.type = f64_type; return info; }
        if (sel == "Exp")   { info.symbol = make_member_builtin(BuiltinId::MathAbs);   info.type = f64_type; return info; }
        if (sel == "Exp2")  { info.symbol = make_member_builtin(BuiltinId::MathAbs);   info.type = f64_type; return info; }
        if (sel == "Hypot") { info.symbol = make_member_builtin(BuiltinId::MathAbs);   info.type = f64_type; return info; }
        if (sel == "Atan2") { info.symbol = make_member_builtin(BuiltinId::MathAbs);   info.type = f64_type; return info; }
        if (sel == "Atan")  { info.symbol = make_member_builtin(BuiltinId::MathAbs);   info.type = f64_type; return info; }
        if (sel == "Asin")  { info.symbol = make_member_builtin(BuiltinId::MathAbs);   info.type = f64_type; return info; }
        if (sel == "Acos")  { info.symbol = make_member_builtin(BuiltinId::MathAbs);   info.type = f64_type; return info; }
    }

    if (pkg == "sort") {
        if (sel == "Ints")    { info.symbol = make_member_builtin(BuiltinId::SortInts);    info.type = nullptr; return info; }
        if (sel == "Strings") { info.symbol = make_member_builtin(BuiltinId::SortStrings); info.type = nullptr; return info; }
        if (sel == "Slice")   { info.symbol = make_member_builtin(BuiltinId::SortSlice);   info.type = nullptr; return info; }
    }

    if (pkg == "bufio") {
        if (sel == "NewScanner") {
            info.symbol = make_member_builtin(BuiltinId::BufioNewScanner);
            info.type = bufio_scanner_ptr_type();
            return info;
        }
        if (sel == "NewReader") {
            info.symbol = make_member_builtin(BuiltinId::BufioNewReader);
            info.type = bufio_reader_ptr_type();
            return info;
        }
    }

    if (pkg == "time") {
        auto* i64_ty = basic_type(BasicKind::Int64);
        if (sel == "Sleep")       { info.symbol = make_member_builtin(BuiltinId::TimeSleep);      info.type = nullptr; return info; }
        if (sel == "Now")         { info.symbol = make_member_builtin(BuiltinId::TimeNow);        info.type = i64_ty; return info; }
        if (sel == "Since")       { info.symbol = make_member_builtin(BuiltinId::TimeSince);      info.type = i64_ty; return info; }
        if (sel == "Hour")        { info.symbol = make_member_builtin(BuiltinId::TimeDurationHour);   info.type = i64_ty; return info; }
        if (sel == "Minute")      { info.symbol = make_member_builtin(BuiltinId::TimeDurationMinute); info.type = i64_ty; return info; }
        if (sel == "Second")      { info.symbol = make_member_builtin(BuiltinId::TimeDurationSecond); info.type = i64_ty; return info; }
        if (sel == "Millisecond") { info.symbol = make_member_builtin(BuiltinId::TimeDurationMs);     info.type = i64_ty; return info; }
        if (sel == "Duration") {
            // time.Duration is a named type alias for int64 — return int64 type
            info.symbol = make_member_builtin(BuiltinId::TimeDurationSecond);
            info.type = i64_ty;
            return info;
        }
    }

    if (pkg == "unicode") {
        auto* bool_ty = basic_type(BasicKind::Bool);
        auto* int_ty  = basic_type(BasicKind::Int);
        if (sel == "IsLetter") { info.symbol = make_member_builtin(BuiltinId::UnicodeIsLetter); info.type = bool_ty; return info; }
        if (sel == "IsDigit")  { info.symbol = make_member_builtin(BuiltinId::UnicodeIsDigit);  info.type = bool_ty; return info; }
        if (sel == "IsSpace")  { info.symbol = make_member_builtin(BuiltinId::UnicodeIsSpace);  info.type = bool_ty; return info; }
        if (sel == "IsUpper")  { info.symbol = make_member_builtin(BuiltinId::UnicodeIsUpper);  info.type = bool_ty; return info; }
        if (sel == "IsLower")  { info.symbol = make_member_builtin(BuiltinId::UnicodeIsLower);  info.type = bool_ty; return info; }
        if (sel == "ToUpper")  { info.symbol = make_member_builtin(BuiltinId::UnicodeToUpper);  info.type = int_ty;  return info; }
        if (sel == "ToLower")  { info.symbol = make_member_builtin(BuiltinId::UnicodeToLower);  info.type = int_ty;  return info; }
    }

    if (pkg == "bytes") {
        if (sel == "NewBuffer") {
            info.symbol = make_member_builtin(BuiltinId::BytesNewBuffer);
            info.type = bytes_buffer_ptr_type();
            return info;
        }
        if (sel == "NewBufferString") {
            info.symbol = make_member_builtin(BuiltinId::BytesNewBufferString);
            info.type = bytes_buffer_ptr_type();
            return info;
        }
    }

    if (pkg == "rand") {
        auto* int_ty = basic_type(BasicKind::Int);
        auto* f64_ty = basic_type(BasicKind::Float64);
        auto* i64_ty = basic_type(BasicKind::Int64);
        if (sel == "Intn")      { info.symbol = make_member_builtin(BuiltinId::RandIntn);      info.type = int_ty;  return info; }
        if (sel == "Float64")   { info.symbol = make_member_builtin(BuiltinId::RandFloat64);   info.type = f64_ty;  return info; }
        if (sel == "Seed")      { info.symbol = make_member_builtin(BuiltinId::RandSeed);      info.type = nullptr; return info; }
        if (sel == "New")       { info.symbol = make_member_builtin(BuiltinId::RandNew);       info.type = i64_ty;  return info; }
        if (sel == "NewSource") { info.symbol = make_member_builtin(BuiltinId::RandNewSource); info.type = i64_ty;  return info; }
        if (sel == "Int63n")    { info.symbol = make_member_builtin(BuiltinId::RandIntn);      info.type = i64_ty;  return info; }
        if (sel == "Int31n")    { info.symbol = make_member_builtin(BuiltinId::RandIntn);      info.type = int_ty;  return info; }
    }

    diag_.error(expr.loc, "undefined: {}.{}", pkg, sel);
    return info;
}

// ============================================================================
// Selector expression (x.y)
// ============================================================================

ExprInfo Checker::check_selector(ast::SelectorExpr& expr) {
    ExprInfo info;

    // ---- Pseudo-package qualified access ----
    if (expr.x && expr.x->kind == ast::ExprKind::Ident) {
        auto* pkg_sym = lookup(expr.x->ident.name);
        if (pkg_sym && pkg_sym->kind == SymbolKind::PseudoPkg) {
            pkg_sym->used = true;
            // Record the package identifier itself so it is not flagged as undefined.
            ExprInfo pkg_info;
            pkg_info.symbol = pkg_sym;
            record_expr(expr.x, pkg_info);
            info = check_pseudo_pkg_selector(*pkg_sym, expr);
            return info;
        }
    }

    auto x_info = check_expr(expr.x);
    if (!x_info.type) return info;

    Type* base = x_info.type;
    std::string_view sel_name = expr.sel ? expr.sel->name : "";

    // Auto-deref pointers
    if (base->kind == TypeKind::Pointer) {
        base = base->pointer.base;
    }

    // Check struct fields
    const Type* u = underlying(base);
    if (u && u->kind == TypeKind::Struct && u->struct_) {
        for (const auto& field : u->struct_->fields) {
            if (field.name == sel_name) {
                info.type = field.type;
                info.is_lvalue = true;
                return info;
            }
        }
    }

    // Check interface methods
    const Type* ui = underlying(base);
    if (ui && ui->kind == TypeKind::Interface && ui->interface_) {
        for (const auto& m : ui->interface_->methods) {
            if (m.name == sel_name && m.signature) {
                info.type = make_func_type(m.signature);
                return info;
            }
        }
    }

    // Check methods on named type
    Type* method = lookup_method(x_info.type, sel_name);
    if (!method) {
        // Also try with auto-deref
        method = lookup_method(base, sel_name);
    }
    if (method) {
        // strings.Builder methods: expose as builtin symbols so check_call
        // accepts them permissively (no arg-count enforcement on runtime methods).
        if (base->kind == TypeKind::Named && base->named &&
            base->named->name == "strings.Builder") {
            auto* str_ret  = basic_type(BasicKind::String);
            auto* int_ret  = basic_type(BasicKind::Int);
            static const struct {
                std::string_view sel; BuiltinId id; int ret; // 0=void,1=str,2=int
            } builder_map[] = {
                {"WriteString", BuiltinId::StringsBuilderWriteString, 0},
                {"WriteByte",   BuiltinId::StringsBuilderWriteByte,   0},
                {"String",      BuiltinId::StringsBuilderString,      1},
                {"Reset",       BuiltinId::StringsBuilderReset,        0},
                {"Len",         BuiltinId::StringsBuilderLen,          2},
            };
            for (const auto& bm : builder_map) {
                if (sel_name == bm.sel) {
                    auto* sym = arena_.create<Symbol>();
                    sym->kind = SymbolKind::Builtin;
                    sym->builtin_id = static_cast<int>(bm.id);
                    sym->used = true;
                    // Stable name for IR dispatch: "strings.Builder.WriteString" etc.
                    std::string full = "strings.Builder." + std::string(sel_name);
                    auto* stored = arena_.allocate_array<char>(full.size() + 1);
                    std::memcpy(stored, full.data(), full.size() + 1);
                    sym->name = std::string_view(stored, full.size());
                    info.symbol = sym;
                    info.type = bm.ret == 1 ? str_ret
                               : bm.ret == 2 ? int_ret
                                             : nullptr;
                    info.needs_addr_for_recv = false;
                    return info;
                }
            }
        }

        // sync.Mutex methods: expose as builtin symbols for IR dispatch.
        if (base->kind == TypeKind::Named && base->named &&
            base->named->name == "sync.Mutex") {
            auto* bool_ret = basic_type(BasicKind::Bool);
            static const struct {
                std::string_view sel; BuiltinId id; bool returns_bool;
            } mutex_map[] = {
                {"Lock",    BuiltinId::SyncMutexLock,    false},
                {"Unlock",  BuiltinId::SyncMutexUnlock,  false},
                {"TryLock", BuiltinId::SyncMutexTryLock, true},
            };
            for (const auto& mm : mutex_map) {
                if (sel_name == mm.sel) {
                    auto* sym = arena_.create<Symbol>();
                    sym->kind = SymbolKind::Builtin;
                    sym->builtin_id = static_cast<int>(mm.id);
                    sym->used = true;
                    std::string full = "sync.Mutex." + std::string(sel_name);
                    auto* stored = arena_.allocate_array<char>(full.size() + 1);
                    std::memcpy(stored, full.data(), full.size() + 1);
                    sym->name = std::string_view(stored, full.size());
                    info.symbol = sym;
                    info.type = mm.returns_bool ? bool_ret : nullptr;
                    info.needs_addr_for_recv = false;
                    return info;
                }
            }
        }

        // os.File methods: expose as builtin symbols for IR dispatch.
        if (base->kind == TypeKind::Named && base->named &&
            base->named->name == "os.File") {
            auto* int_ret  = basic_type(BasicKind::Int);
            static const struct {
                std::string_view sel; BuiltinId id; bool returns_int;
            } file_map[] = {
                {"Close",       BuiltinId::OsFileClose,       false},
                {"WriteString", BuiltinId::OsFileWriteString, true},
            };
            for (const auto& fm : file_map) {
                if (sel_name == fm.sel) {
                    auto* sym = arena_.create<Symbol>();
                    sym->kind = SymbolKind::Builtin;
                    sym->builtin_id = static_cast<int>(fm.id);
                    sym->used = true;
                    std::string full = "os.File." + std::string(sel_name);
                    auto* stored = arena_.allocate_array<char>(full.size() + 1);
                    std::memcpy(stored, full.data(), full.size() + 1);
                    sym->name = std::string_view(stored, full.size());
                    info.symbol = sym;
                    if (fm.returns_int)
                        info.type = make_tuple_type({int_ret, error_type()});
                    else
                        info.type = error_type();
                    info.needs_addr_for_recv = false;
                    return info;
                }
            }
        }

        // bufio.Scanner methods: expose as builtin symbols for IR dispatch.
        if (base->kind == TypeKind::Named && base->named &&
            base->named->name == "bufio.Scanner") {
            auto* bool_ret = basic_type(BasicKind::Bool);
            auto* str_ret  = basic_type(BasicKind::String);
            static const struct {
                std::string_view sel; BuiltinId id; int ret; // 0=void,1=bool,2=str,3=err
            } scanner_map[] = {
                {"Scan", BuiltinId::BufioScannerScan, 1},
                {"Text", BuiltinId::BufioScannerText, 2},
                {"Err",  BuiltinId::BufioScannerErr,  3},
            };
            for (const auto& sm : scanner_map) {
                if (sel_name == sm.sel) {
                    auto* sym = arena_.create<Symbol>();
                    sym->kind = SymbolKind::Builtin;
                    sym->builtin_id = static_cast<int>(sm.id);
                    sym->used = true;
                    std::string full = "bufio.Scanner." + std::string(sel_name);
                    auto* stored = arena_.allocate_array<char>(full.size() + 1);
                    std::memcpy(stored, full.data(), full.size() + 1);
                    sym->name = std::string_view(stored, full.size());
                    info.symbol = sym;
                    info.type = sm.ret == 1 ? bool_ret
                              : sm.ret == 2 ? str_ret
                              : sm.ret == 3 ? error_type()
                                            : nullptr;
                    info.needs_addr_for_recv = false;
                    return info;
                }
            }
        }

        // bufio.Reader methods: expose as builtin symbols for IR dispatch.
        if (base->kind == TypeKind::Named && base->named &&
            base->named->name == "bufio.Reader") {
            if (sel_name == "ReadString") {
                auto* sym = arena_.create<Symbol>();
                sym->kind = SymbolKind::Builtin;
                sym->builtin_id = static_cast<int>(BuiltinId::BufioReaderReadString);
                sym->used = true;
                std::string full = "bufio.Reader.ReadString";
                auto* stored = arena_.allocate_array<char>(full.size() + 1);
                std::memcpy(stored, full.data(), full.size() + 1);
                sym->name = std::string_view(stored, full.size());
                info.symbol = sym;
                info.type = make_tuple_type({basic_type(BasicKind::String), error_type()});
                info.needs_addr_for_recv = false;
                return info;
            }
        }

        // bytes.Buffer methods: expose as builtin symbols for IR dispatch.
        if (base->kind == TypeKind::Named && base->named &&
            base->named->name == "bytes.Buffer") {
            auto* str_ret  = basic_type(BasicKind::String);
            auto* int_ret  = basic_type(BasicKind::Int);
            static const struct {
                std::string_view sel; BuiltinId id; int ret; // 0=void,1=str,2=int
            } buf_map[] = {
                {"WriteString", BuiltinId::BytesBufferWriteString, 0},
                {"WriteByte",   BuiltinId::BytesBufferWriteByte,   0},
                {"Write",       BuiltinId::BytesBufferWrite,       0},
                {"String",      BuiltinId::BytesBufferString,      1},
                {"Reset",       BuiltinId::BytesBufferReset,       0},
                {"Len",         BuiltinId::BytesBufferLen,         2},
            };
            for (const auto& bm : buf_map) {
                if (sel_name == bm.sel) {
                    auto* sym = arena_.create<Symbol>();
                    sym->kind = SymbolKind::Builtin;
                    sym->builtin_id = static_cast<int>(bm.id);
                    sym->used = true;
                    std::string full = "bytes.Buffer." + std::string(sel_name);
                    auto* stored = arena_.allocate_array<char>(full.size() + 1);
                    std::memcpy(stored, full.data(), full.size() + 1);
                    sym->name = std::string_view(stored, full.size());
                    info.symbol = sym;
                    info.type = bm.ret == 1 ? str_ret
                              : bm.ret == 2 ? int_ret
                                            : nullptr;
                    info.needs_addr_for_recv = false;
                    return info;
                }
            }
        }

        // sync.WaitGroup methods: expose as builtin symbols for IR dispatch.
        if (base->kind == TypeKind::Named && base->named &&
            base->named->name == "sync.WaitGroup") {
            static const struct {
                std::string_view sel; BuiltinId id;
            } wg_map[] = {
                {"Add",  BuiltinId::SyncWaitGroupAdd},
                {"Done", BuiltinId::SyncWaitGroupDone},
                {"Wait", BuiltinId::SyncWaitGroupWait},
            };
            for (const auto& wm : wg_map) {
                if (sel_name == wm.sel) {
                    auto* sym = arena_.create<Symbol>();
                    sym->kind = SymbolKind::Builtin;
                    sym->builtin_id = static_cast<int>(wm.id);
                    sym->used = true;
                    std::string full = "sync.WaitGroup." + std::string(sel_name);
                    auto* stored = arena_.allocate_array<char>(full.size() + 1);
                    std::memcpy(stored, full.data(), full.size() + 1);
                    sym->name = std::string_view(stored, full.size());
                    info.symbol = sym;
                    info.type = nullptr; // all void
                    info.needs_addr_for_recv = false;
                    return info;
                }
            }
        }

        info.type = method;
        // Check if this is a pointer-receiver method called on a value type.
        // In that case, IR gen must take &recv before the call.
        Type* named_base = base;
        if (named_base->kind == TypeKind::Named && named_base->named) {
            for (const auto& m : named_base->named->methods) {
                if (m.name == sel_name && m.pointer_receiver &&
                    x_info.type->kind != TypeKind::Pointer) {
                    info.needs_addr_for_recv = true;
                    break;
                }
            }
        }
        return info;
    }

    diag_.error(expr.loc, "{} has no field or method '{}'",
               type_string(x_info.type), sel_name);
    return info;
}

// ============================================================================
// Index expression (x[i])
// ============================================================================

ExprInfo Checker::check_index(ast::IndexExpr& expr) {
    ExprInfo info;

    auto x_info = check_expr(expr.x);
    auto idx_info = check_expr(expr.index);

    if (!x_info.type) return info;

    const Type* u = underlying(x_info.type);
    if (!u) return info;

    switch (u->kind) {
        case TypeKind::Array:
            info.type = u->array.element;
            info.is_lvalue = true;
            break;
        case TypeKind::Slice:
            info.type = u->slice.element;
            info.is_lvalue = true;
            break;
        case TypeKind::Map:
            info.type = u->map.value;
            // map index requires integer/string key
            if (idx_info.type && !assignable_to(idx_info.type, u->map.key)) {
                diag_.error(expr.loc, "cannot use {} as {} in map index",
                           type_string(idx_info.type), type_string(u->map.key));
            }
            break;
        case TypeKind::Basic:
            if (u->basic == BasicKind::String ||
                u->basic == BasicKind::UntypedString) {
                info.type = basic_type(BasicKind::Uint8); // byte
            } else {
                diag_.error(expr.loc, "cannot index {}", type_string(x_info.type));
            }
            break;
        default:
            diag_.error(expr.loc, "cannot index {}", type_string(x_info.type));
            break;
    }

    return info;
}

// ============================================================================
// Call expression
// ============================================================================

ExprInfo Checker::check_call(ast::CallExpr& expr) {
    ExprInfo info;

    // Check if it's a builtin call
    if (expr.func && expr.func->kind == ast::ExprKind::Ident) {
        auto* sym = lookup(expr.func->ident.name);
        if (sym && sym->kind == SymbolKind::Builtin) {
            sym->used = true;
            return check_builtin_call(expr, sym);
        }
        // Check for type conversion: Type(expr)
        if (sym && sym->kind == SymbolKind::Type) {
            sym->used = true;
            // Type conversion
            if (expr.args.count != 1) {
                diag_.error(expr.loc, "type conversion requires exactly one argument");
                return info;
            }
            auto arg_info = check_expr(expr.args[0]);
            (void)arg_info;
            info.type = sym->type;
            return info;
        }
    }

    // Check for type conversion where the "function" is a type expression:
    // e.g. []byte(s), []string(x) — represented as CompositeLit type carrier.
    // The type carrier has a TypeExpr but no lbrace (filename is empty) and no elements.
    if (expr.func && expr.func->kind == ast::ExprKind::CompositeLit &&
        expr.func->composite_lit.type != nullptr &&
        expr.func->composite_lit.elts.count == 0 &&
        expr.func->composite_lit.lbrace.filename.empty()) {
        // This is a type conversion []T(x) or [N]T(x)
        Type* conv_type = resolve_type(expr.func->composite_lit.type);
        if (conv_type) {
            if (expr.args.count == 1) {
                auto arg_info = check_expr(expr.args[0]);
                (void)arg_info;
            }
            info.type = conv_type;
            return info;
        }
    }

    auto func_info = check_expr(expr.func);

    // Check if this is a pseudo-package builtin call (e.g. fmt.Println, strconv.Itoa).
    // The check_selector path records a Builtin symbol on the ExprInfo for these.
    if (func_info.symbol && func_info.symbol->kind == SymbolKind::Builtin) {
        // Type-check args permissively — pseudo-package functions accept any types.
        for (auto* arg : expr.args.span()) {
            (void)check_expr(arg);
        }
        info.type = func_info.type; // Return type already set by check_pseudo_pkg_selector
        info.symbol = func_info.symbol;
        return info;
    }

    if (!func_info.type) return info;

    Type* func_type = func_info.type;
    // If it's a named type, follow to underlying
    const Type* u = underlying(func_type);
    if (!u || u->kind != TypeKind::Func || !u->func) {
        diag_.error(expr.loc, "cannot call non-function {}", type_string(func_type));
        return info;
    }

    const auto* ft = u->func;

    // Check argument count
    size_t expected = ft->params.size();
    size_t got = expr.args.count;

    if (ft->is_variadic) {
        if (got < expected - 1) {
            diag_.error(expr.loc, "too few arguments in call to function (expected at least {}, got {})",
                       expected - 1, got);
            return info;
        }
    } else {
        if (got != expected) {
            diag_.error(expr.loc, "wrong number of arguments (expected {}, got {})",
                       expected, got);
            return info;
        }
    }

    // Check argument types
    for (uint32_t i = 0; i < expr.args.count; ++i) {
        auto arg_info = check_expr(expr.args[i]);
        if (i < ft->params.size()) {
            Type* param_type = ft->params[i].type;
            // For variadic params, unwrap []T → T unless this is a spread call (f(s...))
            bool is_spread_arg = expr.has_ellipsis && (i == expr.args.count - 1);
            if (!is_spread_arg && ft->params[i].is_variadic && param_type &&
                param_type->kind == TypeKind::Slice) {
                param_type = param_type->slice.element;
            }
            if (arg_info.type && param_type && !assignable_to(arg_info.type, param_type)) {
                diag_.error(expr.args[i]->ident.loc,
                    "cannot use {} as {} in argument",
                    type_string(arg_info.type), type_string(param_type));
            }
        }
    }

    // Result type
    if (ft->results.empty()) {
        info.type = nullptr; // void
    } else if (ft->results.size() == 1) {
        info.type = ft->results[0].type;
    } else {
        std::vector<Type*> result_types;
        for (const auto& r : ft->results) {
            result_types.push_back(r.type);
        }
        info.type = make_tuple_type(result_types);
    }

    return info;
}

// ============================================================================
// Built-in function calls
// ============================================================================

ExprInfo Checker::check_builtin_call(ast::CallExpr& expr, Symbol* sym) {
    ExprInfo info;
    auto id = static_cast<BuiltinId>(sym->builtin_id);

    // Check arguments
    std::vector<ExprInfo> arg_infos;
    for (auto* arg : expr.args.span()) {
        arg_infos.push_back(check_expr(arg));
    }

    switch (id) {
        case BuiltinId::Println:
        case BuiltinId::Print:
            // Accept any number of any-typed arguments
            info.type = nullptr;
            break;

        case BuiltinId::Len: {
            if (arg_infos.size() != 1) {
                diag_.error(expr.loc, "len requires exactly one argument");
                break;
            }
            auto* arg_type = arg_infos[0].type;
            if (arg_type) {
                const Type* u = underlying(arg_type);
                if (u && (u->kind == TypeKind::Array || u->kind == TypeKind::Slice ||
                          u->kind == TypeKind::Map || u->kind == TypeKind::Chan ||
                          (u->kind == TypeKind::Basic &&
                           (u->basic == BasicKind::String ||
                            u->basic == BasicKind::UntypedString)))) {
                    info.type = basic_type(BasicKind::Int);
                } else {
                    diag_.error(expr.loc, "invalid argument type {} for len",
                               type_string(arg_type));
                }
            }
            break;
        }

        case BuiltinId::Cap: {
            if (arg_infos.size() != 1) {
                diag_.error(expr.loc, "cap requires exactly one argument");
                break;
            }
            auto* arg_type = arg_infos[0].type;
            if (arg_type) {
                const Type* u = underlying(arg_type);
                if (u && (u->kind == TypeKind::Array || u->kind == TypeKind::Slice ||
                          u->kind == TypeKind::Chan)) {
                    info.type = basic_type(BasicKind::Int);
                } else {
                    diag_.error(expr.loc, "invalid argument type {} for cap",
                               type_string(arg_type));
                }
            }
            break;
        }

        case BuiltinId::Make: {
            // make(Type, ...sizes)
            // First arg should be a type — but it's parsed as an expression
            // We need to resolve it as a type
            if (expr.args.count < 1) {
                diag_.error(expr.loc, "missing argument to make");
                break;
            }

            Type* make_type = nullptr;
            auto* first_arg = expr.args[0];

            // Case 1: Identifier referring to a type (e.g., make(MySlice))
            if (first_arg && first_arg->kind == ast::ExprKind::Ident) {
                auto* type_sym = lookup(first_arg->ident.name);
                if (type_sym && type_sym->kind == SymbolKind::Type) {
                    make_type = type_sym->type;
                    type_sym->used = true;
                }
            }

            // Case 2: CompositeLit wrapper for type keywords (chan, map, etc.)
            // Parser wraps these as CompositeLit with no elements
            if (!make_type && first_arg &&
                first_arg->kind == ast::ExprKind::CompositeLit &&
                first_arg->composite_lit.type) {
                make_type = resolve_type(first_arg->composite_lit.type);
            }

            // Case 3: Index expression for slice type (e.g., make([]int, 10))
            if (!make_type && first_arg && arg_infos[0].type) {
                make_type = arg_infos[0].type;
            }

            if (!make_type) {
                diag_.error(expr.loc, "first argument to make must be a type");
                break;
            }

            const Type* u = underlying(make_type);
            if (u && (u->kind == TypeKind::Slice || u->kind == TypeKind::Map ||
                      u->kind == TypeKind::Chan)) {
                info.type = make_type;
            } else {
                diag_.error(expr.loc, "cannot make {}", type_string(make_type));
            }
            break;
        }

        case BuiltinId::New: {
            if (expr.args.count != 1) {
                diag_.error(expr.loc, "new requires exactly one argument");
                break;
            }
            // Similar to make, first arg is a type
            Type* new_type = nullptr;
            auto* first_arg = expr.args[0];
            if (first_arg && first_arg->kind == ast::ExprKind::Ident) {
                auto* type_sym = lookup(first_arg->ident.name);
                if (type_sym && type_sym->kind == SymbolKind::Type) {
                    new_type = type_sym->type;
                    type_sym->used = true;
                }
            }
            if (new_type) {
                info.type = make_pointer_type(new_type);
            } else {
                diag_.error(expr.loc, "first argument to new must be a type");
            }
            break;
        }

        case BuiltinId::Append: {
            if (arg_infos.size() < 1) {
                diag_.error(expr.loc, "missing arguments to append");
                break;
            }
            info.type = arg_infos[0].type;
            break;
        }

        case BuiltinId::Copy: {
            if (arg_infos.size() != 2) {
                diag_.error(expr.loc, "copy requires two arguments");
                break;
            }
            info.type = basic_type(BasicKind::Int);
            break;
        }

        case BuiltinId::Delete:
        case BuiltinId::Close:
            info.type = nullptr;
            break;

        case BuiltinId::Panic:
            info.type = nullptr;
            break;

        case BuiltinId::Recover:
            // recover() returns interface{}
            info.type = make_interface_type(arena_.create<InterfaceType>());
            break;

        default:
            break;
    }

    return info;
}

// ============================================================================
// Unary expression
// ============================================================================

ExprInfo Checker::check_unary(ast::UnaryExpr& expr) {
    ExprInfo info;
    auto x_info = check_expr(expr.x);
    if (!x_info.type) return info;

    switch (expr.op) {
        case TokenKind::Plus:
        case TokenKind::Minus:
            if (!is_numeric(x_info.type)) {
                diag_.error(expr.loc, "invalid operation: operator {} not defined on {}",
                           token_kind_to_string(expr.op), type_string(x_info.type));
                return info;
            }
            info.type = x_info.type;
            info.const_val = x_info.const_val;
            if (expr.op == TokenKind::Minus && info.const_val) {
                auto neg = const_neg(*info.const_val);
                if (neg.is_valid()) info.const_val = arena_.create<ConstValue>(neg);
            }
            break;

        case TokenKind::Not:
            if (!is_boolean(x_info.type)) {
                diag_.error(expr.loc, "invalid operation: ! on {}", type_string(x_info.type));
                return info;
            }
            info.type = x_info.type;
            if (x_info.const_val) {
                auto r = const_not(*x_info.const_val);
                if (r.is_valid()) info.const_val = arena_.create<ConstValue>(r);
            }
            break;

        case TokenKind::Caret: // ^x (bitwise complement)
            if (!is_integer(x_info.type)) {
                diag_.error(expr.loc, "invalid operation: ^ on {}", type_string(x_info.type));
                return info;
            }
            info.type = x_info.type;
            break;

        case TokenKind::Ampersand: // &x (address-of)
            info.type = make_pointer_type(x_info.type);
            break;

        case TokenKind::Arrow: // <-ch (channel receive)
        {
            const Type* u = underlying(x_info.type);
            if (!u || u->kind != TypeKind::Chan) {
                diag_.error(expr.loc, "invalid operation: cannot receive from {}",
                           type_string(x_info.type));
                return info;
            }
            if (u->chan.dir == ChanDir::SendOnly) {
                diag_.error(expr.loc, "invalid operation: cannot receive from send-only channel");
                return info;
            }
            info.type = u->chan.element;
            break;
        }

        default:
            diag_.error(expr.loc, "invalid unary operator {}", token_kind_to_string(expr.op));
            break;
    }

    return info;
}

// ============================================================================
// Binary expression
// ============================================================================

ExprInfo Checker::check_binary(ast::BinaryExpr& expr) {
    ExprInfo info;
    auto left_info = check_expr(expr.left);
    auto right_info = check_expr(expr.right);

    if (!left_info.type || !right_info.type) return info;

    Type* left_type = left_info.type;
    Type* right_type = right_info.type;

    switch (expr.op) {
        // Arithmetic operators
        case TokenKind::Plus:
            if (is_string(left_type) && is_string(right_type)) {
                info.type = promote_untyped(left_type, right_type);
                if (!info.type) info.type = basic_type(BasicKind::String);
                // Constant folding for string concatenation
                if (left_info.const_val && right_info.const_val) {
                    auto r = const_add(*left_info.const_val, *right_info.const_val);
                    if (r.is_valid()) info.const_val = arena_.create<ConstValue>(r);
                }
                return info;
            }
            [[fallthrough]];
        case TokenKind::Minus:
        case TokenKind::Star:
        case TokenKind::Slash:
        case TokenKind::Percent: {
            if (!is_numeric(left_type) || !is_numeric(right_type)) {
                diag_.error(expr.op_loc, "invalid operation: {} {} {}",
                           type_string(left_type), token_kind_to_string(expr.op),
                           type_string(right_type));
                return info;
            }
            info.type = promote_untyped(left_type, right_type);
            if (!info.type) {
                // Types are different typed types
                diag_.error(expr.op_loc, "invalid operation: mismatched types {} and {}",
                           type_string(left_type), type_string(right_type));
                return info;
            }

            // Constant folding
            if (left_info.const_val && right_info.const_val) {
                ConstValue r;
                switch (expr.op) {
                    case TokenKind::Plus:    r = const_add(*left_info.const_val, *right_info.const_val); break;
                    case TokenKind::Minus:   r = const_sub(*left_info.const_val, *right_info.const_val); break;
                    case TokenKind::Star:    r = const_mul(*left_info.const_val, *right_info.const_val); break;
                    case TokenKind::Slash:   r = const_div(*left_info.const_val, *right_info.const_val); break;
                    case TokenKind::Percent: r = const_mod(*left_info.const_val, *right_info.const_val); break;
                    default: break;
                }
                if (r.is_valid()) info.const_val = arena_.create<ConstValue>(r);
            }
            break;
        }

        // Bit operators
        case TokenKind::Ampersand:
        case TokenKind::Pipe:
        case TokenKind::Caret:
        case TokenKind::AmpCaret:
        case TokenKind::ShiftLeft:
        case TokenKind::ShiftRight: {
            if (!is_integer(left_type)) {
                diag_.error(expr.op_loc, "invalid operation: {} on non-integer type {}",
                           token_kind_to_string(expr.op), type_string(left_type));
                return info;
            }
            info.type = promote_untyped(left_type, right_type);
            if (!info.type) info.type = left_type;
            break;
        }

        // Comparison operators
        case TokenKind::Equal:
        case TokenKind::NotEqual: {
            // Both types must be comparable
            info.type = basic_type(BasicKind::UntypedBool);

            if (left_info.const_val && right_info.const_val) {
                ConstValue r;
                if (expr.op == TokenKind::Equal)
                    r = const_eq(*left_info.const_val, *right_info.const_val);
                else
                    r = const_neq(*left_info.const_val, *right_info.const_val);
                if (r.is_valid()) info.const_val = arena_.create<ConstValue>(r);
            }
            break;
        }

        case TokenKind::Less:
        case TokenKind::LessEqual:
        case TokenKind::Greater:
        case TokenKind::GreaterEqual: {
            if (!is_ordered(left_type) && !is_ordered(right_type)) {
                diag_.error(expr.op_loc, "invalid operation: cannot compare {} and {}",
                           type_string(left_type), type_string(right_type));
                return info;
            }
            info.type = basic_type(BasicKind::UntypedBool);

            if (left_info.const_val && right_info.const_val) {
                ConstValue r;
                switch (expr.op) {
                    case TokenKind::Less:         r = const_lt(*left_info.const_val, *right_info.const_val); break;
                    case TokenKind::LessEqual:    r = const_le(*left_info.const_val, *right_info.const_val); break;
                    case TokenKind::Greater:      r = const_gt(*left_info.const_val, *right_info.const_val); break;
                    case TokenKind::GreaterEqual: r = const_ge(*left_info.const_val, *right_info.const_val); break;
                    default: break;
                }
                if (r.is_valid()) info.const_val = arena_.create<ConstValue>(r);
            }
            break;
        }

        // Logical operators
        case TokenKind::LogicalAnd:
        case TokenKind::LogicalOr: {
            if (!is_boolean(left_type) || !is_boolean(right_type)) {
                diag_.error(expr.op_loc, "invalid operation: {} {} {}",
                           type_string(left_type), token_kind_to_string(expr.op),
                           type_string(right_type));
                return info;
            }
            info.type = promote_untyped(left_type, right_type);
            if (!info.type) info.type = basic_type(BasicKind::UntypedBool);
            break;
        }

        default:
            diag_.error(expr.op_loc, "invalid binary operator {}",
                       token_kind_to_string(expr.op));
            break;
    }

    return info;
}

// ============================================================================
// Star expression (*x — dereference)
// ============================================================================

ExprInfo Checker::check_star(ast::StarExpr& expr) {
    ExprInfo info;
    auto x_info = check_expr(expr.x);
    if (!x_info.type) return info;

    const Type* u = underlying(x_info.type);
    if (!u || u->kind != TypeKind::Pointer) {
        diag_.error(expr.loc, "invalid operation: cannot dereference {}",
                   type_string(x_info.type));
        return info;
    }

    info.type = u->pointer.base;
    info.is_lvalue = true;
    return info;
}

// ============================================================================
// Type assert expression (x.(T))
// ============================================================================

ExprInfo Checker::check_type_assert(ast::TypeAssertExpr& expr) {
    ExprInfo info;
    auto x_info = check_expr(expr.x);

    if (expr.type) {
        info.type = resolve_type(expr.type);
    }

    // The expression must be of interface type
    if (x_info.type) {
        const Type* u = underlying(x_info.type);
        if (!u || u->kind != TypeKind::Interface) {
            diag_.error(expr.loc, "invalid type assertion: {} is not an interface",
                       type_string(x_info.type));
        }
    }

    return info;
}

// ============================================================================
// Slice expression (x[lo:hi] or x[lo:hi:max])
// ============================================================================

ExprInfo Checker::check_slice_expr(ast::SliceExpr& expr) {
    ExprInfo info;
    auto x_info = check_expr(expr.x);
    if (!x_info.type) return info;

    if (expr.low) (void)check_expr(expr.low);
    if (expr.high) (void)check_expr(expr.high);
    if (expr.max) (void)check_expr(expr.max);

    const Type* u = underlying(x_info.type);
    if (!u) return info;

    switch (u->kind) {
        case TypeKind::Array:
            info.type = make_slice_type(u->array.element);
            break;
        case TypeKind::Slice:
            info.type = x_info.type;
            break;
        case TypeKind::Basic:
            if (u->basic == BasicKind::String || u->basic == BasicKind::UntypedString) {
                info.type = basic_type(BasicKind::String);
            } else {
                diag_.error(expr.loc, "cannot slice {}", type_string(x_info.type));
            }
            break;
        default:
            diag_.error(expr.loc, "cannot slice {}", type_string(x_info.type));
            break;
    }

    return info;
}

} // namespace sema
} // namespace golangc
