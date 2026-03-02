#include "sema/checker.hpp"
#include "sema/universe.hpp"

#include <fmt/format.h>

namespace golangc {
namespace sema {

Type* Checker::resolve_type(ast::TypeExpr* texpr) {
    if (!texpr) return nullptr;

    switch (texpr->kind) {
        case ast::TypeExprKind::Bad:
            return nullptr;

        case ast::TypeExprKind::Ident: {
            auto& ident = texpr->ident;
            auto* sym = lookup(ident.name);
            if (!sym) {
                diag_.error(ident.loc, "undefined: {}", ident.name);
                return nullptr;
            }
            if (sym->kind != SymbolKind::Type) {
                diag_.error(ident.loc, "{} is not a type", ident.name);
                return nullptr;
            }
            sym->used = true;
            return sym->type;
        }

        case ast::TypeExprKind::Qualified: {
            auto& qual = texpr->qualified;
            // strings.Builder → opaque pointer type registered in universe
            if (qual.package == "strings" && qual.name == "Builder") {
                return strings_builder_ptr_type();
            }
            // sync.Mutex → opaque pointer type registered in universe
            if (qual.package == "sync" && qual.name == "Mutex") {
                return sync_mutex_ptr_type();
            }
            // sync.WaitGroup → opaque pointer type registered in universe
            if (qual.package == "sync" && qual.name == "WaitGroup") {
                return sync_waitgroup_ptr_type();
            }
            // os.File → opaque pointer type registered in universe
            if (qual.package == "os" && qual.name == "File") {
                return os_file_ptr_type();
            }
            // bytes.Buffer → opaque pointer type registered in universe
            if (qual.package == "bytes" && qual.name == "Buffer") {
                return bytes_buffer_ptr_type();
            }
            // strings.Reader → opaque pointer type registered in universe
            if (qual.package == "strings" && qual.name == "Reader") {
                return strings_reader_ptr_type();
            }
            diag_.error(qual.loc, "qualified types ({}.{}) not yet supported",
                       qual.package, qual.name);
            return nullptr;
        }

        case ast::TypeExprKind::Array: {
            auto& arr = texpr->array;
            Type* elem = resolve_type(arr.element);
            if (!elem) return nullptr;

            // Evaluate length expression
            auto* len_val = eval_const_expr(arr.length);
            if (!len_val || !len_val->is_int() || len_val->as_int() < 0) {
                diag_.error(arr.loc, "array length must be a non-negative integer constant");
                return nullptr;
            }
            return make_array_type(elem, len_val->as_int());
        }

        case ast::TypeExprKind::Slice: {
            auto& sl = texpr->slice;
            Type* elem = resolve_type(sl.element);
            if (!elem) return nullptr;
            return make_slice_type(elem);
        }

        case ast::TypeExprKind::Map: {
            auto& m = texpr->map;
            Type* key = resolve_type(m.key);
            Type* value = resolve_type(m.value);
            if (!key || !value) return nullptr;
            return make_map_type(key, value);
        }

        case ast::TypeExprKind::Chan: {
            auto& ch = texpr->chan;
            Type* elem = resolve_type(ch.element);
            if (!elem) return nullptr;
            ChanDir dir = ChanDir::SendRecv;
            switch (ch.dir) {
                case ast::ChanDir::SendRecv: dir = ChanDir::SendRecv; break;
                case ast::ChanDir::SendOnly: dir = ChanDir::SendOnly; break;
                case ast::ChanDir::RecvOnly: dir = ChanDir::RecvOnly; break;
            }
            return make_chan_type(elem, dir);
        }

        case ast::TypeExprKind::Pointer: {
            auto& ptr = texpr->pointer;
            Type* base = resolve_type(ptr.base);
            if (!base) return nullptr;
            return make_pointer_type(base);
        }

        case ast::TypeExprKind::Func: {
            auto& func = texpr->func;
            return resolve_func_type(&func);
        }

        case ast::TypeExprKind::Struct: {
            auto& s = texpr->struct_;
            return resolve_struct_type(&s);
        }

        case ast::TypeExprKind::Interface: {
            auto& iface = texpr->interface_;
            return resolve_interface_type(&iface);
        }

        case ast::TypeExprKind::Paren:
            return resolve_type(texpr->paren.type);

        case ast::TypeExprKind::Ellipsis: {
            // ...T for variadic — resolve element type, result is a slice
            Type* elem = resolve_type(texpr->ellipsis.element);
            if (!elem) return nullptr;
            return make_slice_type(elem);
        }
    }

    return nullptr;
}

Type* Checker::resolve_func_type(ast::FuncTypeExpr* ftype) {
    if (!ftype) return nullptr;

    auto* ft = arena_.create<FuncType>();

    // Parameters
    if (ftype->params && !ftype->params->fields.empty()) {
        for (auto* field : ftype->params->fields.span()) {
            if (!field) continue;
            Type* param_type = resolve_type(field->type);
            if (!param_type) param_type = basic_type(BasicKind::Invalid);

            bool is_variadic = false;
            if (field->type && field->type->kind == ast::TypeExprKind::Ellipsis) {
                is_variadic = true;
                ft->is_variadic = true;
            }

            if (field->names.empty()) {
                ft->params.push_back(FuncParam{"", param_type, is_variadic});
            } else {
                for (auto* name : field->names.span()) {
                    ft->params.push_back(
                        FuncParam{name ? name->name : "", param_type, is_variadic});
                }
            }
        }
    }

    // Results
    if (ftype->results && !ftype->results->fields.empty()) {
        for (auto* field : ftype->results->fields.span()) {
            if (!field) continue;
            Type* result_type = resolve_type(field->type);
            if (!result_type) result_type = basic_type(BasicKind::Invalid);

            if (field->names.empty()) {
                ft->results.push_back(FuncParam{"", result_type});
            } else {
                for (auto* name : field->names.span()) {
                    ft->results.push_back(
                        FuncParam{name ? name->name : "", result_type});
                }
            }
        }
    }

    return make_func_type(ft);
}

Type* Checker::resolve_struct_type(ast::StructTypeExpr* stype) {
    if (!stype) return nullptr;

    auto* st = arena_.create<StructType>();

    if (stype->fields && !stype->fields->fields.empty()) {
        for (auto* field : stype->fields->fields.span()) {
            if (!field) continue;
            Type* field_type = resolve_type(field->type);
            if (!field_type) field_type = basic_type(BasicKind::Invalid);

            if (field->names.empty()) {
                // Anonymous/embedded field
                std::string_view name;
                // Derive name from type
                if (field->type && field->type->kind == ast::TypeExprKind::Ident) {
                    name = field->type->ident.name;
                }
                st->fields.push_back(
                    StructField{name, field_type, "", true});
            } else {
                for (auto* name_node : field->names.span()) {
                    std::string_view tag;
                    if (field->tag && field->tag->kind == ast::ExprKind::BasicLit) {
                        tag = field->tag->basic_lit.value;
                    }
                    st->fields.push_back(StructField{
                        name_node ? name_node->name : "", field_type, tag, false});
                }
            }
        }
    }

    // Check for duplicate fields
    for (size_t i = 0; i < st->fields.size(); ++i) {
        for (size_t j = i + 1; j < st->fields.size(); ++j) {
            if (!st->fields[i].name.empty() &&
                st->fields[i].name == st->fields[j].name) {
                diag_.error(stype->loc, "duplicate field '{}'", st->fields[i].name);
            }
        }
    }

    return make_struct_type(st);
}

Type* Checker::resolve_interface_type(ast::InterfaceTypeExpr* itype) {
    if (!itype) return nullptr;

    auto* it = arena_.create<InterfaceType>();

    for (auto* method : itype->methods.span()) {
        if (!method) continue;

        if (method->name && method->signature) {
            // Regular method
            auto func_texpr = method->signature;
            auto* ft = arena_.create<FuncType>();

            if (func_texpr->params && !func_texpr->params->fields.empty()) {
                for (auto* field : func_texpr->params->fields.span()) {
                    if (!field) continue;
                    Type* pt = resolve_type(field->type);
                    if (!pt) pt = basic_type(BasicKind::Invalid);
                    if (field->names.empty()) {
                        ft->params.push_back(FuncParam{"", pt});
                    } else {
                        for (auto* n : field->names.span()) {
                            ft->params.push_back(FuncParam{n ? n->name : "", pt});
                        }
                    }
                }
            }

            if (func_texpr->results && !func_texpr->results->fields.empty()) {
                for (auto* field : func_texpr->results->fields.span()) {
                    if (!field) continue;
                    Type* rt = resolve_type(field->type);
                    if (!rt) rt = basic_type(BasicKind::Invalid);
                    if (field->names.empty()) {
                        ft->results.push_back(FuncParam{"", rt});
                    } else {
                        for (auto* n : field->names.span()) {
                            ft->results.push_back(FuncParam{n ? n->name : "", rt});
                        }
                    }
                }
            }

            it->methods.push_back(InterfaceMethod{method->name->name, ft});
        } else if (method->embedded_type) {
            // Embedded interface
            Type* embedded = resolve_type(method->embedded_type);
            if (embedded) {
                it->embedded.push_back(embedded);
            }
        }
    }

    return make_interface_type(it);
}

} // namespace sema
} // namespace golangc
