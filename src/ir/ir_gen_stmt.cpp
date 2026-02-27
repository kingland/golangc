#include "ir/ir_gen.hpp"

#include <cassert>

namespace golangc {
namespace ir {

void IRGenerator::gen_stmt(ast::Stmt* stmt) {
    if (!stmt) return;

    // If current block already has a terminator, skip (dead code)
    if (builder_.insert_block() && builder_.insert_block()->has_terminator()) return;

    switch (stmt->kind) {
        case ast::StmtKind::Block:
            gen_block(stmt->block);
            break;
        case ast::StmtKind::Expr:
            gen_expr_stmt(stmt->expr);
            break;
        case ast::StmtKind::Assign:
            gen_assign(stmt->assign);
            break;
        case ast::StmtKind::ShortVarDecl:
            gen_short_var_decl(stmt->short_var_decl);
            break;
        case ast::StmtKind::Return:
            gen_return(stmt->return_);
            break;
        case ast::StmtKind::If:
            gen_if(stmt->if_);
            break;
        case ast::StmtKind::For:
            gen_for(stmt->for_);
            break;
        case ast::StmtKind::Range:
            gen_range(stmt->range);
            break;
        case ast::StmtKind::Switch:
            gen_switch(stmt->switch_);
            break;
        case ast::StmtKind::TypeSwitch:
            gen_type_switch(stmt->type_switch);
            break;
        case ast::StmtKind::Select:
            gen_select(stmt->select);
            break;
        case ast::StmtKind::IncDec:
            gen_inc_dec(stmt->inc_dec);
            break;
        case ast::StmtKind::Send:
            gen_send(stmt->send);
            break;
        case ast::StmtKind::Go:
            gen_go(stmt->go);
            break;
        case ast::StmtKind::Defer:
            gen_defer(stmt->defer);
            break;
        case ast::StmtKind::Branch:
            gen_branch(stmt->branch);
            break;
        case ast::StmtKind::Decl:
            gen_decl_stmt(stmt->decl);
            break;
        case ast::StmtKind::Empty:
            // No-op
            break;
        default:
            // Label, TypeSwitch, Select, CaseClause, CommClause — not yet
            break;
    }
}

void IRGenerator::gen_block(ast::BlockStmt& block) {
    for (auto* s : block.stmts) {
        gen_stmt(s);
        // If block is terminated, stop generating
        if (builder_.insert_block() && builder_.insert_block()->has_terminator()) break;
    }
}

void IRGenerator::gen_assign(ast::AssignStmt& stmt) {
    // Handle compound assignments (+=, -=, etc.)
    if (stmt.tok != TokenKind::Assign) {
        if (stmt.lhs.count >= 1 && stmt.rhs.count >= 1) {
            auto* lhs_addr = gen_addr(stmt.lhs[0]);
            auto* lhs_val = gen_expr(stmt.lhs[0]);
            auto* rhs_val = gen_expr(stmt.rhs[0]);
            if (!lhs_addr || !lhs_val || !rhs_val) return;

            bool is_float = lhs_val->type && lhs_val->type->is_float();
            Value* result = nullptr;

            switch (stmt.tok) {
                case TokenKind::PlusAssign:
                    result = is_float ? builder_.create_fadd(lhs_val, rhs_val, "add")
                                      : builder_.create_add(lhs_val, rhs_val, "add");
                    break;
                case TokenKind::MinusAssign:
                    result = is_float ? builder_.create_fsub(lhs_val, rhs_val, "sub")
                                      : builder_.create_sub(lhs_val, rhs_val, "sub");
                    break;
                case TokenKind::StarAssign:
                    result = is_float ? builder_.create_fmul(lhs_val, rhs_val, "mul")
                                      : builder_.create_mul(lhs_val, rhs_val, "mul");
                    break;
                case TokenKind::SlashAssign:
                    result = is_float ? builder_.create_fdiv(lhs_val, rhs_val, "div")
                                      : builder_.create_div(lhs_val, rhs_val, "div");
                    break;
                case TokenKind::PercentAssign:
                    result = builder_.create_rem(lhs_val, rhs_val, "rem");
                    break;
                case TokenKind::AmpAssign:
                    result = builder_.create_and(lhs_val, rhs_val, "and");
                    break;
                case TokenKind::PipeAssign:
                    result = builder_.create_or(lhs_val, rhs_val, "or");
                    break;
                case TokenKind::CaretAssign:
                    result = builder_.create_xor(lhs_val, rhs_val, "xor");
                    break;
                case TokenKind::ShlAssign:
                    result = builder_.create_shl(lhs_val, rhs_val, "shl");
                    break;
                case TokenKind::ShrAssign:
                    result = builder_.create_shr(lhs_val, rhs_val, "shr");
                    break;
                case TokenKind::AmpCaretAssign:
                    result = builder_.create_andnot(lhs_val, rhs_val, "andnot");
                    break;
                default:
                    result = rhs_val;
                    break;
            }
            if (result) {
                builder_.create_store(result, lhs_addr);
            }
            return;
        }
    }

    // Simple assignment: x = y or x, y = a, b
    for (uint32_t i = 0; i < stmt.lhs.count && i < stmt.rhs.count; ++i) {
        auto* lhs_expr = stmt.lhs[i];

        // Detect map-index LHS: m[k] = v
        if (lhs_expr->kind == ast::ExprKind::Index) {
            auto* base_info = expr_info(lhs_expr->index.x);
            if (base_info && base_info->type &&
                sema::underlying(base_info->type)->kind == sema::TypeKind::Map) {
                auto* m = gen_expr(lhs_expr->index.x);
                auto* key = gen_expr(lhs_expr->index.index);
                auto* val = gen_expr(stmt.rhs[i]);
                if (m && key && val) {
                    builder_.create_map_set(m, key, val);
                }
                continue;
            }
        }

        auto* rhs = gen_expr(stmt.rhs[i]);
        if (!rhs) continue;

        auto* lhs_addr = gen_addr(lhs_expr);
        if (lhs_addr) {
            builder_.create_store(rhs, lhs_addr);
        }
    }
}

void IRGenerator::gen_short_var_decl(ast::ShortVarDeclStmt& stmt) {
    // Special case: v, ok := m[key]  (map index with two LHS variables)
    if (stmt.lhs.count == 2 && stmt.rhs.count == 1 &&
        stmt.rhs[0]->kind == ast::ExprKind::Index) {
        auto* base_info = expr_info(stmt.rhs[0]->index.x);
        if (base_info && base_info->type &&
            sema::underlying(base_info->type)->kind == sema::TypeKind::Map) {
            auto* m      = gen_expr(stmt.rhs[0]->index.x);
            auto* key    = gen_expr(stmt.rhs[0]->index.index);
            auto* map_t  = sema::underlying(base_info->type);
            IRType* val_ir = type_map_.i64_type();
            if (map_t->map.value) val_ir = map_sema_type(map_t->map.value);

            // Allocate a local ok slot
            auto* ok_alloca = builder_.create_alloca(type_map_.i64_type(), "ok.addr");
            auto* mg = builder_.create_map_get(m, key, val_ir, "map_get");

            // Bind value variable (lhs[0])
            if (stmt.lhs[0]->kind == ast::ExprKind::Ident) {
                auto* info = expr_info(stmt.lhs[0]);
                if (info && info->symbol) {
                    auto* a = builder_.create_alloca(val_ir, std::string(stmt.lhs[0]->ident.name) + ".addr");
                    builder_.create_store(mg, a);
                    var_map_[info->symbol] = a;
                }
            }

            // Bind ok variable (lhs[1]) — load from the ok_alloca
            // Note: golangc_map_get will set it via out_ok parameter in codegen
            if (stmt.lhs[1]->kind == ast::ExprKind::Ident) {
                auto* info = expr_info(stmt.lhs[1]);
                if (info && info->symbol) {
                    var_map_[info->symbol] = ok_alloca;
                }
            }
            return;
        }
    }

    // Special case: multi-return function call  a, b := f()
    if (stmt.lhs.count > 1 && stmt.rhs.count == 1) {
        auto* rhs_val = gen_expr(stmt.rhs[0]);
        if (rhs_val && rhs_val->type && rhs_val->type->is_struct() &&
            rhs_val->type->fields.size() == stmt.lhs.count) {
            for (uint32_t i = 0; i < stmt.lhs.count; ++i) {
                if (stmt.lhs[i]->kind != ast::ExprKind::Ident) continue;
                auto* info = expr_info(stmt.lhs[i]);
                if (!info || !info->symbol) continue;

                IRType* field_type = rhs_val->type->fields[i];
                auto* extracted = builder_.create_extract_value(
                    rhs_val, i, field_type,
                    std::string(stmt.lhs[i]->ident.name));
                auto* alloca = builder_.create_alloca(
                    field_type, std::string(stmt.lhs[i]->ident.name) + ".addr");
                builder_.create_store(extracted, alloca);
                var_map_[info->symbol] = alloca;
            }
            return;
        }
    }

    // Normal single-assignment short var decl
    for (uint32_t i = 0; i < stmt.lhs.count && i < stmt.rhs.count; ++i) {
        auto* rhs = gen_expr(stmt.rhs[i]);
        if (!rhs) continue;

        if (stmt.lhs[i]->kind != ast::ExprKind::Ident) continue;
        auto& ident = stmt.lhs[i]->ident;

        auto* info = expr_info(stmt.lhs[i]);
        if (!info || !info->symbol) continue;

        IRType* var_type = map_sema_type(info->symbol->type);

        // Create alloca in entry block
        auto* alloca = builder_.create_alloca(var_type, std::string(ident.name) + ".addr");
        builder_.create_store(rhs, alloca);
        var_map_[info->symbol] = alloca;
    }
}

void IRGenerator::gen_return(ast::ReturnStmt& stmt) {
    if (stmt.results.count == 0) {
        builder_.create_ret();
        return;
    }

    if (stmt.results.count == 1) {
        auto* val = gen_expr(stmt.results[0]);
        builder_.create_ret(val);
        return;
    }

    // Multiple return values: pack into a struct via InsertValue sequence
    // Build the tuple IR type from the current function's return type
    IRType* tuple_type = current_func_ ? current_func_->return_type : nullptr;
    if (!tuple_type || !tuple_type->is_struct()) {
        // Fallback: just return the first value
        auto* val = gen_expr(stmt.results[0]);
        builder_.create_ret(val);
        return;
    }

    // Start with a nil/zero struct
    auto* packed = builder_.create_const_nil(tuple_type, "ret.pack");
    for (uint32_t i = 0; i < stmt.results.count; ++i) {
        auto* val = gen_expr(stmt.results[i]);
        if (!val) continue;
        packed = builder_.create_insert_value(packed, val, i, "ret.pack");
    }
    builder_.create_ret(packed);
}

void IRGenerator::gen_if(ast::IfStmt& stmt) {
    // Init statement
    if (stmt.init) gen_stmt(stmt.init);

    auto* cond = gen_expr(stmt.cond);
    if (!cond) {
        cond = builder_.create_const_bool(false);
    }

    auto* then_bb = current_func_->create_block(fresh_block_name("if.then"));
    auto* merge_bb = current_func_->create_block(fresh_block_name("if.merge"));
    BasicBlock* else_bb = nullptr;

    if (stmt.else_body) {
        else_bb = current_func_->create_block(fresh_block_name("if.else"));
        builder_.create_condbr(cond, then_bb, else_bb);
    } else {
        builder_.create_condbr(cond, then_bb, merge_bb);
    }

    // Then block
    builder_.set_insert_block(then_bb);
    gen_stmt(stmt.body);
    if (!builder_.insert_block()->has_terminator()) {
        builder_.create_br(merge_bb);
    }

    // Else block
    if (else_bb) {
        builder_.set_insert_block(else_bb);
        gen_stmt(stmt.else_body);
        if (!builder_.insert_block()->has_terminator()) {
            builder_.create_br(merge_bb);
        }
    }

    builder_.set_insert_block(merge_bb);
}

void IRGenerator::gen_for(ast::ForStmt& stmt) {
    auto* cond_bb = current_func_->create_block(fresh_block_name("for.cond"));
    auto* body_bb = current_func_->create_block(fresh_block_name("for.body"));
    auto* post_bb = current_func_->create_block(fresh_block_name("for.post"));
    auto* done_bb = current_func_->create_block(fresh_block_name("for.done"));

    // Init
    if (stmt.init) gen_stmt(stmt.init);

    builder_.create_br(cond_bb);

    // Condition
    builder_.set_insert_block(cond_bb);
    if (stmt.cond) {
        auto* cond = gen_expr(stmt.cond);
        if (!cond) cond = builder_.create_const_bool(true);
        builder_.create_condbr(cond, body_bb, done_bb);
    } else {
        // Infinite loop
        builder_.create_br(body_bb);
    }

    // Body
    loop_stack_.push_back({done_bb, post_bb});
    builder_.set_insert_block(body_bb);
    gen_stmt(stmt.body);
    if (!builder_.insert_block()->has_terminator()) {
        builder_.create_br(post_bb);
    }
    loop_stack_.pop_back();

    // Post
    builder_.set_insert_block(post_bb);
    if (stmt.post) gen_stmt(stmt.post);
    if (!builder_.insert_block()->has_terminator()) {
        builder_.create_br(cond_bb);
    }

    builder_.set_insert_block(done_bb);
}

void IRGenerator::gen_range(ast::RangeStmt& stmt) {
    // Range loops — simplified implementation
    // for key, value := range expr { body }
    auto* range_val = gen_expr(stmt.x);
    if (!range_val) return;

    auto* range_info = expr_info(stmt.x);
    if (!range_info || !range_info->type) return;

    auto* base_type = sema::underlying(range_info->type);

    // ---- Map range: for k, v := range m ----
    if (base_type->kind == sema::TypeKind::Map) {
        // Create iterator and loop blocks
        auto* cond_bb = current_func_->create_block(fresh_block_name("range.cond"));
        auto* body_bb = current_func_->create_block(fresh_block_name("range.body"));
        auto* done_bb = current_func_->create_block(fresh_block_name("range.done"));

        // iter = golangc_map_iter_make(m)
        auto* iter = builder_.create_map_iter_make(range_val, "range.iter");
        builder_.create_br(cond_bb);

        // Condition: ok = golangc_map_iter_next(iter, &key, &val)  → ok != 0
        builder_.set_insert_block(cond_bb);

        IRType* key_type = type_map_.i64_type();
        IRType* val_type = type_map_.i64_type();
        if (base_type->map.key)   key_type = map_sema_type(base_type->map.key);
        if (base_type->map.value) val_type  = map_sema_type(base_type->map.value);

        // Pre-allocate key/val storage for the iterator to write into
        auto* key_alloca = builder_.create_alloca(key_type, "range.key.addr");
        auto* val_alloca = builder_.create_alloca(val_type,  "range.val.addr");

        // MapIterNext writes key+val into key_alloca/val_alloca and returns 1 if more
        auto* ok = builder_.create_map_iter_next(iter, key_type, val_type, "range.ok");
        // imm_int on MapIterNext carries key_alloca->id and val_alloca->id via field_index
        // We relay these IDs through the instruction so codegen knows where to write
        {
            auto* ok_inst = dynamic_cast<ir::Instruction*>(ok);
            if (ok_inst) {
                ok_inst->operands.push_back(key_alloca);
                ok_inst->operands.push_back(val_alloca);
            }
        }

        auto* cond = builder_.create_ne(ok, builder_.create_const_int(type_map_.i64_type(), 0), "range.cond");
        builder_.create_condbr(cond, body_bb, done_bb);

        // Body
        loop_stack_.push_back({done_bb, cond_bb});
        builder_.set_insert_block(body_bb);

        // Bind key variable
        if (stmt.key && stmt.key->kind == ast::ExprKind::Ident) {
            auto* key_info = expr_info(stmt.key);
            if (key_info && key_info->symbol && stmt.key->ident.name != "_") {
                var_map_[key_info->symbol] = key_alloca;
            }
        }
        // Bind value variable
        if (stmt.value && stmt.value->kind == ast::ExprKind::Ident) {
            auto* val_info = expr_info(stmt.value);
            if (val_info && val_info->symbol && stmt.value->ident.name != "_") {
                var_map_[val_info->symbol] = val_alloca;
            }
        }

        gen_stmt(stmt.body);
        if (!builder_.insert_block()->has_terminator()) {
            builder_.create_br(cond_bb);
        }
        loop_stack_.pop_back();

        // Done: free iterator
        builder_.set_insert_block(done_bb);
        builder_.create_map_iter_free(iter);
        return;
    }

    auto* cond_bb = current_func_->create_block(fresh_block_name("range.cond"));
    auto* body_bb = current_func_->create_block(fresh_block_name("range.body"));
    auto* post_bb = current_func_->create_block(fresh_block_name("range.post"));
    auto* done_bb = current_func_->create_block(fresh_block_name("range.done"));

    // Create loop index variable
    auto* idx_alloca = builder_.create_alloca(type_map_.i64_type(), "range.idx.addr");
    auto* zero = builder_.create_const_int(type_map_.i64_type(), 0);
    builder_.create_store(zero, idx_alloca);

    // Get length
    Value* length = nullptr;
    if (base_type->kind == sema::TypeKind::Slice) {
        length = builder_.create_slice_len(range_val, "range.len");
    } else if (sema::is_string(range_info->type)) {
        length = builder_.create_string_len(range_val, "range.len");
    } else if (base_type->kind == sema::TypeKind::Array) {
        length = builder_.create_const_int(type_map_.i64_type(), base_type->array.length, "range.len");
    } else {
        length = builder_.create_const_int(type_map_.i64_type(), 0, "range.len");
    }

    builder_.create_br(cond_bb);

    // Condition: idx < len
    builder_.set_insert_block(cond_bb);
    auto* idx = builder_.create_load(idx_alloca, type_map_.i64_type(), "range.idx");
    auto* cond = builder_.create_lt(idx, length, "range.cond");
    builder_.create_condbr(cond, body_bb, done_bb);

    // Body
    loop_stack_.push_back({done_bb, post_bb});
    builder_.set_insert_block(body_bb);

    // Bind key variable
    if (stmt.key && stmt.key->kind == ast::ExprKind::Ident) {
        auto* key_info = expr_info(stmt.key);
        if (key_info && key_info->symbol) {
            auto* key_alloca = builder_.create_alloca(type_map_.i64_type(),
                std::string(stmt.key->ident.name) + ".addr");
            auto* key_val = builder_.create_load(idx_alloca, type_map_.i64_type(), "key");
            builder_.create_store(key_val, key_alloca);
            var_map_[key_info->symbol] = key_alloca;
        }
    }

    // Bind value variable — load the actual element from the collection
    if (stmt.value && stmt.value->kind == ast::ExprKind::Ident) {
        auto* val_info = expr_info(stmt.value);
        if (val_info && val_info->symbol) {
            IRType* elem_type = type_map_.i64_type();
            Value* elem_val = nullptr;

            auto* idx_val = builder_.create_load(idx_alloca, type_map_.i64_type(), "range.idx.val");

            if (base_type->kind == sema::TypeKind::Slice && base_type->slice.element) {
                elem_type = map_sema_type(base_type->slice.element);
                // Load via SliceIndex — returns the element value
                elem_val = builder_.create_slice_index(range_val, idx_val, elem_type, "range.elem");
            } else if (base_type->kind == sema::TypeKind::Array && base_type->array.element) {
                elem_type = map_sema_type(base_type->array.element);
                // Get address of array element, then load
                auto* elem_ptr = builder_.create_getptr(
                    gen_addr(stmt.x) ? gen_addr(stmt.x)
                                     : builder_.create_alloca(map_sema_type(range_info->type), "arr.tmp"),
                    idx_val, type_map_.ptr_type(), "arr.elem.addr");
                elem_val = builder_.create_load(elem_ptr, elem_type, "range.elem");
            } else if (sema::is_string(range_info->type)) {
                elem_type = type_map_.i8_type();
                elem_val = builder_.create_string_index(range_val, idx_val, "range.char");
            }

            auto* val_alloca = builder_.create_alloca(elem_type,
                std::string(stmt.value->ident.name) + ".addr");
            if (elem_val) {
                builder_.create_store(elem_val, val_alloca);
            }
            var_map_[val_info->symbol] = val_alloca;
        }
    }

    gen_stmt(stmt.body);
    if (!builder_.insert_block()->has_terminator()) {
        builder_.create_br(post_bb);
    }
    loop_stack_.pop_back();

    // Post: increment index
    builder_.set_insert_block(post_bb);
    auto* idx2 = builder_.create_load(idx_alloca, type_map_.i64_type(), "range.idx");
    auto* one = builder_.create_const_int(type_map_.i64_type(), 1);
    auto* next = builder_.create_add(idx2, one, "range.next");
    builder_.create_store(next, idx_alloca);
    builder_.create_br(cond_bb);

    builder_.set_insert_block(done_bb);
}

void IRGenerator::gen_switch(ast::SwitchStmt& stmt) {
    // Init statement
    if (stmt.init) gen_stmt(stmt.init);

    // Evaluate tag (or use true for tagless switch)
    Value* tag = nullptr;
    if (stmt.tag) {
        tag = gen_expr(stmt.tag);
    }

    // Determine if the tag is a string type (computed once, before the loop)
    auto* tag_sema_type = stmt.tag ? expr_type(stmt.tag) : nullptr;
    bool tag_is_string = tag_sema_type && sema::is_string(tag_sema_type);

    auto* merge_bb = current_func_->create_block(fresh_block_name("switch.merge"));

    // Generate case blocks
    struct CaseInfo {
        BasicBlock* block;
        std::vector<ast::Expr*> values;
        ast::List<ast::Stmt*>* body;
        bool is_default;
    };
    std::vector<CaseInfo> cases;
    BasicBlock* default_bb = merge_bb;

    for (auto* cc : stmt.cases) {
        auto* case_bb = current_func_->create_block(fresh_block_name("switch.case"));
        bool is_default = cc->values.count == 0;
        if (is_default) {
            default_bb = case_bb;
        }
        std::vector<ast::Expr*> vals;
        for (auto* v : cc->values) vals.push_back(v);
        cases.push_back({case_bb, std::move(vals), &cc->body, is_default});
    }

    // Populate fallthrough map: case[i].block → case[i+1].block
    fallthrough_map_.clear();
    for (size_t i = 0; i + 1 < cases.size(); ++i) {
        fallthrough_map_[cases[i].block] = cases[i + 1].block;
    }

    // Generate chain of conditional branches
    if (tag) {
        for (size_t i = 0; i < cases.size(); ++i) {
            auto& ci = cases[i];
            if (ci.is_default) continue;

            for (auto* val_expr : ci.values) {
                auto* val = gen_expr(val_expr);
                if (!val) continue;
                Value* cmp = nullptr;
                if (tag_is_string)
                    cmp = builder_.create_string_eq(tag, val, "switch.cmp");
                else
                    cmp = builder_.create_eq(tag, val, "switch.cmp");
                auto* next_bb = current_func_->create_block(fresh_block_name("switch.next"));
                builder_.create_condbr(cmp, ci.block, next_bb);
                builder_.set_insert_block(next_bb);
            }
        }
        // Fall through to default
        if (!builder_.insert_block()->has_terminator()) {
            builder_.create_br(default_bb);
        }
    } else {
        // Tagless switch: each case is a boolean expression
        for (size_t i = 0; i < cases.size(); ++i) {
            auto& ci = cases[i];
            if (ci.is_default) continue;

            for (auto* val_expr : ci.values) {
                auto* cond = gen_expr(val_expr);
                if (!cond) continue;
                auto* next_bb = current_func_->create_block(fresh_block_name("switch.next"));
                builder_.create_condbr(cond, ci.block, next_bb);
                builder_.set_insert_block(next_bb);
            }
        }
        if (!builder_.insert_block()->has_terminator()) {
            builder_.create_br(default_bb);
        }
    }

    // Generate case bodies
    // Push loop context for break (switch breaks go to merge)
    loop_stack_.push_back({merge_bb, nullptr});
    for (auto& ci : cases) {
        builder_.set_insert_block(ci.block);
        for (auto* s : *ci.body) {
            gen_stmt(s);
        }
        if (!builder_.insert_block()->has_terminator()) {
            builder_.create_br(merge_bb);
        }
    }
    loop_stack_.pop_back();

    // Clear the fallthrough map after the switch is done
    fallthrough_map_.clear();

    builder_.set_insert_block(merge_bb);
}

void IRGenerator::gen_inc_dec(ast::IncDecStmt& stmt) {
    auto* addr = gen_addr(stmt.x);
    if (!addr) return;

    auto* val_info = expr_info(stmt.x);
    IRType* val_type = type_map_.i64_type();
    if (val_info && val_info->type) {
        val_type = map_sema_type(val_info->type);
    }

    auto* val = builder_.create_load(addr, val_type, "inc.val");
    auto* one = builder_.create_const_int(val_type, 1);

    Value* result = nullptr;
    if (stmt.tok == TokenKind::Increment) {
        result = builder_.create_add(val, one, "inc");
    } else {
        result = builder_.create_sub(val, one, "dec");
    }

    builder_.create_store(result, addr);
}

void IRGenerator::gen_select(ast::SelectStmt& stmt) {
    // ----------------------------------------------------------------
    // Classify CommClauses
    // ----------------------------------------------------------------
    struct CaseDesc {
        ast::CommClause* cc;
        bool is_default = false;
        bool is_send    = false;
        ast::Expr* ch_expr  = nullptr; // channel expression
        ast::Expr* val_expr = nullptr; // send value, or null for recv
        Value* recv_buf     = nullptr; // alloca for receive output buffer
        const sema::Symbol* recv_sym = nullptr; // symbol bound by v := <-ch
        int32_t chan_index  = -1;      // index into channel-cases array
    };

    std::vector<CaseDesc> descs;
    int32_t default_index = -1; // index into descs
    int32_t num_chan_cases = 0;

    for (uint32_t i = 0; i < stmt.cases.count; ++i) {
        auto* cc = stmt.cases[i];
        CaseDesc d;
        d.cc = cc;

        if (!cc->comm) {
            // default case
            d.is_default = true;
            default_index = static_cast<int32_t>(descs.size());
        } else if (cc->comm->kind == ast::StmtKind::Send) {
            // send case: ch <- val
            d.is_send  = true;
            d.ch_expr  = cc->comm->send.chan;
            d.val_expr = cc->comm->send.value;
            d.chan_index = num_chan_cases++;
        } else if (cc->comm->kind == ast::StmtKind::ShortVarDecl) {
            // v := <-ch
            auto& svd = cc->comm->short_var_decl;
            if (svd.rhs.count > 0 &&
                svd.rhs[0]->kind == ast::ExprKind::Unary &&
                svd.rhs[0]->unary.op == TokenKind::Arrow) {
                d.is_send = false;
                d.ch_expr = svd.rhs[0]->unary.x;
                // Pre-alloca receive buffer
                auto* rhs0_info = expr_info(svd.rhs[0]);
                IRType* elem_type = (rhs0_info && rhs0_info->type) ? map_sema_type(rhs0_info->type) : type_map_.i64_type();
                d.recv_buf = builder_.create_alloca(elem_type, "sel.recv.buf");
                // Bind symbol
                if (svd.lhs.count > 0 && svd.lhs[0]->kind == ast::ExprKind::Ident) {
                    d.recv_sym = checker_.decl_symbol(&svd.lhs[0]->ident);
                }
                d.chan_index = num_chan_cases++;
            } else {
                // Unrecognized comm form — treat as default
                d.is_default = true;
                if (default_index < 0) default_index = static_cast<int32_t>(descs.size());
            }
        } else if (cc->comm->kind == ast::StmtKind::Assign) {
            // v = <-ch (existing variable)
            auto& asgn = cc->comm->assign;
            if (asgn.rhs.count > 0 &&
                asgn.rhs[0]->kind == ast::ExprKind::Unary &&
                asgn.rhs[0]->unary.op == TokenKind::Arrow) {
                d.is_send = false;
                d.ch_expr = asgn.rhs[0]->unary.x;
                auto* rhs_info = expr_info(asgn.rhs[0]);
                IRType* elem_type = (rhs_info && rhs_info->type) ? map_sema_type(rhs_info->type) : type_map_.i64_type();
                d.recv_buf = builder_.create_alloca(elem_type, "sel.recv.buf");
                d.chan_index = num_chan_cases++;
            } else {
                d.is_default = true;
                if (default_index < 0) default_index = static_cast<int32_t>(descs.size());
            }
        } else if (cc->comm->kind == ast::StmtKind::Expr) {
            // Bare receive: <-ch (result discarded)
            auto* x = cc->comm->expr.x;
            if (x && x->kind == ast::ExprKind::Unary && x->unary.op == TokenKind::Arrow) {
                d.is_send = false;
                d.ch_expr = x->unary.x;
                d.chan_index = num_chan_cases++;
            } else {
                d.is_default = true;
                if (default_index < 0) default_index = static_cast<int32_t>(descs.size());
            }
        } else {
            d.is_default = true;
            if (default_index < 0) default_index = static_cast<int32_t>(descs.size());
        }

        descs.push_back(std::move(d));
    }

    // ----------------------------------------------------------------
    // Build the SelectCase array on the stack.
    // Each entry is 3 QWORDs: {ch_ptr, val_ptr, op}  (24 bytes each).
    // We use a flat i64 array of size num_chan_cases * 3.
    // ----------------------------------------------------------------
    Value* cases_ptr = nullptr;
    if (num_chan_cases > 0) {
        IRType* i64_t   = type_map_.i64_type();
        int64_t arr_len = static_cast<int64_t>(num_chan_cases) * 3;
        IRType* arr_t   = type_map_.make_array_type(i64_t, arr_len);
        cases_ptr = builder_.create_alloca(arr_t, "sel.cases");

        // Fill each case entry
        for (auto& d : descs) {
            if (d.is_default) continue;

            // Evaluate channel pointer
            Value* ch = d.ch_expr ? gen_expr(d.ch_expr) : nullptr;
            if (!ch) ch = builder_.create_const_int(type_map_.ptr_type(), 0);

            // ch_ptr at slot [d.chan_index * 3 + 0]
            auto* idx_ch  = builder_.create_const_int(i64_t, static_cast<int64_t>(d.chan_index) * 3 + 0);
            auto* ptr_ch  = builder_.create_getptr(cases_ptr, idx_ch, type_map_.ptr_type(), "sel.ch.ptr");
            builder_.create_store(ch, ptr_ch);

            // val_ptr at slot [d.chan_index * 3 + 1]
            auto* idx_val = builder_.create_const_int(i64_t, static_cast<int64_t>(d.chan_index) * 3 + 1);
            auto* ptr_val = builder_.create_getptr(cases_ptr, idx_val, type_map_.ptr_type(), "sel.val.ptr");
            if (d.is_send && d.val_expr) {
                // For send: evaluate and spill the value, store its address
                Value* send_val = gen_expr(d.val_expr);
                if (!send_val) send_val = builder_.create_const_int(i64_t, 0);
                // Spill to an alloca so we have an addressable slot
                auto* val_alloca = builder_.create_alloca(send_val->type ? send_val->type : i64_t, "sel.send.val");
                builder_.create_store(send_val, val_alloca);
                builder_.create_store(val_alloca, ptr_val);
            } else if (!d.is_send && d.recv_buf) {
                // For recv-with-assign: store address of recv buffer
                builder_.create_store(d.recv_buf, ptr_val);
            } else {
                // Bare recv or unknown: store null
                auto* null_val = builder_.create_const_int(type_map_.ptr_type(), 0);
                builder_.create_store(null_val, ptr_val);
            }

            // op at slot [d.chan_index * 3 + 2]
            auto* idx_op  = builder_.create_const_int(i64_t, static_cast<int64_t>(d.chan_index) * 3 + 2);
            auto* ptr_op  = builder_.create_getptr(cases_ptr, idx_op, type_map_.ptr_type(), "sel.op.ptr");
            auto* op_val  = builder_.create_const_int(i64_t, d.is_send ? 1 : 0);
            builder_.create_store(op_val, ptr_op);
        }
    } else {
        // No channel cases — pass a null pointer
        cases_ptr = builder_.create_const_int(type_map_.ptr_type(), 0);
    }

    // ----------------------------------------------------------------
    // Get or create golangc_select function reference
    // ----------------------------------------------------------------
    Function* sel_fn = nullptr;
    {
        auto it = func_name_map_.find("golangc_select");
        if (it != func_name_map_.end()) {
            sel_fn = it->second;
        } else {
            // Register a synthetic external function with i64 return type
            sel_fn = module_->create_function(builder_.next_id(), type_map_.i64_type(), "golangc_select");
            sel_fn->return_type = type_map_.i64_type();
            func_name_map_["golangc_select"] = sel_fn;
        }
    }

    // ----------------------------------------------------------------
    // Call golangc_select(cases_ptr, num_cases, has_default)
    // ----------------------------------------------------------------
    auto* n_val       = builder_.create_const_int(type_map_.i64_type(), num_chan_cases);
    auto* has_def_val = builder_.create_const_int(type_map_.i64_type(), default_index >= 0 ? 1 : 0);
    auto* fired_idx   = builder_.create_call(sel_fn, {cases_ptr, n_val, has_def_val},
                                              type_map_.i64_type(), "sel.fired");

    // ----------------------------------------------------------------
    // Create basic blocks for each case and merge
    // ----------------------------------------------------------------
    auto* merge_bb = current_func_->create_block(fresh_block_name("sel.merge"));
    loop_stack_.push_back({merge_bb, nullptr}); // break → merge

    std::vector<BasicBlock*> case_bbs;
    for (size_t i = 0; i < descs.size(); ++i) {
        case_bbs.push_back(current_func_->create_block(fresh_block_name("sel.case")));
    }

    // ----------------------------------------------------------------
    // Branch chain: channel cases first (by chan_index), then default
    // ----------------------------------------------------------------
    // For channel cases, fired_idx == chan_index means that case fired.
    for (auto& d : descs) {
        if (d.is_default) continue;
        auto* cmp      = builder_.create_eq(fired_idx,
                             builder_.create_const_int(type_map_.i64_type(), d.chan_index),
                             "sel.cmp");
        auto* next_bb  = current_func_->create_block(fresh_block_name("sel.check"));
        builder_.create_condbr(cmp, case_bbs[&d - descs.data()], next_bb);
        builder_.set_insert_block(next_bb);
    }
    // Unconditional jump to default case (or merge if no default)
    if (default_index >= 0) {
        builder_.create_br(case_bbs[static_cast<size_t>(default_index)]);
    } else {
        builder_.create_br(merge_bb);
    }

    // ----------------------------------------------------------------
    // Emit case bodies
    // ----------------------------------------------------------------
    for (size_t i = 0; i < descs.size(); ++i) {
        auto& d = descs[i];
        builder_.set_insert_block(case_bbs[i]);

        // If this is a recv-with-assign case, bind the symbol to recv_buf
        if (!d.is_default && !d.is_send && d.recv_buf && d.recv_sym) {
            var_map_[d.recv_sym] = d.recv_buf;
        }

        // Generate body statements
        for (auto* s : d.cc->body) {
            gen_stmt(s);
        }

        if (!builder_.insert_block()->has_terminator()) {
            builder_.create_br(merge_bb);
        }
    }

    loop_stack_.pop_back();
    builder_.set_insert_block(merge_bb);
}

void IRGenerator::gen_send(ast::SendStmt& stmt) {
    auto* ch = gen_expr(stmt.chan);
    auto* val = gen_expr(stmt.value);
    if (ch && val) {
        builder_.create_chan_send(ch, val);
    }
}

void IRGenerator::gen_go(ast::GoStmt& stmt) {
    if (stmt.call->kind != ast::ExprKind::Call) return;
    auto& call = stmt.call->call;

    auto* callee = gen_expr(call.func);
    if (!callee) return;

    std::vector<Value*> args;
    for (auto* arg_expr : call.args) {
        auto* arg = gen_expr(arg_expr);
        if (arg) args.push_back(arg);
    }

    builder_.create_go_spawn(callee, args);
}

void IRGenerator::gen_defer(ast::DeferStmt& stmt) {
    if (stmt.call->kind != ast::ExprKind::Call) return;
    auto& call = stmt.call->call;

    auto* callee = gen_expr(call.func);
    if (!callee) return;

    std::vector<Value*> args;
    for (auto* arg_expr : call.args) {
        auto* arg = gen_expr(arg_expr);
        if (arg) args.push_back(arg);
    }

    builder_.create_defer_call(callee, args);
}

void IRGenerator::gen_branch(ast::BranchStmt& stmt) {
    switch (stmt.tok) {
        case TokenKind::KW_break:
            if (!loop_stack_.empty() && loop_stack_.back().break_target) {
                builder_.create_br(loop_stack_.back().break_target);
            }
            break;
        case TokenKind::KW_continue:
            if (!loop_stack_.empty() && loop_stack_.back().continue_target) {
                builder_.create_br(loop_stack_.back().continue_target);
            }
            break;
        case TokenKind::KW_fallthrough: {
            auto it = fallthrough_map_.find(builder_.insert_block());
            if (it != fallthrough_map_.end()) {
                builder_.create_br(it->second);
            }
            break;
        }
        default:
            // goto — not yet handled
            break;
    }
}

void IRGenerator::gen_expr_stmt(ast::ExprStmt& stmt) {
    (void)gen_expr(stmt.x);
}

void IRGenerator::gen_decl_stmt(ast::DeclStmt& stmt) {
    if (!stmt.decl) return;
    switch (stmt.decl->kind) {
        case ast::DeclKind::Var:
            gen_local_var(stmt.decl->var);
            break;
        default:
            // Const and Type decls in statement context — ignore for IR
            break;
    }
}

void IRGenerator::gen_local_var(ast::VarDecl& decl) {
    for (auto* spec : decl.specs) {
        gen_local_var_spec(*spec);
    }
}

void IRGenerator::gen_local_var_spec(ast::VarSpec& spec) {
    for (uint32_t i = 0; i < spec.names.count; ++i) {
        auto* name_expr = spec.names[i];
        auto* sym = checker_.decl_symbol(name_expr);
        if (!sym) continue;

        IRType* var_type = map_sema_type(sym->type);
        auto* alloca = builder_.create_alloca(var_type,
            std::string(name_expr->name) + ".addr");
        var_map_[sym] = alloca;

        // Initialize if there's a value
        if (i < spec.values.count) {
            auto* val = gen_expr(spec.values[i]);
            if (val) {
                builder_.create_store(val, alloca);
            }
        }
    }
}

// ============================================================================
// Type ID helper
// ============================================================================

int64_t IRGenerator::type_id_for(const sema::Type* t) {
    if (!t) return 0;
    auto it = type_id_map_.find(t);
    if (it != type_id_map_.end()) return it->second;
    int64_t id = next_type_id_++;
    type_id_map_[t] = id;
    return id;
}

// ============================================================================
// Type switch statement
// ============================================================================

void IRGenerator::gen_type_switch(ast::TypeSwitchStmt& stmt) {
    // Evaluate the interface expression
    Value* iface = nullptr;
    std::string bound_name;

    if (stmt.assign) {
        if (stmt.assign->kind == ast::StmtKind::Expr && stmt.assign->expr.x &&
            stmt.assign->expr.x->kind == ast::ExprKind::TypeAssert) {
            iface = gen_expr(stmt.assign->expr.x->type_assert.x);
        } else if (stmt.assign->kind == ast::StmtKind::ShortVarDecl) {
            auto& svd = stmt.assign->short_var_decl;
            if (svd.rhs.count > 0 && svd.rhs[0] &&
                svd.rhs[0]->kind == ast::ExprKind::TypeAssert) {
                iface = gen_expr(svd.rhs[0]->type_assert.x);
            }
            if (svd.lhs.count > 0 && svd.lhs[0] &&
                svd.lhs[0]->kind == ast::ExprKind::Ident) {
                bound_name = std::string(svd.lhs[0]->ident.name);
            }
        }
    }
    if (!iface) return;

    // Extract the type tag from the interface (first QWORD)
    auto* tag = builder_.create_interface_type(iface, "ts.tag");

    auto* merge_bb = current_func_->create_block(fresh_block_name("ts.merge"));

    // Collect case info
    struct CaseInfo {
        BasicBlock* block;
        std::vector<const sema::Type*> types;
        ast::List<ast::Stmt*>* body;
        bool is_default;
    };
    std::vector<CaseInfo> cases;
    BasicBlock* default_bb = merge_bb;

    for (auto* cc : stmt.cases.span()) {
        if (!cc) continue;
        auto* case_bb = current_func_->create_block(fresh_block_name("ts$case"));
        bool is_default = (cc->values.count == 0);
        if (is_default) default_bb = case_bb;

        std::vector<const sema::Type*> types;
        for (auto* v : cc->values.span()) {
            if (!v) continue;
            auto* info = expr_info(v);
            if (info && info->symbol &&
                info->symbol->kind == sema::SymbolKind::Type) {
                types.push_back(sema::underlying(info->symbol->type));
            } else if (info && info->type) {
                types.push_back(sema::underlying(info->type));
            }
        }
        cases.push_back({case_bb, std::move(types), &cc->body, is_default});
    }

    // Emit comparison chain: for each non-default case, compare tag against type IDs
    for (auto& ci : cases) {
        if (ci.is_default) continue;
        for (auto* t : ci.types) {
            int64_t tid = type_id_for(t);
            auto* expected = builder_.create_const_int(type_map_.i64_type(), tid, "ts.id");
            auto* cmp = builder_.create_eq(tag, expected, "ts.cmp");
            auto* next_bb = current_func_->create_block(fresh_block_name("ts.next"));
            builder_.create_condbr(cmp, ci.block, next_bb);
            builder_.set_insert_block(next_bb);
        }
    }
    // Fall through to default (or merge if no default)
    if (!builder_.insert_block()->has_terminator())
        builder_.create_br(default_bb);

    // Emit case bodies
    loop_stack_.push_back({merge_bb, nullptr});
    for (auto& ci : cases) {
        builder_.set_insert_block(ci.block);

        // Bind the variable (v := x.(type)) for single-type cases
        if (!bound_name.empty() && bound_name != "_") {
            IRType* data_type = type_map_.i64_type();
            if (!ci.is_default && ci.types.size() == 1 && ci.types[0])
                data_type = map_sema_type(const_cast<sema::Type*>(ci.types[0]));
            // Extract data pointer from interface
            auto* data = builder_.create_interface_data(iface, data_type, "ts.data");
            // Look up the bound symbol in var_map_
            // Since sema opens a new scope per case with the bound var,
            // we can look it up by name in the current case's sema ExprInfo.
            // Simplest approach: allocate a fresh alloca and map it for
            // any symbol whose name matches bound_name.
            for (auto& [sym, slot] : var_map_) {
                if (sym && sym->name == bound_name) {
                    builder_.create_store(data, slot);
                    break;
                }
            }
        }

        for (auto* s : *ci.body) gen_stmt(s);
        if (!builder_.insert_block()->has_terminator())
            builder_.create_br(merge_bb);
    }
    loop_stack_.pop_back();

    builder_.set_insert_block(merge_bb);
}

} // namespace ir
} // namespace golangc
