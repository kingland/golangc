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
        auto* rhs = gen_expr(stmt.rhs[i]);
        if (!rhs) continue;

        auto* lhs_addr = gen_addr(stmt.lhs[i]);
        if (lhs_addr) {
            builder_.create_store(rhs, lhs_addr);
        }
    }
}

void IRGenerator::gen_short_var_decl(ast::ShortVarDeclStmt& stmt) {
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

    // Multiple return values: pack into a struct
    // For now, just return the first value
    auto* val = gen_expr(stmt.results[0]);
    builder_.create_ret(val);
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

    auto* cond_bb = current_func_->create_block(fresh_block_name("range.cond"));
    auto* body_bb = current_func_->create_block(fresh_block_name("range.body"));
    auto* post_bb = current_func_->create_block(fresh_block_name("range.post"));
    auto* done_bb = current_func_->create_block(fresh_block_name("range.done"));

    // Create loop index variable
    auto* idx_alloca = builder_.create_alloca(type_map_.i64_type(), "range.idx.addr");
    auto* zero = builder_.create_const_int(type_map_.i64_type(), 0);
    builder_.create_store(zero, idx_alloca);

    // Get length
    auto* base_type = sema::underlying(range_info->type);
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

    // Bind value variable (simplified)
    if (stmt.value && stmt.value->kind == ast::ExprKind::Ident) {
        auto* val_info = expr_info(stmt.value);
        if (val_info && val_info->symbol) {
            IRType* elem_type = type_map_.i64_type();
            if (base_type->kind == sema::TypeKind::Slice) {
                elem_type = map_sema_type(base_type->slice.element);
            } else if (base_type->kind == sema::TypeKind::Array) {
                elem_type = map_sema_type(base_type->array.element);
            } else if (sema::is_string(range_info->type)) {
                elem_type = type_map_.i32_type(); // rune
            }
            auto* val_alloca = builder_.create_alloca(elem_type,
                std::string(stmt.value->ident.name) + ".addr");
            var_map_[val_info->symbol] = val_alloca;
            // TODO: actually load the element from the collection
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

    // Generate chain of conditional branches
    if (tag) {
        for (size_t i = 0; i < cases.size(); ++i) {
            auto& ci = cases[i];
            if (ci.is_default) continue;

            for (auto* val_expr : ci.values) {
                auto* val = gen_expr(val_expr);
                if (!val) continue;
                auto* cmp = builder_.create_eq(tag, val, "switch.cmp");
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
        default:
            // goto, fallthrough — not yet handled
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

} // namespace ir
} // namespace golangc
