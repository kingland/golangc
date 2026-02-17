#include "ir/ir_gen.hpp"

#include <cassert>

namespace golangc {
namespace ir {

IRGenerator::IRGenerator(sema::Checker& checker)
    : checker_(checker), builder_(type_map_) {}

std::unique_ptr<Module> IRGenerator::generate(ast::File* file) {
    auto mod = std::make_unique<Module>(
        file->package ? std::string(file->package->name->name) : "main");
    module_ = mod.get();

    gen_file(file);

    return mod;
}

std::string IRGenerator::fresh_block_name(const std::string& prefix) {
    return prefix + "_" + std::to_string(block_counter_++);
}

IRType* IRGenerator::map_sema_type(sema::Type* t) {
    return type_map_.map_type(t);
}

const sema::ExprInfo* IRGenerator::expr_info(const ast::Expr* expr) {
    return checker_.expr_info(expr);
}

sema::Type* IRGenerator::expr_type(const ast::Expr* expr) {
    auto* info = expr_info(expr);
    if (info && info->type) {
        // If it's an untyped type, use the default typed version
        if (sema::is_untyped(info->type)) {
            return sema::default_type(info->type);
        }
        return info->type;
    }
    return nullptr;
}

} // namespace ir
} // namespace golangc
