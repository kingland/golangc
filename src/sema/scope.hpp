#pragma once

#include "sema/types.hpp"
#include "common/source_location.hpp"

#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace golangc {

namespace ast {
struct Expr;   // Forward declaration
struct Decl;
} // namespace ast

namespace sema {

// ============================================================================
// Symbols
// ============================================================================

enum class SymbolKind : uint8_t {
    Bad,
    Const,
    Var,
    Type,
    Func,
    Label,
    Package,
    Builtin,
    Nil,
    PseudoPkg, // Recognized pseudo-package (fmt, strconv, os) — not a real import
};

/// Forward declare ConstValue
struct ConstValue;

/// A symbol in the symbol table.
struct Symbol {
    SymbolKind kind = SymbolKind::Bad;
    std::string_view name;
    Type* type = nullptr;
    SourceLocation loc;

    // Optional: link back to AST declaration node
    ast::Decl* decl_node = nullptr;

    // For constants: the constant value
    ConstValue* const_val = nullptr;

    // Usage tracking (for unused variable detection)
    mutable bool used = false;

    // For built-in functions: an ID to identify which builtin
    int builtin_id = -1;

    // For PseudoPkg symbols: the package name (e.g. "fmt", "strconv", "os")
    std::string_view pkg_name;
};

// ============================================================================
// Scopes
// ============================================================================

enum class ScopeKind : uint8_t {
    Universe,
    Package,
    File,
    Function,
    Block,
};

/// A lexical scope containing symbols.
class Scope {
public:
    explicit Scope(ScopeKind kind, Scope* parent = nullptr)
        : kind_(kind), parent_(parent) {
        if (parent) {
            parent->children_.push_back(this);
        }
    }

    /// Look up a symbol in this scope only.
    [[nodiscard]] Symbol* lookup_local(std::string_view name) const {
        auto it = symbols_.find(name);
        if (it != symbols_.end()) {
            return it->second;
        }
        return nullptr;
    }

    /// Look up a symbol in this scope and all parent scopes.
    [[nodiscard]] Symbol* lookup(std::string_view name) const {
        if (auto* sym = lookup_local(name)) {
            return sym;
        }
        if (parent_) {
            return parent_->lookup(name);
        }
        return nullptr;
    }

    /// Insert a symbol into this scope.
    /// Returns nullptr if a symbol with the same name already exists in this scope.
    /// Returns the existing symbol on conflict.
    Symbol* insert(Symbol* sym) {
        auto [it, inserted] = symbols_.emplace(sym->name, sym);
        if (!inserted) {
            return it->second; // Return existing symbol (conflict)
        }
        return nullptr; // Success: no conflict
    }

    [[nodiscard]] ScopeKind kind() const { return kind_; }
    [[nodiscard]] Scope* parent() const { return parent_; }
    [[nodiscard]] const std::vector<Scope*>& children() const { return children_; }

    /// Get all symbols in this scope.
    [[nodiscard]] const auto& symbols() const { return symbols_; }

private:
    ScopeKind kind_;
    Scope* parent_;
    std::vector<Scope*> children_;

    // Using string_view as key — the underlying strings must outlive the scope
    struct SVHash {
        using is_transparent = void;
        [[nodiscard]] size_t operator()(std::string_view sv) const {
            return std::hash<std::string_view>{}(sv);
        }
    };
    struct SVEqual {
        using is_transparent = void;
        [[nodiscard]] bool operator()(std::string_view a, std::string_view b) const {
            return a == b;
        }
    };
    std::unordered_map<std::string_view, Symbol*, SVHash, SVEqual> symbols_;
};

} // namespace sema
} // namespace golangc
