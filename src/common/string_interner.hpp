#pragma once

#include <string>
#include <string_view>
#include <unordered_set>

namespace golangc {

/// Interns strings so that identical strings share the same storage.
/// Interned string_views remain valid for the lifetime of the StringInterner.
class StringInterner {
public:
    /// Intern a string. Returns a stable string_view that is valid for the
    /// lifetime of this interner. If the string was already interned, returns
    /// the existing view.
    [[nodiscard]] std::string_view intern(std::string_view str) {
        auto it = strings_.find(str);
        if (it != strings_.end()) {
            return *it;
        }
        auto [inserted, _] = strings_.emplace(str);
        return *inserted;
    }

    /// Check if a string has been interned.
    [[nodiscard]] bool contains(std::string_view str) const {
        return strings_.find(str) != strings_.end();
    }

    /// Number of interned strings.
    [[nodiscard]] size_t size() const { return strings_.size(); }

    /// Clear all interned strings.
    void clear() { strings_.clear(); }

private:
    // We use an unordered_set of std::string so that the string data is stable.
    // Heterogeneous lookup with string_view avoids unnecessary allocations on find.
    struct StringHash {
        using is_transparent = void;
        [[nodiscard]] size_t operator()(std::string_view sv) const {
            return std::hash<std::string_view>{}(sv);
        }
    };

    struct StringEqual {
        using is_transparent = void;
        [[nodiscard]] bool operator()(std::string_view a, std::string_view b) const {
            return a == b;
        }
    };

    std::unordered_set<std::string, StringHash, StringEqual> strings_;
};

} // namespace golangc
