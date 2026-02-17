#pragma once

#include <cstdint>
#include <string>
#include <variant>

namespace golangc {
namespace sema {

/// A constant value used during compile-time evaluation.
struct ConstValue {
    using ValueType = std::variant<std::monostate, bool, int64_t, double, std::string>;
    ValueType value;

    ConstValue() = default;
    explicit ConstValue(bool b) : value(b) {}
    explicit ConstValue(int64_t i) : value(i) {}
    explicit ConstValue(double d) : value(d) {}
    explicit ConstValue(std::string s) : value(std::move(s)) {}

    [[nodiscard]] bool is_valid() const {
        return !std::holds_alternative<std::monostate>(value);
    }

    [[nodiscard]] bool is_bool() const { return std::holds_alternative<bool>(value); }
    [[nodiscard]] bool is_int() const { return std::holds_alternative<int64_t>(value); }
    [[nodiscard]] bool is_float() const { return std::holds_alternative<double>(value); }
    [[nodiscard]] bool is_string() const { return std::holds_alternative<std::string>(value); }

    [[nodiscard]] bool as_bool() const { return std::get<bool>(value); }
    [[nodiscard]] int64_t as_int() const { return std::get<int64_t>(value); }
    [[nodiscard]] double as_float() const { return std::get<double>(value); }
    [[nodiscard]] const std::string& as_string() const { return std::get<std::string>(value); }

    /// Convert to a displayable string.
    [[nodiscard]] std::string to_string() const;

    /// Try to convert to int64_t. Returns true on success.
    [[nodiscard]] bool to_int(int64_t& out) const;

    /// Try to convert to double. Returns true on success.
    [[nodiscard]] bool to_float(double& out) const;

    /// Try to convert to bool. Returns true on success.
    [[nodiscard]] bool to_bool_val(bool& out) const;
};

/// Arithmetic on constants.
[[nodiscard]] ConstValue const_add(const ConstValue& a, const ConstValue& b);
[[nodiscard]] ConstValue const_sub(const ConstValue& a, const ConstValue& b);
[[nodiscard]] ConstValue const_mul(const ConstValue& a, const ConstValue& b);
[[nodiscard]] ConstValue const_div(const ConstValue& a, const ConstValue& b);
[[nodiscard]] ConstValue const_mod(const ConstValue& a, const ConstValue& b);
[[nodiscard]] ConstValue const_neg(const ConstValue& a);
[[nodiscard]] ConstValue const_not(const ConstValue& a);

/// Comparison on constants. Returns a bool ConstValue.
[[nodiscard]] ConstValue const_eq(const ConstValue& a, const ConstValue& b);
[[nodiscard]] ConstValue const_neq(const ConstValue& a, const ConstValue& b);
[[nodiscard]] ConstValue const_lt(const ConstValue& a, const ConstValue& b);
[[nodiscard]] ConstValue const_le(const ConstValue& a, const ConstValue& b);
[[nodiscard]] ConstValue const_gt(const ConstValue& a, const ConstValue& b);
[[nodiscard]] ConstValue const_ge(const ConstValue& a, const ConstValue& b);

} // namespace sema
} // namespace golangc
