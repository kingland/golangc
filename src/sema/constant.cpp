#include "sema/constant.hpp"

#include <fmt/format.h>

#include <cmath>

namespace golangc {
namespace sema {

std::string ConstValue::to_string() const {
    if (is_bool()) return as_bool() ? "true" : "false";
    if (is_int()) return fmt::format("{}", as_int());
    if (is_float()) return fmt::format("{}", as_float());
    if (is_string()) return fmt::format("\"{}\"", as_string());
    return "<invalid>";
}

bool ConstValue::to_int(int64_t& out) const {
    if (is_int()) { out = as_int(); return true; }
    if (is_float()) {
        double d = as_float();
        if (d == std::floor(d) && std::isfinite(d)) {
            out = static_cast<int64_t>(d);
            return true;
        }
        return false;
    }
    if (is_bool()) { out = as_bool() ? 1 : 0; return true; }
    return false;
}

bool ConstValue::to_float(double& out) const {
    if (is_float()) { out = as_float(); return true; }
    if (is_int()) { out = static_cast<double>(as_int()); return true; }
    return false;
}

bool ConstValue::to_bool_val(bool& out) const {
    if (is_bool()) { out = as_bool(); return true; }
    return false;
}

// ============================================================================
// Arithmetic helpers
// ============================================================================

namespace {

// Promote both values to a common numeric type for arithmetic
enum class NumKind { None, Int, Float };

NumKind common_kind(const ConstValue& a, const ConstValue& b) {
    if (a.is_float() || b.is_float()) return NumKind::Float;
    if (a.is_int() || b.is_int()) return NumKind::Int;
    return NumKind::None;
}

} // namespace

ConstValue const_add(const ConstValue& a, const ConstValue& b) {
    if (a.is_string() && b.is_string()) {
        return ConstValue(a.as_string() + b.as_string());
    }
    auto kind = common_kind(a, b);
    if (kind == NumKind::Float) {
        double da = 0, db = 0;
        if (a.to_float(da) && b.to_float(db)) return ConstValue(da + db);
    } else if (kind == NumKind::Int) {
        int64_t ia = 0, ib = 0;
        if (a.to_int(ia) && b.to_int(ib)) return ConstValue(ia + ib);
    }
    return ConstValue{};
}

ConstValue const_sub(const ConstValue& a, const ConstValue& b) {
    auto kind = common_kind(a, b);
    if (kind == NumKind::Float) {
        double da = 0, db = 0;
        if (a.to_float(da) && b.to_float(db)) return ConstValue(da - db);
    } else if (kind == NumKind::Int) {
        int64_t ia = 0, ib = 0;
        if (a.to_int(ia) && b.to_int(ib)) return ConstValue(ia - ib);
    }
    return ConstValue{};
}

ConstValue const_mul(const ConstValue& a, const ConstValue& b) {
    auto kind = common_kind(a, b);
    if (kind == NumKind::Float) {
        double da = 0, db = 0;
        if (a.to_float(da) && b.to_float(db)) return ConstValue(da * db);
    } else if (kind == NumKind::Int) {
        int64_t ia = 0, ib = 0;
        if (a.to_int(ia) && b.to_int(ib)) return ConstValue(ia * ib);
    }
    return ConstValue{};
}

ConstValue const_div(const ConstValue& a, const ConstValue& b) {
    auto kind = common_kind(a, b);
    if (kind == NumKind::Float) {
        double da = 0, db = 0;
        if (a.to_float(da) && b.to_float(db) && db != 0.0) return ConstValue(da / db);
    } else if (kind == NumKind::Int) {
        int64_t ia = 0, ib = 0;
        if (a.to_int(ia) && b.to_int(ib) && ib != 0) return ConstValue(ia / ib);
    }
    return ConstValue{};
}

ConstValue const_mod(const ConstValue& a, const ConstValue& b) {
    int64_t ia = 0, ib = 0;
    if (a.to_int(ia) && b.to_int(ib) && ib != 0) {
        return ConstValue(ia % ib);
    }
    return ConstValue{};
}

ConstValue const_neg(const ConstValue& a) {
    if (a.is_int()) return ConstValue(-a.as_int());
    if (a.is_float()) return ConstValue(-a.as_float());
    return ConstValue{};
}

ConstValue const_not(const ConstValue& a) {
    if (a.is_bool()) return ConstValue(!a.as_bool());
    return ConstValue{};
}

// ============================================================================
// Comparison helpers
// ============================================================================

ConstValue const_eq(const ConstValue& a, const ConstValue& b) {
    if (a.is_bool() && b.is_bool()) return ConstValue(a.as_bool() == b.as_bool());
    if (a.is_string() && b.is_string()) return ConstValue(a.as_string() == b.as_string());
    auto kind = common_kind(a, b);
    if (kind == NumKind::Float) {
        double da = 0, db = 0;
        if (a.to_float(da) && b.to_float(db)) return ConstValue(da == db);
    } else if (kind == NumKind::Int) {
        int64_t ia = 0, ib = 0;
        if (a.to_int(ia) && b.to_int(ib)) return ConstValue(ia == ib);
    }
    return ConstValue{};
}

ConstValue const_neq(const ConstValue& a, const ConstValue& b) {
    auto r = const_eq(a, b);
    if (r.is_bool()) return ConstValue(!r.as_bool());
    return ConstValue{};
}

ConstValue const_lt(const ConstValue& a, const ConstValue& b) {
    if (a.is_string() && b.is_string()) return ConstValue(a.as_string() < b.as_string());
    auto kind = common_kind(a, b);
    if (kind == NumKind::Float) {
        double da = 0, db = 0;
        if (a.to_float(da) && b.to_float(db)) return ConstValue(da < db);
    } else if (kind == NumKind::Int) {
        int64_t ia = 0, ib = 0;
        if (a.to_int(ia) && b.to_int(ib)) return ConstValue(ia < ib);
    }
    return ConstValue{};
}

ConstValue const_le(const ConstValue& a, const ConstValue& b) {
    if (a.is_string() && b.is_string()) return ConstValue(a.as_string() <= b.as_string());
    auto kind = common_kind(a, b);
    if (kind == NumKind::Float) {
        double da = 0, db = 0;
        if (a.to_float(da) && b.to_float(db)) return ConstValue(da <= db);
    } else if (kind == NumKind::Int) {
        int64_t ia = 0, ib = 0;
        if (a.to_int(ia) && b.to_int(ib)) return ConstValue(ia <= ib);
    }
    return ConstValue{};
}

ConstValue const_gt(const ConstValue& a, const ConstValue& b) {
    return const_lt(b, a);
}

ConstValue const_ge(const ConstValue& a, const ConstValue& b) {
    return const_le(b, a);
}

} // namespace sema
} // namespace golangc
