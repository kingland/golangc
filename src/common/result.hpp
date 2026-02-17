#pragma once

#include <string>
#include <variant>

namespace golangc {

/// A simple Result type for error handling.
/// On MSVC with C++23, std::expected may be available, but we provide
/// a portable fallback that works across all compilers.
template <typename T, typename E = std::string>
class Result {
public:
    /// Construct a success result.
    [[nodiscard]] static Result ok(T value) { return Result(std::move(value)); }

    /// Construct an error result.
    [[nodiscard]] static Result err(E error) { return Result(InError{std::move(error)}); }

    /// Check if this is a success result.
    [[nodiscard]] bool is_ok() const { return std::holds_alternative<T>(data_); }

    /// Check if this is an error result.
    [[nodiscard]] bool is_err() const { return std::holds_alternative<InError>(data_); }

    /// Get the success value. Undefined behavior if is_err().
    [[nodiscard]] T& value() & { return std::get<T>(data_); }
    [[nodiscard]] const T& value() const& { return std::get<T>(data_); }
    [[nodiscard]] T&& value() && { return std::get<T>(std::move(data_)); }

    /// Get the error value. Undefined behavior if is_ok().
    [[nodiscard]] E& error() & { return std::get<InError>(data_).err; }
    [[nodiscard]] const E& error() const& { return std::get<InError>(data_).err; }

    /// Explicit bool conversion: true if ok.
    [[nodiscard]] explicit operator bool() const { return is_ok(); }

private:
    // Wrap E so T and E can be the same type
    struct InError {
        E err;
    };

    explicit Result(T value) : data_(std::move(value)) {}
    explicit Result(InError error) : data_(std::move(error)) {}

    std::variant<T, InError> data_;
};

/// Specialization for void success type.
template <typename E>
class Result<void, E> {
public:
    [[nodiscard]] static Result ok() { return Result(true); }
    [[nodiscard]] static Result err(E error) { return Result(std::move(error)); }

    [[nodiscard]] bool is_ok() const { return std::holds_alternative<bool>(data_); }
    [[nodiscard]] bool is_err() const { return std::holds_alternative<E>(data_); }

    [[nodiscard]] E& error() & { return std::get<E>(data_); }
    [[nodiscard]] const E& error() const& { return std::get<E>(data_); }

    [[nodiscard]] explicit operator bool() const { return is_ok(); }

private:
    explicit Result(bool) : data_(true) {}
    explicit Result(E error) : data_(std::move(error)) {}

    std::variant<bool, E> data_;
};

} // namespace golangc
