#include "common/source_location.hpp"

#include <gtest/gtest.h>

using namespace golangc;

TEST(SourcePositionTest, DefaultConstruction) {
    SourcePosition pos;
    EXPECT_EQ(pos.line, 1u);
    EXPECT_EQ(pos.column, 1u);
}

TEST(SourcePositionTest, Comparison) {
    SourcePosition a{1, 5};
    SourcePosition b{1, 10};
    SourcePosition c{2, 1};

    EXPECT_LT(a, b);
    EXPECT_LT(b, c);
    EXPECT_LT(a, c);
    EXPECT_EQ(a, a);
}

TEST(SourceLocationTest, ToString) {
    SourceLocation loc{"main.go", {10, 5}, 42};
    EXPECT_EQ(loc.to_string(), "main.go:10:5");
}

TEST(SourceRangeTest, FromSingle) {
    SourceLocation loc{"test.go", {1, 1}, 0};
    auto range = SourceRange::from_single(loc);
    EXPECT_EQ(range.start.position.line, 1u);
    EXPECT_EQ(range.end.position.line, 1u);
}

TEST(SourceRangeTest, Merge) {
    SourceLocation start{"test.go", {1, 1}, 0};
    SourceLocation end{"test.go", {5, 10}, 50};
    auto range_a = SourceRange::from_single(start);
    auto range_b = SourceRange::from_single(end);
    auto merged = SourceRange::merge(range_a, range_b);
    EXPECT_EQ(merged.start.position.line, 1u);
    EXPECT_EQ(merged.end.position.line, 5u);
}
