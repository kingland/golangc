#include "common/string_interner.hpp"

#include <gtest/gtest.h>

using namespace golangc;

TEST(StringInternerTest, InternNewString) {
    StringInterner interner;
    auto sv = interner.intern("hello");
    EXPECT_EQ(sv, "hello");
    EXPECT_EQ(interner.size(), 1u);
}

TEST(StringInternerTest, InternDuplicateReturnsSameView) {
    StringInterner interner;
    auto sv1 = interner.intern("hello");
    auto sv2 = interner.intern("hello");

    // Must return the exact same pointer (same interned string)
    EXPECT_EQ(sv1.data(), sv2.data());
    EXPECT_EQ(interner.size(), 1u);
}

TEST(StringInternerTest, InternDifferentStrings) {
    StringInterner interner;
    auto sv1 = interner.intern("hello");
    auto sv2 = interner.intern("world");

    EXPECT_NE(sv1, sv2);
    EXPECT_EQ(interner.size(), 2u);
}

TEST(StringInternerTest, Contains) {
    StringInterner interner;
    (void)interner.intern("hello");

    EXPECT_TRUE(interner.contains("hello"));
    EXPECT_FALSE(interner.contains("world"));
}

TEST(StringInternerTest, Clear) {
    StringInterner interner;
    (void)interner.intern("hello");
    (void)interner.intern("world");
    interner.clear();

    EXPECT_EQ(interner.size(), 0u);
    EXPECT_FALSE(interner.contains("hello"));
}

TEST(StringInternerTest, InternFromTemporary) {
    StringInterner interner;

    // Intern a string constructed from a temporary - the view must remain valid
    std::string temp = "temporary";
    auto sv = interner.intern(temp);
    temp = "modified"; // modify the original

    // The interned view should still be valid and unchanged
    EXPECT_EQ(sv, "temporary");
}

TEST(StringInternerTest, EmptyString) {
    StringInterner interner;
    auto sv = interner.intern("");
    EXPECT_EQ(sv, "");
    EXPECT_EQ(interner.size(), 1u);
}
