#include "common/arena_allocator.hpp"

#include <gtest/gtest.h>

using namespace golangc;

TEST(ArenaAllocatorTest, BasicAllocation) {
    ArenaAllocator arena;
    void* ptr = arena.allocate(64);
    EXPECT_NE(ptr, nullptr);
    EXPECT_EQ(arena.total_allocated(), 64u);
}

TEST(ArenaAllocatorTest, MultipleAllocations) {
    ArenaAllocator arena;
    void* ptr1 = arena.allocate(32);
    void* ptr2 = arena.allocate(64);

    EXPECT_NE(ptr1, nullptr);
    EXPECT_NE(ptr2, nullptr);
    EXPECT_NE(ptr1, ptr2);
    EXPECT_EQ(arena.total_allocated(), 96u);
}

TEST(ArenaAllocatorTest, Alignment) {
    ArenaAllocator arena;
    // Allocate 1 byte to offset alignment
    (void)arena.allocate(1, 1);
    // Now allocate with 16-byte alignment
    void* ptr = arena.allocate(32, 16);
    EXPECT_EQ(reinterpret_cast<uintptr_t>(ptr) % 16, 0u);
}

TEST(ArenaAllocatorTest, CreateObject) {
    ArenaAllocator arena;

    struct Point {
        int x, y;
    };

    Point* p = arena.create<Point>(10, 20);
    EXPECT_NE(p, nullptr);
    EXPECT_EQ(p->x, 10);
    EXPECT_EQ(p->y, 20);
}

TEST(ArenaAllocatorTest, AllocateArray) {
    ArenaAllocator arena;
    int* arr = arena.allocate_array<int>(10);
    EXPECT_NE(arr, nullptr);

    // Default-constructed ints should be 0
    for (int i = 0; i < 10; ++i) {
        EXPECT_EQ(arr[i], 0);
    }

    // Should be writable
    for (int i = 0; i < 10; ++i) {
        arr[i] = i * 2;
    }
    for (int i = 0; i < 10; ++i) {
        EXPECT_EQ(arr[i], i * 2);
    }
}

TEST(ArenaAllocatorTest, LargeAllocation) {
    ArenaAllocator arena(1024); // small block size
    // Allocate more than block size
    void* ptr = arena.allocate(2048);
    EXPECT_NE(ptr, nullptr);
    EXPECT_EQ(arena.total_allocated(), 2048u);
}

TEST(ArenaAllocatorTest, Reset) {
    ArenaAllocator arena;
    (void)arena.allocate(128);
    (void)arena.allocate(256);
    arena.reset();

    EXPECT_EQ(arena.total_allocated(), 0u);
    EXPECT_EQ(arena.block_count(), 0u);
}

TEST(ArenaAllocatorTest, ManySmallAllocations) {
    ArenaAllocator arena(256); // small block size
    for (int i = 0; i < 1000; ++i) {
        void* ptr = arena.allocate(16);
        EXPECT_NE(ptr, nullptr);
    }
    EXPECT_EQ(arena.total_allocated(), 16000u);
}
