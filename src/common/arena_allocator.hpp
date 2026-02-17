#pragma once

#include <cassert>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <memory>
#include <vector>

namespace golangc {

/// A simple arena allocator that allocates memory in large blocks.
/// Memory is freed all at once when the arena is destroyed or reset.
/// This is ideal for AST nodes and other compiler data structures
/// that live for the duration of a compilation unit.
class ArenaAllocator {
public:
    static constexpr size_t kDefaultBlockSize = 64 * 1024; // 64 KB

    explicit ArenaAllocator(size_t block_size = kDefaultBlockSize)
        : block_size_(block_size) {}

    // Non-copyable, movable
    ArenaAllocator(const ArenaAllocator&)            = delete;
    ArenaAllocator& operator=(const ArenaAllocator&) = delete;
    ArenaAllocator(ArenaAllocator&&)                 = default;
    ArenaAllocator& operator=(ArenaAllocator&&)      = default;

    /// Allocate memory with the given size and alignment.
    [[nodiscard]] void* allocate(size_t size, size_t alignment = alignof(std::max_align_t)) {
        assert(alignment > 0 && (alignment & (alignment - 1)) == 0 && "alignment must be power of 2");

        // Align the current offset
        size_t aligned_offset = align_up(current_offset_, alignment);

        // Check if we need a new block
        if (blocks_.empty() || aligned_offset + size > block_size_) {
            allocate_block(std::max(size + alignment, block_size_));
            aligned_offset = align_up(current_offset_, alignment);
        }

        void* ptr = blocks_.back().get() + aligned_offset;
        current_offset_ = aligned_offset + size;
        total_allocated_ += size;
        return ptr;
    }

    /// Allocate and construct an object in the arena.
    template <typename T, typename... Args>
    [[nodiscard]] T* create(Args&&... args) {
        void* mem = allocate(sizeof(T), alignof(T));
        return new (mem) T(std::forward<Args>(args)...);
    }

    /// Allocate an array of objects in the arena (default-constructed).
    template <typename T>
    [[nodiscard]] T* allocate_array(size_t count) {
        void* mem = allocate(sizeof(T) * count, alignof(T));
        T* arr = static_cast<T*>(mem);
        for (size_t i = 0; i < count; ++i) {
            new (&arr[i]) T();
        }
        return arr;
    }

    /// Reset the arena, freeing all allocated memory.
    void reset() {
        blocks_.clear();
        current_offset_ = 0;
        total_allocated_ = 0;
    }

    /// Total bytes allocated from the arena.
    [[nodiscard]] size_t total_allocated() const { return total_allocated_; }

    /// Number of blocks allocated.
    [[nodiscard]] size_t block_count() const { return blocks_.size(); }

private:
    void allocate_block(size_t size) {
        blocks_.push_back(std::make_unique<uint8_t[]>(size));
        block_size_ = size; // update in case we needed a larger block
        current_offset_ = 0;
    }

    [[nodiscard]] static size_t align_up(size_t value, size_t alignment) {
        return (value + alignment - 1) & ~(alignment - 1);
    }

    std::vector<std::unique_ptr<uint8_t[]>> blocks_;
    size_t block_size_       = kDefaultBlockSize;
    size_t current_offset_   = 0;
    size_t total_allocated_  = 0;
};

} // namespace golangc
