// test_runtime_leaks.cpp — Memory leak detection for golangc_runtime.
//
// Strategy: use MSVC's _CrtSetAllocHook to intercept every malloc/free pair
// and maintain a net-allocation counter.  Each TEST wraps a runtime operation
// in a LeakScope that asserts the counter returns to zero.

#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <windows.h>
#define _CRTDBG_MAP_ALLOC
#include <crtdbg.h>
#include <cstdlib>
#include <cstring>
#include <cstdint>
#include <atomic>

#include <gtest/gtest.h>

// Pull in the full runtime API
#include "runtime/runtime.hpp"

// ============================================================================
// Allocation tracking hook
// ============================================================================

static std::atomic<int64_t> g_net_allocs{0};

static int alloc_hook(int alloc_type, void* /*user_data*/, size_t /*size*/,
                      int block_type, long /*request_number*/,
                      const unsigned char* /*filename*/, int /*line_number*/) {
    // Only track normal heap blocks (ignore CRT internal allocations)
    if (block_type != _NORMAL_BLOCK) return TRUE;
    if (alloc_type == _HOOK_ALLOC)   ++g_net_allocs;
    if (alloc_type == _HOOK_FREE)    --g_net_allocs;
    // _HOOK_REALLOC counts as free+alloc: no net change — skip
    return TRUE;  // TRUE = allow the allocation to proceed
}

// ============================================================================
// RAII leak scope — reset counter, run body, assert zero net allocs
// ============================================================================

class LeakScope {
public:
    LeakScope() {
        g_net_allocs.store(0);
        _CrtSetAllocHook(alloc_hook);
    }
    ~LeakScope() {
        _CrtSetAllocHook(nullptr);
    }
    int64_t net() const { return g_net_allocs.load(); }
};

// Helper: make a GoString from a C string literal (points into .rodata — no alloc)
static GoString gs(const char* s) {
    return GoString{s, static_cast<int64_t>(strlen(s))};
}

// Helper: release an RC-tracked sret GoString (allocated via rc_alloc_string).
// All sret string functions now use rc_alloc_string; use golangc_release to free.
static void free_sret_string(char* sret) {
    char* ptr = nullptr;
    memcpy(&ptr, sret, sizeof(char*));
    golangc_release(ptr);
}

// Helper: free a sret []string slice (GoString array from regexp runtime).
// The GoStr.ptr fields are RC-allocated via rc_alloc_string (dup_string).
// The GoStr array itself is plain malloc'd.
static void free_sret_string_slice(char* sret) {
    struct GoStr { char* ptr; int64_t len; };
    struct GoSlice { GoStr* ptr; int64_t len; int64_t cap; };
    GoSlice* s = reinterpret_cast<GoSlice*>(sret);
    if (!s->ptr) return;
    for (int64_t i = 0; i < s->len; ++i)
        golangc_release(s->ptr[i].ptr);  // RC-tracked via dup_string
    free(s->ptr);                        // plain-malloc'd array header
}

// Helper: free a sret []int slice (int64_t array returned by runtime)
static void free_sret_int_slice(char* sret) {
    struct GoSlice { void* ptr; int64_t len; int64_t cap; };
    GoSlice* s = reinterpret_cast<GoSlice*>(sret);
    free(s->ptr);
}

// ============================================================================
// strings.Builder tests
// ============================================================================

TEST(RuntimeLeakTest, BuilderMakeAndFree) {
    LeakScope ls;
    golangc_builder* b = golangc_builder_make();
    golangc_builder_free(b);
    EXPECT_EQ(ls.net(), 0) << "strings.Builder make+free leaked " << ls.net() << " allocation(s)";
}

TEST(RuntimeLeakTest, BuilderWriteStringAndFree) {
    LeakScope ls;
    golangc_builder* b = golangc_builder_make();
    auto s = gs("hello world");
    golangc_builder_write_string(b, &s);
    golangc_builder_free(b);
    EXPECT_EQ(ls.net(), 0);
}

TEST(RuntimeLeakTest, BuilderStringAndFree) {
    // golangc_builder_string returns a sret GoString whose ptr is rc_alloc'd —
    // caller must golangc_release it.
    LeakScope ls;
    golangc_builder* b = golangc_builder_make();
    auto s = gs("abc");
    golangc_builder_write_string(b, &s);

    char sret[16] = {};
    golangc_builder_string(sret, b);

    // Release the returned string buffer via RC
    char* ptr = nullptr;
    memcpy(&ptr, sret, sizeof(char*));
    golangc_release(ptr);

    golangc_builder_free(b);
    EXPECT_EQ(ls.net(), 0);
}

// ============================================================================
// strings.Replacer tests
// ============================================================================

TEST(RuntimeLeakTest, StringsReplacerMakeAndFree) {
    LeakScope ls;
    auto old_s = gs("foo");
    auto new_s = gs("bar");
    golangc_strings_replacer* r = golangc_strings_new_replacer(&old_s, &new_s);
    golangc_strings_replacer_free(r);
    EXPECT_EQ(ls.net(), 0);
}

TEST(RuntimeLeakTest, StringsReplacerReplaceAndFree) {
    LeakScope ls;
    auto old_s = gs("foo");
    auto new_s = gs("bar");
    golangc_strings_replacer* r = golangc_strings_new_replacer(&old_s, &new_s);

    auto input = gs("foo is foo");
    char sret[16] = {};
    golangc_strings_replacer_replace(sret, r, &input);
    free_sret_string(sret);

    golangc_strings_replacer_free(r);
    EXPECT_EQ(ls.net(), 0);
}

TEST(RuntimeLeakTest, StringsReplacerNullOld) {
    // Replacer with empty old string — should still be freeable
    LeakScope ls;
    auto old_s = gs("");
    auto new_s = gs("X");
    golangc_strings_replacer* r = golangc_strings_new_replacer(&old_s, &new_s);
    golangc_strings_replacer_free(r);
    EXPECT_EQ(ls.net(), 0);
}

// ============================================================================
// sync.Map tests
// ============================================================================

TEST(RuntimeLeakTest, SyncMapNewAndFree) {
    LeakScope ls;
    golangc_sync_map* m = golangc_sync_map_new();
    golangc_sync_map_free(m);
    EXPECT_EQ(ls.net(), 0);
}

TEST(RuntimeLeakTest, SyncMapStoreDeleteAndFree) {
    LeakScope ls;
    golangc_sync_map* m = golangc_sync_map_new();
    golangc_sync_map_store(m, 1, 100);
    golangc_sync_map_store(m, 2, 200);
    golangc_sync_map_delete(m, 1);
    // Entry 2 still alive — freed by sync_map_free
    golangc_sync_map_free(m);
    EXPECT_EQ(ls.net(), 0);
}

TEST(RuntimeLeakTest, SyncMapMultipleEntriesFree) {
    LeakScope ls;
    golangc_sync_map* m = golangc_sync_map_new();
    for (int64_t i = 0; i < 64; ++i)
        golangc_sync_map_store(m, i, i * 10);
    golangc_sync_map_free(m);
    EXPECT_EQ(ls.net(), 0);
}

// ============================================================================
// sync.Once tests
// ============================================================================

TEST(RuntimeLeakTest, SyncOnceMakeAndFree) {
    LeakScope ls;
    golangc_sync_once* o = golangc_sync_once_new();
    golangc_sync_once_free(o);
    EXPECT_EQ(ls.net(), 0);
}

// ============================================================================
// sync.Mutex tests
// ============================================================================

TEST(RuntimeLeakTest, MutexMakeAndFree) {
    LeakScope ls;
    golangc_mutex* m = golangc_mutex_make();
    golangc_mutex_free(m);
    EXPECT_EQ(ls.net(), 0);
}

TEST(RuntimeLeakTest, MutexLockUnlockFree) {
    LeakScope ls;
    golangc_mutex* m = golangc_mutex_make();
    golangc_mutex_lock(m);
    golangc_mutex_unlock(m);
    golangc_mutex_free(m);
    EXPECT_EQ(ls.net(), 0);
}

// ============================================================================
// sync.WaitGroup tests
// ============================================================================

TEST(RuntimeLeakTest, WaitGroupMakeAndFree) {
    LeakScope ls;
    golangc_waitgroup* wg = golangc_waitgroup_make();
    golangc_waitgroup_free(wg);
    EXPECT_EQ(ls.net(), 0);
}

// ============================================================================
// bytes.Buffer tests
// ============================================================================

TEST(RuntimeLeakTest, BytesBufferMakeAndFree) {
    LeakScope ls;
    golangc_bytes_buffer* b = golangc_bytes_new_buffer();
    golangc_bytes_free(b);
    EXPECT_EQ(ls.net(), 0);
}

TEST(RuntimeLeakTest, BytesBufferWriteAndFree) {
    LeakScope ls;
    golangc_bytes_buffer* b = golangc_bytes_new_buffer();
    auto s = gs("hello");
    golangc_bytes_write_string(b, &s);
    golangc_bytes_write_byte(b, '!');
    // golangc_bytes_string returns a plain malloc'd string (not RC)
    char sret[16] = {};
    golangc_bytes_string(sret, b);
    free_sret_string(sret);  // plain free
    golangc_bytes_free(b);
    EXPECT_EQ(ls.net(), 0);
}

TEST(RuntimeLeakTest, BytesBufferGrowAndFree) {
    LeakScope ls;
    golangc_bytes_buffer* b = golangc_bytes_new_buffer();
    golangc_bytes_grow(b, 4096);
    golangc_bytes_free(b);
    EXPECT_EQ(ls.net(), 0);
}

// ============================================================================
// regexp tests
// ============================================================================

TEST(RuntimeLeakTest, RegexpCompileAndFree) {
    LeakScope ls;
    auto pat = gs("[0-9]+");
    golangc_regexp* r = golangc_regexp_compile(&pat);
    ASSERT_NE(r, nullptr);
    golangc_regexp_free(r);
    EXPECT_EQ(ls.net(), 0);
}

TEST(RuntimeLeakTest, RegexpFindStringAndFree) {
    LeakScope ls;
    auto pat = gs("[a-z]+");
    golangc_regexp* r = golangc_regexp_compile(&pat);
    ASSERT_NE(r, nullptr);

    auto input = gs("hello123");
    char sret[16] = {};
    golangc_regexp_find_string(sret, r, &input);
    free_sret_string(sret);

    golangc_regexp_free(r);
    EXPECT_EQ(ls.net(), 0);
}

TEST(RuntimeLeakTest, RegexpFindAllStringAndFree) {
    LeakScope ls;
    auto pat = gs("[0-9]+");
    golangc_regexp* r = golangc_regexp_compile(&pat);
    ASSERT_NE(r, nullptr);

    auto input = gs("a1b22c333");
    char sret[24] = {};
    golangc_regexp_find_all_string(sret, r, &input, -1);
    free_sret_string_slice(sret);

    golangc_regexp_free(r);
    EXPECT_EQ(ls.net(), 0);
}

TEST(RuntimeLeakTest, RegexpFindStringSubmatchAndFree) {
    LeakScope ls;
    auto pat = gs("([a-z]+)([0-9]+)");
    golangc_regexp* r = golangc_regexp_compile(&pat);
    ASSERT_NE(r, nullptr);

    auto input = gs("abc123");
    char sret[24] = {};
    golangc_regexp_find_string_submatch(sret, r, &input);
    free_sret_string_slice(sret);

    golangc_regexp_free(r);
    EXPECT_EQ(ls.net(), 0);
}

TEST(RuntimeLeakTest, RegexpReplaceAllStringAndFree) {
    LeakScope ls;
    auto pat  = gs("[aeiou]");
    auto repl = gs("*");
    golangc_regexp* r = golangc_regexp_compile(&pat);
    ASSERT_NE(r, nullptr);

    auto input = gs("hello world");
    char sret[16] = {};
    golangc_regexp_replace_all_string(sret, r, &input, &repl);
    free_sret_string(sret);

    golangc_regexp_free(r);
    EXPECT_EQ(ls.net(), 0);
}

TEST(RuntimeLeakTest, RegexpSplitAndFree) {
    LeakScope ls;
    auto pat = gs(",");
    golangc_regexp* r = golangc_regexp_compile(&pat);
    ASSERT_NE(r, nullptr);

    auto input = gs("a,b,c,d");
    char sret[24] = {};
    golangc_regexp_split(sret, r, &input, -1);
    free_sret_string_slice(sret);

    golangc_regexp_free(r);
    EXPECT_EQ(ls.net(), 0);
}

TEST(RuntimeLeakTest, RegexpFindStringIndexAndFree) {
    LeakScope ls;
    auto pat = gs("[0-9]+");
    golangc_regexp* r = golangc_regexp_compile(&pat);
    ASSERT_NE(r, nullptr);

    auto input = gs("abc123def");
    char sret[24] = {};
    golangc_regexp_find_string_index(sret, r, &input);
    free_sret_int_slice(sret);

    golangc_regexp_free(r);
    EXPECT_EQ(ls.net(), 0);
}

// ============================================================================
// RC (reference counting) tests
// ============================================================================

TEST(RuntimeLeakTest, RCStringRetainRelease) {
    LeakScope ls;
    // rc_alloc_string is internal; use golangc_string_concat which calls it
    auto a = gs("hello");
    auto b = gs(" world");
    char sret[16] = {};
    golangc_string_concat(sret, a.ptr, a.len, b.ptr, b.len);

    char* ptr = nullptr;
    memcpy(&ptr, sret, sizeof(char*));
    golangc_retain(ptr);
    golangc_release(ptr);  // undo retain
    golangc_release(ptr);  // release original refcount=1 → free
    EXPECT_EQ(ls.net(), 0);
}

// ============================================================================
// golangc_map tests
// ============================================================================

TEST(RuntimeLeakTest, MapMakeAndFree) {
    LeakScope ls;
    golangc_map* m = golangc_map_make(8, 8);
    // golangc_map is RC'd (tag=2) — release to free
    golangc_release(m);
    EXPECT_EQ(ls.net(), 0);
}

TEST(RuntimeLeakTest, MapSetGetAndFree) {
    LeakScope ls;
    golangc_map* m = golangc_map_make(8, 8);
    int64_t key = 42, val = 99;
    golangc_map_set(m, &key, &val);
    int64_t ok = 0;
    void* got = golangc_map_get(m, &key, &ok);
    EXPECT_EQ(ok, 1);
    EXPECT_EQ(*static_cast<int64_t*>(got), 99);
    golangc_release(m);
    EXPECT_EQ(ls.net(), 0);
}

TEST(RuntimeLeakTest, MapIteratorMakeAndFree) {
    LeakScope ls;
    golangc_map* m = golangc_map_make(8, 8);
    int64_t k1 = 1, v1 = 10, k2 = 2, v2 = 20;
    golangc_map_set(m, &k1, &v1);
    golangc_map_set(m, &k2, &v2);

    golangc_map_iter* it = golangc_map_iter_make(m);
    int64_t out_k = 0, out_v = 0;
    while (golangc_map_iter_next(it, &out_k, &out_v)) {}
    golangc_map_iter_free(it);

    golangc_release(m);
    EXPECT_EQ(ls.net(), 0);
}

// ============================================================================
// strings.TrimFunc sret ownership
// ============================================================================

TEST(RuntimeLeakTest, StringsTrimFuncAndFree) {
    LeakScope ls;
    auto input = gs("  hello  ");
    auto is_space = [](int64_t c) -> int64_t { return c == ' ' ? 1 : 0; };
    char sret[16] = {};
    golangc_strings_trim_func(sret, &input, is_space);
    free_sret_string(sret);
    EXPECT_EQ(ls.net(), 0);
}

// ============================================================================
// Main
// ============================================================================

int main(int argc, char** argv) {
    ::testing::InitGoogleTest(&argc, argv);
    // Enable CRT debug heap so _CrtSetAllocHook works
    _CrtSetDbgFlag(_CRTDBG_ALLOC_MEM_DF);
    return RUN_ALL_TESTS();
}
