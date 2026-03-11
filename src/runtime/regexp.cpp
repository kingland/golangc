// regexp.cpp — Go regexp package runtime (Phase 38G)
// Uses C++11 std::regex (ECMAScript flavor) as the engine.
// Go uses RE2 syntax which is largely compatible with ECMAScript for common patterns.

#include "runtime/runtime.hpp"

#include <cstdlib>
#include <cstring>
#include <regex>
#include <string>
#include <vector>

// RC allocator from rc.cpp
extern void* rc_alloc_string(size_t bytes);

// ============================================================================
// Internal helpers
// ============================================================================

struct golangc_regexp {
    std::string  pattern;
    std::regex   re;
    int          num_subexp; // number of capturing groups
    bool         valid;
};

static std::string gs_to_string(const GoString* s) {
    if (!s || !s->ptr || s->len == 0) return {};
    return std::string(s->ptr, static_cast<size_t>(s->len));
}

static char* dup_string(const std::string& s) {
    char* p = static_cast<char*>(rc_alloc_string(s.size() + 1));
    if (p) {
        memcpy(p, s.data(), s.size());
        p[s.size()] = '\0';
    }
    return p;
}

// Write a GoString (16-byte {ptr*, len}) into sret_out
static void write_gostring(char* sret_out, const char* ptr, int64_t len) {
    struct GoStrOut { const char* ptr; int64_t len; };
    GoStrOut* out = reinterpret_cast<GoStrOut*>(sret_out);
    out->ptr = ptr;
    out->len = len;
}

// Build a []string slice header from a vector of strings.
// Allocates memory for both the GoString array and each string content.
static void write_string_slice(char* sret_out, const std::vector<std::string>& strs) {
    struct GoSliceHdr { void* ptr; int64_t len; int64_t cap; };
    GoSliceHdr* out = reinterpret_cast<GoSliceHdr*>(sret_out);

    if (strs.empty()) {
        out->ptr = nullptr; out->len = 0; out->cap = 0;
        return;
    }

    // Allocate array of GoString structs
    struct GoStr { const char* ptr; int64_t len; };
    GoStr* arr = static_cast<GoStr*>(malloc(strs.size() * sizeof(GoStr)));
    for (size_t i = 0; i < strs.size(); ++i) {
        arr[i].ptr = dup_string(strs[i]);
        arr[i].len = static_cast<int64_t>(strs[i].size());
    }
    out->ptr = arr;
    out->len = static_cast<int64_t>(strs.size());
    out->cap = out->len;
}

// Build a []int slice header from a vector of int64
static void write_int_slice(char* sret_out, const std::vector<int64_t>& ints) {
    struct GoSliceHdr { void* ptr; int64_t len; int64_t cap; };
    GoSliceHdr* out = reinterpret_cast<GoSliceHdr*>(sret_out);

    if (ints.empty()) {
        out->ptr = nullptr; out->len = 0; out->cap = 0;
        return;
    }

    int64_t* arr = static_cast<int64_t*>(malloc(ints.size() * sizeof(int64_t)));
    for (size_t i = 0; i < ints.size(); ++i) arr[i] = ints[i];
    out->ptr = arr;
    out->len = static_cast<int64_t>(ints.size());
    out->cap = out->len;
}

// Count capturing groups in pattern string (simplified: count unescaped '(')
static int count_groups(const std::string& pattern) {
    int n = 0;
    bool escaped = false;
    bool in_class = false;
    for (char c : pattern) {
        if (escaped)         { escaped = false; continue; }
        if (c == '\\')       { escaped = true; continue; }
        if (c == '[')        { in_class = true; continue; }
        if (c == ']')        { in_class = false; continue; }
        if (in_class)        continue;
        if (c == '(' )       { ++n; }
    }
    return n;
}

// ============================================================================
// Public API
// ============================================================================

golangc_regexp* golangc_regexp_compile(const GoString* pattern) {
    std::string pat = gs_to_string(pattern);
    golangc_regexp* r = new golangc_regexp();
    r->pattern    = pat;
    r->num_subexp = count_groups(pat);
    r->valid      = false;
    try {
        r->re    = std::regex(pat, std::regex::ECMAScript | std::regex::optimize);
        r->valid = true;
    } catch (...) {
        // Leave valid=false; callers check for null or invalid
    }
    return r;
}

golangc_regexp* golangc_regexp_must_compile(const GoString* pattern) {
    golangc_regexp* r = golangc_regexp_compile(pattern);
    if (!r->valid) {
        fprintf(stderr, "regexp: MustCompile: invalid pattern\n");
        exit(1);
    }
    return r;
}

int64_t golangc_regexp_match_string_pkg(const GoString* pattern, const GoString* s) {
    golangc_regexp* r = golangc_regexp_compile(pattern);
    if (!r->valid) { delete r; return 0; }
    int64_t result = golangc_regexp_match_string(r, s);
    delete r;
    return result;
}

int64_t golangc_regexp_match_string(golangc_regexp* r, const GoString* s) {
    if (!r || !r->valid) return 0;
    std::string str = gs_to_string(s);
    return std::regex_search(str, r->re) ? 1 : 0;
}

void golangc_regexp_find_string(char* sret_out, golangc_regexp* r, const GoString* s) {
    if (!r || !r->valid) { write_gostring(sret_out, nullptr, 0); return; }
    std::string str = gs_to_string(s);
    std::smatch m;
    if (std::regex_search(str, m, r->re)) {
        std::string match = m[0].str();
        char* p = dup_string(match);
        write_gostring(sret_out, p, static_cast<int64_t>(match.size()));
    } else {
        write_gostring(sret_out, nullptr, 0);
    }
}

void golangc_regexp_find_all_string(char* sret_out, golangc_regexp* r,
                                     const GoString* s, int64_t n) {
    if (!r || !r->valid) { write_string_slice(sret_out, {}); return; }
    std::string str = gs_to_string(s);
    std::vector<std::string> results;
    auto begin = std::sregex_iterator(str.begin(), str.end(), r->re);
    auto end   = std::sregex_iterator();
    for (auto it = begin; it != end; ++it) {
        if (n >= 0 && static_cast<int64_t>(results.size()) >= n) break;
        results.push_back((*it)[0].str());
    }
    write_string_slice(sret_out, results);
}

void golangc_regexp_find_string_submatch(char* sret_out, golangc_regexp* r, const GoString* s) {
    if (!r || !r->valid) { write_string_slice(sret_out, {}); return; }
    std::string str = gs_to_string(s);
    std::smatch m;
    std::vector<std::string> results;
    if (std::regex_search(str, m, r->re)) {
        for (size_t i = 0; i < m.size(); ++i)
            results.push_back(m[i].matched ? m[i].str() : "");
    }
    write_string_slice(sret_out, results);
}

void golangc_regexp_replace_all_string(char* sret_out, golangc_regexp* r,
                                        const GoString* s, const GoString* repl) {
    if (!r || !r->valid) {
        std::string orig = gs_to_string(s);
        char* p = dup_string(orig);
        write_gostring(sret_out, p, static_cast<int64_t>(orig.size()));
        return;
    }
    std::string str  = gs_to_string(s);
    std::string rep  = gs_to_string(repl);
    // Convert Go backreference syntax ($1) to ECMAScript ($1) — already compatible
    std::string result = std::regex_replace(str, r->re, rep);
    char* p = dup_string(result);
    write_gostring(sret_out, p, static_cast<int64_t>(result.size()));
}

void golangc_regexp_replace_all_literal_string(char* sret_out, golangc_regexp* r,
                                                const GoString* s, const GoString* repl) {
    if (!r || !r->valid) {
        std::string orig = gs_to_string(s);
        char* p = dup_string(orig);
        write_gostring(sret_out, p, static_cast<int64_t>(orig.size()));
        return;
    }
    std::string str = gs_to_string(s);
    std::string rep = gs_to_string(repl);
    // Literal replacement: escape any '$' in repl so they're not treated as backrefs
    std::string escaped_rep;
    escaped_rep.reserve(rep.size() * 2);
    for (char c : rep) {
        if (c == '$') escaped_rep += "$$";
        else          escaped_rep += c;
    }
    std::string result = std::regex_replace(str, r->re, escaped_rep);
    char* p = dup_string(result);
    write_gostring(sret_out, p, static_cast<int64_t>(result.size()));
}

void golangc_regexp_split(char* sret_out, golangc_regexp* r,
                           const GoString* s, int64_t n) {
    if (!r || !r->valid) {
        std::string str = gs_to_string(s);
        write_string_slice(sret_out, {str});
        return;
    }
    std::string str = gs_to_string(s);
    std::vector<std::string> parts;

    auto begin = std::sregex_iterator(str.begin(), str.end(), r->re);
    auto end   = std::sregex_iterator();
    size_t last = 0;
    for (auto it = begin; it != end; ++it) {
        if (n >= 0 && static_cast<int64_t>(parts.size()) >= n - 1) break;
        size_t match_pos = static_cast<size_t>(it->position());
        parts.push_back(str.substr(last, match_pos - last));
        last = match_pos + static_cast<size_t>(it->length());
    }
    parts.push_back(str.substr(last));
    write_string_slice(sret_out, parts);
}

void golangc_regexp_string(char* sret_out, golangc_regexp* r) {
    if (!r) { write_gostring(sret_out, nullptr, 0); return; }
    char* p = dup_string(r->pattern);
    write_gostring(sret_out, p, static_cast<int64_t>(r->pattern.size()));
}

int64_t golangc_regexp_num_subexp(golangc_regexp* r) {
    return r ? static_cast<int64_t>(r->num_subexp) : 0;
}

void golangc_regexp_find_string_index(char* sret_out, golangc_regexp* r, const GoString* s) {
    if (!r || !r->valid) { write_int_slice(sret_out, {}); return; }
    std::string str = gs_to_string(s);
    std::smatch m;
    if (std::regex_search(str, m, r->re)) {
        std::vector<int64_t> idx = {
            static_cast<int64_t>(m.position(0)),
            static_cast<int64_t>(m.position(0) + m.length(0))
        };
        write_int_slice(sret_out, idx);
    } else {
        write_int_slice(sret_out, {});
    }
}

void golangc_regexp_subexp_names(char* sret_out, golangc_regexp* r) {
    // std::regex doesn't expose named group names; return empty slice
    write_string_slice(sret_out, {});
    (void)r;
}

void golangc_regexp_free(golangc_regexp* r) {
    delete r;
}
