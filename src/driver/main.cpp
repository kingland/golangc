#include "ast/ast_printer.hpp"
#include "common/diagnostic.hpp"
#include "ir/ir_gen.hpp"
#include "ir/ir_printer.hpp"
#include "lexer/lexer.hpp"
#include "parser/parser.hpp"
#include "sema/checker.hpp"

#include <fmt/format.h>

#include <cstdlib>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <string_view>

namespace {

void print_usage(std::string_view program) {
    fmt::print("Usage: {} [options] <source.go>\n", program);
    fmt::print("\nOptions:\n");
    fmt::print("  --help          Show this help message\n");
    fmt::print("  --version       Show version information\n");
    fmt::print("  --dump-tokens   Dump lexer tokens and exit\n");
    fmt::print("  --dump-ast      Dump parsed AST and exit\n");
    fmt::print("  --check         Run semantic analysis and exit\n");
    fmt::print("  --dump-ir       Dump IR and exit\n");
}

void print_version() {
    fmt::print("golangc 0.4.0\n");
    fmt::print("Go compiler implemented in C++\n");
}

std::string read_file(const std::string& path) {
    std::ifstream file(path);
    if (!file.is_open()) {
        fmt::print(stderr, "error: cannot open file '{}'\n", path);
        std::exit(1);
    }
    std::ostringstream ss;
    ss << file.rdbuf();
    return ss.str();
}

} // namespace

int main(int argc, char* argv[]) {
    if (argc < 2) {
        print_usage(argv[0]);
        return 1;
    }

    bool dump_tokens = false;
    bool dump_ast = false;
    bool check_only = false;
    bool dump_ir = false;
    std::string source_file;

    for (int i = 1; i < argc; ++i) {
        std::string_view arg = argv[i];
        if (arg == "--help") {
            print_usage(argv[0]);
            return 0;
        } else if (arg == "--version") {
            print_version();
            return 0;
        } else if (arg == "--dump-tokens") {
            dump_tokens = true;
        } else if (arg == "--dump-ast") {
            dump_ast = true;
        } else if (arg == "--check") {
            check_only = true;
        } else if (arg == "--dump-ir") {
            dump_ir = true;
        } else if (arg[0] == '-') {
            fmt::print(stderr, "error: unknown option '{}'\n", arg);
            return 1;
        } else {
            source_file = std::string(arg);
        }
    }

    if (source_file.empty()) {
        fmt::print(stderr, "error: no input file\n");
        return 1;
    }

    std::string source = read_file(source_file);

    golangc::DiagnosticEngine diag;
    diag.set_handler([](const golangc::Diagnostic& d) {
        fmt::print(stderr, "{}\n", golangc::format_diagnostic(d));
    });

    golangc::Lexer lexer(source, source_file, diag);

    if (dump_tokens) {
        auto tokens = lexer.tokenize_all();
        for (const auto& tok : tokens) {
            fmt::print("{:>4}:{:<3} {:20s} '{}'\n", tok.location.position.line,
                       tok.location.position.column, golangc::token_kind_to_string(tok.kind),
                       tok.text);
        }
        return diag.has_errors() ? 1 : 0;
    }

    golangc::Parser parser(lexer, diag);
    if (!parser.parse()) {
        fmt::print(stderr, "compilation failed with {} error(s)\n", diag.error_count());
        return 1;
    }

    if (dump_ast) {
        golangc::ast::AstPrinter printer;
        fmt::print("{}", printer.print(parser.file()));
        return 0;
    }

    if (check_only) {
        golangc::sema::Checker checker(diag);
        if (!checker.check(parser.file())) {
            fmt::print(stderr, "type checking failed with {} error(s)\n", diag.error_count());
            return 1;
        }
        fmt::print("type checking succeeded\n");
        return 0;
    }

    if (dump_ir) {
        golangc::sema::Checker checker(diag);
        if (!checker.check(parser.file())) {
            fmt::print(stderr, "type checking failed with {} error(s)\n", diag.error_count());
            return 1;
        }
        golangc::ir::IRGenerator gen(checker);
        auto module = gen.generate(parser.file());
        golangc::ir::IRPrinter printer;
        fmt::print("{}", printer.print(*module));
        return 0;
    }

    fmt::print("compilation succeeded\n");
    return 0;
}
