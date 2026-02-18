#include "ast/ast_printer.hpp"
#include "codegen/x64_codegen.hpp"
#include "common/diagnostic.hpp"
#include "ir/ir_gen.hpp"
#include "ir/ir_printer.hpp"
#include "lexer/lexer.hpp"
#include "parser/parser.hpp"
#include "sema/checker.hpp"

#include <fmt/format.h>

#include <cstdlib>
#include <filesystem>
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
    fmt::print("  --emit-asm      Generate x64 MASM assembly and exit\n");
    fmt::print("  -o <file>       Compile to executable\n");
}

void print_version() {
    fmt::print("golangc 0.5.0\n");
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

void write_file(const std::string& path, const std::string& content) {
    std::ofstream file(path);
    if (!file.is_open()) {
        fmt::print(stderr, "error: cannot write file '{}'\n", path);
        std::exit(1);
    }
    file << content;
}

/// Run the full pipeline: source → IR → asm → obj → exe.
/// Returns 0 on success, non-zero on failure.
int compile_to_exe(const std::string& asm_text,
                   const std::string& output_path,
                   const std::string& runtime_lib_path) {
    namespace fs = std::filesystem;

    // Create temp directory for intermediate files
    auto temp_dir = fs::temp_directory_path() / "golangc_build";
    fs::create_directories(temp_dir);

    auto asm_path = temp_dir / "output.asm";
    auto obj_path = temp_dir / "output.obj";

    // Write assembly to file
    write_file(asm_path.string(), asm_text);

    // Assemble with ml64
    std::string ml64_cmd = fmt::format(
        "ml64 /c /nologo /Fo\"{}\" \"{}\"",
        obj_path.string(), asm_path.string());

    fmt::print(stderr, "assembling: {}\n", ml64_cmd);
    int rc = std::system(ml64_cmd.c_str());
    if (rc != 0) {
        fmt::print(stderr, "error: ml64 failed with exit code {}\n", rc);
        return 1;
    }

    // Link with link.exe
    std::string link_cmd = fmt::format(
        "link /NOLOGO /SUBSYSTEM:CONSOLE /OUT:\"{}\" \"{}\"",
        output_path, obj_path.string());

    // Add runtime library if it exists
    if (!runtime_lib_path.empty() && fs::exists(runtime_lib_path)) {
        link_cmd += fmt::format(" \"{}\"", runtime_lib_path);
    }

    // Add system libraries
    link_cmd += " kernel32.lib legacy_stdio_definitions.lib msvcrt.lib ucrt.lib vcruntime.lib";

    fmt::print(stderr, "linking: {}\n", link_cmd);
    rc = std::system(link_cmd.c_str());
    if (rc != 0) {
        fmt::print(stderr, "error: link failed with exit code {}\n", rc);
        return 1;
    }

    fmt::print(stderr, "compiled: {}\n", output_path);

    // Cleanup temp files
    fs::remove(asm_path);
    fs::remove(obj_path);

    return 0;
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
    bool emit_asm = false;
    std::string output_file;
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
        } else if (arg == "--emit-asm") {
            emit_asm = true;
        } else if (arg == "-o") {
            if (i + 1 < argc) {
                output_file = argv[++i];
            } else {
                fmt::print(stderr, "error: -o requires an argument\n");
                return 1;
            }
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

    // From here on, we need IR
    golangc::sema::Checker checker(diag);
    if (!checker.check(parser.file())) {
        fmt::print(stderr, "type checking failed with {} error(s)\n", diag.error_count());
        return 1;
    }

    golangc::ir::IRGenerator gen(checker);
    auto module = gen.generate(parser.file());

    if (dump_ir) {
        golangc::ir::IRPrinter printer;
        fmt::print("{}", printer.print(*module));
        return 0;
    }

    // Code generation
    golangc::codegen::X64CodeGenerator codegen;
    std::string asm_text = codegen.generate(*module);

    if (emit_asm) {
        fmt::print("{}", asm_text);
        return 0;
    }

    if (!output_file.empty()) {
        // Find the runtime library — look relative to the executable
        namespace fs = std::filesystem;
        std::string runtime_lib;
        auto exe_dir = fs::path(argv[0]).parent_path();

        // Try several locations for the runtime lib
        std::vector<fs::path> search_paths = {
            exe_dir / "golangc_runtime.lib",
            exe_dir / ".." / "lib" / "Debug" / "golangc_runtime.lib",
            exe_dir / ".." / "lib" / "golangc_runtime.lib",
        };

        for (const auto& p : search_paths) {
            if (fs::exists(p)) {
                runtime_lib = fs::canonical(p).string();
                break;
            }
        }

        if (runtime_lib.empty()) {
            fmt::print(stderr, "warning: golangc_runtime.lib not found, linking may fail\n");
        }

        return compile_to_exe(asm_text, output_file, runtime_lib);
    }

    fmt::print("compilation succeeded\n");
    return 0;
}
