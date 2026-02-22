# Common utilities library (header-only for now, but we create a target for dependencies)
add_library(golangc_common STATIC
    common/diagnostic.cpp
    common/string_interner.cpp
)
target_include_directories(golangc_common PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(golangc_common PUBLIC fmt::fmt)

# Lexer library
add_library(golangc_lexer STATIC
    lexer/lexer.cpp
)
target_include_directories(golangc_lexer PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(golangc_lexer PUBLIC golangc_common)

# AST library
add_library(golangc_ast STATIC
    ast/ast.cpp
    ast/ast_printer.cpp
)
target_include_directories(golangc_ast PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(golangc_ast PUBLIC golangc_common)

# Parser library
add_library(golangc_parser STATIC
    parser/parser.cpp
)
target_include_directories(golangc_parser PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(golangc_parser PUBLIC golangc_lexer golangc_ast)

# Semantic analysis library
add_library(golangc_sema STATIC
    sema/types.cpp
    sema/scope.cpp
    sema/constant.cpp
    sema/universe.cpp
    sema/checker.cpp
    sema/checker_type.cpp
    sema/checker_decl.cpp
    sema/checker_expr.cpp
    sema/checker_stmt.cpp
)
target_include_directories(golangc_sema PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(golangc_sema PUBLIC golangc_ast golangc_common)

# IR library
add_library(golangc_ir STATIC
    ir/ir.cpp
    ir/ir_type_map.cpp
    ir/ir_builder.cpp
    ir/ir_gen.cpp
    ir/ir_gen_decl.cpp
    ir/ir_gen_expr.cpp
    ir/ir_gen_stmt.cpp
    ir/ir_printer.cpp
)
target_include_directories(golangc_ir PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(golangc_ir PUBLIC golangc_sema golangc_ast golangc_common)

# Code generation library
add_library(golangc_codegen STATIC
    codegen/x64_codegen.cpp
    codegen/x64_codegen_inst.cpp
)
target_include_directories(golangc_codegen PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(golangc_codegen PUBLIC golangc_ir golangc_common)

# Runtime library (linked into generated executables)
add_library(golangc_runtime STATIC
    runtime/runtime.cpp
    runtime/goroutine_channel.cpp
)
target_include_directories(golangc_runtime PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

# Compiler driver executable
add_executable(golangc
    driver/main.cpp
)
target_link_libraries(golangc PRIVATE golangc_parser golangc_sema golangc_ir golangc_codegen)
