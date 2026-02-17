# Common utilities tests
add_executable(test_common
    common/test_source_location.cpp
    common/test_diagnostic.cpp
    common/test_string_interner.cpp
    common/test_arena_allocator.cpp
)
target_link_libraries(test_common PRIVATE golangc_common GTest::gtest GTest::gtest_main)
add_test(NAME CommonTests COMMAND test_common)

# Lexer tests
add_executable(test_lexer
    lexer/test_lexer.cpp
)
target_link_libraries(test_lexer PRIVATE golangc_lexer GTest::gtest GTest::gtest_main)
add_test(NAME LexerTests COMMAND test_lexer)

# Parser tests
add_executable(test_parser
    parser/test_parser.cpp
)
target_link_libraries(test_parser PRIVATE golangc_parser golangc_ast GTest::gtest GTest::gtest_main)
add_test(NAME ParserTests COMMAND test_parser)

# Semantic analysis tests
add_executable(test_sema
    sema/test_types.cpp
    sema/test_scope.cpp
    sema/test_sema.cpp
)
target_link_libraries(test_sema PRIVATE golangc_sema golangc_parser golangc_lexer GTest::gtest GTest::gtest_main)
add_test(NAME SemaTests COMMAND test_sema)

# IR tests
add_executable(test_ir
    ir/test_ir.cpp
    ir/test_ir_gen.cpp
)
target_link_libraries(test_ir PRIVATE golangc_ir golangc_sema golangc_parser golangc_lexer GTest::gtest GTest::gtest_main)
add_test(NAME IRTests COMMAND test_ir)
