project(tests)

add_executable(mcf_test src/test_main.cpp
	src/Evaluation_tests.cpp
	src/Lexer_tests.cpp
	src/Parser_tests.cpp
	src/SourceText_tests.cpp
	src/AnnotatedText.cpp
	src/AssertingHelper.cpp)

target_include_directories(mcf_test 
	PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/include)

find_package(Catch2 REQUIRED)
target_link_libraries(mcf_test PRIVATE Catch2::Catch2)

target_link_libraries(mcf_test PUBLIC MCF)

include(CTest)
include(Catch)
catch_discover_tests(mcf_test)