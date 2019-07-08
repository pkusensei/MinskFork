cmake_minimum_required(VERSION 3.8)

project(tests LANGUAGES CXX)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY 
	${CMAKE_SOURCE_DIR}/build/${CMAKE_BUILD_TYPE})

enable_testing()

add_executable(mcf_test src/test_main.cpp
	src/Evaluation_tests.cpp
	src/Lexer_tests.cpp
	src/Parser_tests.cpp
	src/SourceText_tests.cpp
	src/AnnotatedText.cpp
	src/AssertingHelper.cpp)

target_include_directories(mcf_test 
	PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/include)

target_link_libraries(mcf_test PUBLIC MCF)

add_test(test_all mcf_test)
