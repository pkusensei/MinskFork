project(tests)

add_executable(${PROJECT_NAME} src/test_main.cpp
	src/Evaluation_tests.cpp
	src/Lexer_tests.cpp
	src/Parser_tests.cpp
	src/SourceText_tests.cpp
	src/AnnotatedText.cpp
	src/AssertingHelper.cpp)

target_include_directories(${PROJECT_NAME} 
	PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/include)

find_package(Catch2 REQUIRED)
target_link_libraries(${PROJECT_NAME} PRIVATE Catch2::Catch2)

target_link_libraries(${PROJECT_NAME} PUBLIC MCF)
target_compile_features(${PROJECT_NAME} PRIVATE cxx_std_17)

include(CTest)
include(Catch)
catch_discover_tests(${PROJECT_NAME})