project(MCF)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY 
	${CMAKE_SOURCE_DIR}/build/${CMAKE_BUILD_TYPE})

add_library(MCF SHARED src/Binding.cpp
	src/BoundNode.cpp
	src/BoundNodePrinter.cpp
	src/Compilation.cpp
	src/ConsoleHelper.cpp
	src/ControlFlowGraph.cpp
	src/Conversion.cpp
	src/Diagnostic.cpp
	src/Emitter.cpp
	src/helpers.cpp
	src/IO.cpp
	src/Lexer.cpp
	src/Lowering.cpp
	src/Parsing.cpp
	src/SourceText.cpp
	src/SymbolPrinter.cpp
	src/Symbols.cpp
	src/SyntaxExpressions.cpp
	src/SyntaxKind.cpp
	src/SyntaxStatements.cpp
	src/SyntaxToken.cpp)

target_include_directories(
    ${PROJECT_NAME} PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include/>
    $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>)

find_package(LLVM REQUIRED)
target_link_libraries(
	${PROJECT_NAME} PRIVATE LLVMMC LLVMLTO LLVMMCA LLVMipo)
target_include_directories(
	${PROJECT_NAME} PRIVATE ${LLVM_INCLUDE_DIRS})

target_compile_definitions(
	${PROJECT_NAME} PRIVATE -DMCF_EXPORTS -D_SILENCE_ALL_CXX17_DEPRECATION_WARNINGS
)


# install(
# 	TARGETS ${PROJECT_NAME}
# 	EXPORT ${PROJECT_NAME}-config
# 	LIBRARY DESTINATION ${CMAKE_INSTALL_DIR})

# install(
#     EXPORT ${PROJECT_NAME}-config
#     NAMESPACE ${PROJECT_NAME}::
#     DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME})

# install(
#     DIRECTORY ${CMAKE_CURRENT_LIST_DIR}/include/
#     DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME})

# add_custom_command(
# 	TARGET ${PROJECT_NAME} POST_BUILD
# 	COMMAND ${CMAKE_COMMAND} -E copy_directory
#     $<TARGET_FILE_DIR:${PROJECT_NAME}>
# 	${CMAKE_SOURCE_DIR}/build/${CMAKE_BUILD_TYPE}
#     VERBATIM
# )