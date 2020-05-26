project(MCF)

add_library(MCF SHARED src/Binding.cpp
	src/BoundExpressions.cpp
	src/BoundNode.cpp
	src/BoundNodePrinter.cpp
	src/Compilation.cpp
	src/ConsoleHelper.cpp
	src/ControlFlowGraph.cpp
	src/Diagnostic.cpp
	src/Emitter.cpp
	src/StringHelper.cpp
	src/IO.cpp
	src/Lowering.cpp
	src/Parsing.cpp
	src/SourceText.cpp
	src/SymbolPrinter.cpp
	src/Symbols.cpp
	src/SyntaxExpressions.cpp
	src/SyntaxKind.cpp
	src/SyntaxStatements.cpp
	src/SyntaxToken.cpp
	src/Values.cpp)

target_include_directories(
    ${PROJECT_NAME} PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include/>
    $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>)

find_package(LLVM REQUIRED)
target_link_libraries(
	${PROJECT_NAME} PRIVATE 
	LLVMMC 
	LLVMLTO 
	LLVMMCA 
	LLVMipo

	LLVMX86AsmParser
	LLVMX86CodeGen
	LLVMX86Desc
	LLVMX86Info
	LLVMX86Utils
)
target_include_directories(
	${PROJECT_NAME} PRIVATE ${LLVM_INCLUDE_DIRS})

target_compile_features(${PROJECT_NAME} PUBLIC cxx_std_17)
target_compile_definitions(
	${PROJECT_NAME} PRIVATE 
	-DMCF_EXPORTS 
	-D_SILENCE_ALL_CXX17_DEPRECATION_WARNINGS
	${LLVM_DEFINITIONS}
)
