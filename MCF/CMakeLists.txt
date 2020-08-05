project(MCF)

add_library(MCF SHARED src/Binding.cpp
	src/BoundExpressions.cpp
	src/BoundNode.cpp
	src/BoundNodePrinter.cpp
	src/BoundStatements.cpp
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

#[[ HACK
    There's probably a bug between 
    MSVC's implementation of aligned new operator (c++17)
	and
	LLVM calling deallocate_buffer(void *Ptr, size_t Size, size_t Alignment)
	/Zc:alignedNew- is to make adding llvm::DIBuilder not crash the whole thing
]]
if(MSVC)
	target_compile_options(${PROJECT_NAME} PRIVATE /Zc:alignedNew-)
endif()

target_compile_features(${PROJECT_NAME} PUBLIC cxx_std_20)
target_compile_definitions(
	${PROJECT_NAME} PRIVATE 
	-DMCF_EXPORTS 
	-D_SILENCE_ALL_CXX17_DEPRECATION_WARNINGS
	${LLVM_DEFINITIONS}
)
