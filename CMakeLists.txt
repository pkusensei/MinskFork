cmake_minimum_required(VERSION 3.16)

project(mcf)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY 
	${CMAKE_SOURCE_DIR}/build/${CMAKE_BUILD_TYPE})

if(MSVC)
  #[[ HACK
      There's probably a bug between 
      MSVC's implementation of aligned new operator (c++17)
	  and
	  LLVM calling deallocate_buffer(void *Ptr, size_t Size, size_t Alignment)
	  /Zc:alignedNew- is to make adding llvm::DIBuilder not crash the whole thing
  ]]
  add_compile_options(/W4 /WX /permissive- /Zc:alignedNew-)
else()
  add_compile_options(-Wall -Wextra -pedantic -Werror)
endif()

add_subdirectory(MCF)
add_subdirectory(mcfc)
add_subdirectory(mcfi)

enable_testing()
add_subdirectory(tests)
