cmake_minimum_required(VERSION 3.16)

project(mcf)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY 
	${CMAKE_SOURCE_DIR}/build/${CMAKE_BUILD_TYPE})

if(MSVC)
  add_compile_options(/W4 /WX)
else()
  add_compile_options(-Wall -Wextra -pedantic -Werror)
endif()

add_subdirectory(MCF)
add_subdirectory(mcfc)
add_subdirectory(mcfi)

enable_testing()
add_subdirectory(tests)
