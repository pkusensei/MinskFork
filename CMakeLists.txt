cmake_minimum_required(VERSION 3.8)

project(mcf LANGUAGES CXX)
set(CMAKE_CXX_STANDARD 17)

if(MSVC)
  add_compile_options(/W4 /WX)
else()
  add_compile_options(-Wall -Wextra -pedantic -Werror)
endif()

add_subdirectory(MCF)
add_subdirectory(mcfi)
