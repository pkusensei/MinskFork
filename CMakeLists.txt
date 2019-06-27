cmake_minimum_required(VERSION 3.8)

project(mcf LANGUAGES CXX)
set(CMAKE_CXX_STANDARD 17)

add_subdirectory(MCF)
add_subdirectory(mcfi)

if(MSVC)
  add_compile_options(/W4 /WX)
else()
  add_compile_options(-Wall -Wextra -pedantic -Werror)
endif()