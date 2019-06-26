cmake_minimum_required(VERSION 3.8)

project(mcf LANGUAGES CXX)
set(CMAKE_CXX_STANDARD 17)

add_subdirectory(MCF)

set(MCFI_DIR mcfi)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/build/${CMAKE_BUILD_TYPE})

add_executable(mcfi ${MCFI_DIR}/src/mcfi.cpp
    ${MCFI_DIR}/src/Repl.cpp)

list(APPEND INCLUDE_PATH 
	${CMAKE_SOURCE_DIR}/${MCFI_DIR}/include/
	${CMAKE_SOURCE_DIR}/MCF/include/)

target_include_directories(
    mcfi PUBLIC
    "$<BUILD_INTERFACE:${INCLUDE_PATH}>" #cmake needs quotes here
    $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>)

target_link_libraries(mcfi PUBLIC MCF)
