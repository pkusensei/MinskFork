project(mcfi)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY 
	${CMAKE_SOURCE_DIR}/build/${CMAKE_BUILD_TYPE})

add_executable(${PROJECT_NAME} src/mcfi.cpp
    src/Repl.cpp)

target_include_directories(${PROJECT_NAME}
	PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/include)

target_link_libraries(${PROJECT_NAME} PUBLIC MCF)
