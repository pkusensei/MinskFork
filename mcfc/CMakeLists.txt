project(mcfc)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY 
	${CMAKE_SOURCE_DIR}/build/${CMAKE_BUILD_TYPE})

add_executable(${PROJECT_NAME}
    src/mcfc.cpp)

target_link_libraries(${PROJECT_NAME} PUBLIC MCF)

find_package(cxxopts REQUIRED)
target_link_libraries(${PROJECT_NAME} PRIVATE cxxopts::cxxopts)