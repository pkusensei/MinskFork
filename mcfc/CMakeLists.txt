project(mcfc)

add_executable(${PROJECT_NAME}
    src/mcfc.cpp)

target_link_libraries(${PROJECT_NAME} PUBLIC MCF)

find_package(cxxopts REQUIRED)
target_link_libraries(${PROJECT_NAME} PRIVATE cxxopts::cxxopts)
target_compile_features(${PROJECT_NAME} PRIVATE cxx_std_17)
