project(mcfi)

add_executable(${PROJECT_NAME} src/mcfi.cpp
    src/Repl.cpp 
	src/Authoring.cpp)

target_include_directories(${PROJECT_NAME}
	PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/include)

target_link_libraries(${PROJECT_NAME} PUBLIC MCF)
target_compile_features(${PROJECT_NAME} PRIVATE cxx_std_20)
