cmake_minimum_required(VERSION 3.19.3)

project(Extreme-Carpaccio-Client CXX)

set(CMAKE_CXX_STANDARD 14)
enable_testing()

set_property(GLOBAL PROPERTY USE_FOLDERS ON)

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib CACHE PATH "Where to place compiled static libraries.")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib CACHE PATH "Where to place compiled shared libraries.")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin CACHE PATH "Where to place compiled executables.")

# We use CPM for 3rd-party dependencies management
include(cmake/CPM.cmake)
include(cmake/FindGoogleTest.cmake)
include(cmake/FindNlohmannJson.cmake)
include(cmake/FindBoost.cmake)

add_subdirectory(OrderManagement)

#Shared library
add_library(Extreme-Carpaccio-Client-shared SHARED)

target_compile_definitions(Extreme-Carpaccio-Client-shared PRIVATE EXTREME_CARPACCIO_CLIENT_EXPORTS)

target_sources(Extreme-Carpaccio-Client-shared
    PRIVATE
        ${CMAKE_CURRENT_SOURCE_DIR}/src/extreme_carpaccio_client/ExtremeCarpaccioClient.cpp
		${CMAKE_CURRENT_SOURCE_DIR}/src/extreme_carpaccio_client/HTTPConfig.hpp
    PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}/include/extreme_carpaccio_client/ExtremeCarpaccioClient.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/extreme_carpaccio_client/config.hpp
)

target_include_directories(Extreme-Carpaccio-Client-shared
    PRIVATE
        ${CMAKE_CURRENT_SOURCE_DIR}/src
    PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}/include
)

target_link_libraries(Extreme-Carpaccio-Client-shared
    PRIVATE
        nlohmann_json
    PUBLIC 
        Boost::boost
        Extreme-Carpaccio-Order-Management-shared
)

# Client executable
add_executable(Extreme-Carpaccio-Client)

target_sources(Extreme-Carpaccio-Client
    PRIVATE
        ${CMAKE_CURRENT_SOURCE_DIR}/src/main.cpp
)

target_link_libraries(Extreme-Carpaccio-Client
    PRIVATE
        Extreme-Carpaccio-Client-shared)

# Test executable
add_executable(Extreme-Carpaccio-Client-test)

target_sources(Extreme-Carpaccio-Client-test
    PRIVATE
        ${CMAKE_CURRENT_SOURCE_DIR}/test/ExtremeCarpaccioClientTest.cpp
)

target_link_libraries(Extreme-Carpaccio-Client-test
    PRIVATE
        Extreme-Carpaccio-Client-shared
        gmock_main
		Boost::boost
		nlohmann_json)

add_test(NAME ctest-Extreme-Carpaccio-Client COMMAND Extreme-Carpaccio-Client-test)

#Globals
add_custom_target(Docs
    SOURCES
        ${CMAKE_CURRENT_SOURCE_DIR}/Retrospective.md
        ${CMAKE_CURRENT_SOURCE_DIR}/TODOLIST.md
        ${CMAKE_CURRENT_SOURCE_DIR}/../README.md)

set_property(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} PROPERTY VS_STARTUP_PROJECT Extreme-Carpaccio-Client-test)
set_property(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} PROPERTY XCODE_STARTUP_PROJECT Extreme-Carpaccio-Client-test)

set_property(TARGET Extreme-Carpaccio-Client-shared PROPERTY FOLDER "ExtremeCarpaccioClient")
set_property(TARGET Extreme-Carpaccio-Client-test PROPERTY FOLDER "ExtremeCarpaccioClient")
set_property(TARGET Extreme-Carpaccio-Client PROPERTY FOLDER "ExtremeCarpaccioClient")
set_property(TARGET Docs PROPERTY FOLDER "ExtremeCarpaccioClient")

set_property(TARGET gtest gmock_main PROPERTY FOLDER "Third-Party/GoogleTest")

set_property(GLOBAL PROPERTY PREDEFINED_TARGETS_FOLDER "CMake Generated")
