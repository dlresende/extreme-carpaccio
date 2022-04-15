cmake_minimum_required(VERSION 3.19.3)

project(Extreme-Carpaccio CXX)

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
add_subdirectory(Client)

#Globals
add_custom_target(Docs
    SOURCES
        ${CMAKE_CURRENT_SOURCE_DIR}/Retrospective.md
        ${CMAKE_CURRENT_SOURCE_DIR}/TODOLIST.md
        ${CMAKE_CURRENT_SOURCE_DIR}/../README.md)

set_property(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} PROPERTY VS_STARTUP_PROJECT Order-Management-tests)
set_property(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} PROPERTY XCODE_STARTUP_PROJECT Order-Management-tests)

set_property(TARGET gtest gtest_main gmock gmock_main PROPERTY FOLDER "Third-Party/GoogleTest")

set_property(GLOBAL PROPERTY PREDEFINED_TARGETS_FOLDER "CMake Generated")
