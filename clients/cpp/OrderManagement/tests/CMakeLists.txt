
add_executable(Order-Management-tests)

set(ORDER_MANAGEMENT_TESTS_PRIVATE_SOURCES
   ${CMAKE_CURRENT_SOURCE_DIR}/sources/OrderParsingTests.cpp
   ${CMAKE_CURRENT_SOURCE_DIR}/sources/TotalAmountTests.cpp
)

target_sources(Order-Management-tests
    PRIVATE ${ORDER_MANAGEMENT_TESTS_PRIVATE_SOURCES}  
)

source_group(
   TREE ${CMAKE_CURRENT_SOURCE_DIR}
   FILES
      ${ORDER_MANAGEMENT_TESTS_PRIVATE_SOURCES}
)

target_include_directories(Order-Management-tests
    PRIVATE
        ${CMAKE_CURRENT_SOURCE_DIR}/sources
)

target_link_libraries(Order-Management-tests
   PRIVATE
      Order-Management-shared
      gmock_main)

add_test(NAME ctest-Extreme-Carpaccio-Order-Management-tests COMMAND Order-Management-tests)

set_property(TARGET Order-Management-tests PROPERTY FOLDER "Order Management")
