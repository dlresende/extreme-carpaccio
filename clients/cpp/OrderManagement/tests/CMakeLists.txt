
add_executable(Extreme-Carpaccio-Order-Management-tests)

target_sources(Extreme-Carpaccio-Order-Management-tests
    PRIVATE
        ${CMAKE_CURRENT_SOURCE_DIR}/sources/OrderParsingTests.cpp
		  ${CMAKE_CURRENT_SOURCE_DIR}/sources/TotalAmountTests.cpp
)

target_include_directories(Extreme-Carpaccio-Order-Management-tests
    PRIVATE
        ${CMAKE_CURRENT_SOURCE_DIR}/sources
)

target_link_libraries(Extreme-Carpaccio-Order-Management-tests
   PRIVATE
      Extreme-Carpaccio-Order-Management-shared
      gmock_main)

add_test(NAME ctest-Extreme-Carpaccio-Order-Management-tests COMMAND Extreme-Carpaccio-Order-Management-tests)

set_property(TARGET Extreme-Carpaccio-Order-Management-tests PROPERTY FOLDER "OrderManagement")
