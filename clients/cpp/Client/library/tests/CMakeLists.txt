
add_executable(Client-tests)

set(CLIENT_TESTS_PRIVATE_SOURCES
   ${CMAKE_CURRENT_SOURCE_DIR}/sources/ClientTests.cpp
)

target_sources(Client-tests
   PRIVATE
      ${CLIENT_TESTS_PRIVATE_SOURCES}
)

source_group(
   TREE ${CMAKE_CURRENT_SOURCE_DIR}
   FILES
      ${CLIENT_TESTS_PRIVATE_SOURCES}
)

target_link_libraries(Client-tests
   PRIVATE
      Client-shared
      gmock_main
      Boost::boost
      nlohmann_json
)

add_test(NAME ctest-Extreme-Carpaccio-Client-tests COMMAND Client-tests)

set_property(TARGET Client-tests PROPERTY FOLDER "Client")
