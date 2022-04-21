
add_executable(Client-tests)

set(CLIENT_TESTS_PRIVATE_SOURCES
   ${CMAKE_CURRENT_SOURCE_DIR}/sources/ClientTests.cpp
   ${CMAKE_CURRENT_SOURCE_DIR}/sources/helpers/CarpaccioStream.hpp
   ${CMAKE_CURRENT_SOURCE_DIR}/sources/helpers/CarpaccioStream.cpp
   ${CMAKE_CURRENT_SOURCE_DIR}/sources/helpers/TestServerResponse.hpp
   ${CMAKE_CURRENT_SOURCE_DIR}/sources/helpers/TestServerResponse.cpp
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

target_include_directories(Client-tests
   PRIVATE
      ${CMAKE_CURRENT_SOURCE_DIR}/sources
)

target_link_libraries(Client-tests
   PRIVATE
      Client-shared
      gmock_main
      Boost::boost
      nlohmann_json
)

install(TARGETS Client-tests)

add_test(NAME ctest-Extreme-Carpaccio-Client-tests COMMAND Client-tests)

set_property(TARGET Client-tests PROPERTY FOLDER "Client")
