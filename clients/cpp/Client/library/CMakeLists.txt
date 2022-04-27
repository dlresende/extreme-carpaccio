
add_library(Client-shared SHARED)

target_compile_definitions(Client-shared PRIVATE EXTREME_CARPACCIO_CLIENT_EXPORTS)

set(CLIENT_SHARED_PRIVATE_SOURCES
   ${CMAKE_CURRENT_SOURCE_DIR}/sources/extreme_carpaccio/client/CarpaccioHttpWorker.cpp
   ${CMAKE_CURRENT_SOURCE_DIR}/sources/extreme_carpaccio/client/Client.cpp
)

set(CLIENT_SHARED_PUBLIC_SOURCES
   ${CMAKE_CURRENT_SOURCE_DIR}/includes/extreme_carpaccio/client/CarpaccioHttpWorker.hpp
   ${CMAKE_CURRENT_SOURCE_DIR}/includes/extreme_carpaccio/client/Client.hpp
   ${CMAKE_CURRENT_SOURCE_DIR}/includes/extreme_carpaccio/client/config.hpp
   ${CMAKE_CURRENT_SOURCE_DIR}/includes/extreme_carpaccio/client/HttpConfig.hpp
)

target_sources(Client-shared
   PRIVATE
      ${CLIENT_SHARED_PRIVATE_SOURCES}
   PUBLIC
      ${CLIENT_SHARED_PUBLIC_SOURCES}
)

source_group(
   TREE ${CMAKE_CURRENT_SOURCE_DIR}
   FILES
      ${CLIENT_SHARED_PRIVATE_SOURCES}
      ${CLIENT_SHARED_PUBLIC_SOURCES}
)

target_include_directories(Client-shared
   PRIVATE
      ${CMAKE_CURRENT_SOURCE_DIR}/sources
   PUBLIC
      ${CMAKE_CURRENT_SOURCE_DIR}/includes
)

target_link_libraries(Client-shared
   PRIVATE
      nlohmann_json
   PUBLIC
      Threads::Threads
      Boost::boost
      Order-Management-shared
)

install(TARGETS Client-shared)

set_property(TARGET Client-shared PROPERTY FOLDER "Client")

add_subdirectory(tests)
