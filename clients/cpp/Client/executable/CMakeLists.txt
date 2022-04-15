
add_executable(Client-exe)

set(CLIENT_EXE_PRIVATE_SOURCES
   ${CMAKE_CURRENT_SOURCE_DIR}/sources/main.cpp
)

target_sources(Client-exe
   PRIVATE
      ${CLIENT_EXE_PRIVATE_SOURCES}
)

source_group(
   TREE ${CMAKE_CURRENT_SOURCE_DIR}
   FILES
      ${CLIENT_EXE_PRIVATE_SOURCES}
)

target_link_libraries(Client-exe
    PRIVATE
        Client-shared)

set_property(TARGET Client-exe PROPERTY FOLDER "Client")
