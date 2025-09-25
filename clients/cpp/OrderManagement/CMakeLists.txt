
#Functional library to be modified by participants
add_library(Order-Management-shared SHARED)

target_compile_definitions(Order-Management-shared PRIVATE EXTREME_CARPACCIO_ORDER_MANAGEMENT_EXPORTS)

set(ORDER_MANAGEMENT_SHARED_PRIVATE_SOURCES
   ${CMAKE_CURRENT_SOURCE_DIR}/sources/extreme_carpaccio/order_management/OrderParsing.cpp
   ${CMAKE_CURRENT_SOURCE_DIR}/sources/extreme_carpaccio/order_management/Order.cpp
   ${CMAKE_CURRENT_SOURCE_DIR}/sources/extreme_carpaccio/order_management/TotalAmount.cpp
)

set(ORDER_MANAGEMENT_SHARED_PUBLIC_SOURCES
   ${CMAKE_CURRENT_SOURCE_DIR}/includes/extreme_carpaccio/order_management/Order.hpp
   ${CMAKE_CURRENT_SOURCE_DIR}/includes/extreme_carpaccio/order_management/OrderParsing.hpp
   ${CMAKE_CURRENT_SOURCE_DIR}/includes/extreme_carpaccio/order_management/TotalAmount.hpp
   ${CMAKE_CURRENT_SOURCE_DIR}/includes/extreme_carpaccio/order_management/config.hpp
)

target_sources(Order-Management-shared
   PRIVATE
      ${ORDER_MANAGEMENT_SHARED_PRIVATE_SOURCES}
   PUBLIC
      ${ORDER_MANAGEMENT_SHARED_PUBLIC_SOURCES}
)

source_group(
   TREE ${CMAKE_CURRENT_SOURCE_DIR}
   FILES
      ${ORDER_MANAGEMENT_SHARED_PRIVATE_SOURCES}
      ${ORDER_MANAGEMENT_SHARED_PUBLIC_SOURCES}
)

target_include_directories(Order-Management-shared
    PRIVATE
        ${CMAKE_CURRENT_SOURCE_DIR}/sources
    PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}/includes
)

target_link_libraries(Order-Management-shared PUBLIC Boost::boost PRIVATE nlohmann_json::nlohmann_json)

install(TARGETS Order-Management-shared)

set_property(TARGET Order-Management-shared PROPERTY FOLDER "Order Management")

add_subdirectory(tests)