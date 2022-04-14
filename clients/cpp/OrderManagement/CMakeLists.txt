
#Functional library to be modified by participants
add_library(Extreme-Carpaccio-Order-Management-shared SHARED)

target_compile_definitions(Extreme-Carpaccio-Order-Management-shared PRIVATE EXTREME_CARPACCIO_ORDER_MANAGEMENT_EXPORTS)

target_sources(Extreme-Carpaccio-Order-Management-shared
    PRIVATE
        ${CMAKE_CURRENT_SOURCE_DIR}/sources/extreme_carpaccio/order_management/OrderParsing.cpp
        ${CMAKE_CURRENT_SOURCE_DIR}/sources/extreme_carpaccio/order_management/Order.cpp
		${CMAKE_CURRENT_SOURCE_DIR}/sources/extreme_carpaccio/order_management/TotalAmount.cpp
    PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}/includes/extreme_carpaccio/order_management/Order.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/includes/extreme_carpaccio/order_management/OrderParsing.hpp
		${CMAKE_CURRENT_SOURCE_DIR}/includes/extreme_carpaccio/order_management/TotalAmount.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/includes/extreme_carpaccio/order_management/config.hpp
)

target_include_directories(Extreme-Carpaccio-Order-Management-shared
    PRIVATE
        ${CMAKE_CURRENT_SOURCE_DIR}/sources
    PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}/includes
)

target_link_libraries(Extreme-Carpaccio-Order-Management-shared PUBLIC Boost::boost PRIVATE nlohmann_json)

set_property(TARGET Extreme-Carpaccio-Order-Management-shared PROPERTY FOLDER "OrderManagement")
