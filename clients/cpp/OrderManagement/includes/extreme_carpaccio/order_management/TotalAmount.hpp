
#ifndef EXTREME_CARPACCIO_ORDER_MANAGEMENT_TOTALAMOUNT_HPP
#define EXTREME_CARPACCIO_ORDER_MANAGEMENT_TOTALAMOUNT_HPP

#include <extreme_carpaccio/order_management/config.hpp>
#include <extreme_carpaccio/order_management/Order.hpp>

#include <string>

namespace extreme_carpaccio {
namespace order_management {

EXTREME_CARPACCIO_ORDER_MANAGEMENT_API double computeTotalAmount(const Order & order);

} // namespace order_management
} // namespace extreme_carpaccio

#endif // EXTREME_CARPACCIO_ORDER_MANAGEMENT_TOTALAMOUNT_HPP

