
#include <extreme_carpaccio/order_management/OrderParsing.hpp>

#include <nlohmann/json.hpp>


namespace extreme_carpaccio {
namespace order_management {

Order parseOrder(const std::string& jsonOrder)
{
   Order order;

   auto parsedOrder = nlohmann::json::parse(jsonOrder);

   order.country = parsedOrder["country"].get<std::string>();
   order.reduction = parsedOrder["reduction"].get<std::string>();
   order.prices = parsedOrder["prices"].get<std::vector<double>>();
   order.quantities = parsedOrder["quantities"].get<std::vector<unsigned int>>();

   return order;
}

} // namespace order_management
} // namespace extreme_carpaccio
