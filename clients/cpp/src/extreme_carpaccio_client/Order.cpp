#include <extreme_carpaccio_client/Order.hpp>

#include <nlohmann/json.hpp>


namespace extreme_carpaccio_client {

EXTREME_CARPACCIO_CLIENT_API Order parseOrder(const std::string & jsonOrder)
{
   Order order;

   auto parsedOrder = nlohmann::json::parse(jsonOrder);

   order.country = parsedOrder["country"].get<std::string>();
   order.reduction = parsedOrder["reduction"].get<std::string>();
   order.prices = parsedOrder["prices"].get<std::vector<double>>();
   order.quantities = parsedOrder["quantities"].get<std::vector<unsigned int>>();

   return order;
}

} // namespace extreme_carpaccio_client

