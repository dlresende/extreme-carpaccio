#include <extreme_carpaccio_client/Order.hpp>

#include <nlohmann/json.hpp>


namespace extreme_carpaccio_client {

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

	template <class T> static std::ostream& operator<< (std::ostream& stream, const std::vector<T>& v)
	{
		stream << "{";
		for (std::size_t i = 0; i < v.size(); ++i)
		{
			if (i > 0)
			{
				stream << ",";
			}
			stream << v[i];
		}
		stream << '}';
		return stream;
	}

	std::ostream& operator<< (std::ostream& stream, const Order& order)
	{
		stream << "Order{" <<
			"prices=" << order.prices <<
			", quantities=" << order.quantities <<
			", country='" << order.country + '\'' <<
			", reduction='" << order.reduction << '\'' <<
			'}';
		return stream;
	}

} // namespace extreme_carpaccio_client

