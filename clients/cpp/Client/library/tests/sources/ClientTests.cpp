
#include <helpers/TestServerResponse.hpp>

#include <extreme_carpaccio/client/Client.hpp>

#include <extreme_carpaccio/order_management/OrderParsing.hpp>

#include <boost/asio/connect.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <boost/beast/core.hpp>
#include <boost/beast/http.hpp>
#include <boost/beast/version.hpp>

#include <cstdlib>
#include <iostream>
#include <string>

#include <gmock/gmock.h>
#include <gtest/gtest.h>

namespace extreme_carpaccio {
namespace client {
namespace test {

namespace beast = boost::beast;
namespace http = beast::http;
namespace net = boost::asio;
using tcp = net::ip::tcp;

using testing::ElementsAre;

TEST(ExtremeCarpaccioClient, should_answer_404_to_incorrect_request)
{
   const std::string target = "/je-suis-trop-fort";

   // Receive the HTTP response
   const auto res = helpers::generateServerResponse(http::verb::get, target);
   
   // Write the message to standard out
   std::cout << "Response" << std::endl << res << std::endl;

   EXPECT_EQ(http::status::not_found, res.result());
}

TEST(ExtremeCarpaccioClient, should_handle_post_feedback)
{
   const std::string target = "/feedback";

   // Save cout's buffer here
   std::streambuf * sbuf = std::cout.rdbuf();

   // Redirect cout to our stringstream buffer
   std::stringstream buffer;
   std::cout.rdbuf(buffer.rdbuf());

   const auto res = helpers::generateServerResponse(http::verb::post, target, "application/json", "{\"type\": \"ERROR\", \"content\": \"Field total in response is missing\"}");

   EXPECT_EQ(http::status::ok, res.result());
   EXPECT_EQ("ERROR : Field total in response is missing\n", buffer.str());

   // When done redirect cout to its old self
   std::cout.rdbuf(sbuf);
}

TEST(ExtremeCarpaccioClient, should_trace_order_description)
{
	const std::string target = "/order";

	// Save cout's buffer here
	std::streambuf * sbuf = std::cout.rdbuf();

	// Redirect cout to our stringstream buffer
	std::stringstream buffer;
	std::cout.rdbuf(buffer.rdbuf());
	
	const auto res = helpers::generateServerResponse(http::verb::post, target, "application/json", "{\"prices\": [1,1.5], \"quantities\": [100, 200], \"country\": \"DE\", \"reduction\": \"STANDARD\"}");
	
   EXPECT_EQ("Order received: Order{prices={1,1.5}, quantities={100,200}, country='DE', reduction='STANDARD'}\n", buffer.str());

	// When done redirect cout to its old self
	std::cout.rdbuf(sbuf);
}

} // namespace test
} // namespace client
} // namespace extreme_carpaccio
