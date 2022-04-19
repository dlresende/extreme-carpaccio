#include <extreme_carpaccio/client/Client.hpp>

#include <extreme_carpaccio/order_management/OrderParsing.hpp>

#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include <iostream>
#include <vector>

#include <boost/asio/connect.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <boost/beast/core.hpp>
#include <boost/beast/http.hpp>
#include <boost/beast/version.hpp>
#include <cstdlib>
#include <iostream>
#include <string>
#include <thread>
#include <nlohmann/json.hpp>

namespace extreme_carpaccio {
namespace client {
namespace test {

namespace beast = boost::beast;
namespace http = beast::http;
namespace net = boost::asio;
using tcp = net::ip::tcp;

using testing::ElementsAre;

namespace {

const char serverHost[] = "localhost";
const unsigned short serverPort = 8081;

static http::response<http::dynamic_body> generateServerResponse(boost::beast::http::verb requestType, const std::string & target, const std::string & contentType = "", const std::string & body = "")
{
   CarpaccioServer server(8081);
   std::thread thread(&CarpaccioServer::start, &server);
   std::this_thread::sleep_for(std::chrono::seconds(1));

   CarpaccioStream stream(serverHost, serverPort);

   // Send the HTTP request to the remote host
   stream.write(requestType, target, contentType, body);

   // This buffer is used for reading and must be persisted
   beast::flat_buffer buffer;

   // Receive the HTTP response
   auto res = stream.read(buffer);

   server.stop();

   thread.detach();

   return res;
}

} // namespace

TEST(ExtremeCarpaccioClient, should_answer_404_to_incorrect_request)
{
   const std::string target = "/je-suis-trop-fort";

   // Receive the HTTP response
   const auto res = generateServerResponse(http::verb::get, target);
   
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

   const auto res = generateServerResponse(http::verb::post, target, "application/json", "{\"type\": \"ERROR\", \"content\": \"Field total in response is missing\"}");

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
	
	const auto res = generateServerResponse(http::verb::post, target, "application/json", "{\"prices\": [1,1.5], \"quantities\": [100, 200], \"country\": \"DE\", \"reduction\": \"STANDARD\"}");
	
   EXPECT_EQ("Order received: Order{prices={1,1.5}, quantities={100,200}, country='DE', reduction='STANDARD'}\n", buffer.str());

	// When done redirect cout to its old self
	std::cout.rdbuf(sbuf);
}

} // namespace test
} // namespace client
} // namespace extreme_carpaccio
