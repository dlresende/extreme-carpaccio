
#include <extreme_carpaccio_client/ExtremeCarpaccioClient.hpp>

#include <gtest/gtest.h>

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

namespace beast = boost::beast;     // from <boost/beast.hpp>
namespace http = beast::http;       // from <boost/beast/http.hpp>
namespace net = boost::asio;        // from <boost/asio.hpp>
using tcp = net::ip::tcp;           // from <boost/asio/ip/tcp.hpp>


using namespace extreme_carpaccio_client;

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

TEST(ExtremeCarpaccioClient, should_answer_404_to_incorrect_request)
{
   std::string target = "/je-suis-trop-fort";

   // Receive the HTTP response
   auto res = generateServerResponse(http::verb::get, target);
   
   // Write the message to standard out
   std::cout << "Response" << std::endl << res << std::endl;
   EXPECT_EQ(http::status::not_found, res.result());
}

TEST(ExtremeCarpaccioClient, should_return_valid_amount_on_order_request)
{
   std::string target = "/order";

   // Receive the HTTP response
   auto res = generateServerResponse(http::verb::post, target, "application/json", "{\"prices\": [], \"quantities\": [], \"country\": \"DE\", \"reduction\": \"STANDARD\"}");

   ASSERT_EQ(http::status::ok, res.result());

   auto totalAmountJson = nlohmann::json::parse(boost::beast::buffers_to_string(res.body().data()));

   EXPECT_NO_THROW(totalAmountJson["total"].get<double>());
}

//it('should handle feedback', function(done) {
//   var feedback = request('http://' + conf.host + ':' + conf.port + '/feedback', function(error, response) {
//      expect(response.statusCode).toBe(204);
//      done();
//   });
//   feedback.write('{type: "INFO", content: "this is my precious feedback"}');
//});
//
//it('should handle order', function(done) {
//   var order = request('http://' + conf.host + ':' + conf.port + '/order', function(error, response, body) {
//      expect(response.statusCode).toBe(200);
//      expect(body).toBe('{total:0}');
//      done();
//   });
//   order.write('{prices: [], quantities: [], country: "DE", reduction: "STANDARD"}');
//});

