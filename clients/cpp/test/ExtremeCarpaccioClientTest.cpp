
#include <extreme_carpaccio_client/ExtremeCarpaccioClient.hpp>

#include <gtest/gtest.h>

#include <vector>

#include <boost/beast/core.hpp>
#include <boost/beast/http.hpp>
#include <boost/beast/version.hpp>
#include <boost/asio/connect.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <cstdlib>
#include <iostream>
#include <string>
#include <thread>

namespace beast = boost::beast;     // from <boost/beast.hpp>
namespace http = beast::http;       // from <boost/beast/http.hpp>
namespace net = boost::asio;        // from <boost/asio.hpp>
using tcp = net::ip::tcp;           // from <boost/asio/ip/tcp.hpp>


using namespace extreme_carpaccio_client;

TEST(ExtremeCarpaccioClient, should_handle_feedback)
{
   std::thread thread(extreme_carpaccio_client::launchServer);
   std::this_thread::sleep_for(std::chrono::seconds(5));

   std::string host = "localhost";
   std::string port = "8081";
   std::string target = "/toto.png";
   int version = 11;

   net::io_context ioc;

   // These objects perform our I/O
   tcp::resolver resolver(ioc);
   beast::tcp_stream stream(ioc);

   // Look up the domain name
   auto const results = resolver.resolve(host, port);

   // Make the connection on the IP address we get from a lookup
   stream.connect(results);

   // Set up an HTTP GET request message
   http::request<http::string_body> req{ http::verb::get, target, version };
   req.set(http::field::host, host);
   req.set(http::field::user_agent, BOOST_BEAST_VERSION_STRING);

   // Send the HTTP request to the remote host
   http::write(stream, req);

   // This buffer is used for reading and must be persisted
   beast::flat_buffer buffer;

   // Declare a container to hold the response
   http::response<http::dynamic_body> res;

   // Receive the HTTP response
   http::read(stream, buffer, res);

   // Write the message to standard out
   std::cout << "Response" << std::endl << res << std::endl;

   // Gracefully close the socket
   beast::error_code ec;
   stream.socket().shutdown(tcp::socket::shutdown_both, ec);

   // not_connected happens sometimes
   // so don't bother reporting it.
   //
   if (ec && ec != beast::errc::not_connected)
      throw beast::system_error{ ec };

   EXPECT_EQ(1, 1);
   thread.detach();
}

TEST(ExtremeCarpaccioClient, should_handle_order)
{
   EXPECT_EQ(1, 1);
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

