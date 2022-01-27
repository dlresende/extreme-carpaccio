
#include <extreme_carpaccio_client/ExtremeCarpaccioClient.hpp>

#include <boost/asio.hpp>
#include <boost/beast/core.hpp>
#include <boost/beast/http.hpp>
#include <boost/beast/version.hpp>
#include <chrono>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <list>
#include <memory>
#include <string>
#include <nlohmann/json.hpp>

namespace beast = boost::beast;         // from <boost/beast.hpp>
namespace http = beast::http;           // from <boost/beast/http.hpp>
namespace net = boost::asio;            // from <boost/asio.hpp>
using tcp = boost::asio::ip::tcp;       // from <boost/asio/ip/tcp.hpp>

const int version = 11;

namespace extreme_carpaccio_client {

   http_worker::http_worker(tcp::acceptor& acceptor, const std::string& doc_root) :
   acceptor_(acceptor),
   doc_root_(doc_root)
{
}

void http_worker::start()
{
   accept();
   check_deadline();
}

void http_worker::accept()
{
   // Clean up any previous connection.
   beast::error_code ec;
   socket_.close(ec);
   buffer_.consume(buffer_.size());

   acceptor_.async_accept(
      socket_,
      [this](beast::error_code ec)
   {
      if (ec)
      {
         accept();
      }
      else
      {
         // Request must be fully processed within 60 seconds.
         request_deadline_.expires_after(
            std::chrono::seconds(60));

         read_request();
      }
   });
}

void http_worker::read_request()
{
   // On each read the parser needs to be destroyed and
   // recreated. We store it in a boost::optional to
   // achieve that.
   //
   // Arguments passed to the parser constructor are
   // forwarded to the message object. A single argument
   // is forwarded to the body constructor.
   //
   // We construct the dynamic body with a 1MB limit
   // to prevent vulnerability to buffer attacks.
   //
   parser_.emplace(
      std::piecewise_construct,
      std::make_tuple(),
      std::make_tuple(alloc_));

   http::async_read(
      socket_,
      buffer_,
      *parser_,
      [this](beast::error_code ec, std::size_t)
   {
      if (ec)
         accept();
      else
         process_request(parser_->get());
   });
}

double computeTotalAmount()
{
   return 0.0;
}

bool http_worker::handleRequest(http::verb requestType, const std::string & target, const std::string & contentType, const std::string & body)
{
   bool error = true;

   if (requestType == http::verb::post && contentType == "application/json")
   {
      if (target == "/order")
      {
         try
         {
            auto requestJson = nlohmann::json::parse(body);
         }
         catch (nlohmann::json::exception& e)
         {
            return true;
         }

         nlohmann::json totalAmountJson;

         totalAmountJson["total"] = computeTotalAmount();
         send_bad_response(http::status::ok, totalAmountJson.dump());
         error = false;
      }
   }

   return error;
}

void http_worker::process_request(http::request<request_body_t, http::basic_fields<alloc_t>> const& req)
{
   const std::string contentType = req[http::field::content_type].to_string();
   const std::string body = req[http::field::body].to_string();

   if (this->handleRequest(req.method(), req.target().to_string(), contentType, body))
   {
      this->send_bad_response(http::status::not_found, "HTTP code 404\r\n");
   }
}

void http_worker::send_bad_response(
   http::status status,
   std::string const& error)
{
   string_response_.emplace(
      std::piecewise_construct,
      std::make_tuple(),
      std::make_tuple(alloc_));

   string_response_->result(status);
   string_response_->keep_alive(false);
   string_response_->set(http::field::server, "Beast");
   string_response_->set(http::field::content_type, "text/plain");
   string_response_->body() = error;
   string_response_->prepare_payload();

   http::async_write(
      socket_,
      *string_response_,
      [this](beast::error_code ec, std::size_t)
   {
      socket_.shutdown(tcp::socket::shutdown_send, ec);
      string_response_.reset();
      accept();
   });
}

void http_worker::check_deadline()
{
   // The deadline may have moved, so check it has really passed.
   if (request_deadline_.expiry() <= std::chrono::steady_clock::now())
   {
      // Close socket to cancel any outstanding operation.
      socket_.close();

      // Sleep indefinitely until we're given a new deadline.
      request_deadline_.expires_at(
         (std::chrono::steady_clock::time_point::max)());
   }

   request_deadline_.async_wait(
      [this](beast::error_code)
   {
      check_deadline();
   });
}

   CarpaccioServer::CarpaccioServer(unsigned short port)
      : ioc(1)
      , acceptor(ioc, {boost::asio::ip::make_address("127.0.0.1"), port } ),
      worker(acceptor, "./feedback")
   {
      
   }

   void CarpaccioServer::start()
   {
      worker.start();
      ioc.run();
   }

   void CarpaccioServer::stop()
   {
      ioc.stop();
   }

   CarpaccioStream::CarpaccioStream(const std::string & host, unsigned short port)
      : m_serverHost(host)
      , m_ioContext()
      , m_resolver(m_ioContext) // These objects perform our I/O
      , m_resolverResults(m_resolver.resolve(host, std::to_string(port))) // Look up the domain name
      , m_stream(m_ioContext)
   {
      // Make the connection on the IP address we get from a lookup
      m_stream.connect(m_resolverResults);
   }

   CarpaccioStream::~CarpaccioStream()
   {
      // Gracefully close the socket
      beast::error_code ec;
      m_stream.socket().shutdown(tcp::socket::shutdown_both, ec);

      //// not_connected happens sometimes
      //// so don't bother reporting it.
      //if (ec && ec != beast::errc::not_connected)
      //   throw beast::system_error{ ec };
   }

   http::response<http::dynamic_body> CarpaccioStream::read(beast::flat_buffer & buffer)
   {
      // Declare a container to hold the response
      http::response<http::dynamic_body> response;

      // Receive the HTTP response
      http::read(m_stream, buffer, response);

      return response;
   }

   void CarpaccioStream::write(boost::beast::http::verb requestType, const std::string & target, const std::string & contentType, const std::string & body)
   {
      // Set up an HTTP GET request message
      http::request<http::string_body> request{ requestType, target, version };
      request.set(http::field::host, m_serverHost);
      request.set(http::field::user_agent, BOOST_BEAST_VERSION_STRING);
      request.set(http::field::content_type, contentType);
      request.set(http::field::body, body);

      // Send the HTTP request to the remote host
      http::write(m_stream, request);
   }
   
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

