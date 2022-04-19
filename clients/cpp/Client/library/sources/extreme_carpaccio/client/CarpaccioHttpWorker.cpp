
#include <extreme_carpaccio/client/CarpaccioHttpWorker.hpp>
#include <extreme_carpaccio/client/HttpConfig.hpp>

#include <extreme_carpaccio/order_management/Order.hpp>
#include <extreme_carpaccio/order_management/OrderParsing.hpp>
#include <extreme_carpaccio/order_management/TotalAmount.hpp>

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


namespace extreme_carpaccio {
namespace client {

namespace beast = boost::beast;
namespace http = beast::http;
namespace net = boost::asio;
using tcp = boost::asio::ip::tcp;

namespace {

const int version = 11;

struct Feedback
{
   std::string type;
   std::string content;
};

static Feedback parseFeedback(const std::string& jsonFeedback)
{
   auto parsedFeedback = nlohmann::json::parse(jsonFeedback);
   Feedback feedback = {};

   feedback.type = parsedFeedback["type"].get<std::string>();
   feedback.content = parsedFeedback["content"].get<std::string>();
   return feedback;
}

} // namespace

CarpaccioHttpWorker::CarpaccioHttpWorker(tcp::acceptor& acceptor, const std::string& doc_root) :
     m_acceptor(acceptor)
   , m_doc_root(doc_root)
   , m_socket{ m_acceptor.get_executor() }
   , m_buffer()
   , m_alloc()
   , m_parser()
   , m_request_deadline{ m_acceptor.get_executor(), (std::chrono::steady_clock::time_point::max)() }
   , m_string_response()
   , m_file_response()
{
}

void CarpaccioHttpWorker::start()
{
   accept();
   check_deadline();
}

void CarpaccioHttpWorker::accept()
{
   // Clean up any previous connection.
   beast::error_code ec;
   m_socket.close(ec);
   m_buffer.consume(m_buffer.size());

   m_acceptor.async_accept(
      m_socket,
      [this](beast::error_code ec)
   {
      if (ec)
      {
         accept();
      }
      else
      {
         // Request must be fully processed within 60 seconds.
         m_request_deadline.expires_after(
            std::chrono::seconds(60));

         read_request();
      }
   });
}

void CarpaccioHttpWorker::read_request()
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
   m_parser.emplace(
      std::piecewise_construct,
      std::make_tuple(),
      std::make_tuple(m_alloc));

   http::async_read(
      m_socket,
      m_buffer,
      *m_parser,
      [this](beast::error_code ec, std::size_t)
   {
      if (ec)
         accept();
      else
         process_request(m_parser->get());
   });
}

bool CarpaccioHttpWorker::handleRequest(http::verb requestType, const std::string & target, const std::string & contentType, const std::string & body)
{
   const bool error = (requestType != http::verb::post || contentType != "application/json");

   if (!error)
   {
      if (target == "/order")
      {
         auto order = order_management::parseOrder(body);
         std::cout << "Order received: " << order << std::endl;

         auto totalAmountResponse = order_management::computeTotalAmount(body);

         nlohmann::json totalAmountJson;

         totalAmountJson["total"] = totalAmountResponse.m_totalAmount;
         send_response(totalAmountResponse.m_status, totalAmountJson.dump());
      }
      else if (target == "/feedback")
      {
         Feedback feedback = parseFeedback(body);

         std::cout << feedback.type << " : " << feedback.content << std::endl;
         send_response(http::status::ok, "Feedback received");
      }
   }
   return error;
}

void CarpaccioHttpWorker::process_request(http::request<request_body_t, http::basic_fields<alloc_t>> const& req)
{
   const std::string contentType = req[http::field::content_type].to_string();
   const std::string body = req.body();

   if (this->handleRequest(req.method(), req.target().to_string(), contentType, body))
   {
      this->send_response(http::status::not_found, "HTTP code 404\r\n");
   }
}

void CarpaccioHttpWorker::send_response(http::status status, std::string const& body)
{
   m_string_response.emplace(
      std::piecewise_construct,
      std::make_tuple(),
      std::make_tuple(m_alloc));

   m_string_response->result(status);
   m_string_response->keep_alive(false);
   m_string_response->set(http::field::server, "Beast");
   m_string_response->set(http::field::content_type, "text/plain");
   m_string_response->body() = body;
   m_string_response->prepare_payload();

   http::async_write(
      m_socket,
      *m_string_response,
      [this](beast::error_code ec, std::size_t)
   {
      m_socket.shutdown(tcp::socket::shutdown_send, ec);
      m_string_response.reset();
      accept();
   });
}

void CarpaccioHttpWorker::check_deadline()
{
   // The deadline may have moved, so check it has really passed.
   if (m_request_deadline.expiry() <= std::chrono::steady_clock::now())
   {
      // Close socket to cancel any outstanding operation.
      m_socket.close();

      // Sleep indefinitely until we're given a new deadline.
      m_request_deadline.expires_at(
         (std::chrono::steady_clock::time_point::max)());
   }

   m_request_deadline.async_wait(
      [this](beast::error_code)
   {
      check_deadline();
   });
}
  
} // namespace client
} // namespace extreme_carpaccio
