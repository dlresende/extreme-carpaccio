
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

namespace beast = boost::beast;         // from <boost/beast.hpp>
namespace http = beast::http;           // from <boost/beast/http.hpp>
namespace net = boost::asio;            // from <boost/asio.hpp>
using tcp = boost::asio::ip::tcp;       // from <boost/asio/ip/tcp.hpp>

const int version = 11;

// Return a reasonable mime type based on the extension of a file.
beast::string_view
mime_type(beast::string_view path)
{
   using beast::iequals;
   auto const ext = [&path]
   {
      auto const pos = path.rfind(".");
      if (pos == beast::string_view::npos)
         return beast::string_view{};
      return path.substr(pos);
   }();
   if (iequals(ext, ".htm"))  return "text/html";
   if (iequals(ext, ".html")) return "text/html";
   if (iequals(ext, ".php"))  return "text/html";
   if (iequals(ext, ".css"))  return "text/css";
   if (iequals(ext, ".txt"))  return "text/plain";
   if (iequals(ext, ".js"))   return "application/javascript";
   if (iequals(ext, ".json")) return "application/json";
   if (iequals(ext, ".xml"))  return "application/xml";
   if (iequals(ext, ".swf"))  return "application/x-shockwave-flash";
   if (iequals(ext, ".flv"))  return "video/x-flv";
   if (iequals(ext, ".png"))  return "image/png";
   if (iequals(ext, ".jpe"))  return "image/jpeg";
   if (iequals(ext, ".jpeg")) return "image/jpeg";
   if (iequals(ext, ".jpg"))  return "image/jpeg";
   if (iequals(ext, ".gif"))  return "image/gif";
   if (iequals(ext, ".bmp"))  return "image/bmp";
   if (iequals(ext, ".ico"))  return "image/vnd.microsoft.icon";
   if (iequals(ext, ".tiff")) return "image/tiff";
   if (iequals(ext, ".tif"))  return "image/tiff";
   if (iequals(ext, ".svg"))  return "image/svg+xml";
   if (iequals(ext, ".svgz")) return "image/svg+xml";
   return "application/text";
}


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

void http_worker::process_request(http::request<request_body_t, http::basic_fields<alloc_t>> const& req)
{
   switch (req.method())
   {
   case http::verb::get:
      send_file(req.target());
      break;

   default:
      // We return responses indicating an error if
      // we do not recognize the request method.
      send_bad_response(
         http::status::bad_request,
         "Invalid request-method '" + std::string(req.method_string()) + "'\r\n");
      break;
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

void http_worker::send_file(beast::string_view target)
{
   // Request path must be absolute and not contain "..".
   if (target.empty() || target[0] != '/' || target.find("..") != std::string::npos)
   {
      send_bad_response(
         http::status::not_found,
         "File not found\r\n");
      return;
   }

   std::string full_path = doc_root_;
   full_path.append(
      target.data(),
      target.size());

   http::file_body::value_type file;
   beast::error_code ec;
   file.open(
      full_path.c_str(),
      beast::file_mode::read,
      ec);
   if (ec)
   {
      send_bad_response(
         http::status::not_found,
         "File not found\r\n");
      return;
   }

   file_response_.emplace(
      std::piecewise_construct,
      std::make_tuple(),
      std::make_tuple(alloc_));

   file_response_->result(http::status::ok);
   file_response_->keep_alive(false);
   file_response_->set(http::field::server, "Beast");
   file_response_->set(http::field::content_type, mime_type(std::string(target)));
   file_response_->body() = std::move(file);
   file_response_->prepare_payload();

   http::async_write(
      socket_,
      *file_response_,
      [this](beast::error_code ec, std::size_t)
   {
      socket_.shutdown(tcp::socket::shutdown_send, ec);
      file_response_.reset();
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

   void CarpaccioStream::write(boost::beast::http::verb requestType, const std::string & target)
   {
      // Set up an HTTP GET request message
      http::request<http::string_body> request{ requestType, target, version };
      request.set(http::field::host, m_serverHost);
      request.set(http::field::user_agent, BOOST_BEAST_VERSION_STRING);

      // Send the HTTP request to the remote host
      http::write(m_stream, request);
   }
   
} // namespace extreme_carpaccio_client

