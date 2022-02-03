#include <boost/asio.hpp>
#include <boost/beast/core.hpp>
#include <boost/beast/http.hpp>
#include <boost/beast/version.hpp>

#include <extreme_carpaccio_client/config.hpp>

#include <string>

namespace extreme_carpaccio_client {

class http_worker
{
public:
   http_worker(http_worker const&) = delete;
   http_worker& operator=(http_worker const&) = delete;

   http_worker(boost::asio::ip::tcp::acceptor& acceptor, const std::string& doc_root);

   void start();

private:
   using alloc_t = std::allocator<char>;
   //using request_body_t = http::basic_dynamic_body<beast::flat_static_buffer<1024 * 1024>>;
   using request_body_t = boost::beast::http::string_body;

   // The acceptor used to listen for incoming connections.
   boost::asio::ip::tcp::acceptor& acceptor_;

   // The path to the root of the document directory.
   std::string doc_root_;

   // The socket for the currently connected client.
   boost::asio::ip::tcp::socket socket_{ acceptor_.get_executor() };

   // The buffer for performing reads
   boost::beast::flat_static_buffer<8192> buffer_;

   // The allocator used for the fields in the request and reply.
   alloc_t alloc_;

   // The parser for reading the requests
   boost::optional<boost::beast::http::request_parser<request_body_t, alloc_t>> parser_;

   // The timer putting a time limit on requests.
   boost::asio::steady_timer request_deadline_{
       acceptor_.get_executor(), (std::chrono::steady_clock::time_point::max)() };

   // The string-based response message.
   boost::optional<boost::beast::http::response<boost::beast::http::string_body, boost::beast::http::basic_fields<alloc_t>>> string_response_;

   // The file-based response message.
   boost::optional<boost::beast::http::response<boost::beast::http::file_body, boost::beast::http::basic_fields<alloc_t>>> file_response_;

   void accept();

   void read_request();

   void process_request(boost::beast::http::request<request_body_t, boost::beast::http::basic_fields<alloc_t>> const& req);

   void send_bad_response(
      boost::beast::http::status status,
      std::string const& error);

   void check_deadline();

   bool handleRequest(boost::beast::http::verb requestType, const std::string & target, const std::string & contentType, const std::string & body);
};

class EXTREME_CARPACCIO_CLIENT_API CarpaccioServer
{
public:
   CarpaccioServer(unsigned short port);
   void start();
   void stop();

private:
   boost::asio::io_context ioc;
   boost::asio::ip::tcp::acceptor acceptor;
   http_worker worker;
};

class EXTREME_CARPACCIO_CLIENT_API CarpaccioStream
{
public:
   CarpaccioStream(const std::string & host, unsigned short port);
   ~CarpaccioStream();

   boost::beast::http::response<boost::beast::http::dynamic_body> read(boost::beast::flat_buffer&);
   void write(boost::beast::http::verb requestType, const std::string & target, const std::string & contentType, const std::string & body);

private:
   std::string m_serverHost;
   boost::asio::io_context m_ioContext;
   boost::asio::ip::tcp::resolver m_resolver;
   boost::asio::ip::tcp::resolver::results_type m_resolverResults;
   boost::beast::tcp_stream m_stream;
};

} // namespace extreme_carpaccio_client
