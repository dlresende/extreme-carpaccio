
#include <extreme_carpaccio/client/Client.hpp>
#include <extreme_carpaccio/client/HttpConfig.hpp>

#include <cstdlib>
#include <iostream>

int main()
{
   std::cout << "Extreme Carpaccio server starting." << std::endl;
   extreme_carpaccio::client::CarpaccioServer server(DEFAULT_HTTP_SERVER_IP, DEFAULT_HTTP_SERVER_PORT);
   server.start();
   std::cout << "Extreme Carpaccio server stopping." << std::endl;

   return EXIT_SUCCESS;
}
