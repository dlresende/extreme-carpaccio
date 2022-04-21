
#include <extreme_carpaccio/client/Client.hpp>

#include <cstdlib>
#include <iostream>

int main()
{
   std::cout << "Extreme Carpaccio server starting." << std::endl;
   extreme_carpaccio::client::CarpaccioServer server;
   server.start();
   std::cout << "Extreme Carpaccio server stopping." << std::endl;

   return EXIT_SUCCESS;
}
