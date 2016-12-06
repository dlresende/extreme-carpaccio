using System.IO;
using System.Linq;
using Microsoft.AspNetCore.Hosting;
using XCarpaccio;

namespace WebApplication
{
    public class Program
    {
        public static void Main(string[] args)
        {
            var port = 5000.ToString();
            if (args != null && args.Any())
            {
                port = args[0];
            }
            
            var host = new WebHostBuilder()
                .UseKestrel()
                .UseContentRoot(Directory.GetCurrentDirectory())
                .UseStartup<Startup>()
                .UseUrls($"http://*:{port}")
                .Build();

            host.Run();
        }
    }
}
