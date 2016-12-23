using System.IO;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;

namespace XCarpaccio
{
  
  public class Program
  {
    public static void Main(params string[] args)
    {
      var host = new WebHostBuilder()
                .UseContentRoot(Directory.GetCurrentDirectory())
                .UseKestrel()
                .UseStartup<Startup>()
                .Build();
 
            host.Run();
    }
  }

}
