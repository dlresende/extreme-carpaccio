using System.Globalization;
using System.Threading;

namespace xCarpaccio.client
{
    using System;
    using Nancy.Hosting.Self;

    class Program
    {
        static void Main(string[] args)
        {
            var uri =
                new Uri("http://localhost:8080");

            //Thread.CurrentThread.CurrentCulture = new CultureInfo("en-us");
            //Thread.CurrentThread.CurrentUICulture = new CultureInfo("en-us");

            using (var host = new NancyHost(uri))
            {
                host.Start();

                Console.WriteLine("Your application is running on " + uri);
                Console.WriteLine("Press any [Enter] to close the host.");
                Console.ReadLine();
            }
        }
    }
}
