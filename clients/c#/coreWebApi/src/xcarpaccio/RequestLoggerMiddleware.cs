using System.IO;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.Logging;

namespace XCarpaccio
{
    public class RequestLoggerMiddleware
    {
        private readonly RequestDelegate _next;
        private readonly ILogger _logger;

        public RequestLoggerMiddleware(RequestDelegate next, ILoggerFactory loggerFactory)
        {
            _next = next;
            _logger = loggerFactory.CreateLogger<RequestLoggerMiddleware>();
        }

        public async Task Invoke(HttpContext context)
        {
            using (var bodyReader = new StreamReader(context.Request.Body))
            {
                string body = await bodyReader.ReadToEndAsync();
                context.Request.Body = new MemoryStream(System.Text.Encoding.UTF8.GetBytes(body));
                _logger.LogDebug(body);
            }   
            await _next.Invoke(context);
        }
    }
}