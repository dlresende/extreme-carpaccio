namespace XCarpaccio
{
    using Microsoft.AspNet.Builder;
    using Nancy.Owin;
    using static System.Console;

    public class Startup
    {
        public void Configure(IApplicationBuilder app)
        {
          app.Use( (ctx,next) => {
            WriteLine($"[{ctx.Request.Method}] {ctx.Request.Path}");
            return next();
          });
          app.UseOwin(x => x.UseNancy());
        }
    }
}
