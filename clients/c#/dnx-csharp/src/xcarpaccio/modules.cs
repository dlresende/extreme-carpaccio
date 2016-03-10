namespace XCarpaccio
{
  using System.Collections.Generic;
  using Nancy;
  using Nancy.ModelBinding;
  using System;
  using System.Linq;
  using static System.Console;

  public class HomeModule : NancyModule
  {
      public HomeModule()
      {
          Get["/"] = p => "<h1>DNX-CSharp Carpaccio Online!</h1>";

          Post["/order"] = p =>
          {
              WriteLine($"[{DateTime.Now}]");
              return new {Ignored=true};
          };

          Post["/feedback"] = p =>
          {
              var feedback = this.Bind<Feedback>();

              WriteLine($"[{feedback.Type}] {feedback.Content}");
              return Negotiate.WithStatusCode(200);
          };
      }
  }
}
