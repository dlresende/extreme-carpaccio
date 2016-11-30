using System.Collections.Generic;
using Nancy;
using Nancy.ModelBinding;
using System;
using System.Linq;
using static System.Console;

namespace XCarpaccio
{
  public class HomeModule : NancyModule
  {
      public HomeModule()
      {
          
          Get("/" ,p => "<h1>DNX-CSharp Carpaccio Online!</h1>");

          Post("/order", p =>
          {
              WriteLine($"[{DateTime.Now}]");
              return new {Ignored=true};
          });

          Post("/feedback", p =>
          {
              try{
                var feedback = this.Bind<Feedback>();
                 WriteLine($"[{feedback.Type}] {feedback.Content}");
              }  
              catch (Exception e )
              {
                  int i = 1;
              }
              return Negotiate.WithStatusCode(200);
              
              
          });
      }
  }
}
