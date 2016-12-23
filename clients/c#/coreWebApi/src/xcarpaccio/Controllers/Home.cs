using Microsoft.AspNetCore.Mvc;

namespace XCarpaccio.Controllers
{
    [Route("/")]
    public class HomeController : Controller
    {
       
       [HttpGet]
       public IActionResult Get()
       {
           return Content("Hello world of Carpaccio!");
       }       
    }
}