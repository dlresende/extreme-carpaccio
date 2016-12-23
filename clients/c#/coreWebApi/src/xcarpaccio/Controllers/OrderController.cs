using Microsoft.AspNetCore.Mvc;
using XCarpaccio.Models;

namespace XCarpaccio.Controllers
{
    [Route("[controller]")]
    public class OrderController : Controller
    {
        public OrderController(){
        }
        
        [HttpPost]
        public IActionResult Post([FromBody]Order order)
        {
            return this.NotFound();
        }
    }
}