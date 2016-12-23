using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Logging;
using XCarpaccio.Models;

namespace XCarpaccio.Controllers
{
    [Route("[controller]")]
    public class FeedbackController : Controller
    {
        ILogger<FeedbackController> logger;
        
        public FeedbackController(ILogger<FeedbackController> logger){
            this.logger = logger;
        }
        
        [HttpPost]
        public IActionResult Post([FromBody]Feedback feedBack)
        {
            logger.LogDebug($"Feedback: Type is {feedBack.Type}, Content is {feedBack.Content}");  
            return this.Ok();          
        }
    }
}