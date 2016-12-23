using System.Net;
using Xunit;
using XCarpaccio.Models;

namespace XCarpaccio.Tests
{
    
    public class WalkingSkeleton
    {
        [Fact]
        public void feedback_model_prints_nicely()
        {
            var feedback = new Feedback() { Type = "Error", Content = "Hello" };
            Assert.Equal("[Error] Hello", feedback.ToString());
        }
    }    
}
