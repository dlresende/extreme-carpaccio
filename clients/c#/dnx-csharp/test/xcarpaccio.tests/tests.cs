namespace XCarpaccio.Tests
{
    using Xunit;

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
