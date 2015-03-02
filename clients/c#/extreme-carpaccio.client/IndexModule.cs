using Nancy.Extensions;

namespace katazon.client
{
    using Nancy;
    using System;
    using Nancy.ModelBinding;

    public class IndexModule : NancyModule
    {
        public IndexModule()
        {
            Post["/"] = _ =>
            {
                Console.WriteLine("{0}", this.Request.Body.ToString());
                return "";
            };

            Post["/feedback"] = _ =>
            {
                var feedback = this.Bind<Feedback>();
                HandleFeedback(feedback);
                return "";
            };
        }

        private void HandleFeedback(Feedback feedback)
        {
            Console.Write("Type: {0}: ", feedback.type);
            Console.WriteLine(feedback.content);
        }        
    }
}