
using System.IO;
using System.Text;

namespace xCarpaccio.client
{
    using Nancy;
    using System;
    using Nancy.ModelBinding;

    public class IndexModule : NancyModule
    {
        public IndexModule()
        {
            Post["/order"] = _ =>
            {
                var request = RequestAsString();

                Console.WriteLine("{0}", request);
                return "";
            };

            Post["/feedback"] = _ =>
            {
                var feedback = this.Bind<Feedback>();
                HandleFeedback(feedback);
                return "";
            };
        }

        private string RequestAsString()
        {
            string request;
            using (var reader = new StreamReader(Request.Body, Encoding.UTF8))
            {
                request = reader.ReadToEnd();
            }
            return request;
        }

        private void HandleFeedback(Feedback feedback)
        {
            Console.Write("Type: {0}: ", feedback.type);
            Console.WriteLine(feedback.content);
        }        
    }
}