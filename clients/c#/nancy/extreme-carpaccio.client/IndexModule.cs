
using System.IO;
using System.Net;
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
                var order = this.Bind<Order>();
               var bill = HandlerOrder(order);
                if (bill == null)
                {
                    return new {};
                 
                }
                else
                {
                    return bill;

                }
               
            };

            Post["/feedback"] = _ =>
            {
                var feedback = this.Bind<Feedback>();
       
                HandleFeedback(feedback);
                return Negotiate.WithStatusCode(HttpStatusCode.OK);
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


        private Bill HandlerOrder(Order order)
        {
            // TODO HERE
            return null;
        }        
    }
}