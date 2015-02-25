namespace katazon.client
{
    using Nancy;
    using System;
    using System.Collections.Generic;
    using Nancy.ModelBinding;

    public class IndexModule : NancyModule
    {
        public IndexModule()
        {
            Post["/"] = _ =>
            {
                var param = this.Bind<Order>();
                return Response.AsJson(new { total = 0 });
            };
        }
    }

    public class Order
    {
        public List<decimal> prices { get; set; }
        public List<int> quantities { get; set; }
        public string country { get; set; }
    }
}