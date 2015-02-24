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
                return Response.AsJson(new { total=0 });
            };
        }
    }
}