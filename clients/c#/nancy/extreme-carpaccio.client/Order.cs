using System.Collections.Generic;

namespace xCarpaccio.client
{
    class Order
    {
        public decimal[] prices { get; set; }
        public int [] quantities { get; set; }
        public string country { get; set; }
        public string reduction { get; set; }
    }
}