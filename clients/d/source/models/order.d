module models.order;

struct Order
{
    string reduction;
    long[] quantities;
    string country;
    double[] prices;
}