package xcarpaccio;
import java.io.Serializable;
import java.util.Arrays;

public class Order implements Serializable {
    public Double[] prices;
    public Long[] quantities;
    public String country;
    public String reduction;

    public Order() {
    }

    @Override
    public String toString() {
        return "Order{" +
                "prices=" + Arrays.toString(prices) +
                ", quantities=" + Arrays.toString(quantities) +
                ", country='" + country + '\'' +
                ", reduction='" + reduction + '\'' +
                '}';
    }
}