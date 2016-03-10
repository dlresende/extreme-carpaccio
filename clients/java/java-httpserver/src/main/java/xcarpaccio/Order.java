package xcarpaccio;

import java.util.Arrays;

public class Order {
	public Double[] prices;
	public Double[] quantities;
	public String country;
	public String reduction;

	@Override
	public String toString() {
		final StringBuilder sb = new StringBuilder("Order{");
		sb.append("prices=").append(Arrays.toString(prices));
		sb.append(", quantities=").append(Arrays.toString(quantities));
		sb.append(", country='").append(country).append('\'');
		sb.append(", reduction='").append(reduction).append('\'');
		sb.append('}');
		return sb.toString();
	}
}
