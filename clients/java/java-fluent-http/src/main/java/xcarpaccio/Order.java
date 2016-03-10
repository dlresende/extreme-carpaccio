package xcarpaccio;

import java.util.List;

public class Order {
	public List<Double> prices;
	public List<Integer> quantities;
	public List<String> names;
	public String country;
	public String reduction;

	@Override
	public String toString() {
		final StringBuilder sb = new StringBuilder("Order{");
		sb.append("prices=").append(prices);
		sb.append(", quantities=").append(quantities);
		sb.append(", names=").append(names);
		sb.append(", country='").append(country).append('\'');
		sb.append(", reduction='").append(reduction).append('\'');
		sb.append('}');
		return sb.toString();
	}
}
