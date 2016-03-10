package xcarpaccio;

public class Result {
	public Double total;

	public Result() { // Empty constructor required by Jackson
	}

	public Result(Double total) {
		this.total = total;
	}

}
