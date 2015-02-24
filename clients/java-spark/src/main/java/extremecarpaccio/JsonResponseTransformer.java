package extremecarpaccio;

import com.google.gson.Gson;
import spark.ResponseTransformer;

public class JsonResponseTransformer implements ResponseTransformer {
    private final Gson gson;

    public JsonResponseTransformer(Gson gson) {
        this.gson = gson;
    }

    @Override
    public String render(Object o) throws Exception {
        return gson.toJson(o);
    }
}
