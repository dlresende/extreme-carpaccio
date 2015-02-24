package katazon;

import com.google.gson.Gson;
import spark.ResponseTransformer;

/**
* @author <a href="http://twitter.com/aloyer">@aloyer</a>
*/
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
