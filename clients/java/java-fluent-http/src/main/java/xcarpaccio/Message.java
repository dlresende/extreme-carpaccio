package xcarpaccio;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public class Message {

    final String type;
    final String content;

    @JsonCreator
    public Message(
            @JsonProperty("type") String type,
            @JsonProperty("content") String content) {
        this.type = type;
        this.content = content;
    }
}
