package xcarpaccio;

import org.codehaus.jackson.map.ObjectMapper;

import java.io.IOException;

public class Message {
    private final ObjectMapper objectMapper = new ObjectMapper();

    private String type;
    private String content;

    // Required by Jackson
    private Message() {
    }

    public Message(String type, String content) {
        this.type = type;
        this.content = content;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public String getContent() {
        return content;
    }

    public void setContent(String content) {
        this.content = content;
    }

    public String json() throws IOException {
        return objectMapper.writeValueAsString(this);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Message message = (Message) o;

        if (type != null ? !type.equals(message.type) : message.type != null) return false;
        return !(content != null ? !content.equals(message.content) : message.content != null);

    }

    @Override
    public int hashCode() {
        int result = type != null ? type.hashCode() : 0;
        result = 31 * result + (content != null ? content.hashCode() : 0);
        return result;
    }

    @Override
    public String toString() {
        return "Message{" +
                "type='" + type + '\'' +
                ", content='" + content + '\'' +
                '}';
    }
}
