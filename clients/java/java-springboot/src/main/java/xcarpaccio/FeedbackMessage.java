package xcarpaccio;

public class FeedbackMessage {
    public String type;
    public String content;

    @Override
    public String toString() {
        return "FeedbackMessage{" +
                "type='" + type + '\'' +
                ", content='" + content + '\'' +
                '}';
    }

    public FeedbackMessage() {
    }
}