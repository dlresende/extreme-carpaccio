package xcarpaccio;

public class Logger {
    public void log(String message) {
        System.out.println(message);
    }

    public void log(Exception ex) {
        ex.printStackTrace();
    }

    public void error(String message) {
        System.err.println(message);
    }
}
