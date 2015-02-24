package katazon;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;

public class Strings {

    public static String stringify(InputStream is, String charset) throws IOException {
        byte[] buffer = new byte[1024];
        ByteArrayOutputStream bout = new ByteArrayOutputStream();
        int read;
        while ((read = is.read(buffer)) >= 0) {
            bout.write(buffer, 0, read);
        }
        return bout.toString(charset);
    }
}
