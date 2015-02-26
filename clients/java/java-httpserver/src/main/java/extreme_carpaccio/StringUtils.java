package extreme_carpaccio;

import java.io.InputStream;
import java.util.Scanner;

public class StringUtils {
    public static String stringify(InputStream is) {
        Scanner scanner = new Scanner(is).useDelimiter("\\A");
        return scanner.hasNext() ? scanner.next() : "";
    }
}
