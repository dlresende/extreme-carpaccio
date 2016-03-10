package xcarpaccio;

import java.io.DataOutputStream;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.ProtocolException;
import java.net.URL;

import static xcarpaccio.StringUtils.stringify;

public abstract class AbstractHttpServerTest
{
    private static final String UTF_8 = "utf-8";

    protected String get(String url) throws IOException {
        return get(url, "");
    }

    protected String get(String url, String query) throws IOException {
        URL localhost = new URL(url + "?" + query);
        HttpURLConnection connection = (HttpURLConnection) localhost.openConnection();
        connection.setRequestMethod( "GET" );
        connection.disconnect();
        return stringify(connection.getInputStream());
    }

    protected String post(String path, String body) throws IOException {
        URL url = new URL(path);
        HttpURLConnection connection = (HttpURLConnection) url.openConnection();
        connection.setDoOutput(true);
        connection.setDoInput(true);
        connection.setInstanceFollowRedirects(false);
        connection.setRequestMethod("POST");
        connection.setRequestProperty("Content-Type", "application/json");
        connection.setRequestProperty("charset", UTF_8);
        connection.setRequestProperty("Content-Length", Integer.toString(body.length()));
        connection.setUseCaches(false);
        try( DataOutputStream wr = new DataOutputStream( connection.getOutputStream())) {
            wr.write( body.getBytes(UTF_8) );
            return stringify(connection.getInputStream());
        } finally {
            connection.disconnect();
        }
    }
}
