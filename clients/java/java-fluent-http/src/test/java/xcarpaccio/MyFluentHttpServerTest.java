package xcarpaccio;

import static com.jayway.restassured.RestAssured.given;
import static com.jayway.restassured.RestAssured.when;
import static com.jayway.restassured.http.ContentType.JSON;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.equalTo;

import org.junit.Test;
import com.jayway.restassured.RestAssured;

public class MyFluentHttpServerTest {

    private static final int PORT = 9001;
    private static final MyFluentHttpServer myFluentHttpServer = new MyFluentHttpServer(PORT);

    static {
        RestAssured.port = PORT;
    }

    @Test public void
    should_respond_pong_when_requesting_ping() {
        when().
                get("/ping").
        then().
                statusCode(200).
                body(containsString("pong"))
        ;
    }

    @Test public void
    should_post_feedback() {
        given().
                contentType(JSON).
                body(new Message("myType", "myContent")).
        when().
                post("/feedback").
        then().
                statusCode(204)
        ;
    }

    @Test public void
    should_post_an_order() {
        given().
                contentType(JSON).
                body("{\"prices\":[31.01],\"quantities\":[8],\"names\":[\"Tea\"],\"country\":\"IT\",\"reduction\":\"STANDARD\"}").
        when().
                post("/order").
        then().
                statusCode(200).
                body(equalTo(""))
        ;
    }
}
