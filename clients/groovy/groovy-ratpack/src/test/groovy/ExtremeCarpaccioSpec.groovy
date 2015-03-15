import ratpack.groovy.test.GroovyRatpackMainApplicationUnderTest
import ratpack.http.Status;
import ratpack.http.client.RequestSpec;
import ratpack.test.ServerBackedApplicationUnderTest
import ratpack.test.http.TestHttpClient
import spock.lang.Specification


class ExtremeCarpaccioSpec extends Specification {

	ServerBackedApplicationUnderTest aut = new GroovyRatpackMainApplicationUnderTest()
	@Delegate TestHttpClient client = TestHttpClient.testHttpClient(aut)
	
	
	def "a GET on /hello returns 'Hello world!"() {
		when:
			get("hello")
			
		then: 
			response.statusCode == 200
			response.body.text == "Hello world!"
	}
	
	def "a POST request on /ping returns 'pong'"() {
		when:
			post("ping")
			
		then:
			response.statusCode == 200
			response.body.text == "pong"
			
	}
	
	def "index accepts JSON body"() {
		when:
		requestSpec { RequestSpec requestSpec ->
			requestSpec.body.type("application/json")
			requestSpec.body.text('{ "foo": "bar" }')
		}
		
		post("/")
		
		then:
			response.statusCode == 200
			response.body.text == "pong"
	}
}
