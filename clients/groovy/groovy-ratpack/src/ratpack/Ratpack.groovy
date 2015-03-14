import static ratpack.groovy.Groovy.ratpack
import groovy.json.JsonSlurper
import groovy.util.logging.Slf4j;




ratpack {
    handlers {
		
		get("hello") {
			render "Hello world!"
		}
		post("ping") {
			
			render "pong"
		}
		
		post("") {
			if (request.body.text) {
				def body = new JsonSlurper().parseText(request.body.text)
				println "Incoming request on '/': $body"
			}
			
			render "pong"
		}
		
    }
}



public String asJSON(total) {
	'{ "total": ' + total + ' }'
} 