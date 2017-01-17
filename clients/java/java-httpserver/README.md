A Java client to with a simple com.sun.net.httpserver.HttpServer implementation.

## Dependencies
- maven 3
- JDK 8

## Install
- `mvn clean install`

## Code
The game server will post orders on `/order` endpoint.
`OrderHttpHandler` inner class inside `MyHttpServer.java` file will handle this so this is where to start coding.

## Run
You don't need any application server. No Tomcat, no WAR to deploy. The server is embedded.

- `PORT=9000 java -jar target/extreme-carpaccio-java-httpserver-1.0-SNAPSHOT-jar-with-dependencies.jar`

In the example above the server listens on port 9000: [http://localhost:9000/ping](http://localhost:9000/ping)

You can also launch the server with your IDE. You just have to launch `MyHttpServer` class. It has a `main` method.
