A Java client to with a simple net.codestory.http.WebServer implementation.

## Dependencies
- maven 3
- JDK 8

## Install
- `mvn clean package`

## Run
You don't need any application server. No Tomcat, no WAR to deploy. The server is embedded.

- `mvn exec:java -Dexec.mainClass="xcarpaccio.MyFluentHttpServer"`

The server listens on port 9000: [http://localhost:9000/ping](http://localhost:9000/ping)

You can also launch the server with your IDE. You just have to launch `MyFluentHttpServer` class. It has a `main` method.
