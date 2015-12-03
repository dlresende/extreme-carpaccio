A Java client to with a simple com.sun.net.httpserver.HttpServer implementation.

## Dependencies
- maven 3
- JDK 8

## Install
- `mvn clean install`

## Code
The game server will post orders on /order endpoint.
OrderHttpHandler inner class inside MyHttpServer.java will handle this.

## Run
- `mvn exec:java -Dexec.mainClass="xcarpaccio.MyHttpServer"`

The server listens on port 9000: [http://localhost:9000/ping](http://localhost:9000/ping)

Note that you can also use your IDE to launch it.
