A Kotlin client with Wasabi HTTP framework

## Dependencies
- gradle
- JDK 8
- Kotlin 1.0.4

## Code
The game server will post orders on `/order` endpoint.
`orderHandler` route handler in `MyServer.kt` file will handle this so this is where to start coding.

## Run
You don't need any application server. The server is embedded. Just execute the main method in the MyServer.kt file

The server listens on port 9000: [http://localhost:9000/ping](http://localhost:9000/ping)