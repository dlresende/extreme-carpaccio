# Extreme Carpaccio - Java JDK HTTP Server Client

Minimal Java starter using the JDK HTTP server, with no runtime dependencies.

## Prerequisites
- JDK 17+
- Maven

## Install & Run
```bash
mvn package
java -cp target/classes xcarpaccio.MyHttpServer
```

## Endpoints
- `POST /ping`: Responds with `pong`

```bash
curl -X POST http://localhost:3000/ping
```

## Tests
```bash
mvn test
```
