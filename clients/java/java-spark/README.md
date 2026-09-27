# Extreme Carpaccio - Java Spark Client

Minimal Java starter using Spark Java.

## Prerequisites
- JDK 17+
- Maven

## Install & Run
```bash
mvn compile exec:java -Dexec.mainClass=extremecarpaccio.HttpServer
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
