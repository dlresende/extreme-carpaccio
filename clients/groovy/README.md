# Extreme Carpaccio - Groovy Client

Minimal Groovy starter using the JDK HTTP server, with no external runtime dependencies.

## Prerequisites
- JDK 17+

## Install & Run
```bash
./gradlew run
```

## Endpoints
- `POST /ping`: Responds with `pong`

```bash
curl -X POST http://localhost:3000/ping
```

## Tests
```bash
./gradlew test
```
