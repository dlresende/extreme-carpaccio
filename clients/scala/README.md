# Extreme Carpaccio - Scala Client

Minimal Scala starter using http4s.

## Prerequisites
- JDK 17+
- [sbt](https://www.scala-sbt.org/)

## Install & Run
```bash
sbt run
```

## Endpoints
- `POST /ping`: Responds with `pong`

```bash
curl -X POST http://localhost:3000/ping
```

## Tests
```bash
sbt test
```
