# Extreme Carpaccio - Clojure Client

Minimal Clojure starter using Compojure and Ring, served by Jetty.

## Prerequisites
- [Leiningen](https://leiningen.org/) (>= 2.0)
- JDK (>= 17)

## Install & Run
```bash
lein run
```

## Endpoint
`POST /ping` responds with `pong`:
```bash
curl -X POST http://localhost:3000/ping
```

## Tests
```bash
lein test
```
