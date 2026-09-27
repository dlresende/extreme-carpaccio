# Extreme Carpaccio - Racket Client

Minimal Racket starter using the built-in web server, with no external packages.

## Prerequisites
- [Racket](https://racket-lang.org/) (>= 8.0)

## Install & Run
```bash
racket -t main.rkt -- --port 3000
```

## Endpoints
- `POST /ping`: Responds with `pong`

```bash
curl -X POST http://localhost:3000/ping
```

## Tests
```bash
raco test tests/
```
