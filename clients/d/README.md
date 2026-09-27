# Extreme Carpaccio - D Client

Minimal D starter using vibe.d.

## Prerequisites
- [DMD](https://dlang.org/download.html) and [DUB](https://dub.pm/)

## Install & Run
```bash
dub run
```

## Endpoint
`POST /ping` responds with `pong`:
```bash
curl -X POST http://localhost:3000/ping
```

## Tests
```bash
dub test
```
