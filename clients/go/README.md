# Extreme Carpaccio - Go Client

Minimalistic Go starter client (standard library `net/http`, 0 external dependencies) for Extreme Carpaccio.

## Prerequisites
- [Go](https://go.dev/) (>= 1.21)

## Install & Run
```bash
go run main.go
```

## Endpoints
- `POST /ping`: Responds with `pong`

```bash
curl -X POST http://localhost:3000/ping
```

## Tests
```bash
go test ./...
```
