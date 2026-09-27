# Extreme Carpaccio - Rust Client

Minimalistic Rust starter client (using `axum`) for Extreme Carpaccio.

## Prerequisites
- [Rust](https://www.rust-lang.org/) (>= 1.75)

## Install & Run
```bash
cargo run
```

## Endpoints
- `POST /ping`: Responds with `pong`

```bash
curl -X POST http://localhost:3000/ping
```

## Tests
```bash
cargo test
```
