# Extreme Carpaccio - Rails Client

Minimal Ruby starter using Rails in API-only mode.

## Prerequisites
- [Ruby](https://www.ruby-lang.org/) (>= 3.2)

## Install & Run
```bash
bundle install
bin/rails server
```

## Endpoint
`POST /ping` responds with `pong`:
```bash
curl -X POST http://localhost:3000/ping
```

## Tests
```bash
bin/rails test
```
