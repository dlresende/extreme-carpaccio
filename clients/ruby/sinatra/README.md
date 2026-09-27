# Extreme Carpaccio - Sinatra Client

Minimal Ruby starter using Sinatra.

## Prerequisites
- [Ruby](https://www.ruby-lang.org/) (>= 3.2)

## Install & Run
```bash
bundle install
bundle exec rackup -p 3000
```

## Endpoints
- `POST /ping`: Responds with `pong`

```bash
curl -X POST http://localhost:3000/ping
```

## Tests
```bash
bundle exec rake test
```
