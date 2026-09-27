# Extreme Carpaccio - Erlang Client

Minimal Erlang starter using Cowboy.

## Prerequisites
- [Erlang/OTP](https://www.erlang.org/downloads) (>= 25)
- [rebar3](https://rebar3.org/)

## Install & Run
```bash
rebar3 compile
rebar3 shell
```
Then start the client from the shell:
```erlang
xcarpaccio:start().
```

## Endpoints
- `POST /ping`: Responds with `pong`

```bash
curl -X POST http://localhost:3000/ping
```

## Tests
```bash
rebar3 eunit
```
