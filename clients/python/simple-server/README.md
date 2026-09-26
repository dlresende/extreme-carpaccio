# Extreme Carpaccio - Python Simple Client

Minimalistic plain Python starter client (no external dependencies, built-in `http.server`) for Extreme Carpaccio.

## Prerequisites
- [Python](https://www.python.org/) (>= 3.8)

## Install & Run
No dependencies to install!

```bash
python3 server.py
```

## Endpoints
- `POST /ping`: Responds with `pong`

```bash
curl -X POST http://localhost:3000/ping
```

## Tests
```bash
python3 -m unittest discover -s tests
```
