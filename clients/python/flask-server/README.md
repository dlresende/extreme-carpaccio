# Extreme Carpaccio - Python Flask Client

Minimalistic Flask starter client for Extreme Carpaccio.

## Prerequisites
- [Python](https://www.python.org/) (>= 3.8)

## Install & Run
```bash
pip install -r requirements.txt
python3 __main__.py
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
