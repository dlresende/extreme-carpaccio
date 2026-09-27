# Extreme Carpaccio - PHP Client

Minimal PHP starter using the built-in web server, with no framework.

## Prerequisites
- [PHP](https://www.php.net/) (>= 8.3)

## Install & Run
```bash
composer install
composer start
```

## Endpoints
- `POST /ping`: Responds with `pong`

```bash
curl -X POST http://localhost:3000/ping
```

## Tests
```bash
composer test
```
