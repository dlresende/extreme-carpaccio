# Extreme Carpaccio - Haskell Client

Minimal Haskell starter using WAI and Warp, with no web framework.

## Prerequisites
- [GHC](https://www.haskell.org/ghc/) (>= 8.10)
- [Cabal](https://cabal.readthedocs.io/) (>= 3.0)

## Install & Run
```bash
cabal run
```

## Endpoint
`POST /ping` responds with `pong`:
```bash
curl -X POST http://localhost:3000/ping
```

## Tests
```bash
cabal test
```
