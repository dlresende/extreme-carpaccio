# Extreme Carpaccio Clojure starting code

## Prerequisites

You will need **Leiningen 2.0.0** or above installed.

https://leiningen.org/#install

## Development
1. Fire up a REPL
2. Hack away, redefine functions they will be hot reloaded, live coding FTW!
3. Beat the shit out of those crappy java servers :)

## Run test
To start a web server and run tests for the application, run:

```
lein midje
```

Tests can be run in watch mode like this

```
lein midje :autotest
```


## Running in "production"
To start a web server for the application, listening on <port>, type:


```
lein ring server [port]
```

