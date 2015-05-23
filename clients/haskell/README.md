# Extreme Carpaccio Haskell starting code

## Prerequisites

You will need [Haskell](https://www.haskell.org/) installed with `ghc`
and `cabal` available.

The application is based on the
[Scotty](https://github.com/scotty-web/scotty) web framework (a Ruby
Sinatra clone).

## Build

To build and unit-test the application:

    $ cabal install
    $ cabal sandbox init
    $ cabal build
    $ cabal test

Launch it:

    $ .cabal-sandbox/bin/carpaccio

## Test Driven Development

Code your algorithm using TDD:

1. Launch the `tdd.sh` script
2. From now, as soon you modify a source file, it will build and test
   it.
3. `ctrl-c` to exit

Happy coding!
