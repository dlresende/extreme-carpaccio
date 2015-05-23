#!/bin/bash

while true; do

    cabal test --test-option=--color
    echo ""

    inotifywait -qr -e modify -e create -e move -e delete src test --exclude "\.\#.*"
done
