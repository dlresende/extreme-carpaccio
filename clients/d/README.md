D language client for extreme-carpaccio game
============================================

This implementation is based on [vibe-d 0.8.6](http://vibed.org/docs). It provides basic routing and models for the following routes:
- POST `/order` (returns 404 by default)
- POST `/feedback` (returns 404 by default)

The default port is `9000`. You can pass a specific ip and port to the `HTTPServerSettings` object in `app.d` with the following syntax :

```d
auto settings = HTTPServerSettings("0.0.0.0:1337");
```


How to use this client
================

Install the [dub package manager](http://code.dlang.org/download) and a [D compiler](https://dlang.org/download.html) (dmd is the best option in this context).

Clone this repository and type dub in current directory.

Have fun!