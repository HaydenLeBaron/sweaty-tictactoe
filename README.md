# Sweaty Tic-Tac-Toe

Tic-Tac-Toe made for sweaty tryhards, by a sweaty tryhard--using the sweatiest language of them all: Haskell.

This repository contains:
- **CLI client**
  - Allows 2 people to play tic-tac-toe locally (hotseat)
  - Supports NxN dimensional tic-tac-toe boards
- **Game history HTTP server**
  - Tracks the result of every game ever played on every client instance.
  - Immortalizes each glorious victory and shameful defeat.

## How to build

If you don't want to build it yourself, check the RELEASES.

Make sure you have `cabal` installed.

```
$ cabal clean && cabal build
```

## How to run

Run the client:

```
$ cabal run sweaty-client
```

Run the server:

```
$ cabal run sweaty-server
```
The default port is `3000`. You can change it by changing the value for `port` in `TTTConfig.hs`


## HTTP Server API

Get the home page:

```
curl http://localhost:3000/
```

Add a game entry to the history:

```
curl -X POST http://localhost:3000/new?winner=<victor-name>&loser=<loser-name>&boardstate=<board-state>
```

Get the HTML of a specific game:

```
curl http://localhost/game/<id>
```

Try an `<id>` of `0` to get a dummy game.
