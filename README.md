# Sweaty Tic-Tac-Toe

Tic-Tac-Toe made for sweaty tryhards, by a sweaty tryhard--using the sweatiest language of them all: Haskell.

This repository contains:
- **CLI client**
  - Allows 2 people to play tic-tac-toe locally (hotseat)
  - Supports NxN dimensional tic-tac-toe boards
- **Game history HTTP server**
  - Tracks the result of every game ever played on every client instance.
  - Immortalizes each glorious victory and shameful defeat.

🚡
<img width="689" alt="Screen Shot 2022-01-13 at 7 27 53 PM" src="https://user-images.githubusercontent.com/43355097/149441230-11be7d28-462f-4cc8-ab5c-2b16891ea127.png">

<img width="689" alt="Screen Shot 2022-01-13 at 7 31 21 PM" src="https://user-images.githubusercontent.com/43355097/149441908-6e201476-a40d-4393-89a7-8abf82716bc4.png">

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
