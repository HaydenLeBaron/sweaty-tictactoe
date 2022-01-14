# README

## Building

```
cabal clean
```

```
cabal build
```
-- TODO: make sure tokens names are presented to the player in a sensible way (e.g. not player 1 and 4).
-- TODO: Write build and run instructions and test them on a fresh clone of the repo
-- TODO: do testing with quickcheck
-- TODO: include a gif or picture in the README
-- TODO: link the project to my website, haydenlebaron.com
-- TODO: Generalize to N players.
-- TODO: prettify code with haskell prettifier.
-- TODO: compile document about what I learned and the resources I used.
-- TODO: Write all functions to never fail.

## Run the client

```
$ cabal run sweaty-client
```

### Client Bugs
- [ ] Throws exception if you do 1x1 matrix and try to make a move out of bounds (can't get next player)


## Run the server

```
$ cabal run sweaty-server
```

## API

- POST localhost:3000/new?winner=VICTORIA&loser=LAUSER&boardstate=hello world%3B

## Change server port

change the value for `port` in `TTTConfig.hs`
