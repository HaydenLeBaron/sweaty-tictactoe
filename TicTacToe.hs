module TicTacToe
( hello
, hola
) where


-- TODO: delete these demo functions
hello = "Hello!"
hola = "Hola!"

-- A Player `V` places marks of `V` on the game board.
data Marker = X | O

{-
From Learn You A Haskell For Great Good: "A Fistful of Monads":
```
Monads are a natural extension of applicative functors and with them we're concerned with this: if you have a value with a context, m a, how do you apply to it a function that takes a normal a and returns a value with a context? That is, how do you apply a function of type a -> m b to a value of type m a? So essentially, we will want this function:
(>>=) :: (Monad m) => m a -> (a -> m b) -> m b
```
type Board = [[Marker]]
data BoardState = { currBoard :: Board      -- The current board
                  , whoMoved  :: Marker     -- The player who moved last
                  , lastMove  :: (Int, Int) -- The coordinate pair of the last move
                  }

Turn is like Maybe, except instead of being
Nothing or Just x, it can be Valid x, Invalid, Victory x
```
instance Monad Maybe where
    return x = Just x
    Nothing >>= f = Nothing
    Just x >>= f  = f x
    fail _ = Nothing
```

```
-- TODO: add constraint that x must be a game board
instance Monad Turn where
   return x = Valid x            -- The default minimal context of x is Valid x
   Invalid x >>= f = Invalid x   -- If x is invalid, we won't apply any transformation f to it.
   Valid x >>= f = f x           -- If x is a valid board, we'll allow playerss to continue moving on it
   Victory x >>= f = x           -- If we have a board that has already been won, we won't apply any further transformations to it.
   fail _ = Invalid x            -- So if there's a pattern match failure in a do expression, a failure occurs within the context of a probability list.
```

Our functions f are going to be of the form:
(BoardState -> TurnState BoardState) which follows the pattern (a -> m b).

But we will always have f(TurnState BoardState)

-}
