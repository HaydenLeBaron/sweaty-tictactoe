module TicTacToeLA where

import Data.Matrix
import qualified Data.Vector as V

type Marker = Int
type Board = Matrix Marker
type Space = (Int, Int) -- NOTE: Spaces of a Data.Matrix (and Board) are 1-indexed
data BoardState = BoardState { currBoard :: Board
                             , prevBoard :: Board
                             } deriving (Show, Eq)

{- Takes a dimension for the board and creates the
initial board state -}
initBoardState :: Int -> BoardState
initBoardState dim = BoardState { currBoard = zero dim dim
                                , prevBoard = identity dim } -- FIXME: Currently, I'm making prevboard be different than currboard so that when whoMovedLast is called, an exception isn't thrown (due to call to head on an empty list)

{- Takes board state, returning the last space moved to
and who moved there -}
-- TODO: handle possibility of empty list (when curr == prev => no changes to board)
whoMovedLast :: BoardState -> Marker
whoMovedLast (BoardState curr prev)
  = abs . getNon0Elt $ prev - curr
  where getNon0Elt = head . filter (/= 0) . toList

whereLastMoved :: BoardState -> Space
whereLastMoved (BoardState curr prev) =
  getNon0EltSpace $ prev - curr
  where dim = nrows curr
        getNon0EltSpace = fst . head . filter (\((_,_), e) -> e /= 0)
                          . zip [(x,y) | x <- [1..dim], y <- [1..dim]]
                          . toList :: Board -> Space


-- Algorithm:
-- 0. Generate new board from Space.
  -- Then between the old and new board:
-- 1. Make sure all non-zero cells stayed the same
-- 2. Make sure only one cell was changed

{-
Takes a
  * Marker, representing whose turn it is
  * Space, representing the place player#Marker wants to mark
  * Board, representing the current board shapshot

Returns Either the updated board state if the move is valid (Right), or
the unchanged board state if the move is invalid (Left).
-}

tryApplyMove :: Marker -> Space -> BoardState -> Either (BoardState, String) BoardState
tryApplyMove mrk (x,y) (BoardState curr prev)
  | not $ inBounds (x,y) curr = Left $
    (BoardState curr prev, "Tried to mark out of bounds.")
  | not $ free (x,y) curr = Left $
    (BoardState curr prev, "That space is already taken!")
  | otherwise =  Right $ BoardState (applyMove curr) curr
  where inBounds (x,y) curr =
          x <= (nrows curr) && y <= (nrows curr)
          && x >= 1 && y >= 1
        free (x,y) curr = getElem x y curr == 0
        applyMove = setElem mrk (x,y)


gameWon :: BoardState -> Bool
gameWon (BoardState curr prev) =
  (checkRows curr) ||
  (checkCols curr) ||
  (checkDiag curr) ||
  (checkAntiDiag curr)
  where heMovedLast = whoMovedLast (BoardState curr prev)
        dim = nrows curr
        unitvec = colVector $ V.replicate dim 1
        checkFor3 = (foldr (\x acc -> acc || x == heMovedLast * dim) False)
          :: Board -> Bool
        antitrace m = sum $ V.generate k $ \i -> m ! ((ncols m) - i, i+1)
          where k = min (nrows m) (ncols m)
        -- TODO: add this to the matrix library with a PR?
        --
        checkCols = checkFor3 . (*unitvec)
        checkRows = checkCols . transpose
        checkDiag b = (heMovedLast * dim) == trace b
        checkAntiDiag b = (heMovedLast * dim) == antitrace b
