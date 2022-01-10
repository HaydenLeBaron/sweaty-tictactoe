module TicTacToeLA where

import Data.Matrix
import qualified Data.Vector as V

type Marker = Int
type Board = Matrix Marker
type Space = (Int, Int) -- NOTE: Spaces of a Data.Matrix (and Board) are 1-indexed
data BoardState = BoardState { currBoard :: Board
                             , prevBoard :: Board
                             } deriving (Show, Eq)

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

Returns Just the resulting Board if the move was valid,
& Nothing if invalid.
-}
-- TODO: make this return a boardstate
tryApplyMove :: Marker -> Space -> BoardState -> Maybe BoardState
tryApplyMove mrk (x,y) (BoardState curr _)
  | isSpaceEmpty (x,y) curr = Just $ BoardState (applyMove curr) curr
  | otherwise = Nothing
  where isSpaceEmpty (x,y) curr = getElem x y curr == 0
        applyMove = setElem mrk (x,y)

{- Tries to declare victory.
Returns Nothing if nobody has won yet.
Else, returns the Marker of the winner.-}
tryDeclareVictory :: BoardState -> Maybe Marker
tryDeclareVictory (BoardState curr prev)
  | checkRows curr ||
    checkCols curr ||
    checkDiag curr ||
    checkAntiDiag curr = Just heMovedLast
  | otherwise = Nothing
  where heMovedLast = whoMovedLast (BoardState curr prev)
        dim = nrows curr
        checkFor3 = (foldr (\x acc -> acc || x == heMovedLast * dim) False)
          :: Board -> Bool
        --
        checkRows b = checkFor3 $ b * (colVector (V.replicate dim 1)) -- TODO: V.replicate dim 1 should be a function called unitvec or something
        checkCols b = checkFor3 $ (rowVector (V.replicate dim 1)) * b
        checkDiag b = (heMovedLast * dim) == sum (getDiag b)
        checkAntiDiag b = False -- TODO: implement
