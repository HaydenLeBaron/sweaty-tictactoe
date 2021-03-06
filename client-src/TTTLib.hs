module TTTLib where

import           Data.Matrix
import qualified Data.Vector as V

{- Represents both a player, and the markers they put on the board. 
 - E.g. the "X" player places "X" markers 
 - (except they are represented as Ints. -}
type Marker = Int

{- A board is a Matrix-}
type Board = Matrix Marker

{- A space is a one-indexed ordered pair -}
type Space = (Int, Int) -- NOTE: Spaces of a Data.Matrix (and Board) are 1-indexed

{- Board state is represented as the two most recent boards. 
 - All information is derived from their delta. -}
data BoardState = BoardState { currBoard :: Board
                             , prevBoard :: Board
                             } deriving (Show, Eq)

{- Takes a dimension for the board and creates the 
 - initial board state -}
initBoardState :: Int -> BoardState
initBoardState dim = BoardState { currBoard = zero dim dim
                                , prevBoard = identity dim }

{- Takes a board state, returning who last moved -}
whoMovedLast :: BoardState -> Marker
whoMovedLast (BoardState curr prev)
  = abs . getNon0Elt $ prev - curr
  where getNon0Elt = head . filter (/= 0) . toList

{- Takes a board state, returning the space last moved to -}
whereLastMoved :: BoardState -> Space
whereLastMoved (BoardState curr prev) =
  getNon0EltSpace $ prev - curr
  where dim = nrows curr
        getNon0EltSpace = fst . head . filter (\((_,_), e) -> e /= 0)
                          . zip [(x,y) | x <- [1..dim], y <- [1..dim]]
                          . toList :: Board -> Space

{- Takes the current player and the dimensions
of the board and returns the next player who would move -}
getNxtPlayer :: Marker -> Int -> Marker
getNxtPlayer curr dim
  | curr == dim + 1 = 1
  | curr < dim = curr + dim
  | otherwise = error "Couldn't get next player"

{- Takes the current player and the dimensions
of the board and returns the player who should move
previous to the current player. -}
getPrevPlayer :: Marker -> Int -> Marker
getPrevPlayer curr dim
  | curr == 1 = dim + 1
  | curr > 1 = curr - dim
  | otherwise = error "Couldn't get previous player"


{-
Takes a Marker, representing whose turn it is
  * Space, representing the place player Marker wants to mark
  * Board, representing the current board shapshot
  Returns Either the updated board state if the move is valid (Right), or
the unchanged board state if the move is invalid (Left).
-}
tryApplyMove :: Marker -> Space -> BoardState -> Either (BoardState, String) BoardState
tryApplyMove mrk (x,y) (BoardState curr prev)
  | not $ inBounds (x,y) curr = Left
    (BoardState curr prev, "Tried to mark out of bounds.")
  | not $ free (x,y) curr = Left
    (BoardState curr prev, "That space is already taken!")
  | otherwise =  Right $ BoardState (applyMove curr) curr
  where inBounds (x,y) curr =
          x <= nrows curr && y <= nrows curr
          && x >= 1 && y >= 1
        free (x,y) curr = getElem x y curr == 0
        applyMove = setElem mrk (x,y)

{- Returns true if the game has been won. False otherwise -}
gameWon :: BoardState -> Bool
gameWon (BoardState curr prev) =
  checkRows curr ||
  checkCols curr ||
  checkDiag curr ||
  checkAntiDiag curr
  where heMovedLast = whoMovedLast (BoardState curr prev)
        dim = nrows curr
        unitvec = colVector $ V.replicate dim 1
        checkFor3 = foldr (\x acc -> acc || x == heMovedLast * dim) False
          :: Board -> Bool
        antitrace m = sum $ V.generate k $ \i -> m ! (ncols m - i, i+1)
          where k = min (nrows m) (ncols m)
        checkCols = checkFor3 . (*unitvec)
        checkRows = checkCols . transpose
        checkDiag b = (heMovedLast * dim) == trace b
        checkAntiDiag b = (heMovedLast * dim) == antitrace b

{- Returns True if there exists an empty space on the board. False otherwise. -}
existsEmptySpace :: BoardState -> Bool
existsEmptySpace (BoardState curr _) = 0 `elem` toList curr
