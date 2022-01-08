import Data.Matrix as M

type Marker = Int
type Board = Matrix Marker
type Space = (Int, Int)

-- TODO: right here we could encode validity in the boardstate itself
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
  getNon0EltIdx $ prev - curr
  where dim = nrows curr
        getNon0EltIdx = fst . head . filter
                        (\((_,_), e) -> e /= 0)
                        . zip [(x,y) | x <- [0..dim-1], y <- [0..dim-1]]
                        . toList :: Board -> Space



{- Takes two boards and checks that moving from the first to
the second is a valid move as per the rules of the game. -}
--isValidMove :: Space -> Board -> Bool
-- isValidMove ... TODO: Implement
-- Algorithm:
-- 0. Generate new board from Space.
  -- Then between the old and new board:
-- 1. Make sure all non-zero cells stayed the same
-- 2. Make sure only one cell was changed

--applyMove :: Marker Space Board -> Board
--applyMove ... = TODO: Implement

-- isVictorious :: BoardState -> Maybe Marker
