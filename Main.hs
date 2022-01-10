import Control.Monad
import Data.Char
import TicTacToeLA
import Text.Read -- TODO: only import readMaybe
-- TODO: try to create the tic-tac-toe game now. Massage lib as needed.

type Turn = Int

main = do
  putStrLn "THIS IS THE START OF THE GAME"
  -- TODO: Ask the dimensions of the board
  -- TODO: Ask how many players there are
  let bs0 = Right $ initBoardState 3
  play 1 1 bs0 -- Start with player 1 TODO: get rid of hardcoded numbers


-- TODO: handle case where there are no spaces left (DRAW)
play :: Marker -> Turn -> Either BoardState BoardState -> IO ()
play mrk turn (Left bs) = do
  putStrLn "=====\nINVALID MOVE\n====="
  play mrk (turn - 1) (Right bs)
play mrk turn (Right bs)
  | gameWon bs = do
      putStrLn $ show $ currBoard bs
      let winner = show $ whoMovedLast bs
      putStrLn $ "Player " ++ winner ++ " won!"
  | otherwise = do
      putStrLn $ show $ currBoard bs
      putStrLn $ "Turn #" ++ show turn ++ ": Player " ++ show mrk ++ ", enter a space to mark. Ex: \"1\n2\""
      xStr <- getLine
      yStr <- getLine
      let space = (read xStr, read yStr)
      play mrk (turn + 1) $ tryApplyMove mrk space bs


{- Get the first two tokens the user enters and -}
getMove :: IO (Maybe Int, Maybe Int)
getMove =
  let tup [x, y] = (x, y)
  in
    getLine
    >>= (return) . (take 2 . words)
    >>= mapM (return . readMaybe)
    >>= return . tup
