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
play :: Marker -> Turn -> Either (BoardState, String) BoardState -> IO ()
play mrk turn (Left (bs, msg)) = do
  putStrLn $ "=====\nINVALID MOVE: " ++ msg ++ "\n====="
  play mrk (turn - 1) (Right bs)
play mrk turn (Right bs)
  | gameWon bs = do
      putStrLn $ show $ currBoard bs
      let winner = show $ whoMovedLast bs
      putStrLn $ "Player " ++ winner ++ " won!"
  | otherwise = do
      putStrLn $ show $ currBoard bs
      putStrLn $ "Turn #" ++ show turn
        ++ ": Player " ++ show mrk
        ++ ", enter a space to mark. Ex: \"1 2\""
      xy <- safeGetMove
      putStrLn $ "MOVE: " ++ show xy
      play mrk (turn + 1) $ tryApplyMove mrk xy bs


safeGetMove :: IO (Int, Int)
safeGetMove =
  getLine
  >>= (return) . (take 2 . words)
  >>= return . map (reads :: ReadS Int)
  >>= tryExtract
    where
      tryExtract [[(x, "")], [(y, "")]] = return (x, y)
      tryExtract _ = putStrLn "Couldn't parse. Please enter like so: x y" >> safeGetMove

{-
-- NOTE: I'm probably not going to use this one since then I have to deal with maybe
safeGetMove' :: IO (Maybe Int, Maybe Int)
safeGetMove' =
  let tup [x, y] = (x, y)
      tup _ = (Nothing, Nothing)
  in
    getLine
    >>= (return) . (take 2 . words)
    >>= mapM (return . readMaybe)
    >>= return . tup


-- NOTE: I'm doing it unsafely I read that exception catching/handling is OK practice in the IO monad
getMove :: IO (Int, Int)
getMove =
  let tup [x, y] = (x, y)
  in
    getLine
    >>= (return) . (take 2 . words)
    >>= return . map (read :: String -> Int)
    >>= return . tup

 getLine >>= (return) . (take 2 . words) >>= return . map (read :: String -> Int) >>= return . tup
-}
