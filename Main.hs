import Control.Monad
import Data.Char
import TicTacToeLA
import qualified Data.Matrix as M

type Turn = Int


main = do
  putStrLn $ "================================================="
  putStrLn "== THIS IS THE START OF THE GAME"
  putStrLn $ "================================================="
  putStrLn "Enter a number for the dimensions of the board. Ex: 3"
  dim <- tenaciousGetNat
  let bs0 = Right $ initBoardState dim
  play 1 1 bs0 -- Starting turn is player 1


play :: Marker -> Turn -> Either (BoardState, String) BoardState -> IO ()
play mrk turn (Left (bs, msg)) = do
  putStrLn $ "!!!!! INVALID MOVE: " ++ msg ++ " !!!!!"
  let prevPlayer = getPrevPlayer mrk $ M.nrows (currBoard bs)
  play prevPlayer (turn - 1) (Right bs)
play mrk turn (Right bs)
  | gameWon bs = do
      putStrLn $ show $ currBoard bs
      let winner = show $ whoMovedLast bs
      putStrLn "***************************************"
      putStrLn $ "PLAYER " ++ winner ++ " WON!"
      putStrLn "***************************************"
  | existsEmptySpace bs = do
      putStrLn $ show $ currBoard bs
      putStrLn "IT'S A TIE!"
  | otherwise = do
      putStrLn $ show $ currBoard bs
      putStrLn "---------------------------------------"
      putStrLn $ "-- TURN #" ++ show turn ++ " :: Player " ++ show mrk
      putStrLn "---------------------------------------"
      putStrLn "Enter a space to mark. Ex: \"1 2\""
      xy <- tenaciousGet2Tuple
      putStrLn $ "MOVE: " ++ show xy
      let nxtPlayer = getNxtPlayer mrk $ M.nrows (currBoard bs)
      play nxtPlayer (turn + 1) $ tryApplyMove mrk xy bs


tenaciousGet2Tuple :: IO (Int, Int)
tenaciousGet2Tuple =
  getLine
  >>= return . (take 2 . words)
  >>= return . map (reads :: ReadS Int)
  >>= tryExtract
    where
      tryExtract [[(x, "")], [(y, "")]] = return (x, y)
      tryExtract _ = putStrLn
        "Couldn't parse. Please enter two numbers like so: `x y`" >> tenaciousGet2Tuple

tenaciousGetNat :: IO Int
tenaciousGetNat =
  getLine
  >>= return . (reads :: ReadS Int)
  >>= tryExtract
  where tryExtract [(d, "")]
          | d > 0 = return d
          | otherwise = putStrLn "Can't choose a negative number!" >> tenaciousGetNat
        tryExtract _ = putStrLn
          "Couldn't parse. Please enter a number like so: `x`" >> tenaciousGetNat
