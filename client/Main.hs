{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad
import           Data.Char
import qualified Data.Matrix   as M
import           TTTLib
import qualified Network.HTTP.Simple as SIMPLEHTTP
import qualified Data.ByteString.Char8 as BSC
import TTTConfig as CFG

-- TODO: save port in a variable in a file instead of hardcoding
-- TODO: save BSC api path in a file instead of hardcoding

type Turn = Int

main = do
  putStrLn "================================================="
  putStrLn "== THIS IS THE START OF THE GAME"
  putStrLn "================================================="
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
      print $ currBoard bs
      let winner = show $ whoMovedLast bs
      let finalBoardStr = show $ M.toLists $ currBoard bs
      putStrLn "***************************************"
      putStrLn $ "PLAYER " ++ winner ++ " WON!"
      putStrLn "***************************************"
      putStrLn "What is the name of the winner?"
      winnerName <- getLine
      putStrLn "What is the name of the loser?"
      loserName <- getLine
      let path = BSC.pack $ "/new?winner=" ++ winnerName ++ "&loser=" ++ loserName ++ "&boardstate=" ++ finalBoardStr
      -- Send HTTP request (https://livebook.manning.com/book/get-programming-with-haskell/chapter-39/74)
      SIMPLEHTTP.httpLBS
        $ SIMPLEHTTP.setRequestMethod "POST"
        $ SIMPLEHTTP.setRequestPath path
        $ SIMPLEHTTP.setRequestPort CFG.port
        $ SIMPLEHTTP.defaultRequest
      putStrLn "\n"

  | not $ existsEmptySpace bs = do
      print $ currBoard bs
      putStrLn "IT'S A TIE!"
  | otherwise = do
      print $ currBoard bs
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

