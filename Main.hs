import Control.Monad
import Data.Char
import TicTacToeLA
-- TODO: try to create the tic-tac-toe game now. Massage lib as needed.

main = do
  putStrLn "THIS IS THE START OF THE GAME"
  let bs0 = Just $ initBoardState 3
  play 1 bs0 -- Start with player 1 TODO: get rid of hardcoded numbers
{-
  forever $ do
    putStr "==========\nTURN #\n==========\n"
    putStrLn "Player X, enter a cell to mark. Ex: \"1 2\""
    xcell <- getLine
    putStrLn $ T.hello
    putStrLn $ "TODO: print f(xcell) here.\n"
    putStrLn "Player O, enter a cell to mark. Ex: \"1 2\""
    ycell <- getLine
    putStrLn $ T.hola
    putStrLn $ "TODO: print f(ycell) here.\n"
-}



play :: Marker -> Maybe BoardState -> IO ()
play _ Nothing = do
  putStrLn "OH NO!"
play mrk (Just bs) = do
  putStrLn $ show $ currBoard bs
  putStrLn "Player TODO, enter a space to mark. Ex: \"1\n2\""
  xStr <- getLine
  yStr <- getLine
  let space = (read xStr, read yStr)
  play 1 $ tryApplyMove mrk space bs
