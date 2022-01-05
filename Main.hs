import Control.Monad
import Data.Char

main = forever $ do
    putStr "==========\nTURN #\n==========\n"
    putStrLn "Player X, enter a cell to mark. Ex: \"1 2\""
    xcell <- getLine
    putStrLn $ "TODO: print f(xcell) here.\n"
    putStrLn "Player O, enter a cell to mark. Ex: \"1 2\""
    ycell <- getLine
    putStrLn $ "TODO: print f(ycell) here.\n"
