import Control.Monad
import Data.Char
import qualified TicTacToeLA as T
-- TODO: try to create the tic-tac-toe game now. Massage lib as needed.

main = forever $ do
    putStr "==========\nTURN #\n==========\n"
    putStrLn "Player X, enter a cell to mark. Ex: \"1 2\""
    xcell <- getLine
    putStrLn $ T.hello
    putStrLn $ "TODO: print f(xcell) here.\n"
    putStrLn "Player O, enter a cell to mark. Ex: \"1 2\""
    ycell <- getLine
    putStrLn $ T.hola
    putStrLn $ "TODO: print f(ycell) here.\n"
