module Main where
import Boards
import UI

main :: IO ()
main = do 
  gameTick initialBoard
  
gameTick :: Board -> IO ()
gameTick board = do
  putStrLn (printBoard board)
  putStrLn (show (listAllMoves board))
  move <- getLine
  gameTick board