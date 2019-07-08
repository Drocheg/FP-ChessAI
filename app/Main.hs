module Main where
import Boards
import UI
main :: IO ()
main = do 
  gameTick initialBoard
  
gameTick :: Board -> IO ()
gameTick board = do
  putStrLn (printBoard board)
  let boards = listAllMoves board
  putStrLn (printPossibleMoves  0 board boards)
  option <- getLine
  let (board, _, _) = boards!!(read option)
  putStrLn (printBoard board)
  let aIboard = chessMinimaxSorted 4 board
  gameTick aIboard

