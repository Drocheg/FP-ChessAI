module Main where
import Boards
import UI
main :: IO ()
main = do 
  gameTick initialBoard True
  
gameTick :: Board -> Bool -> IO ()
gameTick board playerTurn = do
    putStrLn (printBoard board)
    let boards = listAllMoves board
    if (null boards) then putStrLn (printGameEnd board)
    else if playerTurn then do putStrLn (printPossibleMoves 0 board boards)
                               option <- getLine
                               let (board, _, _) = boards!!(read option)
                               gameTick board (not playerTurn)
                       else do let aIBoard = chessMinimaxSorted 4 board
                               gameTick aIBoard (not playerTurn)





