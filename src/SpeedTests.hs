module SpeedTests (speedTest) where

import Boards
import UI
import BoardDataTypes
import Minimax

speedTest :: (Board -> MovePath Board) -> (Board -> MovePath Board) -> IO ()
speedTest whiteMinimax blackMinimax = gameTick whiteMinimax blackMinimax (MovePath 0 0 [initialBoard]) True [] []

gameTick :: (Board -> MovePath Board) -> (Board -> MovePath Board) -> MovePath Board -> Bool -> [Int] -> [Int] -> IO ()
gameTick whiteMinimax blackMinimax (MovePath pathScore nodesExplored (board:bx)) whiteTurn timesArray nodesExploredArray = do
--   print nodesExplored
--   print pathScore
   putStrLn (printBoard board)
   if null (listAllMoves board) then putStrLn (printGameEnd board)
   else let minimax = if whiteTurn then whiteMinimax else blackMinimax in gameTick whiteMinimax blackMinimax (minimax board) (not whiteTurn) timesArray nodesExploredArray


