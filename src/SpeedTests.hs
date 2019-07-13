module SpeedTests (speedTest) where

import Boards
import UI
import BoardDataTypes
import Minimax
import Data.Time.Clock.POSIX

speedTest :: (Board -> MovePath Board) -> (Board -> MovePath Board) -> IO ()
speedTest whiteMinimax blackMinimax = gameTick whiteMinimax blackMinimax (MovePath 0 0 [initialBoard]) True [] []

gameTick :: (Board -> MovePath Board) -> (Board -> MovePath Board) -> MovePath Board -> Bool -> [Int] -> [Int] -> IO ()
gameTick whiteMinimax blackMinimax (MovePath pathScore nodesExplored (board:bx)) whiteTurn timeArray nodesExploredArray = do
   print nodesExplored
   print pathScore
   putStrLn (printBoard board)
   time <- currentTime
   print time
   if null (listAllMoves board) then endSimulation timeArray nodesExploredArray board
   else let minimax = if whiteTurn then whiteMinimax else blackMinimax in gameTick whiteMinimax blackMinimax (minimax board) (not whiteTurn) (time:timeArray) (nodesExplored:nodesExploredArray)

endSimulation :: [Int] -> [Int] -> Board -> IO ()
endSimulation timeArray nodesExploredArray board = do
   putStrLn (printGameEnd board)
   print timeArray
   print nodesExploredArray

currentTime :: IO Int
currentTime = round `fmap` (convertToMilliseconds `fmap` getPOSIXTime)

convertToMilliseconds seg = 1000 * seg
