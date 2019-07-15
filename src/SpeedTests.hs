module SpeedTests (speedTest, testBoard, testBoard2, testBoard3, testBoard4) where

import Boards
import UI
import BoardDataTypes
import Minimax
import Data.Time.Clock.POSIX

import Data.List
import Data.Array

speedTest :: (Board -> MovePath Board) -> (Board -> MovePath Board) -> Board -> IO ()
speedTest whiteMinimax blackMinimax board = gameTick whiteMinimax blackMinimax (MovePath 0 0 [board]) True [] []

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



testBoard = Board {
  _pieceColor = White,
  _score = 0,
  _pieces = (listArray (0, 119) [
        Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                ,
        Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                ,
        Sentinel                , Piece White (Rook False)  , Piece White Knight, Piece White Bishop, Piece White Queen , Piece White (King False)  , Piece White Bishop, Piece White Knight, Piece White (Rook False)  , Sentinel                ,
        Sentinel                , Piece White (Pawn False)  ,None                   , Piece White (Pawn False)  , Piece White (Pawn False)  , Piece White (Pawn False)  , Piece White (Pawn False)  , Piece White (Pawn False)  , Piece White (Pawn False)  , Sentinel                ,
        Sentinel                , None                    ,  Piece White (Pawn True), None                    , None                    , None                    , None                    , None                    , None                    , Sentinel                ,
        Sentinel                , None                    , None                    , None                    , None                    , None                    , None                    , None                    , None                    , Sentinel                ,
        Sentinel                , None                    , None                    , None                    , None                    , None                    , None                    , None                    , None                    , Sentinel                ,
        Sentinel                , None                    , Piece Black (Pawn True) , None                    , None                    , None                    , None                    , None                    , None                    , Sentinel                ,
        Sentinel                , Piece Black (Pawn False)  , None                  , Piece Black (Pawn False)  , Piece Black (Pawn False)  , Piece Black (Pawn False)  , Piece Black (Pawn False)  , Piece Black (Pawn False)  , Piece Black (Pawn False)  , Sentinel                ,
        Sentinel                , Piece Black (Rook False)  , Piece Black Knight, Piece Black Bishop, Piece Black Queen , Piece Black (King False)  , Piece Black Bishop, Piece Black Knight, Piece Black (Rook False)  , Sentinel                ,
        Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                ,
        Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel
        ])
}


testBoard2 = Board {
  _pieceColor = White,
  _score = 0,
  _pieces = (listArray (0, 119) [
        Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                ,
        Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                ,
        Sentinel                , Piece White (Rook False)  , Piece White Knight, Piece White Bishop, Piece White Queen , Piece White (King False)  , Piece White Bishop, Piece White Knight, Piece White (Rook False)  , Sentinel                ,
        Sentinel                , None                      , Piece White (Pawn False)  , Piece White (Pawn False)  , Piece White (Pawn False)  , Piece White (Pawn False)  , Piece White (Pawn False)  , Piece White (Pawn False)  , Piece White (Pawn False)  , Sentinel                ,
        Sentinel                , None                    , None                    , None                    , None                    , None                    , None                    , None                    , None                    , Sentinel                ,
        Sentinel                , Piece White (Pawn True) , None                    , None                    , None                    , None                    , None                    , None                    , None                    , Sentinel                ,
        Sentinel                ,  Piece Black (Pawn True) , None                    , None                    , None                    , None                    , None                    , None                    , None                    , Sentinel                ,
        Sentinel                , None                    , None                    , None                    , None                    , None                    , None                    , None                    , None                    , Sentinel                ,
        Sentinel                ,None    , Piece Black (Pawn False)  , Piece Black (Pawn False)  , Piece Black (Pawn False)  , Piece Black (Pawn False)  , Piece Black (Pawn False)  , Piece Black (Pawn False)  , Piece Black (Pawn False)  , Sentinel                ,
        Sentinel                , Piece Black (Rook False)  , Piece Black Knight, Piece Black Bishop, Piece Black Queen , Piece Black (King False)  , Piece Black Bishop, Piece Black Knight, Piece Black (Rook False)  , Sentinel                ,
        Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                ,
        Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel
        ])
}

testBoard3 = Board {
  _pieceColor = White,
  _score = 0,
  _pieces = (listArray (0, 119) [
        Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                ,
        Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                ,
        Sentinel                , Piece White (Rook False)  , Piece White Knight, Piece White Bishop, Piece White Queen , Piece White (King False)  , Piece White Bishop, Piece White Knight, Piece White (Rook False)  , Sentinel                ,
        Sentinel                , Piece White (Pawn False)  , Piece White (Pawn False)  , Piece White (Pawn False)  , Piece White (Pawn False)  , Piece White (Pawn False)  , Piece White (Pawn False)  , Piece White (Pawn False)  , None  , Sentinel                ,
        Sentinel                , None                    , None                    , None                    , None                    , None                    , None                    , None                    , None                    , Sentinel                ,
        Sentinel                , None                    , None                    , None                    , None                    , None                    , None                    , None                    ,  Piece White (Pawn True) , Sentinel                ,
        Sentinel                ,  None                    , None                    , None                    , None                    , None                    , None                    , None                    , Piece Black (Pawn True) , Sentinel                ,
        Sentinel                , None                    , None                    , None                    , None                    , None                    , None                    , None                    , None                    , Sentinel                ,
        Sentinel                ,Piece Black (Pawn False)    , Piece Black (Pawn False)  , Piece Black (Pawn False)  , Piece Black (Pawn False)  , Piece Black (Pawn False)  , Piece Black (Pawn False)  , Piece Black (Pawn False)  , None  , Sentinel                ,
        Sentinel                , Piece Black (Rook False)  , Piece Black Knight, Piece Black Bishop, Piece Black Queen , Piece Black (King False)  , Piece Black Bishop, Piece Black Knight, Piece Black (Rook False)  , Sentinel                ,
        Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                ,
        Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel
        ])
}


testBoard4 = Board {
  _pieceColor = White,
  _score = 0,
  _pieces = (listArray (0, 119) [
        Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                ,
        Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                ,
        Sentinel                , Piece White (Rook False)  , Piece White Knight, Piece White Bishop, Piece White Queen , Piece White (King False)  , Piece White Bishop, Piece White Knight, Piece White (Rook False)  , Sentinel                ,
        Sentinel                , Piece White (Pawn False)  , Piece White (Pawn True), Piece White (Pawn False)  , None               , Piece White (Pawn False)  , Piece White (Pawn False)  , Piece White (Pawn False)  , Piece White (Pawn False)  , Sentinel                ,
        Sentinel                , None                    , None                    , None                    , Piece White (Pawn False)   , None                    , None                    , None                    , None                    , Sentinel                ,
        Sentinel                , None                    , None                    , None                    , None                    , None                    , None                    , None                    , None                    , Sentinel                ,
        Sentinel                , None                    , None                    , None                    , None                    , None                    , None                    , None                    , None                    , Sentinel                ,
        Sentinel                , None                    ,None                         , None                    , Piece Black (Pawn False), None                    , None                    , None                    , None                    , Sentinel                ,
        Sentinel                , Piece Black (Pawn False)  ,  Piece Black (Pawn True) , Piece Black (Pawn False)  , None  , Piece Black (Pawn False)  , Piece Black (Pawn False)  , Piece Black (Pawn False)  , Piece Black (Pawn False)  , Sentinel                ,
        Sentinel                , Piece Black (Rook False)  , Piece Black Knight, Piece Black Bishop, Piece Black Queen , Piece Black (King False)  , Piece Black Bishop, Piece Black Knight, Piece Black (Rook False)  , Sentinel                ,
        Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                ,
        Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel
        ])
}