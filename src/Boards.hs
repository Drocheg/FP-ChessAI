module Boards (getPiece, getPieces, initialBoard, listAllMoves, chessMinimax, Board (Board), PieceType (..), Color (..), Piece (..), PiecePosition (..), BoardWithMovement (..) ) where

import Data.List
import Data.Ord
import Data.Array
import Minimax
import Scoring
import BoardDataTypes

data MoveType = InvalidMove | TakeMove | SimpleMove

type BoardWithMovement = (Board, PiecePosition, PiecePosition)

initialBoard::Board
-- initialBoard = Board White False False 0 [
--         Piece White (Rook False), Piece White (Knight    ), Piece White (Bishop    ), Piece White (Queen     ), Piece White (King False), Piece White (Bishop    ), Piece White (Knight    ), Piece White (Rook False),
--         Piece White (Pawn False), Piece White (Pawn False), Piece White (Pawn False), Piece White (Pawn False), Piece White (Pawn False), Piece White (Pawn False), Piece White (Pawn False), Piece White (Pawn False), 
--         None                    , None                   , None                     , None                    , None                    , None                    , None                    , None                    ,
--         None                    , None                   , None                     , None                    , None                    , None                    , None                    , None                    ,
--         None                    , None                   , None                     , None                    , None                    , None                    , None                    , None                    ,
--         None                    , None                   , None                     , None                    , None                    , None                    , None                    , None                    ,
--         Piece Black (Pawn False), Piece Black (Pawn False), Piece Black (Pawn False), Piece Black (Pawn False), Piece Black (Pawn False), Piece Black (Pawn False), Piece Black (Pawn False), Piece Black (Pawn False), 
--         Piece Black (Rook False), Piece Black (Knight    ), Piece Black (Bishop    ), Piece Black (Queen     ), Piece Black (King False), Piece Black (Bishop    ), Piece Black (Knight    ), Piece Black (Rook False)
--         ]

-- testBoard::Board
-- testBoard = Board White False False 100 [
--         Piece White (Rook False), Piece White (Knight    ), Piece White (Bishop    ), Piece White (Queen     ), Piece White (King False), Piece White (Bishop    ), Piece White (Knight    ), Piece White (Rook False),
--         Piece White (Pawn False), Piece White (Pawn False), Piece White (Pawn False), Piece White (Pawn False), Piece White (Pawn False), Piece White (Pawn False), Piece White (Pawn False), Piece White (Pawn False), 
--         None                    , Piece White (Pawn False), None                    , None                    , None                    , None                    , None                    , None                    ,
--         None                    , Piece White (Pawn True)    , None                 , None                    , None                    , None                    , None                    , None                    ,
--         None                    , None                    , None                    , None                    , None                    , None                    , None                    , None                    ,
--         None                    , None                    , Piece Black (Pawn False), None                    , None                    , None                    , None                    , None                    ,
--         Piece Black (Pawn False), Piece Black (Pawn False), Piece Black (Pawn False), Piece Black (Pawn False), Piece Black (Pawn False), Piece Black (Pawn False), Piece Black (Pawn False), Piece Black (Pawn False), 
--         Piece Black (Rook False), Piece Black (Knight    ), Piece Black (Bishop    ), Piece Black (Queen     ), Piece Black (King False), Piece Black (Bishop    ), Piece Black (Knight    ), Piece Black (Rook False)
--         ]

-- testBoard2::Board
-- testBoard2 = Board Black False False (-500) [
--         Piece White (King False), None                   , Piece Black (Rook False) , None                    , None                    , None                    , None                    , None                    ,
--         None                    , None                   , None                     , None                    , None                    , None                    , None                    , None                    ,
--         None                    , None                   , None                     , None                    , None                    , None                    , None                    , None                    ,
--         Piece Black (King False), None                   , None                     , None                    , None                    , None                    , None                    , None                    ,
--         None                    , None                   , None                     , None                    , None                    , None                    , None                    , None                    ,
--         None                    , None                   , None                     , None                    , None                    , None                    , None                    , None                    ,
--         None                    , None                   , None                     , None                    , None                    , None                    , None                    , None                    ,
--         None                    , None                   , None                     , None                    , None                    , None                    , None                    , None
--         ]
initialBoard = Board White False False 0 (listArray (0, 119) [
        Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                ,
        Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                ,
        Sentinel                , Piece White (Rook False), Piece White (Knight    ), Piece White (Bishop    ), Piece White (Queen     ), Piece White (King False), Piece White (Bishop    ), Piece White (Knight    ), Piece White (Rook False), Sentinel                ,
        Sentinel                , Piece White (Pawn False), Piece White (Pawn False), Piece White (Pawn False), Piece White (Pawn False), Piece White (Pawn False), Piece White (Pawn False), Piece White (Pawn False), Piece White (Pawn False), Sentinel                , 
        Sentinel                , None                    , None                    , None                    , None                    , None                    , None                    , None                    , None                    , Sentinel                ,
        Sentinel                , None                    , None                    , None                    , None                    , None                    , None                    , None                    , None                    , Sentinel                ,
        Sentinel                , None                    , None                    , None                    , None                    , None                    , None                    , None                    , None                    , Sentinel                ,
        Sentinel                , None                    , None                    , None                    , None                    , None                    , None                    , None                    , None                    , Sentinel                ,
        Sentinel                , Piece Black (Pawn False), Piece Black (Pawn False), Piece Black (Pawn False), Piece Black (Pawn False), Piece Black (Pawn False), Piece Black (Pawn False), Piece Black (Pawn False), Piece Black (Pawn False), Sentinel                , 
        Sentinel                , Piece Black (Rook False), Piece Black (Knight    ), Piece Black (Bishop    ), Piece Black (Queen     ), Piece Black (King False), Piece Black (Bishop    ), Piece Black (Knight    ), Piece Black (Rook False), Sentinel                ,
        Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                ,
        Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                
        ])

testBoard2 = Board White False False 0 (listArray (0, 119) [
        Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                ,
        Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                ,
        Sentinel                , Piece White (Rook False), Piece White (Knight    ), Piece White (Bishop    ), Piece White (Queen     ), Piece White (King False), Piece White (Bishop    ), Piece White (Knight    ), Piece White (Rook False), Sentinel                ,
        Sentinel                , None                    , None                    , None                    , None                    , None                    , None                    , None                    , None                    , Sentinel                ,
        Sentinel                , None                    , None                    , None                    , None                    , None                    , None                    , None                    , None                    , Sentinel                ,
        Sentinel                , None                    , None                    , None                    , None                    , None                    , None                    , None                    , None                    , Sentinel                ,
        Sentinel                , None                    , None                    , None                    , None                    , None                    , None                    , None                    , None                    , Sentinel                ,
        Sentinel                , None                    , None                    , None                    , None                    , None                    , None                    , None                    , None                    , Sentinel                ,
        Sentinel                , None                    , None                    , None                    , None                    , None                    , None                    , None                    , None                    , Sentinel                ,
        Sentinel                , Piece Black (Rook False), Piece Black (Knight    ), Piece Black (Bishop    ), Piece Black (Queen     ), Piece Black (King False), Piece Black (Bishop    ), Piece Black (Knight    ), Piece Black (Rook False), Sentinel                ,
        Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                ,
        Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel
        ])
            
getPieces::Board -> PieceArray
getPieces (Board _ _ _ _ pieces) = pieces;

move:: Int -> Int -> PiecePosition -> PiecePosition
move movX movY (PiecePosition posX posY) = PiecePosition (posX + movX) (posY + movY)

top = move 1 0
topRight = move 1 1
right = move 0 1
rightBottom = move (-1) 1
bottom = move (-1) 0
bottomLeft = move (-1) (-1)
left = move 0 (-1)
leftTop = move 1 (-1)
knightTopRight = move 2 1
knightTopLeft = move 2 (-1)
knightRightTop = move 1 2
knightRightBottom = move (-1) 2
knightBottomRight = move (-2) 1
knightBottomLeft = move (-2) (-1)
knightLeftTop = move 1 (-2)
knightLeftBottom = move (-1) (-2)

listBoards :: PiecePosition -> Board -> [BoardWithMovement]
listBoards piecePosition board = listBoardsAux piecePosition board (listMovePrev piecePosition board)

listBoardsAux :: PiecePosition -> Board -> [PiecePosition] -> [BoardWithMovement]
listBoardsAux piecePosition board [] = []
listBoardsAux piecePosition board (m:mx) = (listBoardsWithMovement piecePosition board m):(listBoardsAux piecePosition board mx)

oppositeColor::Color->Color
oppositeColor White = Black
oppositeColor Black = White

listBoardsWithMovement :: PiecePosition -> Board -> PiecePosition -> BoardWithMovement
listBoardsWithMovement oldPosition (Board color b1 b2 oldScore pieces) newPosition =
    let movingPiece = pieces ! (getIndex oldPosition) in
    let takenPiece = pieces ! (getIndex newPosition) in
    let newScore = oldScore - (scorePiece newPosition takenPiece) + (scorePiece newPosition movingPiece) - (scorePiece oldPosition movingPiece) in
    (Board (oppositeColor color) b1 b2 newScore (listBoardsWithMovementAux oldPosition pieces newPosition movingPiece), oldPosition, newPosition)

listBoardsWithMovementAux :: PiecePosition -> PieceArray -> PiecePosition -> Piece -> PieceArray
listBoardsWithMovementAux oldPosition boardArray newPosition piece = boardArray // [((getIndex oldPosition), None), ((getIndex newPosition), piece)]

getPiecePosition :: Int -> PiecePosition
getPiecePosition idx = PiecePosition ((idx `div` 10) - 2) ((idx `mod` 10) - 1)
--  0  0  -> 2 1 -> 21
-- -1 -1  -> 1 0 -> 10
listMovePrev:: PiecePosition -> Board -> [PiecePosition]
listMovePrev index board = listMoves index (getPiece board index) board 

listMoves::PiecePosition -> Piece -> Board -> [PiecePosition]
listMoves index (Piece color (Rook hasMoved)) board = listMoveUsingDirection True index [top, bottom, left, right] (moveType color board)
listMoves index (Piece color Bishop) board = listMoveUsingDirection True index [topRight, leftTop, bottomLeft, rightBottom] (moveType color board)
listMoves index (Piece color Queen) board = listMoveUsingDirection True index [top, bottom, left, right, topRight, leftTop, bottomLeft, rightBottom] (moveType color board)
listMoves index (Piece color (King hasMoved)) board = listMoveUsingDirection False index [top, bottom, left, right, topRight, leftTop, bottomLeft, rightBottom] (moveType color board)
listMoves index (Piece color Knight) board = listMoveUsingDirection False index [knightBottomLeft, knightBottomRight, knightLeftBottom, knightLeftTop, knightRightBottom, knightRightTop, knightTopLeft, knightTopRight] (moveType color board)
listMoves index (Piece color (Pawn hasMoved)) board = listPawnMovement index color hasMoved board

listPawnMovement :: PiecePosition -> Color -> Bool -> Board -> [PiecePosition]
listPawnMovement index color hasMoved board = (pawnFrontMove index color hasMoved board) ++ (listPawnTakeMovement index color board)

listPawnTakeMovement :: PiecePosition -> Color -> Board -> [PiecePosition]
listPawnTakeMovement index color board = let takeMovements = if color == White then [leftTop, topRight] else [bottomLeft, rightBottom]
                                         in listMoveUsingDirection False index takeMovements (moveTypeOnlyTakeAllowed color board)

pawnFrontMove :: PiecePosition -> Color -> Bool -> Board -> [PiecePosition]
pawnFrontMove index color hasMoved board = let frontMovement = if color == White then top else bottom
                                           in let moveTypeF = moveTypeTakeNonAllowed board
                                           in pawnFrontMoveAux frontMovement moveTypeF hasMoved (listMoveNonRecursive index frontMovement moveTypeF)

pawnFrontMoveAux :: (PiecePosition -> PiecePosition) -> (PiecePosition -> MoveType) -> Bool -> [PiecePosition] -> [PiecePosition]
pawnFrontMoveAux _ _ True positions = positions
pawnFrontMoveAux _ _ _ [] = []
pawnFrontMoveAux moveFunc moveTypeF hasMoved (pp:[]) = pp:(listMoveNonRecursive pp moveFunc moveTypeF)

listMoveUsingDirection :: Bool -> PiecePosition -> [(PiecePosition -> PiecePosition)] -> (PiecePosition -> MoveType) -> [PiecePosition]
listMoveUsingDirection _ _ [] _ = []
listMoveUsingDirection True index (f:fx) moveTypeF = (listMoveRecursive index f moveTypeF) ++ (listMoveUsingDirection True index fx moveTypeF)
listMoveUsingDirection False index (f:fx) moveTypeF = (listMoveNonRecursive index f moveTypeF) ++ (listMoveUsingDirection False index fx moveTypeF)

listMoveNonRecursive::PiecePosition -> (PiecePosition -> PiecePosition) -> (PiecePosition -> MoveType) -> [PiecePosition]
listMoveNonRecursive index moveFunc moveTypeF = let newIndex = moveFunc index; in case (moveTypeF (moveFunc index)) of
    InvalidMove -> []
    TakeMove -> [newIndex]
    SimpleMove -> [newIndex]

listMoveRecursive::PiecePosition -> (PiecePosition -> PiecePosition) -> (PiecePosition -> MoveType) -> [PiecePosition]
listMoveRecursive index moveFunc moveTypeF = let newIndex = moveFunc index; in case (moveTypeF newIndex) of
    InvalidMove -> []
    TakeMove -> [newIndex]
    SimpleMove -> [newIndex] ++ (listMoveRecursive newIndex moveFunc moveTypeF)

moveType::Color -> Board -> PiecePosition -> MoveType
moveType myColor board index = case (getPiece board index) of
  None -> SimpleMove
  Sentinel -> InvalidMove
  Piece otherColor pieceTypes -> if (otherColor == myColor) then InvalidMove else TakeMove

moveTypeTakeNonAllowed:: Board -> PiecePosition -> MoveType
moveTypeTakeNonAllowed board index = case (getPiece board index) of
  None -> SimpleMove
  Sentinel -> InvalidMove
  Piece _ _ -> InvalidMove

moveTypeOnlyTakeAllowed::Color -> Board -> PiecePosition -> MoveType
moveTypeOnlyTakeAllowed myColor board index = case (getPiece board index) of
  None -> InvalidMove
  Sentinel -> InvalidMove
  Piece otherColor pieceTypes -> if (otherColor == myColor) then InvalidMove else TakeMove


getPiece::Board -> PiecePosition -> Piece
getPiece board piecePosition = getPieceByIndex board (getIndex piecePosition)

getPieceByIndex::Board -> Int -> Piece
getPieceByIndex (Board _ _ _ _ pieces) idx = pieces ! idx

listAllMoves::Board -> [BoardWithMovement]
listAllMoves board = iterateAllPositions allPositions board

allPositions = map convert8x8to10x12 [0..63]
iterateAllPositions:: [Int] -> Board -> [BoardWithMovement]
iterateAllPositions [] board = []
iterateAllPositions (x:xs) board = (case (checkPiece (getColor board) (getPieceByIndex board x)) of
  True  ->  listBoards (getPiecePosition x) board
  False -> []
  ) ++ iterateAllPositions xs board

checkPiece::Color -> Piece -> Bool
checkPiece _ None = False
checkPiece _ Sentinel = False
checkPiece playerColor (Piece pieceColor _) = pieceColor == playerColor

getColor::Board -> Color
getColor (Board c _ _ _ _) = c

listAllBoards :: Board -> [Board]
listAllBoards board = map (\(x, _, _) -> x) (listAllMoves board)

listAllBoardsSorted :: Board -> [Board]
listAllBoardsSorted board = let sortF = if (getColor board) == White then (\b1 b2 ->  compare (scoreBoard b2) (scoreBoard b1))
                                                                     else (\b1 b2 ->  compare (scoreBoard b1) (scoreBoard b2))
                                        in sortBy sortF (listAllBoards board)


chessMinimax :: Int -> Board -> Board
chessMinimax deepness board = minimaxAlphaBeta scoreBoard listAllBoards (-1000000) 1000000 deepness ((getColor board) == White) board

chessMinimaxWithInfo :: Int -> Board -> MovePath Board
chessMinimaxWithInfo deepness board = minimaxAlphaBetaWithInfo scoreBoard listAllBoards (-1000000) 1000000 deepness ((getColor board) == White) board

chessMinimaxSorted :: Int -> Board -> Board
chessMinimaxSorted deepness board = minimaxAlphaBeta scoreBoard listAllBoardsSorted (-1000000) 1000000 deepness ((getColor board) == White) board

chessMinimaxSortedWithInfo :: Int -> Board -> MovePath Board
chessMinimaxSortedWithInfo deepness board = minimaxAlphaBetaWithInfo scoreBoard listAllBoardsSorted (-1000000) 1000000 deepness ((getColor board) == White) board

chessMinimaxDeep4 :: Board -> Board
chessMinimaxDeep4 board = chessMinimax 4 board