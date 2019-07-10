module Boards (flippieceColor, isKingBeingChecked, getPiece, getPieces, initialBoard, listAllMoves, chessMinimax, chessMinimaxSorted, BoardWithMovement (..) ) where

import Data.List
import Data.Ord
import Data.Array
import Minimax
import Scoring
import BoardDataTypes
import Data.Maybe

data MoveType = InvalidMove | TakeMove | SimpleMove

type BoardWithMovement = (Board, PiecePosition, PiecePosition)

initialBoard::Board

initialBoard = Board {
  _pieceColor = White,
  _winState = Nothing,
  _whitePieces = [],
  _blackPieces = [],
  _score = 0,
  _pieces = (listArray (0, 119) [
        Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                ,
        Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                ,
        Sentinel                , Piece White (Rook False)  , Piece White Knight, Piece White Bishop, Piece White Queen , Piece White (King False)  , Piece White Bishop, Piece White Knight, Piece White (Rook False)  , Sentinel                ,
        Sentinel                , Piece White (Pawn False)  , Piece White (Pawn False)  , Piece White (Pawn False)  , Piece White (Pawn False)  , Piece White (Pawn False)  , Piece White (Pawn False)  , Piece White (Pawn False)  , Piece White (Pawn False)  , Sentinel                ,
        Sentinel                , None                    , None                    , None                    , None                    , None                    , None                    , None                    , None                    , Sentinel                ,
        Sentinel                , None                    , None                    , None                    , None                    , None                    , None                    , None                    , None                    , Sentinel                ,
        Sentinel                , None                    , None                    , None                    , None                    , None                    , None                    , None                    , None                    , Sentinel                ,
        Sentinel                , None                    , None                    , None                    , None                    , None                    , None                    , None                    , None                    , Sentinel                ,
        Sentinel                , Piece Black (Pawn False)  , Piece Black (Pawn False)  , Piece Black (Pawn False)  , Piece Black (Pawn False)  , Piece Black (Pawn False)  , Piece Black (Pawn False)  , Piece Black (Pawn False)  , Piece Black (Pawn False)  , Sentinel                ,
        Sentinel                , Piece Black (Rook False)  , Piece Black Knight, Piece Black Bishop, Piece Black Queen , Piece Black (King False)  , Piece Black Bishop, Piece Black Knight, Piece Black (Rook False)  , Sentinel                ,
        Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                ,
        Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                
        ])
}

-- testBoard2 = Board {
--   _pieceColor = White,
--   _winState = Nothing,
--   _whitePieces = [],
--   _blackPieces = [],
--   _score = 0,
--   _pieces = (listArray (0, 119) [
--         Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                ,
--         Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                ,
--         Sentinel                , Piece White Rook False  , Piece White Knight False, Piece White Bishop False, Piece White Queen False , Piece White King False  , Piece White Bishop False, Piece White Knight False, Piece White Rook False  , Sentinel                ,
--         Sentinel                , None                    , None                    , None                    , None                    , None                    , None                    , None                    , None                    , Sentinel                ,
--         Sentinel                , None                    , None                    , None                    , None                    , None                    , None                    , None                    , None                    , Sentinel                ,
--         Sentinel                , None                    , None                    , None                    , None                    , None                    , None                    , None                    , None                    , Sentinel                ,
--         Sentinel                , None                    , None                    , None                    , None                    , None                    , None                    , None                    , None                    , Sentinel                ,
--         Sentinel                , None                    , None                    , None                    , None                    , None                    , None                    , None                    , None                    , Sentinel                ,
--         Sentinel                , None                    , None                    , None                    , None                    , None                    , None                    , None                    , None                    , Sentinel                ,
--         Sentinel                , Piece Black Rook False  , Piece Black Knight False, Piece Black Bishop False, Piece Black Queen False , Piece Black King False  , Piece Black Bishop False, Piece Black Knight False, Piece Black Rook False  , Sentinel                ,
--         Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                ,
--         Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel
--         ])
-- }

-- testBoard3 = Board {
--   _pieceColor = White,
--   _winState = Nothing,
--   _whitePieces = [],
--   _blackPieces = [],
--   _score = 0,
--   _pieces = (listArray (0, 119) [
--         Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                ,
--         Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                ,
--         Sentinel                , None                    , None                    , None                    , Piece White Queen False , None                    , Piece White Queen False , Piece White Queen False , None                    , Sentinel                ,
--         Sentinel                , Piece Black (Pawn False)  , None                    , None                    , None                    , None                    , None                    , None                    , None                    , Sentinel                ,
--         Sentinel                , None                    , None                    , None                    , None                    , None                    , None                    , None                    , None                    , Sentinel                ,
--         Sentinel                , None                    , None                    , None                    , None                    , None                    , None                    , None                    , None                    , Sentinel                ,
--         Sentinel                , None                    , None                    , None                    , None                    , None                    , None                    , None                    , None                    , Sentinel                ,
--         Sentinel                , None                    , None                    , None                    , None                    , None                    , None                    , None                    , None                    , Sentinel                ,
--         Sentinel                , None                    , None                    , None                    , None                    , None                    , None                    , None                    , None                    , Sentinel                ,
--         Sentinel                , Piece Black Rook False  , None                    , None                    , None                    , Piece Black King False  , None                    , None                    , Piece Black Rook False  , Sentinel                ,
--         Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                ,
--         Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel                , Sentinel
--         ])
-- }
            
getPieces::Board -> PieceArray
getPieces board = _pieces board;

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
listBoards piecePosition board = filter (not.isKingBeingChecked.(\(x,_,_) -> x)) (listBoardsAux piecePosition board)

listBoardsAux :: PiecePosition -> Board -> [BoardWithMovement]
listBoardsAux piecePosition board = (map (listBoardsWithMovement piecePosition board) (listMoves piecePosition (getPiece board piecePosition) board)) ++ (listCastlingBoards board)

oppositepieceColor::PieceColor->PieceColor
oppositepieceColor White = Black
oppositepieceColor Black = White

listBoardsWithMovement :: PiecePosition -> Board -> PiecePosition -> BoardWithMovement
listBoardsWithMovement oldPosition board newPosition =
    let pieces = _pieces board;
        movingPiece = pieces ! (getIndex oldPosition);
        movedPiece  = calculateMovedPiece newPosition movingPiece;
        takenPiece  = pieces ! (getIndex newPosition);
        newScore    = _score board - (scorePiece newPosition takenPiece) + (scorePiece newPosition movedPiece) - (scorePiece oldPosition movingPiece) in
    (Board {
      _pieceColor = oppositepieceColor $ _pieceColor board,
      _winState = _winState board,
      _whitePieces = _whitePieces board,
      _blackPieces = _blackPieces board,
      _score = newScore,
      _pieces = listBoardsWithMovementAux oldPosition pieces newPosition movedPiece
    }, oldPosition, newPosition)

calculateMovedPiece :: PiecePosition -> Piece -> Piece
calculateMovedPiece (PiecePosition 0 _) (Piece color (Pawn _)) = (Piece color Queen)
calculateMovedPiece (PiecePosition 7 _) (Piece color (Pawn _)) = (Piece color Queen)
calculateMovedPiece _ piece = piece
calculateMovedPiece _ None = None

listBoardsWithMovementAux :: PiecePosition -> PieceArray -> PiecePosition -> Piece -> PieceArray
listBoardsWithMovementAux oldPosition boardArray newPosition piece = boardArray // [((getIndex oldPosition), None), ((getIndex newPosition), piece)]

getPiecePosition :: Int -> PiecePosition
getPiecePosition idx = PiecePosition ((idx `div` 10) - 2) ((idx `mod` 10) - 1)
--  0  0  -> 2 1 -> 21
-- -1 -1  -> 1 0 -> 10

-- PIECE MOVEMENTS -- LIST AVAILABLE MOVES FOR A PIECE
listMoves::PiecePosition -> Piece -> Board -> [PiecePosition]
listMoves index (Piece pieceColor (Rook hasMoved)) board = listMoveUsingDirection True index [top, bottom, left, right] (moveType pieceColor board)
listMoves index (Piece pieceColor Bishop) board = listMoveUsingDirection True index [topRight, leftTop, bottomLeft, rightBottom] (moveType pieceColor board)
listMoves index (Piece pieceColor Queen) board = listMoveUsingDirection True index [top, bottom, left, right, topRight, leftTop, bottomLeft, rightBottom] (moveType pieceColor board)
listMoves index (Piece pieceColor (King hasMoved)) board = listMoveUsingDirection False index [top, bottom, left, right, topRight, leftTop, bottomLeft, rightBottom] (moveType pieceColor board)
listMoves index (Piece pieceColor Knight) board = listMoveUsingDirection False index [knightBottomLeft, knightBottomRight, knightLeftBottom, knightLeftTop, knightRightBottom, knightRightTop, knightTopLeft, knightTopRight] (moveType pieceColor board)
listMoves index (Piece pieceColor (Pawn hasMoved)) board = listPawnMovement index pieceColor hasMoved board

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

moveType::PieceColor -> Board -> PiecePosition -> MoveType
moveType mypieceColor board index = case (getPiece board index) of
  None -> SimpleMove
  Sentinel -> InvalidMove
  Piece otherpieceColor pieceTypes -> if (otherpieceColor == mypieceColor) then InvalidMove else TakeMove

moveTypeTakeNonAllowed:: Board -> PiecePosition -> MoveType
moveTypeTakeNonAllowed board index = case (getPiece board index) of
  None -> SimpleMove
  Sentinel -> InvalidMove
  Piece _ _ -> InvalidMove

moveTypeOnlyTakeAllowed::PieceColor -> Board -> PiecePosition -> MoveType
moveTypeOnlyTakeAllowed mypieceColor board index = case (getPiece board index) of
  None -> InvalidMove
  Sentinel -> InvalidMove
  Piece otherpieceColor pieceTypes -> if (otherpieceColor == mypieceColor) then InvalidMove else TakeMove

-- PAWN MOVEMENTS 
listPawnMovement :: PiecePosition -> PieceColor -> Bool -> Board -> [PiecePosition]
listPawnMovement index pieceColor hasMoved board = (pawnFrontMove index pieceColor hasMoved board) ++ (listPawnTakeMovement index pieceColor board)

listPawnTakeMovement :: PiecePosition -> PieceColor -> Board -> [PiecePosition]
listPawnTakeMovement index pieceColor board = let takeMovements = if pieceColor == White then [leftTop, topRight] else [bottomLeft, rightBottom]
                                         in listMoveUsingDirection False index takeMovements (moveTypeOnlyTakeAllowed pieceColor board)

pawnFrontMove :: PiecePosition -> PieceColor -> Bool -> Board -> [PiecePosition]
pawnFrontMove index pieceColor hasMoved board = let frontMovement = if pieceColor == White then top else bottom
                                           in let moveTypeF = moveTypeTakeNonAllowed board
                                           in pawnFrontMoveAux frontMovement moveTypeF hasMoved (listMoveNonRecursive index frontMovement moveTypeF)

pawnFrontMoveAux :: (PiecePosition -> PiecePosition) -> (PiecePosition -> MoveType) -> Bool -> [PiecePosition] -> [PiecePosition]
pawnFrontMoveAux _ _ _ [] = []
pawnFrontMoveAux _ _ True positions = positions
pawnFrontMoveAux moveFunc moveTypeF hasMoved (pp:[]) = pp:(listMoveNonRecursive pp moveFunc moveTypeF)

getPiece::Board -> PiecePosition -> Piece
getPiece board piecePosition = getPieceByIndex board (getIndex piecePosition)

getPieceByIndex::Board -> Int -> Piece
getPieceByIndex board idx = (_pieces board) ! idx

listAllMoves::Board -> [BoardWithMovement]
listAllMoves board = iterateAllPositions board

listCastlingBoards :: Board -> [BoardWithMovement]
listCastlingBoards board = if (isKingBeingChecked (flippieceColor board)) then [] else
                           if getColor board == White then (listCastlingBoardsRow 0 board)
                                                      else (listCastlingBoardsRow 7 board)

listCastlingBoardsRow:: Int -> Board -> [BoardWithMovement]
listCastlingBoardsRow row board = (castlingBoard row board 0 [1,2,3] 3 2) ++ (castlingBoard row board 7 [6,5] 5 6)

castlingBoard :: Int -> Board -> Int -> [Int] -> Int -> Int -> [BoardWithMovement]
castlingBoard row board rookCol emptyCols rookEndCol kingEndCol =
    let rookStartPosition   = PiecePosition row rookCol
        rookEndPosition     = PiecePosition row rookEndCol
        kingStartPosition   = PiecePosition row 4
        kingEndPosition     = PiecePosition row kingEndCol
        rook                = getPiece board rookStartPosition;
        king                = getPiece board kingStartPosition;
        emptyPieces = map (\x -> getPieceByIndex board (convertXYto10x12 row x)) emptyCols in
    if validCastling emptyPieces rook king
    then let boardAfterRookMovement = flippieceColor ((\(x, _, _) -> x) (listBoardsWithMovement rookStartPosition board rookEndPosition)) in
         [listBoardsWithMovement kingStartPosition boardAfterRookMovement kingEndPosition]
    else []

validCastling :: [Piece] -> Piece -> Piece -> Bool
validCastling emptyPieces (Piece _ (Rook False)) (Piece _ (King False)) = all (\x -> x == None) emptyPieces
validCastling _ _ _ = False




allPositions = map convert8x8to10x12 [0..63]

-- Return every piece that could theoretically move this turn
filterMovablePiecesOnly :: Board -> [Int]
filterMovablePiecesOnly board = filter (\x -> checkPiece (_pieceColor board) (getPieceByIndex board x)) allPositions

iterateAllPositions:: Board -> [BoardWithMovement]
iterateAllPositions board = foldr (\x a -> listBoards (getPiecePosition x) board ++ a) [] (filterMovablePiecesOnly board)

checkPiece::PieceColor -> Piece -> Bool
checkPiece _ None = False
checkPiece _ Sentinel = False
checkPiece playerColor (Piece pieceColor _) = pieceColor == playerColor

getColor::Board -> PieceColor
getColor board = _pieceColor board

listAllBoards :: Board -> [Board]
listAllBoards board = map (\(x, _, _) -> x) (listAllMoves board)

listAllBoardsSorted :: Board -> [Board]
listAllBoardsSorted board = let sortF = if (getColor board) == White then (\b1 b2 ->  compare (scoreBoard b2) (scoreBoard b1))
                                                                     else (\b1 b2 ->  compare (scoreBoard b1) (scoreBoard b2))
                                        in sortBy sortF (listAllBoards board)

chessMinimax :: Int -> Board -> Board
chessMinimax deepness board = minimaxAlphaBeta scoreBoard scoreBoardNoMovements listAllBoards (-1000000) 1000000 deepness ((getColor board) == White) board

chessMinimaxWithInfo :: Int -> Board -> MovePath Board
chessMinimaxWithInfo deepness board = minimaxAlphaBetaWithInfo scoreBoard scoreBoardNoMovements listAllBoards (-1000000) 1000000 deepness ((getColor board) == White) board

chessMinimaxSorted :: Int -> Board -> Board
chessMinimaxSorted deepness board = minimaxAlphaBeta scoreBoard scoreBoardNoMovements listAllBoardsSorted (-1000000) 1000000 deepness ((getColor board) == White) board

chessMinimaxSortedWithInfo :: Int -> Board -> MovePath Board
chessMinimaxSortedWithInfo deepness board = minimaxAlphaBetaWithInfo scoreBoard scoreBoardNoMovements listAllBoardsSorted (-1000000) 1000000 deepness ((getColor board) == White) board

chessMinimaxDeep4 :: Board -> Board
chessMinimaxDeep4 board = chessMinimax 4 board

-- Checks whether the king would be taken in the next turn
isKingBeingChecked :: Board -> Bool
isKingBeingChecked board = 
    let movablePiecesPosition = map getPiecePosition (filterMovablePiecesOnly board);
        allMovesPosition = foldr (\x a -> listMoves x (getPiece board x) board ++ a) [] movablePiecesPosition in
        isJust $ find (wouldTakeKing board) allMovesPosition


-- Receives a position (presumably from a possible next move), checks if it is occupied by an opposite pieceColor King
wouldTakeKing :: Board -> PiecePosition -> Bool
wouldTakeKing board index = case ((_pieces board) ! (getIndex index)) of 
  Piece color (King _) -> if (oppositepieceColor color == _pieceColor board) then True else False
  otherwise -> False


scoreBoardNoMovements :: Board -> Int
scoreBoardNoMovements board = if (isKingBeingChecked (flippieceColor board)) then loseScore (getColor board) else 0

loseScore :: PieceColor -> Int
loseScore White = (-50000)
loseScore Black = 50000
flippieceColor board = board {_pieceColor = oppositepieceColor (_pieceColor board)}
