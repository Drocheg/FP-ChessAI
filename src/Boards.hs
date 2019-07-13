module Boards (getColor, allPieces, flipPieceColor, isKingBeingChecked, getPiece, getPieces, initialBoard, listAllMoves, chessMinimax, chessMinimaxSorted, chessMinimaxWithInfo, chessMinimaxSortedWithInfo, BoardWithMovement (..) ) where

import Data.List
import Data.Ord
import Data.Array
import Minimax
import Scoring
import MovesGeneration
import BoardDataTypes
import Data.Maybe

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


checkPieceColor ::PieceColor -> Piece -> Bool
checkPieceColor _ None = False
checkPieceColor _ Sentinel = False
checkPieceColor playerColor (Piece pieceColor _) = pieceColor == playerColor

--List all next boards with the movement made for a given piece
listBoardsWithMovement :: PiecePosition -> Board -> [BoardWithMovement]
listBoardsWithMovement piecePosition board = filter (not.isKingBeingChecked.(\(x,_,_) -> x))
  ((map (calculateBoardWithMovement piecePosition board) (listMoves piecePosition board)) ++ (listCastlingBoards board))

calculateBoardWithMovement :: PiecePosition -> Board -> PiecePosition -> BoardWithMovement
calculateBoardWithMovement oldPosition board newPosition =
    let pieces = _pieces board;
        movingPiece = pieces ! (getIndex oldPosition);
        movedPiece  = calculateMovedPiece newPosition movingPiece;
        takenPiece  = pieces ! (getIndex newPosition);
        newScore    = _score board - (scorePiece newPosition takenPiece) + (scorePiece newPosition movedPiece) - (scorePiece oldPosition movingPiece) in
    (Board {
      _pieceColor = oppositePieceColor $ _pieceColor board,
      _winState = _winState board,
      _whitePieces = _whitePieces board,
      _blackPieces = _blackPieces board,
      _score = newScore,
      _pieces = pieces // [((getIndex oldPosition), None), ((getIndex newPosition), movedPiece)]
    }, oldPosition, newPosition)

calculateMovedPiece :: PiecePosition -> Piece -> Piece
calculateMovedPiece (PiecePosition 0 _) (Piece color (Pawn _)) = (Piece color Queen)
calculateMovedPiece (PiecePosition 7 _) (Piece color (Pawn _)) = (Piece color Queen)
calculateMovedPiece _ (Piece color (Pawn False)) = (Piece color (Pawn True))
calculateMovedPiece _ (Piece color (Rook False)) = (Piece color (Rook True))
calculateMovedPiece _ (Piece color (King False)) = (Piece color (King True))
calculateMovedPiece _ piece = piece

-- CASTLING MOVEMENTS

listCastlingBoards :: Board -> [BoardWithMovement]
listCastlingBoards board = if (isKingBeingChecked (flipPieceColor board)) then []
                           else let castlingRow = if _pieceColor board == White then 0 else 7
                                in (castlingBoard castlingRow board 0 [1,2,3] 3 2) ++ (castlingBoard castlingRow board 7 [6,5] 5 6)

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
    then let boardAfterRookMovement = flipPieceColor ((\(x, _, _) -> x) (calculateBoardWithMovement rookStartPosition board rookEndPosition)) in
         [calculateBoardWithMovement kingStartPosition boardAfterRookMovement kingEndPosition]
    else []

validCastling :: [Piece] -> Piece -> Piece -> Bool
validCastling emptyPieces (Piece _ (Rook False)) (Piece _ (King False)) = all (\x -> x == None) emptyPieces
validCastling _ _ _ = False


listAllMoves::Board -> [BoardWithMovement]
listAllMoves board = foldr (\x a -> listBoardsWithMovement (getPiecePosition x) board ++ a) [] (filterMovablePiecesOnly board)

-- Return every piece that could theoretically move this turn
filterMovablePiecesOnly :: Board -> [Int]
filterMovablePiecesOnly board = filter (\x -> checkPieceColor (_pieceColor board) (getPieceByIndex board x)) allPositions

allPositions = map convert8x8to10x12 [0..63]
allPieces board = map (\i -> (getPiecePosition i, getPieceByIndex board i)) allPositions

-- Array with all the possibles next states given a board
listNextBoards :: Board -> [Board]
listNextBoards board = map (\(x, _, _) -> x) (listAllMoves board)

-- Array with all the possibles next states sorted by score given a board
listNextBoardsSorted :: Board -> [Board]
listNextBoardsSorted board = let sortF = if (_pieceColor board) == White then (\b1 b2 ->  compare (scoreBoard b2) (scoreBoard b1))
                                                                         else (\b1 b2 ->  compare (scoreBoard b1) (scoreBoard b2))
                                        in sortBy sortF (listNextBoards board)

-- Checks whether the king would be taken in the next turn
isKingBeingChecked :: Board -> Bool
isKingBeingChecked board =
    let movablePiecesPosition = map getPiecePosition (filterMovablePiecesOnly board);
        allMovesPosition = foldr (\x a -> listMoves x board ++ a) [] movablePiecesPosition in
        isJust $ find (wouldTakeKing board) allMovesPosition

-- Receives a position (presumably from a possible next move), checks if it is occupied by an opposite pieceColor King
wouldTakeKing :: Board -> PiecePosition -> Bool
wouldTakeKing board index = case ((_pieces board) ! (getIndex index)) of
  Piece color (King _) -> if (oppositePieceColor color == _pieceColor board) then True else False
  otherwise -> False

scoreBoardNoMovements :: Board -> Int
scoreBoardNoMovements board = if (isKingBeingChecked (flipPieceColor board)) then loseScore (_pieceColor board) else 0

loseScore :: PieceColor -> Int
loseScore White = (-50000)
loseScore Black = 50000


-- Call the minimax function with the chess variables
chessMinimax :: Int -> Board -> Board
chessMinimax deepness board = minimaxAlphaBeta scoreBoard scoreBoardNoMovements listNextBoards (-1000000) 1000000 deepness ((_pieceColor board) == White) board

chessMinimaxWithInfo :: Int -> Board -> MovePath Board
chessMinimaxWithInfo deepness board = minimaxAlphaBetaWithInfo scoreBoard scoreBoardNoMovements listNextBoards (-1000000) 1000000 deepness ((_pieceColor board) == White) board

chessMinimaxSorted :: Int -> Board -> Board
chessMinimaxSorted deepness board = minimaxAlphaBeta scoreBoard scoreBoardNoMovements listNextBoardsSorted (-1000000) 1000000 deepness ((_pieceColor board) == White) board

chessMinimaxSortedWithInfo :: Int -> Board -> MovePath Board
chessMinimaxSortedWithInfo deepness board = minimaxAlphaBetaWithInfo scoreBoard scoreBoardNoMovements listNextBoardsSorted (-1000000) 1000000 deepness ((_pieceColor board) == White) board
