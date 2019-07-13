module MovesGeneration
    ( listMoves,
    ) where

import BoardDataTypes
import Data.Array
import Scoring

-- PIECE MOVEMENTS -- LIST AVAILABLE MOVES FOR A PIECE

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

listMoves::PiecePosition -> Board -> [PiecePosition]
listMoves index board = listMovesAux (getPiece board index) index board

listMovesAux::Piece -> PiecePosition -> Board -> [PiecePosition]
listMovesAux (Piece pieceColor (Rook hasMoved)) index board = listMovesUsingDirections True index [top, bottom, left, right] (moveType pieceColor board)
listMovesAux (Piece pieceColor Knight) index board = listMovesUsingDirections False index [knightBottomLeft, knightBottomRight, knightLeftBottom, knightLeftTop, knightRightBottom, knightRightTop, knightTopLeft, knightTopRight] (moveType pieceColor board)
listMovesAux (Piece pieceColor Bishop) index board = listMovesUsingDirections True index [topRight, leftTop, bottomLeft, rightBottom] (moveType pieceColor board)
listMovesAux (Piece pieceColor Queen) index board = listMovesUsingDirections True index [top, bottom, left, right, topRight, leftTop, bottomLeft, rightBottom] (moveType pieceColor board)
listMovesAux (Piece pieceColor (King hasMoved)) index board = listMovesUsingDirections False index [top, bottom, left, right, topRight, leftTop, bottomLeft, rightBottom] (moveType pieceColor board)
listMovesAux (Piece pieceColor (Pawn hasMoved)) index board = listPawnMovement index pieceColor hasMoved board

listMovesUsingDirections :: Bool -> PiecePosition -> [(PiecePosition -> PiecePosition)] -> (PiecePosition -> MoveType) -> [PiecePosition]
listMovesUsingDirections isRecursive index directions moveTypeF = foldr (\x xs -> listMovesInSingleDirection isRecursive index x moveTypeF ++ xs) [] directions

listMovesInSingleDirection::Bool -> PiecePosition -> (PiecePosition -> PiecePosition) -> (PiecePosition -> MoveType) -> [PiecePosition]
listMovesInSingleDirection isRecursive index moveFunc moveTypeF = let newIndex = moveFunc index; in case (moveTypeF newIndex) of
    InvalidMove -> []
    TakeMove -> [newIndex]
    SimpleMove -> if isRecursive then [newIndex] ++ (listMovesInSingleDirection isRecursive newIndex moveFunc moveTypeF) else [newIndex]

moveType::PieceColor -> Board -> PiecePosition -> MoveType
moveType mypieceColor board index = case (getPiece board index) of
  None -> SimpleMove
  Sentinel -> InvalidMove
  Piece otherpieceColor pieceTypes -> if (otherpieceColor == mypieceColor) then InvalidMove else TakeMove

-- PAWN MOVEMENTS
listPawnMovement :: PiecePosition -> PieceColor -> Bool -> Board -> [PiecePosition]
listPawnMovement index pieceColor hasMoved board = (pawnFrontMove index pieceColor hasMoved board) ++ (listPawnTakeMovement index pieceColor board)

listPawnTakeMovement :: PiecePosition -> PieceColor -> Board -> [PiecePosition]
listPawnTakeMovement index pieceColor board = let directions = if pieceColor == White then [leftTop, topRight] else [bottomLeft, rightBottom]
                                         in listMovesUsingDirections False index directions (moveTypeOnlyTakeAllowed pieceColor board)

pawnFrontMove :: PiecePosition -> PieceColor -> Bool -> Board -> [PiecePosition]
pawnFrontMove index pieceColor hasMoved board = let frontDirection = if pieceColor == White then top else bottom;
                                                    frontMove = listMovesInSingleDirection False index frontDirection (moveTypeTakeNonAllowed board)
                                                 in pawnFrontMoveAux frontDirection (moveTypeTakeNonAllowed board) hasMoved frontMove

pawnFrontMoveAux :: (PiecePosition -> PiecePosition) -> (PiecePosition -> MoveType) -> Bool -> [PiecePosition] -> [PiecePosition]
pawnFrontMoveAux moveFunc moveTypeF False (pp:[]) = pp:(listMovesInSingleDirection False pp moveFunc moveTypeF)
pawnFrontMoveAux _ _ _ positions = positions

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
