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
listMovesAux (Piece pieceColor (Rook hasMoved)) index board = listMoveUsingDirection True index [top, bottom, left, right] (moveType pieceColor board)
listMovesAux (Piece pieceColor Knight) index board = listMoveUsingDirection False index [knightBottomLeft, knightBottomRight, knightLeftBottom, knightLeftTop, knightRightBottom, knightRightTop, knightTopLeft, knightTopRight] (moveType pieceColor board)
listMovesAux (Piece pieceColor Bishop) index board = listMoveUsingDirection True index [topRight, leftTop, bottomLeft, rightBottom] (moveType pieceColor board)
listMovesAux (Piece pieceColor Queen) index board = listMoveUsingDirection True index [top, bottom, left, right, topRight, leftTop, bottomLeft, rightBottom] (moveType pieceColor board)
listMovesAux (Piece pieceColor (King hasMoved)) index board = listMoveUsingDirection False index [top, bottom, left, right, topRight, leftTop, bottomLeft, rightBottom] (moveType pieceColor board)
listMovesAux (Piece pieceColor (Pawn hasMoved)) index board = listPawnMovement index pieceColor hasMoved board

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


