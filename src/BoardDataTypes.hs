module BoardDataTypes
    ( PieceColor(..),
      PieceType(..),
      Piece(..),
      PieceArray,
      Board(..),
      PiecePosition(..),
      MoveType(..),
      BoardWithMovement,
      getIndex,
      convert8x8to10x12,
      convertXYto10x12,
      getPieces,
      flipPieceColor,
      oppositePieceColor,
      getColor,
      getPiecePosition,
      getPiece,
      getPieceByIndex,
    ) where
import Data.Array

data PieceColor = Black | White  deriving (Show, Eq, Ord)
data PieceType = Pawn Bool
  | Rook Bool
  | King Bool
  | Knight
  | Queen
  | Bishop deriving (Show, Ord, Eq)

data PiecePosition = PiecePosition {
  x::Int,
  y::Int
} deriving (Show, Eq)

-- Sentinel --> Invalid board position, useful to simplify bounds checking
data Piece = Sentinel | None | Piece PieceColor PieceType deriving (Show, Eq, Ord)
type PieceArray = Array Int Piece 
data WinState = WhiteWins | BlackWins | Draw deriving (Show)
data Board = Board {
  _pieceColor::PieceColor,
  _score::Int,
  _pieces::PieceArray
} deriving (Show)

data MoveType = InvalidMove | TakeMove | SimpleMove

type BoardWithMovement = (Board, PiecePosition, PiecePosition)

getIndex :: PiecePosition -> Int
getIndex (PiecePosition x y) = convertXYto10x12 x y

getPiecePosition :: Int -> PiecePosition
getPiecePosition idx = PiecePosition ((idx `div` 10) - 2) ((idx `mod` 10) - 1)
--  0  0  -> 2 1 -> 21
-- -1 -1  -> 1 0 -> 10

convert8x8to10x12 :: Int -> Int
convert8x8to10x12 index = convertXYto10x12 (index `div` 8) (index `mod` 8)

convertXYto10x12 :: Int -> Int -> Int
convertXYto10x12 x y = (x + 2) * 10 + (y + 1)

flipPieceColor board = board {_pieceColor = oppositePieceColor (_pieceColor board)}

oppositePieceColor::PieceColor->PieceColor
oppositePieceColor White = Black
oppositePieceColor Black = White

getColor::Board -> PieceColor
getColor board = _pieceColor board

getPieces::Board -> PieceArray
getPieces board = _pieces board;

getPiece::Board -> PiecePosition -> Piece
getPiece board piecePosition = getPieceByIndex board (getIndex piecePosition)

getPieceByIndex::Board -> Int -> Piece
getPieceByIndex board idx = (_pieces board) ! idx