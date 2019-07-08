module BoardDataTypes
    ( Color(..),
      PieceType(..),
      Piece(..),
      PieceArray,
      Board(..),
      PiecePosition(..),
      getIndex,
      convert8x8to10x12,
    ) where
import Data.Array

data Color = Black | White  deriving (Show, Eq)
data PieceType = Pawn Bool
  | Rook Bool
  | King Bool
  | Knight
  | Queen
  | Bishop deriving (Show)

data PiecePosition = PiecePosition {
  x::Int,
  y::Int
} deriving (Show, Eq)

-- Sentinel --> Invalid board position, useful to simplify bounds checking
data Piece = Sentinel | None | Piece Color PieceType deriving (Show)
type PieceArray = Array Int Piece 

getIndex :: PiecePosition -> Int
getIndex (PiecePosition x y) = convertXYto10x12 x y

convert8x8to10x12 :: Int -> Int
convert8x8to10x12 index = convertXYto10x12 (index `div` 8) (index `mod` 8)

convertXYto10x12 :: Int -> Int -> Int
convertXYto10x12 x y = (x + 2) * 10 + (y + 1)

data Board = Board Color Bool Bool Int PieceArray deriving (Show)
