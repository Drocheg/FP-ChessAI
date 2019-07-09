module BoardDataTypes
    ( Color(..),
      PieceType(..),
      Piece(..),
      PieceArray,
      Board(..),
      PiecePosition(..),
      getIndex,
      convert8x8to10x12,
      convertXYto10x12,
      getHasMoved,
    ) where
import Data.Array

data Color = Black | White  deriving (Show, Eq)
data PieceType = Pawn
  | Rook
  | King
  | Knight
  | Queen
  | Bishop deriving (Show, Eq)

data PiecePosition = PiecePosition {
  x::Int,
  y::Int
} deriving (Show, Eq)

-- Sentinel --> Invalid board position, useful to simplify bounds checking
data Piece = Sentinel | None | Piece  Color PieceType Bool deriving (Show, Eq)
type PieceArray = Array Int Piece 
data WinState = WhiteWins | BlackWins | Draw deriving (Show)
data Board = Board {
  _color::Color,
  _winState::Maybe WinState,
  _whitePieces::[PiecePosition],
  _blackPieces::[PiecePosition],
  _score::Int,
  _pieces::PieceArray
} deriving (Show)

getHasMoved (Piece _ _ hasMoved) = hasMoved
getHasMoved None = False

getIndex :: PiecePosition -> Int
getIndex (PiecePosition x y) = convertXYto10x12 x y

convert8x8to10x12 :: Int -> Int
convert8x8to10x12 index = convertXYto10x12 (index `div` 8) (index `mod` 8)

convertXYto10x12 :: Int -> Int -> Int
convertXYto10x12 x y = (x + 2) * 10 + (y + 1)

