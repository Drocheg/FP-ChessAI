module BoardDataTypes
    ( PieceColor(..),
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
  _winState::Maybe WinState,
  _whitePieces::[PiecePosition],
  _blackPieces::[PiecePosition],
  _score::Int,
  _pieces::PieceArray
} deriving (Show)

getHasMoved (Piece _ (Rook hasMoved)) = hasMoved
getHasMoved (Piece _ (Pawn hasMoved)) = hasMoved
getHasMoved (Piece _ (King hasMoved)) = hasMoved
getHasMoved None = False

getIndex :: PiecePosition -> Int
getIndex (PiecePosition x y) = convertXYto10x12 x y

convert8x8to10x12 :: Int -> Int
convert8x8to10x12 index = convertXYto10x12 (index `div` 8) (index `mod` 8)

convertXYto10x12 :: Int -> Int -> Int
convertXYto10x12 x y = (x + 2) * 10 + (y + 1)
