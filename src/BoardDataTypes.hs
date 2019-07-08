module BoardDataTypes
    ( Color(..),
      PieceType(..),
      Piece(..),
      PieceArray,
      Board(..),
    ) where
import Data.Array

data Color = Black | White  deriving (Show, Eq)
data PieceType = Pawn Bool
  | Rook Bool
  | King Bool
  | Knight
  | Queen
  | Bishop deriving (Show)

-- Sentinel --> Invalid board position, useful to simplify bounds checking
data Piece = Sentinel | None | Piece Color PieceType deriving (Show)
type PieceArray = Array Int Piece 
-- Board <CurrentTurn> <WhiteCastled> <BlackCastled>
data Board = Board Color Bool Bool Int PieceArray deriving (Show)