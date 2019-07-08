module BoardDataTypes
    ( Color(..),
      PieceType(..),
      Piece(..),
      Board(..),
    ) where


data Color = Black | White  deriving (Show, Eq)
data PieceType = Pawn Bool
  | Rook Bool
  | King Bool
  | Knight
  | Queen
  | Bishop deriving (Show)

-- Sentinel --> Invalid board position, useful to simplify bounds checking
data Piece = Sentinel | None | Piece Color PieceType deriving (Show)
-- Board <CurrentTurn> <WhiteCastled> <BlackCastled>
data Board = Board Color Bool Bool Int [Piece] deriving (Show)