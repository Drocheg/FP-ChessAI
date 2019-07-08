module Scoring
    ( scoreBoard,
      scorePiece,
    ) where

import BoardDataTypes

scoreBoard :: Board -> Int
scoreBoard (Board _ _ _ score _) = score

--scoreFullBoard :: Board -> Int
--scoreFullBoard board = scoreFullBoardAux allPositions board
--
--scoreFullBoardAux :: [Int] -> Board -> Int
--scoreFullBoardAux [] board = 0
--scoreFullBoardAux (x:xs) board = scorePiece (getPieceByIndex board x) + scoreFullBoardAux xs board

scorePiece :: Piece -> Int
scorePiece None = 0
scorePiece (Piece White pieceType) = scorePieceType pieceType
scorePiece (Piece Black pieceType) = (-scorePieceType pieceType)

scorePieceType :: PieceType -> Int
scorePieceType (Rook _) = 500
scorePieceType Bishop = 300
scorePieceType Queen = 900
scorePieceType (King _) = 100000
scorePieceType Knight = 300 -- Maybe change to make knight better than bishop
scorePieceType (Pawn _) = 100