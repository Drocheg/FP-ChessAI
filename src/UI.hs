module UI (printBoard, printPossibleMoves, printGameEnd) where
import BoardDataTypes
import Boards 
import Data.Char
import Data.Array

printBoard b = printRows b ++ " \t A\t B\t C\t D\t E\t F\t G\t H\n"  ++ printCheckStatus b ++ "\n"

printCheckStatus b = case (isKingBeingChecked (flipColor b)) of 
  True -> "King is Being Checked"
  False -> "King is not being Checked"

printRows:: Board -> String
printRows b = printRows' b 0 (elems (getPieces b))

printRows'::Board -> Int -> [Piece] -> String
printRows' b 12  pieces = printHeader b
printRows' b idx x      = printRows' b (idx + 1) (drop 10 x) ++ show (idx + 1 - 2) ++ "\t" ++  printSquares (take 10 x) ++ "\n"

printHeader::Board -> String
printHeader board = printTurn $ _color board

printTurn::Color -> String
printTurn White = "White turn \n"
printTurn Black = "Black turn \n"

printSquares::[Piece] -> String
printSquares [] = ""
printSquares ((Piece c pt _):xs) = printColorSquare c (printSquare pt) ++ "\t" ++ printSquares xs
printSquares (None:xs) = " x " ++ "\t" ++ printSquares xs
printSquares (Sentinel:xs) = "" ++ printSquares xs

printSquare::PieceType -> String
printSquare Rook        = "R"
printSquare Knight      = "N"
printSquare Bishop      = "B"
printSquare Queen       = "Q"
printSquare King        = "K"
printSquare Pawn        = "P"

printColorSquare::Color -> String -> String
printColorSquare White p = "[" ++ p ++ "]"
printColorSquare Black p = "(" ++ p ++ ")"

printPossibleMoves::Int -> Board -> [BoardWithMovement] -> String
printPossibleMoves i board []     = ""
printPossibleMoves i board (x:xs) = "\t [" ++ show i ++ "]" ++ printMoves board x ++ (if ((i + 1) `mod` 3 == 0) then "\n" else "\t") ++ printPossibleMoves (i + 1) board xs

printMoves::Board -> BoardWithMovement -> String
printMoves board (_, ogPos, newPos) = let ogPiece = getPiece board ogPos; newPiece = getPiece board newPos; in 
  printNotation ogPiece ++ printPosNotation ogPos ++ " " ++ printNotation newPiece ++ printPosNotation newPos

printNotation::Piece -> String
printNotation (Piece _ Rook _)   = "R"
printNotation (Piece _ King _)   = "K"
printNotation (Piece _ Pawn _)   = ""
printNotation (Piece _ Knight _) = "N"
printNotation (Piece _ Queen _)  = "Q"
printNotation (Piece _ Bishop _) = "B"
printNotation (None)         = ""

printPosNotation::PiecePosition -> String
printPosNotation (PiecePosition x y) = printRank y ++ printFile x

printFile::Int -> String
printFile x = show (x + 1)

printRank::Int -> String
printRank y = [chr (ord 'a' + y)]

printGameEnd:: Board -> String
printGameEnd board = if (isKingBeingChecked (flipColor board)) then printGameLose (_color board) else "It's a Tie"

printGameLose :: Color -> String
printGameLose White = "Black won"
printGameLose Black = "White won"

