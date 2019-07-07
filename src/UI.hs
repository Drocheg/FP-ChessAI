module UI (printBoard, printPossibleMoves) where
import Boards 
import Data.Char

printBoard b = printRow 0 b ++ " \t A\t B\t C\t D\t E\t F\t G\t H\n"

printRow::Int -> Board -> String
printRow 12 board = printHeader board
printRow idx (Board a b c x) = printRow (idx + 1) (Board a b c (drop 10 x)) ++ show (idx + 1) ++ "\t" ++  printSquares (take 10 x) ++ "\n"

printHeader (Board turn castleWhite castleBlack _) = printTurn turn

printTurn::Color -> String
printTurn White = "White turn \n"
printTurn Black = "Black turn \n"

printSquares::[Piece] -> String
printSquares [] = ""
printSquares ((Piece c pt):xs) = printColorSquare c (printSquare pt) ++ "\t" ++ printSquares xs
printSquares (None:xs) = " x " ++ "\t" ++ printSquares xs
printSquares (Sentinel:xs) = "" ++ printSquares xs

printSquare::PieceType -> String
printSquare (Rook _)    = "R"
printSquare Knight      = "N"
printSquare Bishop      = "B"
printSquare Queen       = "Q"
printSquare (King _)    = "K"
printSquare (Pawn _)    = "P"

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
printNotation (Piece _ (Rook _)) = "R"
printNotation (Piece _ (King _)) = "K"
printNotation (Piece _ (Pawn _)) = ""
printNotation (Piece _ Knight) = "N"
printNotation (Piece _ Queen)  = "Q"
printNotation (Piece _ Bishop) = "B"
printNotation (None)         = ""

printPosNotation::PiecePosition -> String
printPosNotation (PiecePosition x y) = printRank y ++ printFile x

printFile::Int -> String
printFile x = show (x + 1)

printRank::Int -> String
printRank y = [chr (ord 'a' + y)]
