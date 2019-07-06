module UI (printBoard) where
import Boards 
printBoard b = printRow 0 b ++ " \t A\t B\t C\t D\t E\t F\t G\t H\n"

printRow::Int -> Board -> String
printRow 8 board = printHeader board
printRow idx (Board a b c x) = printRow (idx + 1) (Board a b c (drop 8 x)) ++ show (idx + 1) ++ "\t" ++  printSquares (take 8 x) ++ "\n"

printHeader (Board turn castleWhite castleBlack _) = printTurn turn

printTurn::Color -> String
printTurn White = "White turn \n"
printTurn Black = "Black turn \n"

printSquares::[Piece] -> String
printSquares [] = ""
printSquares ((Piece c pt):xs) = printColorSquare c (printSquare pt) ++ "\t" ++ printSquares xs
printSquares (None:xs) = " x " ++ "\t" ++ printSquares xs

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
