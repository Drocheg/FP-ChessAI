
data Color = Black | White  deriving (Show, Eq)
data PieceTypes = Pawn Bool 
  | Rook Bool
  | King Bool
  | Knight
  | Queen
  | Bishop deriving (Show)

data Piece = None | Piece Color PieceTypes deriving (Show)
data Board = Board [Piece]  deriving (Show)
data MoveType = InvalidMove | TakeMove | SimpleMove
data PiecePosition = PiecePosition {
                                    x::Int,
                                    y::Int
} deriving (Show)

initialBoard::Board
initialBoard = Board [
        Piece White (Rook False), Piece White (Knight    ), Piece White (Bishop    ), Piece White (Queen     ), Piece White (King False), Piece White (Bishop    ), Piece White (Knight    ), Piece White (Rook False),
        Piece White (Pawn False), Piece White (Pawn False), Piece White (Pawn False), Piece White (Pawn False), Piece White (Pawn False), Piece White (Pawn False), Piece White (Pawn False), Piece White (Pawn False), 
        None                    , None                   , None                     , None                    , None                    , None                    , None                    , None                    ,
        None                    , None                   , None                     , None                    , None                    , None                    , None                    , None                    ,
        None                    , None                   , None                     , None                    , None                    , None                    , None                    , None                    ,
        None                    , None                   , None                     , None                    , None                    , None                    , None                    , None                    ,
        Piece Black (Pawn False), Piece Black (Pawn False), Piece Black (Pawn False), Piece Black (Pawn False), Piece Black (Pawn False), Piece Black (Pawn False), Piece Black (Pawn False), Piece Black (Pawn False), 
        Piece Black (Rook False), Piece Black (Knight    ), Piece Black (Bishop    ), Piece Black (Queen     ), Piece Black (King False), Piece Black (Bishop    ), Piece Black (Knight    ), Piece Black (Rook False)
        ]

testBoard::Board
testBoard = Board [
        Piece White (Rook False), Piece White (Knight    ), Piece White (Bishop    ), Piece White (Queen     ), Piece White (King False), Piece White (Bishop    ), Piece White (Knight    ), Piece White (Rook False),
        Piece White (Pawn False), Piece White (Pawn False), Piece White (Pawn False), Piece White (Pawn False), Piece White (Pawn False), Piece White (Pawn False), Piece White (Pawn False), Piece White (Pawn False), 
        None                    , Piece White (Pawn False), None                     , None                    , None                    , None                    , None                    , None                    ,
        None                   , Piece White (Pawn False),   None                     , None                    , None                    , None                    , None                    , None                    ,
        Piece White (Pawn False) , Piece Black (Pawn False), Piece Black (Pawn False) , None                    , None                    , None                    , None                    , None                    ,
        None                    , None                   , Piece Black (Pawn False)  , None                    , None                    , None                    , None                    , None                    ,
        Piece Black (Pawn False), Piece Black (Pawn False), Piece Black (Pawn False), Piece Black (Pawn False), Piece Black (Pawn False), Piece Black (Pawn False), Piece Black (Pawn False), Piece Black (Pawn False), 
        Piece Black (Rook False), Piece Black (Knight    ), Piece Black (Bishop    ), Piece Black (Queen     ), Piece Black (King False), Piece Black (Bishop    ), Piece Black (Knight    ), Piece Black (Rook False)
        ]
            


move:: Int -> Int -> PiecePosition -> Maybe PiecePosition
move movX movY (PiecePosition posX posY) = let newPiecePosition = (PiecePosition (posX + movX) (posY + movY)) in   
  if (x newPiecePosition) >= 8 || (x newPiecePosition) < 0 || (y newPiecePosition) >= 8 || (y newPiecePosition) < 0 then Nothing
  else Just newPiecePosition

top::PiecePosition -> Maybe PiecePosition
top = move 1 0

topRight::PiecePosition -> Maybe PiecePosition
topRight = move 1 1

right::PiecePosition -> Maybe PiecePosition
right = move 0 1

rightBottom::PiecePosition -> Maybe PiecePosition
rightBottom = move (-1) 1

bottom::PiecePosition -> Maybe PiecePosition
bottom = move (-1) 0

bottomLeft::PiecePosition -> Maybe PiecePosition
bottomLeft = move (-1) (-1)

left::PiecePosition -> Maybe PiecePosition
left = move 0 (-1)

leftTop::PiecePosition -> Maybe PiecePosition
leftTop = move 1 (-1)

knightTopRight::PiecePosition -> Maybe PiecePosition
knightTopRight = move 2 1

knightTopLeft::PiecePosition -> Maybe PiecePosition
knightTopLeft = move 2 (-1)

knightRightTop::PiecePosition -> Maybe PiecePosition
knightRightTop = move 1 2

knightRightBottom::PiecePosition -> Maybe PiecePosition
knightRightBottom = move (-1) 2

knightBottomRight::PiecePosition -> Maybe PiecePosition
knightBottomRight = move (-2) 1

knightBottomLeft::PiecePosition -> Maybe PiecePosition
knightBottomLeft = move (-2) (-1)

knightLeftTop::PiecePosition -> Maybe PiecePosition
knightLeftTop = move 1 (-2)

knightLeftBottom::PiecePosition -> Maybe PiecePosition
knightLeftBottom = move (-1) (-2)

listMovePrev:: PiecePosition -> Board -> [PiecePosition]
listMovePrev index board = listMoves index (getPiece board index) board 

listMoves::PiecePosition -> Piece -> Board -> [PiecePosition]
listMoves index (Piece color (Rook hasMoved)) board = listMoveUsingDirection True index [top, bottom, left, right] (moveType color board)
listMoves index (Piece color Bishop) board = listMoveUsingDirection True index [topRight, leftTop, bottomLeft, rightBottom] (moveType color board)
listMoves index (Piece color Queen) board = listMoveUsingDirection True index [top, bottom, left, right, topRight, leftTop, bottomLeft, rightBottom] (moveType color board)
listMoves index (Piece color (King hasMoved)) board = listMoveUsingDirection False index [top, bottom, left, right, topRight, leftTop, bottomLeft, rightBottom] (moveType color board)
listMoves index (Piece color Knight) board = listMoveUsingDirection False index [knightBottomLeft, knightBottomRight, knightLeftBottom, knightLeftTop, knightRightBottom, knightRightTop, knightTopLeft, knightTopRight] (moveType color board)
listMoves index (Piece color (Pawn hasMoved)) board = listPawnMovement index color hasMoved board

listPawnMovement :: PiecePosition -> Color -> Bool -> Board -> [PiecePosition]
listPawnMovement index color hasMoved board = (pawnFrontMove index color hasMoved board) ++ (listPawnTakeMovement index color board)

listPawnTakeMovement :: PiecePosition -> Color -> Board -> [PiecePosition]
listPawnTakeMovement index color board = let takeMovements = if color == White then [leftTop, topRight] else [bottomLeft, rightBottom]
                                         in listMoveUsingDirection False index takeMovements (moveTypeOnlyTakeAllowed color board)

pawnFrontMove :: PiecePosition -> Color -> Bool -> Board -> [PiecePosition]
pawnFrontMove index color hasMoved board = let frontMovement = if color == White then top else bottom
                                           in let moveTypeF = moveTypeTakeNonAllowed board
                                           in pawnFrontMoveAux frontMovement moveTypeF hasMoved (listMoveNonRecursive index frontMovement moveTypeF)

pawnFrontMoveAux :: (PiecePosition -> Maybe PiecePosition) -> (PiecePosition -> MoveType) -> Bool -> [PiecePosition] -> [PiecePosition]
pawnFrontMoveAux _ _ True positions = positions
pawnFrontMoveAux _ _ _ [] = []
pawnFrontMoveAux moveFunc moveTypeF hasMoved (pp:[]) = pp:(listMoveNonRecursive pp moveFunc moveTypeF)

listMoveUsingDirection :: Bool -> PiecePosition -> [(PiecePosition -> Maybe PiecePosition)] -> (PiecePosition -> MoveType) -> [PiecePosition]
listMoveUsingDirection _ _ [] _ = []
listMoveUsingDirection True index (f:fx) moveTypeF = (listMoveRecursive index f moveTypeF) ++ (listMoveUsingDirection True index fx moveTypeF)
listMoveUsingDirection False index (f:fx) moveTypeF = (listMoveNonRecursive index f moveTypeF) ++ (listMoveUsingDirection False index fx moveTypeF)

listMoveNonRecursive::PiecePosition -> (PiecePosition -> Maybe PiecePosition) -> (PiecePosition -> MoveType) -> [PiecePosition]
listMoveNonRecursive index moveFunc moveTypeF = case (moveFunc index) of
  Nothing -> []
  Just newIndex -> case (moveTypeF newIndex) of
    InvalidMove -> []
    TakeMove -> [newIndex]
    SimpleMove -> [newIndex]

listMoveRecursive::PiecePosition -> (PiecePosition -> Maybe PiecePosition) -> (PiecePosition -> MoveType) -> [PiecePosition]
listMoveRecursive index moveFunc moveTypeF = case (moveFunc index) of
  Nothing -> []
  Just newIndex -> case (moveTypeF newIndex) of
    InvalidMove -> []
    TakeMove -> [newIndex]
    SimpleMove -> [newIndex] ++ (listMoveRecursive newIndex moveFunc moveTypeF)

moveType::Color -> Board -> PiecePosition -> MoveType
moveType myColor board index = case (getPiece board index) of
  None -> SimpleMove
  Piece otherColor pieceTypes -> if (otherColor == myColor) then InvalidMove else TakeMove

moveTypeTakeNonAllowed:: Board -> PiecePosition -> MoveType
moveTypeTakeNonAllowed board index = case (getPiece board index) of
  None -> SimpleMove
  Piece _ _ -> InvalidMove

moveTypeOnlyTakeAllowed::Color -> Board -> PiecePosition -> MoveType
moveTypeOnlyTakeAllowed myColor board index = case (getPiece board index) of
  None -> InvalidMove
  Piece otherColor pieceTypes -> if (otherColor == myColor) then InvalidMove else TakeMove

getPiece::Board -> PiecePosition -> Piece
getPiece (Board pieces) (PiecePosition posX posY) = pieces !! (posX * 8 + posY)