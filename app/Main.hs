module Main where
import Boards
import UI
-- main :: IO ()
-- main = do 
--   gameTick initialBoard True
  
-- gameTick :: Board -> Bool -> IO ()
-- gameTick board playerTurn = do
--     putStrLn (printBoard board)
--     let boards = listAllMoves board
--     if (null boards) then putStrLn (printGameEnd board)
--     else if playerTurn then do putStrLn (printPossibleMoves 0 board boards)
--                                option <- getLine
--                                let (board, _, _) = boards!!(read option)
--                                gameTick board (not playerTurn)
--                        else do let aIBoard = chessMinimaxSorted 4 board
--                                gameTick aIBoard (not playerTurn)




import BoardDataTypes
-- import UI
-- main :: IO ()
-- main = do 
--   gameTick initialBoard
  
-- gameTick :: Board -> IO ()
-- gameTick board = do
--   putStrLn (printBoard board)
--   let boards = listAllMoves board
--   putStrLn (printPossibleMoves  0 board boards)
--   option <- getLine
--   let (board, _, _) = boards!!(read option)
--   putStrLn (printBoard board)
--   let aIboard = chessMinimaxSorted 4 board
--   gameTick aIboard

-- | Display "Hello World" in a window.
--
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Maybe
import Data.List
import qualified Data.Map as Map

data GameState = GameState {
  _board::Board,
  _selectedPosition::Maybe PiecePosition
}

window = InWindow "Chess" (512, 512) (10, 10)

handleInput (EventKey (Char 'r') Down _ _) gs = gs {
  _board = initialBoard
}

handleInput (EventKey (MouseButton LeftButton) Down _ offset) (GameState board (Just selectedPosition)) = 
  let nextPiecePosition = toPiecePosition offset; 
      move = find (\(_,startPosition,endPosition) -> endPosition == nextPiecePosition && startPosition == selectedPosition) $ listAllMoves board; in
  case (move) of 
    Nothing -> GameState board Nothing
    Just (newBoard, _, _) -> GameState newBoard Nothing

handleInput (EventKey (MouseButton LeftButton) Down _ offset) (GameState board Nothing) = 
  let nextPiecePosition = toPiecePosition offset; 
      move = find (\(_,startPosition,_) -> startPosition == nextPiecePosition) $ listAllMoves board; in
  case (move) of 
    Nothing -> GameState board Nothing
    Just (_, startPosition, _) -> GameState board (Just startPosition)

handleInput _ gs = gs

toPiecePosition (x, y) = PiecePosition ((256 + floor y) `div` 64) ((256 + floor x) `div` 64)

translatePiecePosition (PiecePosition x y) = let (offsetX, offsetY) = (64 * y - 256 + 32, 64* x - 256 + 32 ) in 
  Translate (fromIntegral offsetX) (fromIntegral offsetY)

drawPiece piecePictureMap board (piecePosition, piece) = 
  translatePiecePosition piecePosition
  $ fromMaybe Blank $ Map.lookup piece piecePictureMap

drawPieces piecePictureMap board = Pictures 
  $ map (drawPiece piecePictureMap board) (allPieces board)

renderMovementSquare = color (makeColor 0 0.2 0 0.2) $ polygon [(-32, -32), (-32, 32), (32, 32), (32, -32)]


drawMovement (Just selectedPosition) (_, startSquare, endSquare) = if (selectedPosition == startSquare) then 
  translatePiecePosition endSquare renderMovementSquare else Blank
drawMovement Nothing (_, startSquare, _) = translatePiecePosition startSquare renderMovementSquare

drawMovements (GameState board selectedPosition) = Pictures
  $ map (drawMovement selectedPosition) (listAllMoves board)

drawFrame db dp dm gs = Pictures [
  db,                            -- Board Picture
  dm gs,                         -- Movements Picture
  dp (_board gs)                 -- Pieces Picture
  ]

main = do
  piecePictureMap <- loadPiecePictures;
  boardImage <- loadBMP "./images/board.bmp"
  let initialGameState = GameState {
    _board = initialBoard,
    _selectedPosition = Nothing
  }
  play window white 60 initialGameState (drawFrame boardImage (drawPieces piecePictureMap) drawMovements) handleInput (const id)
