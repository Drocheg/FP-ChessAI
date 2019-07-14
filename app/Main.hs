module Main where
import Boards
import UI
import SpeedTests

import BoardDataTypes
--main :: IO ()
--main = speedTest (chessMinimaxSortedWithInfo 4) (chessMinimaxWithInfo 4)

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Maybe
import Data.List
import Data.List.Unique
import qualified Data.Map as Map

data Controller = AI | Human deriving (Show)

data GameState = GameState {
  _whitePlayer::Controller,
  _blackPlayer::Controller,
  _board::Board,
  _selectedPosition::Maybe PiecePosition,
  _undoQ::[Board]
}

window = InWindow "Chess" (512, 512) (10, 10)

oppositeController AI = Human
oppositeController Human = AI


handleInputKey (EventKey (Char 'r') Down _ _) gs = gs { _board = initialBoard }
handleInputKey (EventKey (Char 'm') Down _ _) gs = case (_whitePlayer gs) of
  AI -> gs { _whitePlayer = Human, _blackPlayer = AI}
  Human -> case (_blackPlayer gs) of
    AI -> gs { _blackPlayer = Human}
    Human -> gs { _whitePlayer = AI}

handleInputKey (EventKey (Char 'u') Down _ _) (GameState w b board selPos (x: xs)) = GameState w b x selPos xs
handleInputKey (EventKey (Char 'u') Down _ _) (GameState w b board selPos []) = GameState w b board selPos []
handleInputKey _ gs = gs

handleInput (EventKey (MouseButton LeftButton) Down _ offset) (GameState w b board (Just selectedPosition) undo) =
  let nextPiecePosition = toPiecePosition offset;
      move = find (\(_,startPosition,endPosition) -> endPosition == nextPiecePosition && startPosition == selectedPosition) $ listAllMoves board; in
  case (move) of
    Nothing -> GameState w b board Nothing undo
    Just (newBoard, _, _) -> GameState w b newBoard Nothing (board:undo)

handleInput (EventKey (MouseButton LeftButton) Down _ offset) (GameState w b board Nothing undo) =
  let nextPiecePosition = toPiecePosition offset;
      move = find (\(_,startPosition,_) -> startPosition == nextPiecePosition) $ listAllMoves board; in
  case (move) of
    Nothing -> GameState w b board Nothing undo
    Just (_, startPosition, _) -> GameState w b board (Just startPosition) undo

handleInput e gs = handleInputKey e gs

toPiecePosition (x, y) = PiecePosition ((256 + floor y) `div` 64) ((256 + floor x) `div` 64)

translatePiecePosition (PiecePosition x y) = let (offsetX, offsetY) = (64 * y - 256 + 32, 64* x - 256 + 32 ) in
  Translate (fromIntegral offsetX) (fromIntegral offsetY)

drawPiece piecePictureMap board (piecePosition, piece) =
  translatePiecePosition piecePosition
  $ fromMaybe Blank $ Map.lookup piece piecePictureMap

drawPieces piecePictureMap board = Pictures
  $ map (drawPiece piecePictureMap board) (allPieces board)

renderMovementSquare = color (makeColor 0 0.2 0.2 0.4) $ polygon [(-32, -32), (-32, 32), (32, 32), (32, -32)]

getPlayingPlayer gs = case (getColor (_board gs)) of
  White -> _whitePlayer gs
  Black -> _blackPlayer gs

drawMovements gs = case (getPlayingPlayer gs) of
  Human -> Pictures
    $ drawMovementsAux (_selectedPosition gs) (_board gs)
  AI -> Blank

drawMovementsAux (Just selectedPosition) board = map (drawMovement selectedPosition) (listAllMoves board)
drawMovementsAux Nothing board = map drawMovablePieces ((\(uniqueList, _, _) -> uniqueList) (complex (map (\(_, startSquare, _) -> startSquare) (listAllMoves board))))

drawMovement selectedPosition (_, startSquare, endSquare) = if (selectedPosition == startSquare) then
  translatePiecePosition endSquare renderMovementSquare else Blank

drawMovablePieces startSquare = translatePiecePosition startSquare renderMovementSquare

textForWinner White = "White wins"
textForWinner Black = "Black wins"

drawVictory board = let movements = length $ listAllMoves board; in
  if (movements == 0) then
    let endGameText = if (isKingBeingChecked (flipPieceColor board))
        then textForWinner (oppositePieceColor(_pieceColor board))
        else "Draw"; in
    Translate (-100) 0
    $ Pictures [
      color (makeColor 0 0 0 0.5)
      $ polygon [(-16, -8), (-16, 32), (192, 32), (192, -8)],
      color (makeColor 1 1 1 1)
      $ Scale 0.25 0.25
      $ Text endGameText
    ]
  else Blank

drawMode gs = Translate (-255) (240)
  $ Pictures [
    color (makeColor 0 0 0 0.5)
    $ polygon [(-16, -16), (-16, 32), (120, 32), (120, -16)],
    Scale 0.12 0.12
    $ color (makeColor 1 1 1 1)
    $ Pictures [
      Text ("White: " ++ show ( _whitePlayer gs)),
      Translate 0 (-100) $ Text ("Black: " ++ show (_blackPlayer gs))
      ]
  ]

drawFrame db dp dm dw dmode gs = Pictures [
  db,                            -- Board Picture
  dm gs,                         -- Movements Picture
  dp (_board gs),                -- Pieces Picture
  dw (_board gs),                -- Draw win state
  dmode gs                          -- Draw mode
  ]

handleAIAux AI gs = gs {
  _board = chessMinimaxSorted 4 (_board gs)
}
handleAIAux Human gs = gs

handleAI _ gs = selectColor gs (handleAIAux (_whitePlayer gs)) (handleAIAux (_blackPlayer gs))

handleInputWrapperAux AI e gs = handleInputKey e gs
handleInputWrapperAux Human e gs = handleInput e gs

handleInputWrapper e gs = selectColor gs (handleInputWrapperAux (_whitePlayer gs) e) (handleInputWrapperAux (_blackPlayer gs) e)

selectColor gs whiteFunc blackFunc = case (getColor (_board gs)) of
  White -> whiteFunc gs
  Black -> blackFunc gs

main = do
  piecePictureMap <- loadPiecePictures;
  boardImage <- loadBMP "./images/board.bmp"
  let initialGameState = GameState {
    _whitePlayer = Human,
    _blackPlayer = AI,
    _board = initialBoard,
    _selectedPosition = Nothing,
    _undoQ = []
  }
  play window white 60 initialGameState (drawFrame boardImage (drawPieces piecePictureMap) drawMovements drawVictory drawMode) handleInput handleAI
