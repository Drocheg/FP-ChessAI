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
import Data.Maybe
import qualified Data.Map as Map

window = InWindow "Chess" (512, 512) (10, 10)

handleInput event board = board
draw pictureData board = Map.lookup  (PieceKey Black Bishop) pictureData
main = do
  d <- loadPiecePictures;
  play window white 60 initialBoard (\b -> fromMaybe Blank (draw d b)) handleInput (const id)

picture
  = Translate (-170) (-20) -- shift the text to the middle of the window
  $ Scale 0.5 0.5		 -- display it half the original size
  $ Text "Hello World"	 -- text to display