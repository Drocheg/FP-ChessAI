module BinaryTree
    ( freeMinimax,
      Move (Move)
    ) where

data BinaryTree a = Leaf a | Node a (BinaryTree a) (BinaryTree a) deriving (Show)

freeTree :: BinaryTree Int
freeTree =
    Node 1
            (Node 2
                (Node 3
                    (Leaf 4)
                    (Leaf 5)
                )
                (Leaf 5)
            )
            (Node (-1)
                (Node 4
                    (Leaf 7)
                    (Leaf 2)
                )
                (Node 19
                    (Leaf (-2))
                    (Leaf 6)
                )
            )
-- Move: score, indice de la movida.
data Move = Move Int Int deriving(Show)
minimax :: BinaryTree a -> (a -> Int) -> Int -> Bool -> Move -- Tree, function to get score, deepness, isPlayer
minimax (Leaf a) scoreF levels isPlayer = Move (scoreF a) (-1)
minimax (Node a left right) scoreF 0 isPlayer = Move (scoreF a) (-1)
minimax (Node a left right) scoreF levels isPlayer =
    bestMove isPlayer (minimax left scoreF (levels-1) (not isPlayer)) (minimax right scoreF (levels-1) (not isPlayer))

bestMove :: Bool -> Move -> Move -> Move
bestMove True (Move scoreL moveIndexL) (Move scoreR moveIndexR) = if (scoreL >= scoreR) then (Move scoreL moveIndexL) else (Move scoreR moveIndexR)
bestMove False (Move scoreL moveIndexL) (Move scoreR moveIndexR) = if (scoreL < scoreR) then (Move scoreL moveIndexL) else (Move scoreR moveIndexR)

freeMinimax :: Move
freeMinimax = minimax freeTree id 2 True
