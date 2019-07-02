


module RoseTree
    ( freeMinimax,
      MovePath (MovePath)
    ) where

data RoseTree a = Node a [RoseTree a] deriving(Show)

freeTree :: RoseTree Int
freeTree = Node 1
            [
                Node 4 [
                    Node 3 [],
                    Node (-2) [],
                    Node 7 []
                ],
                Node 17[
                    Node 5 [
                        Node 30 [],
                        Node 22 [],
                        Node 17 []
                    ],
                    Node 22 [
                        Node 5 [],
                        Node 10 [],
                        Node 12 []
                    ],
                    Node 10 [
                        Node 15 [],
                        Node 17 [],
                        Node 13 []
                    ]
                ],
                Node (-2)[]
            ]

-- Move: score, indice de las movidas.
data MovePath = MovePath { score::Int,
                   indexes::[Int]
} deriving(Show)
data TreeInfo a = TreeInfo { scoreF::(a -> Int),
                             level::Int,
                             isPlayer::Bool --  function to get score, Level, isPlayer
}

minimax :: RoseTree a -> TreeInfo a -> Int -> MovePath -- Tree, treeInfo, indexMove
minimax (Node a []) (TreeInfo scoreF _ _) indexM = MovePath (scoreF a) []
minimax (Node a _) (TreeInfo scoreF 0 _) indexM = MovePath (scoreF a) []
minimax (Node a (r:rs)) treeInfo indexM = minimaxHorizontal (nextLevelMinimax r treeInfo 0) rs treeInfo 1

minimaxHorizontal :: MovePath -> [RoseTree a] -> TreeInfo a -> Int -> MovePath
minimaxHorizontal move [] _ _ = move
minimaxHorizontal move (r:rs) treeInfo indexM =
    minimaxHorizontal (bestMove (isPlayer treeInfo) move (nextLevelMinimax r treeInfo indexM)) rs treeInfo (indexM+1)

nextLevelMinimax :: RoseTree a -> TreeInfo a -> Int -> MovePath
nextLevelMinimax r (TreeInfo scoreF level isPlayer) indexM  = appendIndex (minimax r (TreeInfo scoreF (level-1) (not isPlayer)) indexM) indexM

appendIndex :: MovePath -> Int -> MovePath
appendIndex (MovePath score indexes) indexM = MovePath score (indexM:indexes)

bestMove :: Bool -> MovePath -> MovePath -> MovePath

bestMove bool (MovePath score1 idx1) (MovePath score2 idx2) = let comp = if bool then (score1 >= score2) else (score1 < score2) in
                                                              if comp then (MovePath score1 idx1) else (MovePath score2 idx2)

freeMinimax :: MovePath
freeMinimax = minimax freeTree (TreeInfo id 2 True) 0
