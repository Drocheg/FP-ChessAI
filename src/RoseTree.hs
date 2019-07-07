


module RoseTree
    ( freeMinimax,
      countNodes
    ) where

import Minimax

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
                Node 8[
                    Node 5 [
                        Node 30 [],
                        Node 8 [],
                        Node 17 []
                    ],
                    Node 88 [
                        Node (-5) [],
                        Node 10 [],
                        Node 11 []
                    ],
                    Node 12 [
                        Node 15 [],
                        Node 10 [],
                        Node 13 []
                    ]
                ],
                Node (-2)[]
            ]


getRoseTreeScore :: RoseTree b -> b
getRoseTreeScore (Node b rs) = b

getRoseNextNodes :: RoseTree b -> [RoseTree b]
getRoseNextNodes (Node b rs) = rs

freeMinimax :: RoseTree Int
freeMinimax = minimaxAlphaBeta freeTree getRoseTreeScore getRoseNextNodes 3 True (-1000000) 1000000



--- Other functions ---

--- Count nodes ---

freeCount :: Int
freeCount = countNodes freeTree

countNodes :: RoseTree a -> Int
countNodes (Node a []) = 1
countNodes (Node a (rs)) = 1 + countNodesAux(rs)

countNodesAux :: [RoseTree a] -> Int
countNodesAux [] = 0
countNodesAux (r:rs) = countNodes(r) + countNodesAux(rs)

-------------------





-- MinimaxAB
--
--minimax :: RoseTree a -> TreeInfo a -> Int -> AlphaBeta -> MovePath -- Tree, treeInfo, indexMove
--minimax (Node a []) (TreeInfo scoreF _ _) indexM ab = MovePath (scoreF a) 0 [(scoreF a)] []
--minimax (Node a _) (TreeInfo scoreF 0 _) indexM ab = MovePath (scoreF a) 0 [(scoreF a)] []
--minimax (Node a (r:rs)) treeInfo indexM ab = minimaxHorizontal (nextLevelMinimax r treeInfo 0 ab) rs treeInfo 1 ab
--
--minimaxHorizontal :: MovePath -> [RoseTree a] -> TreeInfo a -> Int -> AlphaBeta -> MovePath
--minimaxHorizontal movePath [] _ _ _ = movePath
--minimaxHorizontal movePath (r:rs) (TreeInfo scoreF level True) indexM (AlphaBeta alpha beta) =
--    let treeInfo = (TreeInfo scoreF level True) in
--    let score = (pathScore movePath) in
--    if score > beta then movePath
--    else let newAlpha = max score alpha in
--        minimaxHorizontalAux movePath (r:rs) treeInfo indexM (AlphaBeta newAlpha beta)
--minimaxHorizontal movePath (r:rs) (TreeInfo scoreF level False) indexM (AlphaBeta alpha beta) =
--    let treeInfo = (TreeInfo scoreF level False) in
--    let score = (pathScore movePath) in
--    if score < alpha then movePath
--    else let newBeta = min score beta in
--        minimaxHorizontalAux movePath (r:rs) treeInfo indexM (AlphaBeta alpha newBeta)
--
--minimaxHorizontalAux :: MovePath -> [RoseTree a] -> TreeInfo a -> Int -> AlphaBeta -> MovePath
--minimaxHorizontalAux movePath (r:rs) treeInfo indexM ab =  minimaxHorizontal (bestMove (isMax treeInfo) movePath (nextLevelMinimax r treeInfo indexM ab)) rs treeInfo (indexM+1) ab
--
--nextLevelMinimax :: RoseTree a -> TreeInfo a -> Int -> AlphaBeta -> MovePath
--nextLevelMinimax r (TreeInfo scoreF level isMax) indexM ab = appendIndex (minimax r (TreeInfo scoreF (level-1) (not isMax)) indexM ab) indexM
--
--appendIndex :: MovePath -> Int -> MovePath
--appendIndex (MovePath score nExplored scoreSeen indexes) indexM = MovePath score (nExplored + 1) scoreSeen (indexM:indexes)
--
--bestMove :: Bool -> MovePath -> MovePath -> MovePath
--bestMove isMax (MovePath score1 nExplored1 scoreSeen1 idx1) (MovePath score2 nExplored2 scoreSeen2 idx2) = let comp = if isMax then (score1 >= score2) else (score1 <= score2) in -- >= and <= because in case of tie we don't want to change
--    if comp then (MovePath score1 (nExplored1 + nExplored2) (scoreSeen1++scoreSeen2) idx1) else (MovePath score2 (nExplored1 + nExplored2) (scoreSeen1++scoreSeen2) idx2)
--
--freeMinimax :: MovePath
--freeMinimax = minimax freeTree (TreeInfo id 3 True) 0 (AlphaBeta (-1000000) 1000000)

--
--
----- Other functions ---
--
----- Count nodes ---
--
--freeCount :: Int
--freeCount = countNodes freeTree
--
--countNodes :: RoseTree a -> Int
--countNodes (Node a []) = 1
--countNodes (Node a (rs)) = 1 + countNodesAux(rs)
--
--countNodesAux :: [RoseTree a] -> Int
--countNodesAux [] = 0
--countNodesAux (r:rs) = countNodes(r) + countNodesAux(rs)
--
---------------------
--



















-- Minimax without AB
--
--minimax :: RoseTree a -> TreeInfo a -> Int -> MovePath -- Tree, treeInfo, indexMove
--minimax (Node a []) (TreeInfo scoreF _ _) indexM = MovePath (scoreF a) []
--minimax (Node a _) (TreeInfo scoreF 0 _) indexM = MovePath (scoreF a) []
--minimax (Node a (r:rs)) treeInfo indexM = minimaxHorizontal (nextLevelMinimax r treeInfo 0) rs treeInfo 1
--
--minimaxHorizontal :: MovePath -> [RoseTree a] -> TreeInfo a -> Int -> MovePath
--minimaxHorizontal movePath [] _ _ = movePath
--minimaxHorizontal movePath (r:rs) treeInfo indexM =
--    minimaxHorizontal (bestMove (isMax treeInfo) movePath (nextLevelMinimax r treeInfo indexM)) rs treeInfo (indexM+1)
--
--nextLevelMinimax :: RoseTree a -> TreeInfo a -> Int -> MovePath
--nextLevelMinimax r (TreeInfo scoreF level isMax) indexM  = appendIndex (minimax r (TreeInfo scoreF (level-1) (not isMax)) indexM) indexM
--
--appendIndex :: MovePath -> Int -> MovePath
--appendIndex (MovePath score indexes) indexM = MovePath score (indexM:indexes)
--
--bestMove :: Bool -> MovePath -> MovePath -> MovePath
--
--bestMove isMax (MovePath score1 idx1) (MovePath score2 idx2) = let comp = if isMax then (score1 >= score2) else (score1 < score2) in
--                                                              if comp then (MovePath score1 idx1) else (MovePath score2 idx2)
--
--freeMinimax :: MovePath
--freeMinimax = minimax freeTree (TreeInfo id 2 True) 0
--
--





