module Minimax
    ( MovePath (MovePath),
      minimaxAlphaBeta,
    ) where


data MovePath = MovePath { pathScore::Int,
                           nodesExplored::Int,
                           scoreSeen::[Int],
                           indexes::[Int]
} deriving(Show)

data AlphaBeta = AlphaBeta {
                            alpha::Int,
                            beta::Int
}

data TreeInfo a = TreeInfo { scoreF::(a -> Int),
                             nextNodesF::(a -> [a]),
                             level::Int,
                             isMax::Bool --  function to get score, Level, isMax
}

minimaxAlphaBeta :: a -> (a -> Int) -> (a -> [a]) -> Int -> Bool -> Int -> Int -> MovePath
minimaxAlphaBeta node scoreF nextNodesF level isMax minScore maxScore
    = minimax node (TreeInfo scoreF nextNodesF level isMax) 0 (AlphaBeta minScore maxScore)

minimax :: a -> TreeInfo a -> Int -> AlphaBeta -> MovePath -- Tree, treeInfo, indexMove
minimax node treeInfo indexM ab = minimaxAux node ((nextNodesF treeInfo) node) treeInfo indexM ab

minimaxAux :: a -> [a] -> TreeInfo a -> Int -> AlphaBeta -> MovePath
minimaxAux node (r:rs) (TreeInfo scoreF _ 0 _) indexM ab = MovePath (scoreF node) 0 [(scoreF node)] []
minimaxAux node [] (TreeInfo scoreF _ _ _) indexM ab = MovePath (scoreF node) 0 [(scoreF node)] []
minimaxAux node (r:rs) treeInfo indexM ab = minimaxHorizontal (nextLevelMinimax r treeInfo 0 ab) rs treeInfo 1 ab

minimaxHorizontal :: MovePath -> [a] -> TreeInfo a -> Int -> AlphaBeta -> MovePath
minimaxHorizontal movePath [] _ _ _ = movePath
minimaxHorizontal movePath (r:rs) (TreeInfo scoreF nextNodesF level True) indexM (AlphaBeta alpha beta) =
    let treeInfo = (TreeInfo scoreF nextNodesF level True) in
    let score = (pathScore movePath) in
    if score > beta then movePath
    else let newAlpha = max score alpha in
        minimaxHorizontalAux movePath (r:rs) treeInfo indexM (AlphaBeta newAlpha beta)
minimaxHorizontal movePath (r:rs) (TreeInfo scoreF nextNodesF level False) indexM (AlphaBeta alpha beta) =
    let treeInfo = (TreeInfo scoreF nextNodesF level False) in
    let score = (pathScore movePath) in
    if score < alpha then movePath
    else let newBeta = min score beta in
        minimaxHorizontalAux movePath (r:rs) treeInfo indexM (AlphaBeta alpha newBeta)

minimaxHorizontalAux :: MovePath -> [a] -> TreeInfo a -> Int -> AlphaBeta -> MovePath
minimaxHorizontalAux movePath (r:rs) treeInfo indexM ab =  minimaxHorizontal (bestMove (isMax treeInfo) movePath (nextLevelMinimax r treeInfo indexM ab)) rs treeInfo (indexM+1) ab

nextLevelMinimax :: a -> TreeInfo a -> Int -> AlphaBeta -> MovePath
nextLevelMinimax r (TreeInfo scoreF nextNodesF level isMax) indexM ab = appendIndex (minimax r (TreeInfo scoreF nextNodesF (level-1) (not isMax)) indexM ab) indexM

appendIndex :: MovePath -> Int -> MovePath
appendIndex (MovePath score nExplored scoreSeen indexes) indexM = MovePath score (nExplored + 1) scoreSeen (indexM:indexes)

bestMove :: Bool -> MovePath -> MovePath -> MovePath
bestMove isMax (MovePath score1 nExplored1 scoreSeen1 idx1) (MovePath score2 nExplored2 scoreSeen2 idx2) = let comp = if isMax then (score1 >= score2) else (score1 <= score2) in -- >= and <= because in case of tie we don't want to change
    if comp then (MovePath score1 (nExplored1 + nExplored2) (scoreSeen1++scoreSeen2) idx1) else (MovePath score2 (nExplored1 + nExplored2) (scoreSeen1++scoreSeen2) idx2)
