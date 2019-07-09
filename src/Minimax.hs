module Minimax
    ( MovePath (MovePath),
      minimaxAlphaBeta,
      minimaxAlphaBetaWithInfo,
    ) where


data MovePath a = MovePath { pathScore::Int,
                             nodesExplored::Int,
                             nodes::[a]
} deriving(Show)

data AlphaBeta = AlphaBeta {
                            alpha::Int,
                            beta::Int
}

data TreeInfo a = TreeInfo { scoreF::(a -> Int),
                             scoreEmptyF :: (a -> Int),
                             nextNodesF::(a -> [a]),
                             level::Int,
                             isMax::Bool --  function to get score, Level, isMax
}

minimaxAlphaBeta :: (a -> Int) -> (a -> Int) -> (a -> [a]) -> Int -> Int -> Int -> Bool -> a ->  a
minimaxAlphaBeta scoreF scoreEmptyF nextNodesF minScore maxScore level isMax node =
    (nodes (minimaxAlphaBetaWithInfo scoreF scoreEmptyF nextNodesF minScore maxScore level isMax node)) !! 0

minimaxAlphaBetaWithInfo :: (a -> Int) -> (a -> Int) -> (a -> [a]) -> Int -> Int -> Int -> Bool -> a -> MovePath a
minimaxAlphaBetaWithInfo scoreF scoreEmptyF nextNodesF minScore maxScore level isMax node
    = minimax node (TreeInfo scoreF scoreEmptyF nextNodesF level isMax) 0 (AlphaBeta minScore maxScore)

minimax :: a -> TreeInfo a -> Int -> AlphaBeta -> MovePath a -- Tree, treeInfo, indexMove
minimax node treeInfo indexM ab = minimaxAux node ((nextNodesF treeInfo) node) treeInfo indexM ab

minimaxAux :: a -> [a] -> TreeInfo a -> Int -> AlphaBeta -> MovePath a
minimaxAux node _ (TreeInfo scoreF _ _ 0 _) indexM ab = MovePath (scoreF node) 0 []
minimaxAux node [] (TreeInfo _ scoreEmptyF _ _ _) indexM ab = MovePath (scoreEmptyF node) 0 []
minimaxAux node (r:rs) treeInfo indexM ab = minimaxHorizontal (nextLevelMinimax r treeInfo 0 ab) rs treeInfo 1 ab

minimaxHorizontal :: MovePath a -> [a] -> TreeInfo a -> Int -> AlphaBeta -> MovePath a
minimaxHorizontal movePath [] _ _ _ = movePath
minimaxHorizontal movePath (r:rs) (TreeInfo scoreF scoreEmptyF nextNodesF level True) indexM (AlphaBeta alpha beta) =
    let treeInfo = (TreeInfo scoreF scoreEmptyF nextNodesF level True) in
    let score = (pathScore movePath) in
    if score > beta then movePath
    else let newAlpha = max score alpha in
        minimaxHorizontalAux movePath (r:rs) treeInfo indexM (AlphaBeta newAlpha beta)
minimaxHorizontal movePath (r:rs) (TreeInfo scoreF scoreEmptyF nextNodesF level False) indexM (AlphaBeta alpha beta) =
    let treeInfo = (TreeInfo scoreF scoreEmptyF nextNodesF level False) in
    let score = (pathScore movePath) in
    if score < alpha then movePath
    else let newBeta = min score beta in
        minimaxHorizontalAux movePath (r:rs) treeInfo indexM (AlphaBeta alpha newBeta)

minimaxHorizontalAux :: MovePath a -> [a] -> TreeInfo a -> Int -> AlphaBeta -> MovePath a
minimaxHorizontalAux movePath (r:rs) treeInfo indexM ab =  minimaxHorizontal (bestMove (isMax treeInfo) movePath (nextLevelMinimax r treeInfo indexM ab)) rs treeInfo (indexM+1) ab

nextLevelMinimax :: a -> TreeInfo a -> Int -> AlphaBeta -> MovePath a
nextLevelMinimax node (TreeInfo scoreF scoreEmptyF nextNodesF level isMax) indexM ab = appendNode (minimax node (TreeInfo scoreF scoreEmptyF nextNodesF (level-1) (not isMax)) indexM ab) node

appendNode :: MovePath a -> a -> MovePath a
appendNode (MovePath score nExplored nodes) node = MovePath score (nExplored + 1) (node:nodes)

bestMove :: Bool -> MovePath a -> MovePath a -> MovePath a
bestMove isMax (MovePath score1 nExplored1 ndx1) (MovePath score2 nExplored2 ndx2) = let comp = if isMax then (score1 >= score2) else (score1 <= score2) in -- >= and <= because in case of tie we don't want to change
    if comp then (MovePath score1 (nExplored1 + nExplored2) ndx1) else (MovePath score2 (nExplored1 + nExplored2) ndx2)
