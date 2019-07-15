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
                             isMax::Bool,
                             ab::AlphaBeta
}

minimaxAlphaBeta :: (a -> Int) -> (a -> Int) -> (a -> [a]) -> Int -> Int -> Int -> Bool -> a ->  a
minimaxAlphaBeta scoreF scoreEmptyF nextNodesF minScore maxScore level isMax node =
     let resultNodes = (nodes (minimaxAlphaBetaWithInfo scoreF scoreEmptyF nextNodesF minScore maxScore level isMax node))
     in if (length resultNodes) == 0 then node else resultNodes !! 0


minimaxAlphaBetaWithInfo :: (a -> Int) -> (a -> Int) -> (a -> [a]) -> Int -> Int -> Int -> Bool -> a -> MovePath a
minimaxAlphaBetaWithInfo scoreF scoreEmptyF nextNodesF minScore maxScore level isMax node
    = minimax node (TreeInfo scoreF scoreEmptyF nextNodesF level isMax (AlphaBeta minScore maxScore))

minimax :: a -> TreeInfo a -> MovePath a
minimax node (TreeInfo scoreF _ _ 0 _ _) = MovePath (scoreF node) 0 []
minimax node treeInfo = minimaxAux node ((nextNodesF treeInfo) node) treeInfo

minimaxAux :: a -> [a] -> TreeInfo a -> MovePath a
minimaxAux node [] (TreeInfo _ scoreEmptyF _ level isMax ab) = MovePath (leafScore scoreEmptyF level isMax node) 0 []
minimaxAux node (r:rs) treeInfo = minimaxHorizontal (nextLevelMinimax r treeInfo) rs treeInfo

minimaxHorizontal :: MovePath a -> [a] -> TreeInfo a -> MovePath a
minimaxHorizontal movePath [] _  = movePath
minimaxHorizontal movePath (r:rs) (TreeInfo scoreF scoreEmptyF nextNodesF level isMax (AlphaBeta alpha beta)) =
    let score = (pathScore movePath)
        pruneCondition = if isMax then score > beta else score < alpha
        newAlphaBeta = if isMax then (AlphaBeta (max score alpha) beta) else (AlphaBeta alpha (min score beta)) in
    if pruneCondition then movePath
    else let treeInfo = TreeInfo scoreF scoreEmptyF nextNodesF level isMax newAlphaBeta
             bestMove = calculateBestMove isMax movePath (nextLevelMinimax r treeInfo)
         in minimaxHorizontal bestMove rs treeInfo

nextLevelMinimax :: a -> TreeInfo a -> MovePath a
nextLevelMinimax node (TreeInfo scoreF scoreEmptyF nextNodesF level isMax ab) = appendNode (minimax node (TreeInfo scoreF scoreEmptyF nextNodesF (level-1) (not isMax) ab)) node

appendNode :: MovePath a -> a -> MovePath a
appendNode (MovePath score nExplored nodes) node = MovePath score (nExplored + 1) (node:nodes)

calculateBestMove :: Bool -> MovePath a -> MovePath a -> MovePath a
calculateBestMove isMax (MovePath score1 nExplored1 ndx1) (MovePath score2 nExplored2 ndx2) = let comp = if isMax then (score1 >= score2) else (score1 <= score2) in -- >= and <= because in case of tie we don't want to change
    if comp then (MovePath score1 (nExplored1 + nExplored2) ndx1) else (MovePath score2 (nExplored1 + nExplored2) ndx2)

leafScore :: (a -> Int) -> Int -> Bool -> a -> Int
leafScore scoreEmptyF level isMax node =  let levelBonus = if isMax then (-level) else level in levelBonus + (scoreEmptyF node)