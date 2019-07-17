module Minimax
    ( MovePath (MovePath),
      minimaxAlphaBeta,
      minimaxAlphaBetaWithInfo,
    ) where

data MovePath a = EmptyPath | MovePath Int Int [a] deriving(Show)

pathScore (MovePath pathScoreV nodeExploredV nodesV) = pathScoreV
nodesExplored (MovePath pathScoreV nodeExploredV nodesV) = nodeExploredV
nodes (MovePath pathScoreV nodeExploredV nodesV) = nodesV

data TreeInfo a = TreeInfo { scoreF::(a -> Int),
                             scoreEmptyF :: (a -> Int),
                             nextNodesF::(a -> [a]),
                             isValid::(a -> Bool),
                             level::Int,
                             isMax::Bool,
                             alpha::Int,
                             beta::Int
}


minimaxAlphaBeta :: (a -> Int) -> (a -> Int) -> (a -> [a]) -> (a -> Bool) -> Int -> Int -> Int -> Bool -> a ->  a
minimaxAlphaBeta scoreF scoreEmptyF nextNodesF isValid minScore maxScore level isMax node =
     let resultNodes = (nodes (minimaxAlphaBetaWithInfo scoreF scoreEmptyF nextNodesF isValid minScore maxScore level isMax node))
     in if (length resultNodes) == 0 then node else resultNodes !! 0

minimaxAlphaBetaWithInfo :: (a -> Int) -> (a -> Int) -> (a -> [a]) -> (a -> Bool) -> Int -> Int -> Int -> Bool -> a -> MovePath a
minimaxAlphaBetaWithInfo scoreF scoreEmptyF nextNodesF isValid minScore maxScore level isMax node
    = minimax node (TreeInfo scoreF scoreEmptyF nextNodesF isValid level isMax minScore maxScore)

minimax :: a -> TreeInfo a -> MovePath a
minimax node treeInfo = case minimaxAux node ((nextNodesF treeInfo) node) treeInfo of
  EmptyPath -> MovePath (leafScore (scoreEmptyF treeInfo) (level treeInfo) (isMax treeInfo) node) 0 []
  (MovePath score nExplored nodes) -> (MovePath score nExplored nodes)

minimaxAux :: a -> [a] -> TreeInfo a -> MovePath a
minimaxAux node _ (TreeInfo scoreF _ _ _ 0 _ _ _) = MovePath (scoreF node) 0 []
minimaxAux node [] treeInfo = MovePath (leafScore (scoreEmptyF treeInfo) (level treeInfo) (isMax treeInfo) node) 0 []
minimaxAux node nextNodes treeInfo = foldr (minimaxHorizontal treeInfo) EmptyPath nextNodes

minimaxHorizontal :: TreeInfo a -> a -> MovePath a -> MovePath a
minimaxHorizontal treeInfo node EmptyPath = nextLevelMinimax node treeInfo
minimaxHorizontal treeInfo node movePath =
    let score = (pathScore movePath)
        pruneCondition = if (isMax treeInfo) then score > (beta treeInfo) else score < (alpha treeInfo)
        newTreeInfo = if (isMax treeInfo) then treeInfo { alpha = (max score (alpha treeInfo))} else treeInfo { beta = (min score (beta treeInfo))} in
    if pruneCondition then movePath
    else calculateBestMove (isMax treeInfo) movePath (nextLevelMinimax node newTreeInfo)

nextLevelMinimax :: a -> TreeInfo a -> MovePath a
nextLevelMinimax node treeInfo = if (isValid treeInfo) node then appendNode (minimax node (treeInfo {level = (level treeInfo) - 1, isMax = not (isMax treeInfo)})) node
                                 else EmptyPath

appendNode :: MovePath a -> a -> MovePath a
appendNode (MovePath score nExplored nodes) node = MovePath score (nExplored + 1) (node:nodes)

calculateBestMove :: Bool -> MovePath a -> MovePath a -> MovePath a
calculateBestMove isMax (MovePath score1 nExplored1 ndx1) (MovePath score2 nExplored2 ndx2) = let comp = if isMax then (score1 >= score2) else (score1 <= score2) in -- >= and <= because in case of tie we don't want to change
    if comp then (MovePath score1 (nExplored1 + nExplored2) ndx1) else (MovePath score2 (nExplored1 + nExplored2) ndx2)
calculateBestMove _ (MovePath score1 nExplored1 ndx1) _ = (MovePath score1 nExplored1 ndx1)
calculateBestMove _ _ movePath = movePath

leafScore :: (a -> Int) -> Int -> Bool -> a -> Int
leafScore scoreEmptyF level isMax node =  let levelBonus = if isMax then (-level) else level in levelBonus + (scoreEmptyF node)