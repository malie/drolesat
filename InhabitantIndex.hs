module InhabitantIndex()
       where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Ord(comparing)

import RandomCNF(threeCNF)
import OBDD(OBDD, mkObddWithOrder, nodeIdToName, nodes, NodeId)
import PrettyClassExt(printPretty)
import Testing(enumerateModels)
import Dimacs(VarId, Model)

lexsortModels :: [Model] -> [[(VarId, Int)]]
lexsortModels models =
  L.sort
  [ L.sortBy (comparing fst)
    [ (abs lit, if lit > 0 then 1 else 0)
    | lit <- S.toList model]
  | model <- models]

testBuildQueryIndices allModels queryModels =
  [ idx
  | (idx, model) <- zip [0::Int ..] allModels
  , S.member (S.fromList model) queryModelsSet]
  where
    queryModelsSet = S.fromList $ map S.fromList queryModels

-- this function assumes the trivial variable order [1..]
obddModelCount :: OBDD -> M.Map NodeId Int
obddModelCount obdd = foldl count M.empty nodesFromBottomUp
  where
    nodesFromBottomUp :: [(NodeId, (VarId, NodeId, NodeId))]
    nodesFromBottomUp =
      reverse $
      L.sortBy (comparing snd) $
      M.toList (nodes obdd)
    (_, (largestVar, _, _)) = head nodesFromBottomUp
    count counts (nodeId, (varid, left, right)) =
      M.insert
      nodeId
      (countForChild counts varid left
       + countForChild counts varid right)
      counts
    countForChild _ _ 0 = 0
    countForChild _ varid 1 = 2 ^ (largestVar - varid)
    countForChild counts varid nodeId =
      (counts M.! nodeId) * (2 ^ (levelDiff - 1))
      where levelDiff = (nodeVar M.! nodeId) - varid
    nodeVar =
      M.fromList [ (n,v) | (n, (v, _, _)) <- M.toList $ nodes obdd]

prettyCounts = M.mapKeys nodeIdToName

test numVars numClauses numQueryClauses =
  do allCNF <- threeCNF numVars numClauses
     queryCNF <- threeCNF numVars numQueryClauses
     printPretty ("allCNF", allCNF)
     let allModels = lexsortModels $ enumerateModels allCNF
     printPretty ("all models", zip [0::Int ..] allModels)
     printPretty ("queryCNF", queryCNF)
     let queryModels =
           lexsortModels $ enumerateModels $ allCNF ++ queryCNF
     printPretty queryModels
     let testQueryIndices = testBuildQueryIndices allModels queryModels
     printPretty("query models indices", testQueryIndices)
     let varOrder = [1..numVars]
     let allDD = mkObddWithOrder varOrder allCNF
     let queryDD = mkObddWithOrder varOrder queryCNF
     let resultsDD = mkObddWithOrder varOrder $ allCNF ++ queryCNF
     printPretty ("all", allDD)
     printPretty ("query", queryDD)
     printPretty ("results", resultsDD)
     
     let counts = obddModelCount allDD
     printPretty $ ("all counts", prettyCounts counts)
     let resultsCounts = obddModelCount resultsDD
     printPretty $ ("results counts", prettyCounts resultsCounts)


main =
  test 5 6 5
  
  
