module VariableEliminationOBDD ( variableEliminationOBDD ) where

import qualified Data.List as L
import qualified Data.Map as M
import Data.Ord ( comparing )

import Dimacs ( Dimacs , VarId , variableCounts )
import OBDD ( OBDD
            , mostOccuringPlusNeighboursHeuristic
            , mkObddWithOrderX
            , variableOrderComparer
            , conjoin
            )
import PrettyClassExt ( printPretty )




type Buckets = M.Map VarId Bucket
type Bucket = [OBDD]

-- variableEliminationOBDD :: Dimacs -> Bool
variableEliminationOBDD cnf =
  ( eliminationOrder
  , buckets
  , obuckets )
  where eliminationOrder =
          -- reverse $
          map fst $ L.sortBy (comparing snd) $
          M.toList $ variableCounts cnf
        elordIdxMap = M.fromList $ zip eliminationOrder [1..]
        elordIdx = (elordIdxMap M.!)
        clauseBucket = L.minimumBy (comparing elordIdx) . map abs
        buckets = M.fromListWith (++)
                  [ (clauseBucket cl, [cl]) | cl <- cnf]
        variableOrder = mostOccuringPlusNeighboursHeuristic cnf
        obddForSingleClause cl = mkObddWithOrderX variableOrder [cl]
        cmp = variableOrderComparer variableOrder
        conjoinClauses :: Dimacs -> OBDD
        conjoinClauses =
          L.foldl1' (conjoin cmp variableOrder)
          . map obddForSingleClause
        obuckets = M.map conjoinClauses buckets

        

