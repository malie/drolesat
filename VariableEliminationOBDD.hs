-- Implementation of the variable elimination algorithm
-- from the paper
-- Toward Good Elimination Orders for Symbolic SAT Solving
-- by Jinbo Huang and Adnan Darwiche

{-# LANGUAGE BangPatterns #-}
module VariableEliminationOBDD ( variableEliminationOBDD ) where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Ord ( comparing )
import Debug.Trace ( trace )

import Dimacs ( Dimacs , VarId , variableCounts
              , leastFrequentVariables )
import OBDD ( OBDD
            , mostOccuringPlusNeighboursHeuristic
            , mkObddWithOrderX
            , variableOrderComparer
            , conjoin
            , quantifyVariable
            , isTrue
            , isFalse
            , obddVars
            )
import PrettyClassExt ( printPretty )
import Text.PrettyPrint.HughesPJClass ( prettyShow )




type Buckets = M.Map VarId Bucket
type Bucket = [OBDD]

variableEliminationOBDD :: Dimacs -> (Bool, [(VarId, OBDD)])
variableEliminationOBDD cnf =
  eliminate obuckets eliminationOrder []
  where eliminationOrder = leastFrequentVariables cnf
        elordIdxMap = M.fromList $ zip eliminationOrder [1..]
        elordIdx = (elordIdxMap M.!)
        varsBucket = L.minimumBy (comparing elordIdx)
        clauseBucket = varsBucket . map abs
        buckets = M.fromListWith (++)
                  [ (clauseBucket cl, [cl]) | cl <- cnf]
        variableOrder = mostOccuringPlusNeighboursHeuristic cnf
        cmp = variableOrderComparer variableOrder
        obddForSingleClause cl = mkObddWithOrderX variableOrder [cl]
        conjoinX = conjoin cmp variableOrder
        conjoinClauses :: Dimacs -> OBDD
        conjoinClauses = L.foldl1' conjoinX . map obddForSingleClause
        obuckets = M.map conjoinClauses buckets
        eliminate !buckets [] btrace = (True, btrace)
        eliminate !buckets (var:eliminationOrder) btrace =
          case M.lookup var buckets of
            Nothing -> eliminate buckets eliminationOrder btrace
            Just b1 ->
              let b2 = quantifyVariable cmp variableOrder var b1
                  btrace2 = (var, b1) : btrace
              in case (isTrue b2, isFalse b2, obddVars b2) of
                   (True, _, _) -> eliminate buckets eliminationOrder
                                   btrace2
                   (_, True, _) -> (False, btrace2)
                   (_, _, vars) ->
                     let bu = varsBucket $ S.toList vars
                         b3 = case M.lookup bu buckets of
                           Nothing -> b2
                           Just x -> conjoinX b2 x
                     in eliminate
                        (M.insert bu b3 buckets)
                        eliminationOrder
                        btrace2
