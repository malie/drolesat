module DPLL ( dpll, dpllTimed
            , randomHeuristic, mostOftenUsedVarHeuristic
            , momsHeuristic )
       where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad ( liftM )
import Data.Maybe ( catMaybes )
import Data.Ord ( comparing )
import Debug.Trace ( trace )

import IndexedCNF ( IndexedCNF , fromDimacs , toDimacs
                  , clausesWithVar , assign , allVariables
                  , Assignment
                  , Sign(Positive , Negative)
                  , unitPropagate , noClauses )
import Dimacs ( Dimacs, VarId, Literal, readDimacsFile)
import Random ( randomListElement )
import RandomCNF ( threeCNF )
import Timed ( timed )

type NextVarsHeuristic = IndexedCNF -> IO [VarId]

-- heuristics:
--   most often appearing var, mom heuristic
--   most often appearing var, more weight on small clauses
--   most often appearing var, both positive and negative
--   Satz heuristics
--   partition graph of short clauses, vars at cluster border

--   partition graph of clauses, vars at cluster border
--     heuristic should communicate, that the cnf is partitioned
--     into two separately solvable parts after conditioning
--     on the returned vars.
--   (this last one is special: after applying it one knows
--    that the cnf is partitioned and the parts can be solved
--    independently)


-- elements of heuristics dsl:
-- 'best' tries all given heuristics and selects the best one
--    the one which produces most small clauses

singletonList x = [x]

randomHeuristic :: NextVarsHeuristic
randomHeuristic ic =
  do v <- randomListElement $ allVariables ic
     return [v]

mostOftenUsedVarHeuristic :: NextVarsHeuristic
mostOftenUsedVarHeuristic ic =
  return $
  singletonList $
  fst $
  L.maximumBy (comparing snd) $
  M.toList $
  M.unionsWith (+)
  [ M.singleton (abs lit) 1
  | clause <- toDimacs ic
  , lit <- clause ]

tap x = trace (show x) x

momsHeuristic :: NextVarsHeuristic
momsHeuristic ic =
  return $
  singletonList $
  fst $
  L.maximumBy (comparing snd) $
  -- tap $
  M.toList $
  M.unionsWith (+)
  [ M.singleton (abs lit) $ 1.0 / (fromIntegral $ 5^clauseSize)
  | clause <- toDimacs ic
  , let clauseSize = length clause
  , lit <- clause ]

    

-- | A dpllStep will assign one or some variables according to
-- some heuristic, then unit propagate the resulting assignments,
-- possibly thereby assigning more variables and propagating more.
dpllStep :: NextVarsHeuristic -> IndexedCNF
            -> IO [(IndexedCNF, Assignment)]
dpllStep heuristic ic =
  do vars <- heuristic ic
     -- putStrLn $ "heuristic says: " ++ show vars
     return $ L.foldl' fa [(ic,[])] vars
  where fa aics var =
          [ (ic3, upas++lit:as)
          | (ic1, as) <- aics
          , sign <- [Positive, Negative]
          , let lit = if sign == Positive then var else negate var
          , Just ic2 <- [assign var sign ic1]
          , Just (ic3, upas) <- [unitPropagate ic2]]
          
dpll :: NextVarsHeuristic -> IndexedCNF
        -> IO [(IndexedCNF, Assignment)]
dpll heuristic ic = recur [(ic,[])]
  where recur aics = liftM concat $ mapM step aics
        step (ic, as)
          | noClauses ic = return [(ic, as)]
          | otherwise =
            do xs <- dpllStep heuristic ic
               recur
                 [ (xic, xas++as)
                 | (xic, xas) <- xs]

dpllTimed heuristic ic =
  timed "dpll" $ dpll heuristic ic
