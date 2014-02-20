module Testing (main)
       where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad ( foldM )
import Data.Maybe ( catMaybes )

import Data.Ord ( comparing )

import Dimacs ( Dimacs , Clause , Literal , Model )
import RandomCNF ( nCNF , threeCNF , fourCNF )

import NQueens ( nqueensDimacs )

import OBDD ( OBDD , order
            , mkObdd
            , buildObddByConjoiningClauses
            , obddEnumerateModels
            , conjoin
            , disjoin
            , restrict
            , quantifyVariable
            , variableOrderComparer )

import VariableEliminationOBDD ( variableEliminationOBDD )

import Text.PrettyPrint.HughesPJClass ( prettyShow )
import PrettyClassExt ( printPretty )

import IndexedCNF ( fromDimacs )
import DPLL ( dpll
            , mostOftenUsedVarHeuristic
            , momsHeuristic )

main = undefined

-- Enumerate models directly from a cnf. No attempt is made
-- at finding a good clause order.
enumerateModels :: Dimacs -> [Model]
enumerateModels = L.foldl' recur [S.empty]
  where
    recur xs clause =
      catMaybes [ fuse x m | m <- clauseModels clause, x <- xs ]

clauseModels :: Clause -> [Model]
clauseModels clause =
  L.delete (S.fromList $ map negate clause)
     (L.foldl' ca [S.empty] clause)
  where ca xs c =
          let l = abs c
          in concat [ [S.insert l x, S.insert (negate l) x]
                    | x <- xs ]

fuse :: Model -> Model -> Maybe Model
fuse a b | S.size b > S.size a = fuse b a
fuse a b = foldM recur a (S.toList b)
  where recur a c
          | S.member (negate c) a = Nothing
          | otherwise             = Just $ S.insert c a


enumerateModelsDPLL :: Dimacs -> IO [Model]
enumerateModelsDPLL cnf =
  do let ic = fromDimacs cnf
     xs <- dpll momsHeuristic ic
     return $ map (S.fromList . snd) xs

printModel :: Model -> IO ()
printModel = print . L.sortBy (comparing abs) . S.toList

testObddGenerationByConjoining =
  do cnf <- threeCNF 20 80
     putStrLn "input cnf:"
     mapM_ print cnf
     {-let sols = enumerateModels cnf
     putStrLn "cnf solutions"
     mapM_ printModel sols-}
     dpllSols <- enumerateModelsDPLL cnf
     putStrLn "cnf solutions by dpll"
     mapM_ printModel dpllSols
     let obdd = buildObddByConjoiningClauses cnf
     let osols = obddEnumerateModels obdd
     putStrLn "obdd solutions"
     mapM_ printModel osols


testRestrict numVars numClauses =
  do cnf <- threeCNF numVars numClauses
     mapM_ print cnf
     let o = mkObdd cnf
     printPretty o
     printPretty $
       M.unions
       [ M.fromList $
         [ ("restrict var " ++ show var ++ " to 1:",
            restrict var True o)
         , ("restrict var " ++ show var ++ " to 0:",
            restrict var False o)]
       | var <- order o ]

testExistentialQuantification numVars numClauses =
  do cnf <- threeCNF numVars numClauses
     mapM_ print cnf
     let o = mkObdd cnf
     printPretty o
     let ord = order o
         cmp = variableOrderComparer ord
     printPretty $
       M.fromList
       [ ("quantify var " ++ show var,
          quantifyVariable cmp ord var o)
       | var <- order o ]
  

testVariableElimination numVars numClauses =
  do cnf <- threeCNF numVars numClauses
     mapM_ print cnf
     printPretty $ variableEliminationOBDD cnf  
