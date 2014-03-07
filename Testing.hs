{-# LANGUAGE BangPatterns #-}
module Testing ( main , printCNFStatsShort , printCNFStats
               , histogram )
       where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V
import Control.Monad ( foldM )
import Data.Maybe ( catMaybes , fromMaybe )

import Data.Ord ( comparing )

import Dimacs ( Dimacs , Clause , Literal , Model , VarId
              , readDimacsFile , readDimacsFileAndNames )
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

import VariableEliminationOBDD ( variableEliminationOBDD
                               , variableEliminationOBDDUsingDTree )

import Text.PrettyPrint.HughesPJClass ( prettyShow )
import PrettyClassExt ( printPretty )

import IndexedCNF ( fromDimacs , unitPropagate , toDimacs )
import DPLL ( dpll
            , mostOftenUsedVarHeuristic
            , momsHeuristic )

import Hypergraph ( Node )
import HypergraphPartitioning ( partition , partitionMultilevel )
import KMPartitioning ( kmPartition )

import DTreeHGP ( dtreeFromDimacs , singleListElement
                , DTree ( DNode, DLeaf)
                , printDTreeCutsets)

import Timed ( timed )

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
  

pairwiseFold :: (a -> a -> a) -> [a] -> a
pairwiseFold f [!x] = x
pairwiseFold f (!a : (!b) : xs) = pairwiseFold f $ redu (a:b:xs)
  where redu (!a:(!b):xs) = let h = f a b in h `seq` h : redu xs
        redu xs         = xs


histogram :: (Ord a) => [a] -> [(a, Int)]
histogram [] = []
histogram xs = 
  reverse $
  L.sortBy (comparing snd) $
  M.toList $
  pairwiseFold (M.unionWith (+))
  [ M.singleton x 1
  | x <- xs ]

printHistogram :: (Ord a, Show a) => [a] -> IO () 
printHistogram = mapM_ print . histogram

printCNFStatsShort :: Dimacs -> IO ()
printCNFStatsShort cnf =
  do putStrLn $ "num clauses: " ++ show (length cnf)
     let numvars = S.size $ S.fromList $ concat $ map (map abs) cnf
     putStrLn $ "num vars: " ++ show numvars

printCNFStats :: Dimacs -> IO ()
printCNFStats cnf =
  do printCNFStatsShort cnf
     putStrLn "histogram over clause lengths"
     putStrLn "  (clause length, count)"
     printHistogram $ map length cnf
     putStrLn "histogram over number of variable occurences"
     putStrLn "  (#var occurences, count)"
     printHistogram $ map snd $ histogram $ map abs $ concat cnf
     putStrLn $ "histogram over number of "
       ++"variable occurences in clauses of length 2"
     putStrLn "  (#var occurences, count)"
     pcl 2
     putStrLn $ "histogram over number of "
       ++"variable occurences in clauses of length 3"
     putStrLn "  (#var occurences, count)"
     pcl 3
  where pcl n =
          printHistogram $ map snd $ histogram $ map abs $
          concat $ filter (\cl -> length cl == n) cnf



partitionCNF :: Dimacs -> IO ([VarId], [Clause], [Clause])
partitionCNF cnf =
  do (cutEdges, as, bs) <-
       -- partition
       -- partitionMultilevel
       kmPartition
       edges
     return
       ( map evar $ S.toList cutEdges
       , nodesClauses as
       , nodesClauses bs )
  where
    edges =
      map snd $ M.toList $ M.fromListWith (++)
      [ (var, [node])
      | (node, vars) <- idcnf
      , var <- S.toList vars ]
    idcnf :: [(Int, S.Set VarId)]
    idcnf = zip [1..] $ M.keys varsToClauses
    idcnfMap = M.fromList idcnf
    varsToClauses :: M.Map (S.Set VarId) [Clause]
    varsToClauses = M.fromListWith (++)
      [ ( S.fromList $ map abs clause , [clause]) | clause <- cnf ]
    varsForNodes :: [Node] -> [S.Set VarId]
    varsForNodes = map (idcnfMap M.!)
    evar =
      singleListElement . S.toList .
      L.foldl1 S.intersection . varsForNodes . V.toList
    nodesClauses ns = L.concatMap (varsToClauses M.!) $
                      varsForNodes $ S.toList ns


cnfFile =
  -- "out.cnf"
  -- "sudoku-complete.cnf"
  -- "simple.cnf"
  -- "wp-cdcl-example.cnf"
  "../sat-2002-beta/submitted/"
  ++ "goldberg/fpga_routing/term1_gr_rcs_w3.shuffled.cnf"
  -- ++ "goldberg/fpga_routing/term1_gr_rcs_w4.shuffled.cnf"
  -- ++ "goldberg/fpga_routing/term1_gr_2pin_w4.shuffled.cnf"
  -- ++ "aloul/Bart/bart10.shuffled.cnf"
  -- ++ "aloul/Homer/homer17.shuffled.cnf"
  -- ++ "aloul/Bart/bart30.shuffled.cnf"
  -- ++ "aloul/Lisa/lisa20_3_a.shuffled.cnf"
  -- ++ "biere/dinphil/dp06u05.shuffled.cnf"
  -- ++ "biere/dinphil/dp02s02.shuffled.cnf"
  -- ++ "biere/dinphil/dp04s04.shuffled.cnf"
  -- "../sat-2002-beta/generated/"
  -- ++"gen-3/gen-3.1/glassy-v249-s1767284702.cnf"
  -- "vmpc_29.cnf"
  -- "E04F19.cnf"
  -- "partial-10-11-s.cnf"

readFileUP filename =
  do (c1, names) <- readDimacsFileAndNames filename
     let Just (c2, as) = unitPropagate $ fromDimacs c1
     -- printPretty ("unit propagation assignments", as)
     printPretty ("positive unit propagation assignments",
                  filter (>0) as)
     return $ (toDimacs c2, names)
     
testPartition =
  do (cnf, names) <- readFileUP cnfFile
     printCNFStatsShort cnf
     (cv, as, bs) <- partitionCNF cnf
     print $ L.sort $ map (lookupName names) cv
     {-putStrLn "as:"
     mapM_ (printClause names) as
     putStrLn "bs:"
     mapM_ (printClause names) bs-}

printClause names =
  print . (map $ lookupName names)

lookupName names n
  | n < 0 = "-" ++ lookupName names (abs n)
  | otherwise = fromMaybe (show n) $ M.lookup n names

testDTreeHGP =
  do (cnf, _) <- readFileUP cnfFile
     printCNFStatsShort cnf
     dt <- dtreeFromDimacs cnf
     printPretty dt
     return dt

timedDPLL =
  do (cnf, names) <- readFileUP cnfFile
     timed "dpll" $
       do xs <- enumerateModelsDPLL cnf
          print ("number of solutions", length xs)
          mapM_ printModel xs
          return (xs, names)

timedDPLLAssignmentFrq =
  do (xs, names) <- timedDPLL
     printPretty $
       map (\(l,c) -> (lookupName names l, c)) $
       L.sortBy (comparing snd) $ M.toList $ M.unionsWith (+)
       [ M.singleton lit (1::Int)
       | x <- xs
       , lit <- S.toList x
       , lit > 0]

checkAllClausesInDTree cnf dt =
  do printPretty ("clauses in dtree but not in cnf",
                  S.difference dtClauses cnfClauses)
     printPretty ("clauses in cnf but not in dtree",
                  S.difference cnfClauses dtClauses)
  where cnfClauses = clauses cnf
        clauses = S.fromList . map S.fromList
        dtClauses = dc dt
        dc (DNode l r) = S.union (dc l) (dc r)
        dc (DLeaf cs) = clauses cs
          

testCnf numVars numClauses =
  do cnf <- threeCNF numVars numClauses
     putStrLn "cnf:"
     mapM_ print cnf
     return cnf
     
testVariableElimination cnf =
  do putStrLn "models:"
     mapM_ printModel =<< enumerateModelsDPLL cnf
     printPretty $ variableEliminationOBDD cnf  

testVariableEliminationOnRandom numVars numClauses =
  do cnf <- testCnf numVars numClauses
     testVariableElimination cnf
     putStrLn "\n\nnow the same again, but via dtree:"
     dt <- dtreeFromDimacs cnf
     checkAllClausesInDTree cnf dt
     printPretty $ variableEliminationOBDDUsingDTree cnf dt

testVariableEliminationFile cnfFile =
  do c1 <- readDimacsFile cnfFile
     let Just (c2, _) = unitPropagate $ fromDimacs c1
     let c3 = toDimacs c2
     printCNFStats c3
     printPretty $ variableEliminationOBDD c3

testVariableEliminationUsingDTreeFile cnfFile =
  do (cnf, _) <- readFileUP cnfFile
     dt <- timed "build dtree by hypergraph partitioning" $
           dtreeFromDimacs cnf
     printDTreeCutsets dt
     checkAllClausesInDTree cnf dt
     printPretty $ variableEliminationOBDDUsingDTree cnf dt

main =
  testPartition
  -- testDTreeHGP
  -- timedDPLL
  -- timedDPLLAssignmentFrq
  -- testVariableEliminationOnRandom 20 70
  -- testVariableEliminationFile cnfFile
  -- testVariableEliminationUsingDTreeFile cnfFile
  
