-- create a dtree by hypergraph partitioning
{-# LANGUAGE BangPatterns #-}
module DTreeHGP ( dtreeFromDimacs
                , singleListElement
                , DTree ( DNode, DLeaf )
                , dtreeVars
                , DTreevs ( DNodevs , DLeafvs )
                , dvars
                , printDTreeCutsets
                ) where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

import Debug.Trace ( trace )

import Dimacs ( Dimacs , Clause , Literal , VarId )
import Hypergraph ( Node , neighboursMap )
import HypergraphPartitioning ( partitionMultilevel )
import KMPartitioning ( kmPartition )

-- package 'prettyclass'
import Text.PrettyPrint.HughesPJClass
  ( parens , fsep , Pretty , pPrint , text , prettyShow , hang , sep )
import PrettyClassExt ( printPretty , flist )



-- A "full binary tree" of clauses.
-- Only clauses with the exact same set of variables
-- are put together in a DLeaf
data DTree
  = DNode DTree DTree
  | DLeaf [Clause]

instance Pretty DTree where
  pPrint (DNode l r) = parens $ fsep $ [pPrint l, pPrint r]
  pPrint (DLeaf cls) = flist (text . show) cls


allSameVars :: Dimacs -> Bool
allSameVars (c:cl) =
  let vars = S.fromList . map abs
      vs = vars c
  in all (\c -> vars c == vs) cl

singleListElement :: [a] -> a
singleListElement [x] = x

foldPairwise :: (a -> a -> a) -> [a] -> a
foldPairwise _ [x] = x
foldPairwise f xs = foldPairwise f $ recur xs
  where recur [] = []
        recur [x] = [x]
        recur (a:b:xs) = f a b : recur xs

leafs = foldPairwise DNode . map mkLeaf
  where mkLeaf cl = DLeaf [cl]

dtreeFromDimacs :: Dimacs -> IO DTree
dtreeFromDimacs = mkdtree S.empty
  where
    mkdtree _ [cl] = return $ DLeaf [cl]
    mkdtree _ cnf | allSameVars cnf = return $ DLeaf cnf
    mkdtree _ [a,b] = return $ DNode (DLeaf [a]) (DLeaf [b])
    mkdtree parentCutset cnf
      | length edges <= 1 = return $ leafs cnf
      | otherwise =
        do printPretty cnf
           printPretty ("vedges", vedges)
           printPretty ("edges", edges)
           (!cutEdges, !as, !bs) <-
             -- partitionMultilevel
             kmPartition
             $ neighboursMap edges
           let nice = S.fromList $ concat $
                      map V.toList $ S.toList cutEdges
           printPretty ("nodes in cut edges", nice)
           printPretty ("known nodes", M.keys idcnfMap)
           printPretty ("missing nodes",
                        S.difference nice (M.keysSet idcnfMap))
           
           let cutVars = map evar $ S.toList cutEdges
           let parentCutset2 = S.union parentCutset $
                               S.fromList cutVars
           printPretty ("parent cutset", parentCutset2)
           printPretty ("as", as)
           printPretty ("bs", bs)
           let nas0 = nodesClauses as :: [Clause]
               nbs = nodesClauses bs :: [Clause]
               cnfset :: [Clause] -> S.Set (S.Set Literal)
               cnfset = S.fromList . map S.fromList
               nas = nas0 ++
                     -- add again clauses lost due to parent cutset...
                     (map S.toList $ S.toList $
                      S.difference (cnfset cnf)
                      (S.union (cnfset nas0) (cnfset nbs)))
                      :: [Clause]
           printPretty ("nas", nas)
           printPretty ("nbs", nbs)
           case (nas, nbs) of
             ([], xs) -> return $ leafs xs
             (xs, []) -> return $ leafs xs
             (xas, xbs) ->
               do l <- mkdtree parentCutset2 xas
                  r <- mkdtree parentCutset2 xbs
                  return $ DNode l r
      where
        edges = filter (\e-> length e > 1) $ map snd $ M.toList vedges
        vedges = M.fromListWith (++)
                 [ (var, [node])
                 | (node, vars) <- idcnf
                 , var <- S.toList vars
                 , S.notMember var parentCutset ]
        idcnf :: [(Node, S.Set VarId)]
        idcnf = zip [1..] $ M.keys varsToClauses
        idcnfMap = M.fromList idcnf
        lookupInIdcnfMap x = case M.lookup x idcnfMap of
          Nothing -> error $ "no such id in idcnf" ++ show x
          Just r -> r
        varsToClauses :: M.Map (S.Set VarId) [Clause]
        varsToClauses =
          M.fromListWith (++)
          [ ( S.fromList $ map abs clause , [clause]) | clause <- cnf ]
        lookupInVarsToClauses x =
          case M.lookup x varsToClauses of
            Nothing -> error $ "no such var-set in varsToClauses"
                               ++ show x
            Just r -> r
        varsForNodes :: [Node] -> [S.Set VarId]
        varsForNodes = map lookupInIdcnfMap
        evar =
          singleListElement . S.toList .
          L.foldl1 S.intersection . varsForNodes . V.toList
        nodesClauses ns = L.concatMap lookupInVarsToClauses $
                          varsForNodes $ S.toList ns


-- variable sets of a dtree
data DTreevs
  = DNodevs (S.Set VarId) DTreevs DTreevs
  | DLeafvs (S.Set VarId)

dtreeVars :: DTree -> DTreevs
dtreeVars (DLeaf clauses) =
  DLeafvs $ S.fromList $ map abs $ concat clauses
dtreeVars (DNode l r) =
  let cl = dtreeVars l
      cr = dtreeVars r
      varsl = dvars cl
      varsr = dvars cr
  in DNodevs (S.union varsl varsr) cl cr

dvars (DNodevs vs _ _) = vs
dvars (DLeafvs vs) = vs

instance Pretty DTreevs where
  pPrint (DNodevs vs l r) = parens $ hang (text "node") 2 $
                            sep [ pvars vs
                                , fsep $ [pPrint l, pPrint r]]
  pPrint (DLeafvs vs) = pvars vs

pvars vs = flist (text . show) $ S.toList vs




-- cutsets of a dtree
data DTreecs
  = DNodecs (S.Set VarId) DTreecs DTreecs
  | DLeafcs (S.Set VarId)

dtreeCutsets :: DTreevs -> DTreecs
dtreeCutsets d = recur d S.empty
  where
    recur (DLeafvs vars) parentCutset =
      DLeafcs (S.difference vars parentCutset)
    recur (DNodevs vs l r) parentCutset =
      let cs = S.difference (S.intersection (dvars l) (dvars r))
               parentCutset
          npc = S.union parentCutset cs
      in DNodecs cs (recur l npc) (recur r npc)

instance Pretty DTreecs where
  pPrint (DNodecs vs l r) = parens $ hang (text "cs") 2 $
                            sep [ pvars vs
                                , fsep $ [pPrint l, pPrint r]]
  pPrint (DLeafcs vs) = pvars vs


printDTreeCutsets = printPretty . dtreeCutsets . dtreeVars
