-- one try at constructing a dtree by an 'occurence tree'

module DTreeOT
       ( dtreeTest
       , buildOccurenceTree
       , reportOccurenceTree
       , testOccurenceTree
       , randomInitialAssignment
       , reportAssignment
       , improveAssignment
       ) where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad ( liftM )
import Data.Ord ( comparing )
import Data.Maybe ( catMaybes )

import Dimacs ( Literal , VarId , Clause, Dimacs )
import IndexedCNF
import Timed ( timed , timedDeepseq )
import Random ( randomInt , randomVectorElements , randomListElement
              , randomizeList )

{-
input: cnf in dimacs format
       effort to spend as an integer
output: dtree

(maybe: find 30 most common variables,
  delete them before constructing the dtree
  (kind of "accepted cutset"))

find many uncommon variables,
see these as dtree-leafes
assign every clause mentioning an uncommon variable
  to it's dtree-leaf
  (if a clause mentiones many uncommon variables
   assign it later to the best fitting)

after clause assigning, every dtree-leaf
can be seen as a set of variables, the union of
all clauses assigned to it.

next phase, for second level from bottom:
restart with cnf
union those clauses that were already assigned
to some new macro-clause (a pseudo clause...),
mentioning every var in it exactly once.
  (thus the clauses mentioning more than one time,
   are not unioned already, and will cause those clusters
   "come closer")

do the same again,
but find uncommon variables excluding the uncommon
variables from the level below.
and drop those 

?? prefer merging clusters having many uncommon variables in common
   esp. those vars occuring only two times
   in two clusters.


will local search be helpful to improve such a dtree???

should probably be repeated a few times, to find a good dtree


improve initial dtree by local search:
  reinsert random clause
  reinsert clause causing a cutset entry in an upper level
  reinsert all clauses causing a cutset entry in an upper level
    (remove the smaller set of clauses)


  

-}

dtreeTest :: IndexedCNF -> IO ()
dtreeTest ic =
  do timedDeepseq "counts" $ return counts
     timedDeepseq "uncommonVars" $ return uncommonVars
     -- mapM_ print uncommonVars
     let (seeds, seen, used) =
           recur (map fst uncommonVars) S.empty S.empty []
     timedDeepseq "recur" $ return seen
     print ("num uncommon seed vars", length seeds)
     print ("seen vars", S.size seen)
     print ("clauses seen", sum $ map (length.snd) seeds)
     print ("var #clauses #vars")
     flip mapM_ seeds
       (\(v,clauses) ->
         print (v,
                length clauses,
                S.size $ S.fromList $ map abs $ concat clauses))
     print ("num used clauses", S.size used)
     -- mapM_ print $ S.toList used
     let unused = S.difference
                  (S.fromList $ map S.fromList $ toDimacs ic)
                  used
     print ("num unused clauses", S.size unused)
     -- mapM_ print $ S.toList unused
     let xs = [ (v, S.fromList $ map abs $ concat clauses)
              | (v, clauses) <- seeds]
         ovl = [ ((a, b), S.size (S.intersection as bs))
               | (a,as) <- xs
               , (b,bs) <- xs
               , a < b]
     mapM_ print $
       take 100 $
       reverse $
       L.sortBy (comparing snd)
       ovl
  where counts =
          M.unionsWith (+)
          [ M.singleton (abs lit) (1::Int)
          | clause <- toDimacs ic
          , lit <- clause ]
        uncommonVars =
          L.sortBy (comparing snd) $
          M.toList counts
        recur [] seen used res = (reverse res, seen, used)
        recur (u:us) seen used res
          | S.member u seen =
            recur us seen used res
          | otherwise =
            let clauses = clauseBodiesWithVar u ic
            in recur us
                  (S.union seen
                     (S.fromList $ map abs $ concat clauses))
                  (S.union used
                     (S.fromList $ map S.fromList clauses))
                  ((u, clauses) : res)

{-
-- don't move individual clauses in local search, but
-- move all clauses belonging to some variable
randomPartitionInTwo :: Dimacs -> IO (S.Set VarId, S.Set VarId)
randomPartitionInTwo d =
  do initialPartition <- randomVectorElements half allVariables
     localSearch 
  where allVariables = V.fromList $ S.toList $ S.fromList $
                       map abs $ concat d
        half = V.length allVariables `div` 2
        
-}


-- OccurenceTree
-- one clause at each leaf
-- partition all clauses quite randomly
-- each node tracks union of all variables below

data OT n l = Node { otLeft :: OT n l
                   , otRight :: OT n l
                   , occuringVars :: S.Set VarId
                   , nodeData :: n }
            | Leaf { occuringVars :: S.Set VarId
                   , otClause :: Clause
                   , leafData :: l }

buildOccurenceTree :: Dimacs -> OT () ()
buildOccurenceTree = foldPairwise recur . map mkLeaf
  where recur a b =
          let vs = S.union (occuringVars a) (occuringVars b)
          in Node a b vs ()
        mkLeaf clause =
          Leaf (S.fromList $ map abs clause) clause ()

foldPairwise :: (a -> a -> a) -> [a] -> a
foldPairwise _ [x] = x
foldPairwise f xs = foldPairwise f $ recur xs
  where recur [] = []
        recur [x] = [x]
        recur (a:b:xs) = f a b : recur xs
        

reportOccurenceTree ot = rep 1 [ot]
  where rep 30 _ = return ()
        rep n xs =
          do print (n, take 50 $ map (S.size . occuringVars) xs)
             rep (succ n) $
               concat [ case n of
                           Node l r _ _ -> [l, r]
                           _ -> []
                      | n <- xs ]

testOccurenceTree :: OT n l -> Dimacs -> IO ()
testOccurenceTree ot d =
  mapM_ t someVars
  where
    someVars = take 10 $ L.nub $ map abs $ concat d
    t v =
      do putStrLn $ "variable: " ++ show v
         print $ occs v [ot]
    occs _ [] = []
    occs v ns =
      length ns
      : occs v (concat [ (case n of
                             Node l r ov _ ->
                               if S.member v ov then [l, r] else []
                             Leaf _ _ _ -> [])
                       | n <- ns ])
      
type PartitionId = Int
data UVars = UVars { uvLeft :: S.Set VarId
                   , uvRight :: S.Set VarId }

type PT = OT UVars PartitionId

randomInitialAssignment :: OT () () -> IO PT
randomInitialAssignment ot = liftM fst $ recur ot M.empty
  where recur (Node l r vs _) as1 =
          do (ll, as2) <- recur l as1
             (rr, as3) <- recur r as2
             return (Node ll rr vs $ mkUVars ll rr, as3)
        recur (Leaf vs cl _) as =
          do part <- (case catMaybes $
                           map ((flip M.lookup as) . abs) cl of
                         p:_ -> return p
                         [] -> randomInt 0 1) :: IO PartitionId
             return ( Leaf vs cl part
                    , M.union as $
                      M.fromList [(abs lit, part) | lit <- cl])

mkUVars a b =
  let UVars al ar = nodeUVars a
      UVars bl br = nodeUVars b
  in UVars (S.union al bl) (S.union ar br)

nodeUVars (Node _ _ _ uv) = uv
nodeUVars (Leaf vs _ p) | p == 0 = UVars vs S.empty
                        | p == 1 = UVars S.empty vs


reportAssignment :: Int -> PT -> IO ()
reportAssignment = recur 1
  where
    recur n max _ | n > max = return ()
    recur n max (Node l r vs (UVars ul ur)) =
      do print (n, S.size vs, S.size ul, S.size ur,
                S.size (S.intersection ul ur))
         recur (succ n) max l
         recur (succ n) max r
    recur _ _ _ = return ()

cutset :: PT -> S.Set VarId
cutset (Node _ _ _ (UVars ul ur)) =
  S.intersection ul ur
  
improveAssignment :: Int -> PT -> IO PT
improveAssignment 0 pt = return pt
improveAssignment n pt = recur n 0 pt []
  where
    recur n css pt [] =
      do let cs = cutset pt
         vs <- randomizeList $ S.toList cs
         if null vs
           then return pt
           else recur n (S.size cs) pt vs
    recur n css pt (v:vs) =
      do if n `mod` 20 == 19
           then reportAssignment 3 pt
           else return ()
         putStrLn $ "try to improve clauses with var " ++ show v
         let c0 = assignClausesWithVar pt v 0
             c1 = assignClausesWithVar pt v 1
             s0 = S.size $ cutset c0
             s1 = S.size $ cutset c1
         putStrLn $ "  " ++ show css
           ++ " -> " ++ show s0 ++ " " ++ show s1
         let (bs,bc) = if s0 < s1 then (s0,c0) else (s1,c1)
         if bs < css
           then recur (pred n) bs bc vs
           else recur (pred n) css pt vs

assignClausesWithVar :: PT -> VarId -> PartitionId -> PT
assignClausesWithVar pt v p = recur pt
  where recur n@(Node l r vs (UVars ul ur)) =
          if S.member v vs
          then let ll = recur l
                   rr = recur r
               in Node ll rr vs $ mkUVars ll rr
          else n
        recur l@(Leaf vs cl op)
          | op == p       = l
          | S.member v vs = Leaf vs cl p
          | otherwise     = l
          
                 
