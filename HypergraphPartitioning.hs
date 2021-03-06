{-
-- border random walk
-- after (bad) initial partition
-- repeatedly, for every border node:
-- make some short random walks
-- the partition you are most of the time in becomes it's new partition
-- if a partition looses on one side, it has to win on some other side
-- or shrink to some (defined) minimum

-- how to steer away from unbalanced partitions??
-}

-- create random graphs with 90% local connections and
-- only the rest global?


module HypergraphPartitioning
       ( partition
       , partitionMultilevel
       , Node
       , PartitionResult
       ) where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

import Control.Monad ( liftM , replicateM , foldM , replicateM )
import Data.Ord ( comparing )
import Debug.Trace ( trace )

import Random ( randomListElement , randomVectorElement
              , randomDouble )

import Hypergraph
  ( Node, InputEdge, InputGraph , Edge , Graph
  , testGraph , neighboursMap , nodeEdges
  , PartitionResult , partitionResult
  , nodePartition , borderNodes , balance , reportClusterBalance
  , reportNumberOfEdges , numberOfCutEdges , cutEdges )


initialClustering :: Graph -> Node -> S.Set Node
initialClustering graph seed = grow $ S.singleton seed
  where
    half = M.size graph `div` 2
    grow cl
      | S.size cl >= half = cl
      | otherwise =
        case news cl of
          [] -> cl
          ns -> grow $ S.union cl $ S.fromList ns
    news cl =
      take (max 1 $ min (S.size cl) (half - S.size cl)) $
      map fst $ reverse $ L.sortBy (comparing snd) $
      M.toList $ M.unionsWith (+)
      [ M.fromList [(e, 1::Int)
                   | e <- V.toList edge
                   , S.notMember e cl ]
      | n <- S.toList cl
      , edge <- V.toList $ nodeEdges graph n ]

tap f x = trace (show $ f x) x

bestInitialClustering :: Int -> Graph -> IO (S.Set Node)
bestInitialClustering n graph =
  liftM (fst . head . tap (map snd)
         -- . reverse
         . L.sortBy (comparing snd)) $
  replicateM n ic
  where
    ic =
      do r <- randomListElement $ M.keys graph
         -- print ("starting with", r)
         let cl = initialClustering graph r
         return (cl, numberOfCutEdges graph cl)
     

partition :: Graph -> IO PartitionResult
partition graph =
  do reportNumberOfEdges graph
     cl <- borderRandomWalk graph
     return $ partitionResult graph cl

borderRandomWalk :: Graph -> IO (S.Set Node)
borderRandomWalk graph =
  do i <- bestInitialClustering 17 graph
     -- print ("initial", i)
     improve graph i 1 (i, 0, 999999)

improve :: Graph -> S.Set Node -> Int -> (S.Set Node, Int, Int)
           -> IO (S.Set Node)
improve graph cl 400 (bestcl, _, bestcut) =
  do print("max number of improve iterations")
     return bestcl -- cl???
--{-
improve graph _ _ (bestcl, lastimp, bestcut) | lastimp > 23  =
  do print("no improvements for long time", bestcut)
     return bestcl
     ---}
improve graph cl it (bestcl, lastimp, bestcut) =
  do -- print cl
     -- reportClusterBalance graph cl
     let cuts = numberOfCutEdges graph cl
     -- print ("cut edges:", cuts)
     let bns = borderNodes graph cl
     -- print ("border nodes:", bns)
     pot <-
       sequence
       [ do r0 <- replicateM numWalks $ walk cl n 9 0 0
            let r1 = sum r0 / fromIntegral numWalks
            let (r2, d) = if nodePartition cl n
                          then (1 - r1, 0)
                          else (r1, 1)
            return ((n, d), r2)
       | n <- bns ]
     let sort = reverse . L.sortBy (comparing snd)
         sw = sort pot
     -- mapM_ print $ take 20 sw
     let cb = checkBalance graph cl
         fi x | L.notElem x cb = []
              | otherwise = filter (\m@((_,y),_) -> y == x) sw
         tkn = {-if it < 10 then 9
                  else if it < 40 then 5
                   else if it < 60 then 3 else -} 1
         tk = max 1 $ tkn `div` 2
         sf = take tkn $ sort $ take tk (fi 0) ++ take tk (fi 1)
     {-print ("flipping", map fst sf
           , take 13 $ map (fst . fst) sw)-}
     if null sf
       then return cl
       else improve graph (L.foldl' flip cl sf) (succ it)
            (if cuts <= bestcut
             then (cl
                  , if cuts < bestcut then 0 else succ lastimp
                  , cuts)
             else (bestcl, succ lastimp, bestcut))
  where
    numWalks =
      20
      -- 100
    flip :: S.Set Node -> ((Node, Int), Double) -> S.Set Node
    flip cl ((chn, chs), force) =
      if force < 0.5
      then cl
      else
        if chs == 1
        then S.insert chn cl
        else S.delete chn cl
    walk :: S.Set Node -> Node -> Int -> Int -> Int -> IO Double
    walk cl _ 0 co0 co1 = return $
                          fromIntegral co1 / fromIntegral (co0+co1)
    walk cl n i co0 co1 =
      do let es = case M.lookup n graph of
               Just es -> es
               Nothing -> error $ "no such node " ++ show n
         edge <- randomVectorElement es
         next <- randomVectorElement edge
         let p = nodePartition cl next
             (d0,d1) = if p then (co0, succ co1) else (succ co0, co1)
         walk cl next (pred i) d0 d1

checkBalance graph cl =
  let b = balance graph cl
  in (if b > 0.45 then [0] else [])
     ++ if b < 0.55 then [1] else []


-- graph representation for coarsening
type CGraph = V.Vector (S.Set Node)


partitionMultilevel :: Graph -> IO PartitionResult
partitionMultilevel graph =
  do let cg = graphForCoarsening graph
     cs@((_, coarsest):_) <- liftM reverse $ coarseningSteps cg
     let g = cgraphForPartitioning coarsest
     putStrLn "coarse graph:"
     reportNumberOfEdges g
     -- mapM_ print $ map S.toList $ V.toList coarsest
     print("coarsening map sizes:", map (M.size . fst) cs)
     coarsestCl <- borderRandomWalk g
     (fg, cl) <- expand coarsestCl
                 (zip
                  (tail (map snd cs) ++ [cg])
                  (map fst cs))
     return $ partitionResult fg cl

graphForCoarsening :: Graph -> CGraph
graphForCoarsening graph =
  V.fromList $ S.toList $ S.fromList
  [ S.fromList $ V.toList edge
  | (node, edges) <- M.toList graph
  , edge <- V.toList edges ]

cgraphForPartitioning :: CGraph -> Graph
cgraphForPartitioning = neighboursMap . V.toList . V.map S.toList

type CoarseningStep = (M.Map Node Node, CGraph)

coarseningSteps :: CGraph -> IO [CoarseningStep]
coarseningSteps g =
  do -- cs@(_, cg) <- coarsen g
     let cs@(_, cg) = coarsenMF g
     if -- V.length cg > 100
       S.size (S.unions $ V.toList cg) > 150
       then do rest <- coarseningSteps cg
               return $ cs : rest
       else return [cs]

vnub = V.fromList . S.toList . S.fromList . V.toList

coarsen :: CGraph -> IO CoarseningStep
coarsen g =
  do print ("coarsening", V.length g)
     mp <- recur S.empty 0 M.empty
     return ( mp
            , vnub $
              V.filter (\e -> S.size e > 1) $
              V.map (cg mp) g)
  where
    recur :: S.Set Node -> Int -> M.Map Node Node
             -> IO (M.Map Node Node)
    recur _ 30 res   = return res
    -- recur seen _ res | S.size seen >= V.length g = return res
    recur seen n res =
      do edge <- randomVectorElement g
         xs <- twoRandomSetElementsSatisfying edge
               (flip S.notMember seen)
         case xs of
           Nothing -> recur seen (succ n) res
           Just (a,b) ->
             recur (S.insert a $ S.insert b seen)
                   (pred n)
                   (M.insert a b res)
    cg mp edge =
      S.map (\x-> M.findWithDefault x x mp) edge

twoRandomSetElementsSatisfying :: S.Set a -> (a -> Bool)
                                  -> IO (Maybe (a,a))
twoRandomSetElementsSatisfying set pred =
  recur Nothing 0.0 Nothing 0.0 $ S.toList set
  where
    recur (Just x) _ (Just y) _ [] = return $ Just (x,y)
    recur _ _ _ _ []               = return $ Nothing
    recur a av b bv (x:xs)
      | not $ pred x = recur a av b bv xs
      | otherwise =
        do d <- randomDouble 0 1
           case (d > av, d > bv) of
             (True, _) -> recur (Just x) d a av xs
             (_, True) -> recur a av (Just x) d xs
             (_, _)    -> recur a av b bv xs

dropSome l = drop (length l `div` 27) l

-- coarsen graph by combining nodes that already appear often together
coarsenMF :: CGraph -> CoarseningStep
coarsenMF g =
  let mp = recur S.empty M.empty
           (map fst $ reverse $ L.sortBy (comparing snd) $
            -- M.toList mf
            mff
           )
  in ( mp
     , vnub $
       V.filter (\e -> S.size e > 1) $
       V.map (cg mp) g)
  where
    mf :: M.Map (Node, Node) Int
    mf = -- reverse $ -- ????????
         -- dropSome $
         M.unionsWith (+)
         [ M.singleton (a,b) (1 :: Int)
         | edge <- V.toList g
         , a <- S.toList edge
         , b <- S.toList edge
         -- , a /= b -- ???
         , a < b
         ]
    frq = M.unionsWith (+)
         [ M.singleton a 1
         | edge <- V.toList g
         , a <- S.toList edge ]
    lookupFrq a = case M.lookup a frq of
      Just f -> f
      Nothing -> error $ "no such frq entry " ++ show a
    mff :: [((Node, Node), Double)]
    mff = [ let af = lookupFrq a
                bf = lookupFrq b
            in ((a,b),
                c
                {-(fromIntegral (c * max af bf)
                / fromIntegral (min af bf))-}
               )
          | ((a,b), c) <- M.toList mfd ]
    mfd = -- reverse $ -- ????????
         -- dropSome $
         M.unionsWith (+)
         [ M.singleton (a,b) w
         | edge <- V.toList g
         , let w = 1.0 / fromIntegral (S.size edge)
         , a <- S.toList edge
         , b <- S.toList edge
         -- , a /= b -- ???
         , a < b
         ]
    recur :: S.Set Node -> M.Map Node Node -> [(Node, Node)]
             -> M.Map Node Node
    recur _ res [] = res
    -- recur seen res _ | S.size seen >= V.length g = res
    recur seen res ((a,b):mf)
      | S.member a seen
        || S.member b seen = recur seen res mf
      | otherwise =
        recur
          (S.insert a $ S.insert b seen)
          (M.insert a b res)
          mf
    cg mp edge =
      S.map (\x-> M.findWithDefault x x mp) edge


expand :: S.Set Node -> [(CGraph, M.Map Node Node)]
          -> IO (Graph, S.Set Node)
expand = ex
  where ex cl ((cg, mp):xs) =
          do let g = cgraphForPartitioning cg
                 nodes = M.keysSet g
                 newNodes = S.difference nodes cl
                 ncl = [ n
                       | n <- S.toList newNodes
                       , Just nn <- [M.lookup n mp]
                       , S.member nn cl]
                 cl2 = S.union cl $ S.fromList ncl
             cl3 <- improve g cl2 0 (cl2, 0, 999999)
             if null xs
               then return (g, cl3)
               else ex cl3 xs


