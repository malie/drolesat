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

type Node = Int

-- each Edge connects two or more nodes
type InputEdge = [Node]
type InputGraph = [InputEdge]

type Edge = V.Vector Node
type Graph = M.Map Node (V.Vector Edge)

testGraph :: InputGraph
testGraph =
  [ [1,2,3],[1,3,4],[1,9]
  , [2,3,4],[2,5]
  , [3,4,5],[3,4,6]
  , [4,5]
  , [5,6],[5,7,8],[5,7,9]
  , [6,7,8]
  , [7,8],[7,9]
  , [8,9]]


neighboursMap :: InputGraph -> Graph
neighboursMap graph =
  M.map V.fromList $
  M.unionsWith (++)
  [ M.fromList [(n, [vedge])]
  | edge <- graph
  , let vedge = V.fromList edge
  , n <- edge ]

initialClustering :: Graph -> Node -> S.Set Node
initialClustering graph seed = grow $ S.singleton seed
  where
    half = M.size graph `div` 2
    grow cl
      | S.size cl >= half = cl
      | otherwise =
        -- trace ("grow: " ++ show cl) $
        grow $ S.union cl $ S.fromList $
        take (max 1 $ min (S.size cl) (half - S.size cl)) $
        map fst $ reverse $ L.sortBy (comparing snd) $
        M.toList $ M.unionsWith (+)
        [ M.fromList [(e, 1::Int)
                     | e <- V.toList edge
                     , S.notMember e cl ]
        | n <- S.toList cl
        , edge <- V.toList $ graph M.! n ]

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
     
  

partition :: InputGraph -> IO (S.Set Node)
partition inputGraph =
  do let graph = neighboursMap inputGraph
     reportNumberOfEdges graph
     borderRandomWalk graph

borderRandomWalk :: Graph -> IO (S.Set Node)
borderRandomWalk graph =
  do i <- bestInitialClustering 17 graph
     -- print ("initial", i)
     improve graph i 1 (i, 0, 999999)

nodePartition :: S.Set Node -> Node -> Bool
nodePartition cl n = S.member n cl


borderNodes :: Graph -> S.Set Node -> [Node]
borderNodes graph cl =
  S.toList $ S.fromList
  [ n
  | (n, edges) <- M.toList graph
  , let pn = nodePartition cl n
  , edge <- V.toList edges
  , neighbour <- V.toList edge
  , pn /= nodePartition cl neighbour ]


improve :: Graph -> S.Set Node -> Int -> (S.Set Node, Int, Int)
           -> IO (S.Set Node)
improve graph cl 400 (bestcl, _, bestcut) =
  do print("stopping after max number of improve iterations")
     return bestcl -- cl???
--{-
improve graph _ _ (bestcl, lastimp, bestcut) | lastimp > 19 =
  do print("stopping after no improvements for longtime", bestcut)
     return bestcl
     ---}
improve graph cl it (bestcl, lastimp, bestcut) =
  do -- print cl
     -- reportClusterBalance graph cl
     let cuts = numberOfCutEdges graph cl
     print ("cut edges:", cuts)
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
     --- print ("flipping", map (fst . fst) sf)
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
      do edge <- randomVectorElement $ graph M.! n
         next <- randomVectorElement edge
         let p = nodePartition cl next
             (d0,d1) = if p then (co0, succ co1) else (succ co0, co1)
         walk cl next (pred i) d0 d1

balance graph cl =
  let as = fromIntegral $ S.size cl
      bs = fromIntegral $
           S.size (S.difference (M.keysSet graph) cl)
  in as / (as + bs)

checkBalance graph cl =
  let b = balance graph cl
  in (if b > 0.45 then [0] else [])
     ++ if b < 0.55 then [1] else []

reportClusterBalance graph cl =
  print ("balance:",
         S.size cl,
         S.size (S.difference (M.keysSet graph) cl))

reportNumberOfEdges graph =
  print ("number of edges:",
         length $ S.toList $ S.fromList
         [ edge
         | (_, nodeEdges) <- M.toList graph
         , edge <- V.toList nodeEdges ])

numberOfCutEdges graph cl =
  length $ S.toList $ S.fromList $ concat
  [ if edgeComplete edge then [] else [edge]
  | (_, nodeEdges) <- M.toList graph
  , edge <- V.toList nodeEdges]
  where
    partition n = S.member n cl
    edgeComplete edge =
      1 == S.size (S.fromList $ map partition $ V.toList edge)


-- graph representation for coarsening
type CGraph = V.Vector (S.Set Node)


partitionMultilevel :: InputGraph -> IO (S.Set Node)
partitionMultilevel graph =
  do let cg = graphForCoarsening graph
     cs@((_, coarsest):_) <- liftM reverse $ coarseningSteps cg
     let g = cgraphForPartitioning coarsest
     putStrLn "coarse graph:"
     reportNumberOfEdges g
     -- mapM_ print $ map S.toList $ V.toList coarsest
     print("coarsening map sizes:", map (M.size . fst) cs)
     coarsestCl <- borderRandomWalk g
     expand coarsestCl (zip
                        (tail (map snd cs) ++ [cg])
                        (map fst cs))

graphForCoarsening :: InputGraph -> CGraph
graphForCoarsening = V.fromList . map S.fromList

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
    mff :: [((Node, Node), Double)]
    mff = [ let af = frq M.! a
                bf = frq M.! b
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
          -> IO (S.Set Node)
expand = foldM ex
  where ex cl (cg, mp) =
          do let g = cgraphForPartitioning cg
                 nodes = M.keysSet g
                 newNodes = S.difference nodes cl
                 ncl = [ n
                       | n <- S.toList newNodes
                       , Just nn <- [M.lookup n mp]
                       , S.member nn cl]
                 cl2 = S.union cl $ S.fromList ncl
             improve g cl2 0 (cl2, 0, 999999)


