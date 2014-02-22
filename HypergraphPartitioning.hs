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


module HypergraphPartitioning ( partition ) where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

import Control.Monad ( liftM , replicateM )
import Data.Ord ( comparing )
import Debug.Trace ( trace )

import Random ( randomListElement , randomVectorElement )

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

partition :: InputGraph -> IO (S.Set Node)
partition inputGraph =
  do let graph = neighboursMap inputGraph
     reportNumberOfEdges graph
     borderRandomWalk graph

borderRandomWalk :: Graph -> IO (S.Set Node)
borderRandomWalk graph =
  do r <- randomListElement $ M.keys graph
     -- print ("starting with", r)
     let i = initialClustering graph r
     -- print ("initial", i)
     improve i 1
  where
    partition cl n = S.member n cl
    borderNodes cl =
      S.toList $ S.fromList
      [ n
      | (n, edges) <- M.toList graph
      , let pn = partition cl n
      , edge <- V.toList edges
      , neighbour <- V.toList edge
      , pn /= partition cl neighbour ]
    numWalks = 20
    improve :: S.Set Node -> Int -> IO (S.Set Node)
    improve cl it =
      do -- print cl
         reportClusterBalance graph cl
         reportCutEdges graph cl
         let bns = borderNodes cl
         -- print ("border nodes:", bns)
         pot <-
           sequence
           [ do r0 <- replicateM numWalks $ walk cl n 7 0 0
                let r1 = sum r0 / fromIntegral numWalks
                let (r2, d) = if partition cl n
                              then (1 - r1, 0)
                              else (r1, 1)
                return ((n, d), r2)
           | n <- bns ]
         let sort = reverse . L.sortBy (comparing snd)
             sw = sort pot
         -- mapM_ print $ take 20 sw
         let fi x = filter (\m@((_,y),_) -> y == x) sw
             tkn = if it < 20 then 41
                   else if it < 40 then 11
                   else if it < 60 then 7 else 1
             tk = max 1 $ tkn `div` 2
             sf = take tkn $ sort $ take tk (fi 0) ++ take tk (fi 1)
         print ("flipping", map (fst . fst) sf)
         improve (L.foldl' flip cl sf) (succ it)
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
         let p = partition cl next
             (d0,d1) = if p then (co0, succ co1) else (succ co0, co1)
         walk cl next (pred i) d0 d1

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

reportCutEdges graph cl =
  print ("cut edges:",
         length $ S.toList $ S.fromList $ concat
         [ if edgeComplete edge then [] else [edge]
         | (_, nodeEdges) <- M.toList graph
         , edge <- V.toList nodeEdges ])
  where
    partition n = S.member n cl
    edgeComplete edge =
      1 == S.size (S.fromList $ map partition $ V.toList edge)

