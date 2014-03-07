-- trying to adapt K-Means ideas to graph partitioning:
-- first choose randomly N nodes as centers;
-- nodes pertain to closest center;
-- find new centers by random walks within each cluster,
-- the nodes hit most often are the new center.
-- repeat till clusters change no more.
-- a random walk is interrupted when a cluster is left.

module KMPartitioning ( main , kmPartition ) where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

import Control.Monad ( liftM , replicateM , foldM )
import Data.Ord ( comparing )
import Debug.Trace ( trace )

import Random ( randomListElement , randomVectorElement )

-- package 'prettyclass'
import Text.PrettyPrint.HughesPJClass
  ( parens , fsep , Pretty , pPrint , text , prettyShow , hang , sep )
import PrettyClassExt ( printPretty , flist )

import Hypergraph
  ( Node, InputEdge, InputGraph , Edge , Graph
  , testGraph , neighboursMap , nodeEdges , allNeighboursAsSet
  , PartitionResult , partitionResult
  , nodePartition , borderNodes , balance , reportClusterBalance )


-- initially a center is one node
-- but later a single center is defined by multiple nodes.
type Center = S.Set Node
type Centers = [Center]

randomInitialCenters :: Int -> Graph -> IO Centers
randomInitialCenters n graph = recur n (M.keys graph) S.empty
  where
    recur 0 nodes _ = return []
    recur n nodes seen =
      do center <- randomListElement nodes
         if S.member center seen
           then recur n nodes seen
           else
           do more <-
                recur (pred n) nodes
                (S.union seen $ allNeighboursAsSet graph center)
              return $ (S.singleton center) : more

type CenterId = Int
type Distance = Int

type DistanceMap = M.Map Node (CenterId, Distance)

-- todo: improve O(n*diameter) .. even with 'done'
distanceMap :: Graph -> Centers -> DistanceMap
distanceMap graph centers = recur initialDistanceMap S.empty
  where recur dm done
          | M.null ns =
            dm
          | otherwise =
            recur (M.unionWith closer dm ns) (M.keysSet dm)
          where ns =
                  M.fromList -- no 'With', ignore overlaps, for now...
                  [ (neighbour, (centerId, ndistance))
                  | (node, (centerId, distance)) <- M.toList dm
                  , let ndistance = succ distance
                  , S.notMember node done
                  , edge <- V.toList $ nodeEdges graph node
                  , neighbour <- V.toList edge ]
                closer (a, aDistance) (b, bDistance)
                  | aDistance < bDistance = (a, aDistance)
                  | otherwise             = (b, bDistance)
        initialDistanceMap =
          M.fromList
          [ (centerNode, (centerId, 0::Distance))
          | (centerId, center) <- zip [0..] centers
          , centerNode <- S.toList center ]

type Count = Int

randomWalkWithinClusters
  :: Graph -> DistanceMap -> IO (M.Map Node (CenterId, Count))
randomWalkWithinClusters graph distanceMap =
  liftM (M.unionsWith addCounts) $ mapM walk $ M.keys graph
  where walk node =
          do let cl = nodeCluster node
             xs <- replicateM 6 (recur 7 cl node [])
             return $ M.unionsWith addCounts xs
             
        recur 0 _ _ path = return $ M.fromListWith addCounts path
        recur n cl node path =
          do edge <- randomNodeEdge graph node
             neighbour <- randomVectorElement edge
             let ncl = nodeCluster neighbour
             if ncl /= cl
               then recur 0 cl node $ drop 2 path
               else recur (pred n) cl neighbour $
                    (neighbour, (cl, 1)) : path
        -- nodeCluster n = fst $ distanceMap M.! n
        nodeCluster n = case M.lookup n distanceMap of
          Nothing -> error ("rwwc didn't find node " ++ show n) (-1)
          Just x -> fst x
        addCounts (center, count1) (_,count2) = (center, count1+count2)


randomNodeEdge :: Graph -> Node -> IO Edge
randomNodeEdge graph node =
  randomVectorElement $ nodeEdges graph node

findNewCenters :: Int -> M.Map Node (CenterId, Count) -> Int -> Centers
findNewCenters numClusters frqMap centerSize = reverse $ recur 0 []
  where recur clusterId res
          | clusterId == numClusters = res
          | otherwise =
            recur
            (succ clusterId)
            ((-- S.singleton $ fst $ L.maximumBy (comparing snd)
              S.fromList $ map fst $ take centerSize $
              reverse $ L.sortBy (comparing snd)
              [ (node, count)
              | (node, (center, count)) <- M.toList frqMap
              , center == clusterId ]
             ) : res)

numLoopIterations = 20
kmPartitioningLoop graph numClusters cs = recur 0 cs
  where
    recur n cs
      | n == numLoopIterations = return cs
      | otherwise =
        do putStrLn $ "\niteration: " ++ show n
           let dm = distanceMap graph cs
           -- printPerCluster numClusters dm
           print("number of cut edges:",
                 kmNumberOfCutEdges graph $ distanceMapToClusters dm)
           w <- randomWalkWithinClusters graph dm
           -- printPerCluster numClusters w
           let nf = div (M.size graph) (5 * numLoopIterations)
           let newCenters =
                 findNewCenters numClusters w (max 1 (n*nf))
           -- printPretty newCenters
           recur (succ n) newCenters


type Clusters = M.Map Node CenterId

distanceMapToClusters :: DistanceMap -> Clusters
distanceMapToClusters = M.map fst

kmNumberOfCutEdges :: Graph -> Clusters -> Int
kmNumberOfCutEdges graph = length . S.toList . kmCutEdges graph

kmCutEdges :: Graph -> Clusters -> S.Set Edge
kmCutEdges graph cs =
  S.fromList $ concat
  [ if edgeComplete edge then [] else [edge]
  | (_, nodeEdges) <- M.toList graph
  , edge <- V.toList nodeEdges]
  where
    partition n = case M.lookup n cs of
      Nothing -> error ("kmce didn't find node " ++ show n) (-1)
      Just x -> x
    edgeComplete edge =
      1 == S.size (S.fromList $ map partition $ V.toList edge)


printPerCluster :: (Pretty a, Pretty b)
                   => Int -> M.Map a (CenterId, b) -> IO ()
printPerCluster numClusters m = mapM_ recur [0 .. pred numClusters]
  where recur clusterId =
          do printPretty
               ("cluster", clusterId,
                [ (a,b)
                | (a, (c, b)) <- M.toList m
                , c == clusterId ])

main =
  do let graph = neighboursMap testGraph
     let numClusters = 2
     cs <- randomInitialCenters numClusters graph
     printPretty cs
     kmPartitioningLoop graph numClusters cs


-- use 'distanceMap' then toggle border nodes again and again
-- till minimal cut edges?
partitionCarefully graph centers = undefined

swapBorderNodes :: Int -> Graph -> Clusters -> IO Clusters
swapBorderNodes maxNum graph clusters = recur 0 clusters
  where
    recur num cs
      | num == maxNum = return cs
      |otherwise =
        do n <- randomVectorElement nodesVector
           let neighboursClusters =
                 M.unionsWith (+)
                 [ M.singleton (cs M.! neighbour)
                   (1 / (fromIntegral $ V.length edge))
                 | edge <- V.toList $ nodeEdges graph n
                 , neighbour <- V.toList edge]
           if M.size neighboursClusters < 2
             then recur num cs
             else do let mx = fst $ L.maximumBy (comparing snd) $
                              M.toList neighboursClusters
                         ocl = cs M.! n
                     if mx == ocl
                       then recur (succ num) cs
                       else do
                         putStrLn $ concat
                           ["switching node ", show n,
                            " from " , show $ ocl,
                            " to ", show mx ]
                         let cs2 = M.insert n mx cs
                         print("number of cut edges:",
                               kmNumberOfCutEdges graph cs2)
                         recur 0 cs2
    nodesVector = V.fromList $ M.keys graph


kmPartition :: Graph -> IO PartitionResult
kmPartition graph =
  do let numClusters = 2
     cs1 <- randomInitialCenters numClusters graph
     -- printPretty cs
     cs2 <- kmPartitioningLoop graph numClusters cs1
     let dm = distanceMap graph cs2
     clusters <- swapBorderNodes 10000 graph $ M.map fst dm
     return $
       partitionResult graph $
       S.fromList $
       [ node
       | (node, centerId) <- M.toList clusters
       , centerId == 0 ]
