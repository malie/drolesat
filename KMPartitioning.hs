-- trying to adapt K-Means ideas to graph partitioning:
-- first choose randomly N nodes as centers;
-- nodes pertain to closest center;
-- find new centers by random walks within each cluster,
-- the nodes hit most often are the new center.
-- repeat till clusters change no more.
-- a random walk is interrupted when a cluster is left.

module KMPartitioning ( kmPartition ) where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

import Control.Monad ( liftM , replicateM , foldM , replicateM )
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
          | M.null ns = dm
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
             as <- recur 7 cl node []
             bs <- recur 7 cl node []
             cs <- recur 7 cl node []
             return $ M.unionsWith addCounts [as, bs, cs]
             
        recur 0 _ _ path = return $ M.fromListWith addCounts path
        recur n cl node path =
          do edge <- randomNodeEdge graph node
             neighbour <- randomVectorElement edge
             let ncl = nodeCluster neighbour
             if ncl /= cl
               then recur 0 cl node $ drop 2 path
               else recur (pred n) cl neighbour $
                    (neighbour, (cl, 1)) : path
        nodeCluster n = fst $ distanceMap M.! n
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

kmPartitioningLoop numIterations graph numClusters cs = recur 0 cs
  where
    recur n cs
      | n == numIterations = return cs
      | otherwise =
        do putStrLn $ "\niteration: " ++ show n
           let dm = distanceMap graph cs
           -- printPerCluster numClusters dm
           print("number of cut edges:", kmNumberOfCutEdges graph dm)
           w <- randomWalkWithinClusters graph dm
           -- printPerCluster numClusters w
           let newCenters =
                 findNewCenters numClusters w (max 1 n)
           printPretty newCenters
           recur (succ n) newCenters

kmNumberOfCutEdges :: Graph -> DistanceMap -> Int
kmNumberOfCutEdges graph = length . S.toList . kmCutEdges graph

kmCutEdges :: Graph -> DistanceMap -> S.Set Edge
kmCutEdges graph dm =
  S.fromList $ concat
  [ if edgeComplete edge then [] else [edge]
  | (_, nodeEdges) <- M.toList graph
  , edge <- V.toList nodeEdges]
  where
    partition n = fst $ dm M.! n
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
     kmPartitioningLoop 5 graph numClusters cs

kmPartition :: InputGraph -> IO PartitionResult
kmPartition inputGraph = undefined
{-
  do let graph = neighboursMap inputGraph
     reportNumberOfEdges graph
     
     return $ partitionResult graph cl
-}
