-- trying to adapt K-Means ideas to graph partitioning:
-- first choose randomly N nodes as centers;
-- nodes pertain to closest center;
-- find new centers by random walks within each cluster,
-- the nodes hit most often are the new center.
-- repeat till clusters change no more.
-- a random walk is interrupted when a cluster is left.

module KMPartitioning ( main , kmPartition , partitionCarefully
                      , kmPartitionN )
       where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Either as DE

import Control.Monad ( liftM , replicateM , foldM )
import Data.Ord ( comparing )
import Debug.Trace ( trace )

import Random ( randomListElement , randomVectorElement
              , randomChoice')

-- package 'prettyclass'
import Text.PrettyPrint.HughesPJClass
  ( parens , fsep , Pretty , pPrint , text , prettyShow , hang , sep )
import PrettyClassExt ( printPretty , flist )

import Hypergraph
  ( Node, InputEdge, InputGraph , Edge , Graph
  , testGraph , testGraph2
  , neighboursMap , nodeEdges , allNeighboursAsSet
  , PartitionResult , partitionResult
  , nodePartition , borderNodes , balance , reportClusterBalance
  , numberOfEdges , connectedComponents , allEdges )


-- initially a center is one node
-- but later a single center is defined by multiple nodes.
type Center = S.Set Node
type Centers = [Center]

randomInitialCenters :: Int -> Graph -> IO Centers
randomInitialCenters n graph =
  recur n (10+2*n) (M.keys graph) S.empty
  where
    recur _ 0 _ _     = return []
    recur 0 _ nodes _ = return []
    recur n tries nodes seen =
      do center <- randomListElement nodes
         if S.member center seen
           then recur n (pred tries) nodes seen
           else
           do more <-
                recur (pred n) tries nodes
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
             xs <- replicateM 15 (recur 9 cl node [])
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
           if maxnumc < 2 then printPretty cs else return ()
           let dm = distanceMap graph cs
           -- printPerCluster numClusters dm
           printBalance dm
           reportNumberOfCutEdgesForDM graph dm
           w <- randomWalkWithinClusters graph dm
           -- printPerCluster numClusters w
           let nf = max 1 $ div (M.size graph) (15 * numLoopIterations)
           let numc = min maxnumc $ max 1 $ n*nf
               newCenters =
                 trace ("using numc = " ++ show numc) $
                 findNewCenters numClusters w numc
           -- printPretty newCenters
           recur (succ n) newCenters
    maxnumc =
      let ne = numberOfEdges graph
      in if ne < 10
         then 1
         else ne

printBalance dm =
  print $
  M.unionsWith (+)
  [ M.singleton center 1
  | (_, (center, _)) <- M.toList dm ]


type Clusters = M.Map Node CenterId

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
                       else
                         do let cs2 = M.insert n mx cs
                            randomChoice' 0.95 (return ()) $
                              do putStrLn $ concat
                                   ["switching node ", show n,
                                    " from " , show $ ocl,
                                    " to ", show mx ]
                                 print("number of cut edges:",
                                       kmNumberOfCutEdges graph cs2)
                            recur 0 cs2
    nodesVector = V.fromList $ M.keys graph


kmPartition :: Graph -> IO PartitionResult
kmPartition graph
  | numberOfEdges graph < 10
    = partitionSmall graph
  | otherwise =
    do let numClusters = 2
       cs1 <- randomInitialCenters numClusters graph
       if length cs1 < numClusters
         then partitionSmall graph
         else
         do -- printPretty cs
            cs2 <- kmPartitioningLoop graph numClusters cs1
            let dm = distanceMap graph cs2
            clusters <- swapBorderNodes 10000 graph $ M.map fst dm
            return $ partitionResultFromClusters graph clusters


partitionResultFromClusters graph clusters =
  partitionResult graph $
  S.fromList $
  [ node
  | (node, centerId) <- M.toList clusters
  , centerId == 0 ]


reportNumberOfCutEdgesForDM graph dm =
  reportNumberOfCutEdgesForClusters graph $ distanceMapToClusters dm

distanceMapToClusters :: DistanceMap -> Clusters
distanceMapToClusters = M.map fst

reportNumberOfCutEdgesForClusters graph clusters =
  print("number of cut edges:",
        kmNumberOfCutEdges graph clusters)

test =
  do let graph = neighboursMap testGraph2
     cs <- randomInitialCenters 2 graph
     print cs
     let dm = distanceMap graph cs
     printBalance dm
     reportNumberOfCutEdgesForDM graph dm
     clusters <- swapBorderNodes 10 graph $ M.map fst dm
     reportNumberOfCutEdgesForClusters graph clusters
     
partitionSmall graph =
  do print "using partitionSmall"
     rs <- replicateM 10 randpart
     return $ snd $ L.minimumBy (comparing fst) rs
  where
    randpart =
      do cs <- randomInitialCenters 2 graph
         if length cs < 2
           then randpart
           else
           let dm = distanceMap graph cs
               clusters = distanceMapToClusters dm
           in return ( kmNumberOfCutEdges graph clusters
                     , partitionResultFromClusters graph clusters)
  

-- first check for connected components, then
-- partition if really needed
partitionCarefully :: Graph -> IO PartitionResult
partitionCarefully graph =
  case connectedComponents graph of
    [_] ->
      -- all graph nodes are somehow connected, i.e. a single component
      do print ("single component")
         kmPartition
         -- partitionMultilevel
           graph
    components ->
      case reverse $ L.sortBy (comparing M.size) components of
        (largest:_) ->
          return $ partitionResult graph $ M.keysSet largest


kmPartitionN :: Int -> Graph -> IO ([Edge], M.Map CenterId [Edge])
kmPartitionN numClusters graph =
  do cs1 <- randomInitialCenters numClusters graph
     if length cs1 < numClusters
       then error "cannot determine initial centers"
       else
       do -- printPretty cs
          cs2 <- kmPartitioningLoop graph numClusters cs1
          let dm = distanceMap graph cs2
          clusters <- swapBorderNodes 10000 graph $ M.map fst dm
          return $ partitionResultNWay numClusters graph clusters
  

partitionResultNWay :: Int -> Graph -> Clusters
                    -> ([Edge], M.Map CenterId [Edge])
partitionResultNWay numClusters graph clusters =
  ( cutEdges
  , M.unionsWith (++) completeEdges )
  where
    nodePartition = (clusters M.!)
    (cutEdges, completeEdges) =
      DE.partitionEithers
      [ let ps = map nodePartition $ V.toList edge
            dps = S.fromList ps
        in if S.size dps > 1
           then Left edge
           else let [partition] = S.toList dps
                in Right $ M.singleton partition [edge]
      | edge <- allEdges  graph ]
