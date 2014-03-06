module Hypergraph
       ( Node, InputEdge, InputGraph , Edge , Graph
       , testGraph , neighboursMap , nodeEdges , allNeighboursAsSet
       , PartitionResult , partitionResult
       , nodePartition , borderNodes
       , balance , reportClusterBalance
       , numberOfCutEdges , cutEdges )
       where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

type Node = Int

-- each Edge connects two or more nodes
type InputEdge = [Node]
type InputGraph = [InputEdge]

type Edge = V.Vector Node
type Graph = M.Map Node (V.Vector Edge)

testGraph :: InputGraph
testGraph =
  [ [1,2,3],[1,3,4],[1,13]
  , [2,3,4],[2,5]
  , [3,4,5],[3,4,6]
  , [4,5]
  , [5,6],[5,7,8],[5,7,9]
  , [6,7,8]
  , [7,8],[7,9]
  , [8,9]
  , [9, 10, 11]
  , [10, 11, 12], [10, 15]
  , [12, 13]
  , [13, 14], [13, 14, 15]
  , [14, 15]
  , [15, 16]

  , [1, 14], [1, 12]
  -- , [4, 12], [5, 13], [7, 16]
  ]


neighboursMap :: InputGraph -> Graph
neighboursMap graph =
  M.map V.fromList $
  M.unionsWith (++)
  [ M.fromList [(n, [vedge])]
  | edge <- graph
  , let vedge = V.fromList edge
  , n <- edge ]


nodeEdges :: Graph -> Node -> V.Vector Edge
nodeEdges g n =
  case M.lookup n g of
    Just e -> e
    Nothing -> error $ "no such node " ++ show n

allNeighboursAsSet :: Graph -> Node -> S.Set Node
allNeighboursAsSet graph =
  S.fromList . concat . map V.toList . V.toList . nodeEdges graph

type PartitionResult = (S.Set Edge, S.Set Node, S.Set Node)

partitionResult :: Graph -> S.Set Node -> PartitionResult
partitionResult g cl =
  ( cutEdges g cl
  , cl
  , S.difference (M.keysSet g) cl)

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

balance :: Graph -> S.Set Node -> Double
balance graph cl =
  let as = fromIntegral $ S.size cl
      bs = fromIntegral $
           S.size (S.difference (M.keysSet graph) cl)
  in as / (as + bs)

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

numberOfCutEdges :: Graph -> S.Set Node -> Int
numberOfCutEdges graph = length . S.toList . cutEdges graph

cutEdges :: Graph -> S.Set Node -> S.Set Edge
cutEdges graph cl =
  S.fromList $ concat
  [ if edgeComplete edge then [] else [edge]
  | (_, nodeEdges) <- M.toList graph
  , edge <- V.toList nodeEdges]
  where
    partition n = S.member n cl
    edgeComplete edge =
      1 == S.size (S.fromList $ map partition $ V.toList edge)

