module Hypergraph
       ( Node, InputEdge, InputGraph , Edge , Graph
       , testGraph , testGraph2
       , neighboursMap , nodeEdges , allNeighboursAsSet
       , PartitionResult , partitionResult
       , nodePartition , borderNodes
       , balance , reportClusterBalance
       , reportNumberOfEdges , numberOfEdges
       , numberOfCutEdges , cutEdges
       , connectedComponents , allEdges
       , randomGraph )
       where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

import Random ( randomListElement , randomChoice )
import Control.Monad ( liftM , replicateM )

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
  -- , [1, 14], [1, 12]
  -- , [4, 12], [5, 13], [7, 16]
  ]

testGraph2 :: InputGraph
testGraph2 =
  [[27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 17, 14, 13, 12, 8, 4, 3],
   [42, 41, 40, 39, 38, 37, 36, 29, 28, 16, 15, 2, 1],
   [46, 45, 44, 43, 40, 39, 33, 30, 24, 22, 7, 6],
   [52, 51, 50, 49, 48, 47, 44, 41, 40, 34, 31, 25, 22, 9, 5, 3],
   [56, 55, 54, 53, 51, 45, 42, 40, 35, 32, 26, 22, 11, 10]]

neighboursMap :: InputGraph -> Graph
neighboursMap graph =
  M.map V.fromList $
  M.unionsWith (++)
  [ M.fromList [(n, [vedge])]
  | edge <- graph
  , let vedge = V.fromList $ L.sort edge
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
  print ("number of edges:", numberOfEdges graph)

numberOfEdges :: Graph -> Int
numberOfEdges graph =
  length $ allEdges graph

allEdges :: Graph -> [Edge]
allEdges graph =
  S.toList $ S.fromList
  [ edge
  | (_, nodeEdges) <- M.toList graph
  , edge <- V.toList nodeEdges ]

---- graphNumEdges :: Graph -> Int
---- graphNumEdges =
----   S.size . S.fromList
----   . map (S.fromList . V.toList)
----   -- [ Edge ]
----   . V.toList . V.concat
----   -- [ V.Vector Edge ]
----   . map snd
----   . M.toList
  


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


connectedComponents :: Graph -> [Graph]
connectedComponents graph =
  recur M.empty S.empty S.empty []
  where
    recur component frontier seen res
      | S.null frontier =
        if seen == M.keysSet graph
        then reverse $ addIfNotEmpty component res
        else
          let first = S.findMin $ S.difference (M.keysSet graph) seen
          in recur M.empty (S.singleton first) seen
             (addIfNotEmpty component res)
      | otherwise =
          recur
          (M.union component $
           M.fromList [ (front, nodeEdges graph front)
                      | front <- S.toList frontier ])
          (S.fromList
           [ neighbour
           | front <- S.toList frontier
           , edge <- V.toList $ nodeEdges graph front
           , neighbour <- V.toList edge
           , S.notMember neighbour seen])
          (S.union seen frontier)
          res
    addIfNotEmpty comp res
      | M.null comp  = res
      | otherwise = comp:res




-- fancy random graph generation
randomGraph :: IO InputGraph
randomGraph = liftM concat $ mapM gen degreeCounts
  where
    degreeCounts = [ (5, 0.1)
                   , (4, 0.15)
                   , (3, 0.2)
                   , (2, 0.5)]
    numNodes = 50
    gen (count, fraction) =
      do let n = round $ fromIntegral numNodes * fraction
         replicateM n $ randomListElementsX count [1 .. numNodes]

-- with probability 1/2 from first 10 list elements
-- with probability 3/4 from first 20 list elements
randomListElementsX :: Int -> [a] -> IO [a]
randomListElementsX n list = recur n []
  where
    recur 0 res = return $ reverse res
    recur n res =
      do a <- randomChoice
                (randomChoice
                  (randomListElement (take 10 list))
                  (randomListElement (take 20 list)))
                (randomListElement list)
         recur (pred n) (a:res)
     

