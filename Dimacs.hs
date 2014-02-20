module Dimacs
       ( VarId, Literal, Clause, Dimacs
       , readDimacsFile
       , Model
       , variableCounts
       , leastFrequentVariables
       )
       where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Ord ( comparing )

import Timed ( timedDeepseq )

-- | VarId's are strictly positive
type VarId = Int

-- | Literals can be negative
type Literal = Int
type Clause = [Literal]
type Dimacs = [Clause]

type Model = S.Set Literal

readDimacsFile :: String -> IO Dimacs
readDimacsFile filename =
  timedDeepseq ("read " ++ filename) $
  do content <- readFile filename
     return $ 
       map drop0 $
       map (map (read :: String -> Int)) $
       dropc $
       map words $
       lines content
  where dropc (("c":_):xs) = dropc xs
        dropc (("p":_):xs) = dropc xs
        dropc (x:xs)       = x : dropc xs
        dropc []           = []
        drop0 = filter (/=0)



variableCounts :: Dimacs -> M.Map VarId Int
variableCounts cnf =
  M.unionsWith (+)
  [ M.fromList [ (abs lit, 1)
               | lit <- clause ]
  | clause <- cnf]

leastFrequentVariables :: Dimacs -> [VarId]
leastFrequentVariables =
  map fst
  . L.sortBy (comparing snd)
  . M.toList
  . variableCounts
