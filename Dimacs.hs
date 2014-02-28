module Dimacs
       ( VarId, Literal, Clause, Dimacs
       , readDimacsFile
       , readDimacsFileAndNames
       , VarName
       , Model
       , variableCounts
       , leastFrequentVariables
       )
       where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad ( liftM )
import Data.Maybe ( mapMaybe )
import Data.Ord ( comparing )

import Timed ( timedDeepseq )

-- | VarId's are strictly positive
type VarId = Int
type VarName = String

-- | Literals can be negative
type Literal = Int
type Clause = [Literal]
type Dimacs = [Clause]

type Model = S.Set Literal

readDimacsFile :: String -> IO Dimacs
readDimacsFile = liftM fst . readDimacsFileAndNames

readDimacsFileAndNames :: String -> IO (Dimacs, M.Map VarId VarName)
readDimacsFileAndNames filename =
  timedDeepseq ("read " ++ filename) $
  do content <- readFile filename
     let theWords = map words $ lines content
     return
       ( map drop0 $
         map (map (read :: String -> Int)) $
         dropc $ theWords
       , M.fromList $ mapMaybe rname theWords)
  where dropc (("c":_):xs) = dropc xs
        dropc (("p":_):xs) = dropc xs
        dropc (x:xs)       = x : dropc xs
        dropc []           = []
        drop0 = filter (/=0)
        rname ("c" : "name" : var : name : _) = Just (read var, name)
        rname _ = Nothing
            




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
