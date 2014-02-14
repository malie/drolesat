module Dimacs
       ( VarId, Literal, Clause, Dimacs
       , readDimacsFile )
       where

import Timed ( timedDeepseq )

-- | VarId's are strictly positive
type VarId = Int

-- | Literals can be negative
type Literal = Int
type Clause = [Literal]
type Dimacs = [Clause]

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
