module RandomCNF (
  nCNF, threeCNF , fourCNF
  ) where

import Control.Monad ( replicateM )
  
import Dimacs ( Dimacs, Literal )
import Random ( randomInt , randomInts )

threeCNF :: Int -> Int -> IO Dimacs
threeCNF = nCNF 3

fourCNF :: Int -> Int -> IO Dimacs
fourCNF = nCNF 4

nCNF :: Int -> Int -> Int -> IO Dimacs
nCNF clauseSize numVars numClauses =
  replicateM numClauses (randomInts clauseSize 1 numVars)
  >>= mapM (mapM negateHalf)
  where negateHalf x =
          do a <- randomInt 0 1
             return $ x * (2*a-1)


-- randomly generate CNFs with a fixed cutset size?

  
