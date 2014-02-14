module RandomCNF (
  threeCNF , fourCNF
  ) where

import Control.Monad ( replicateM )
  
import Dimacs ( Dimacs, Literal )
import Random ( randomInt , randomInts )

threeCNF :: Int -> Int -> IO Dimacs
threeCNF numVars numClauses =
  replicateM numClauses (randomInts 3 1 numVars)
  >>= mapM (mapM negateHalf)
  where negateHalf x =
          do a <- randomInt 0 1
             return $ x * (2*a-1)

fourCNF :: Int -> Int -> IO Dimacs
fourCNF numVars numClauses =
  replicateM numClauses (randomInts 4 1 numVars)
  >>= mapM (mapM negateHalf)
  where negateHalf x =
          do a <- randomInt 0 1
             return $ x * (2*a-1)



-- 2-3-n-cnf num vars, max clause size,
--           num clauses per size class
-- fixed-cutset-size-cnf

  
