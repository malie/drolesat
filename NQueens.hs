-- parallel haskell evening stylefruits 2014-02-18
module NQueens (
  nqueensDimacs
  ) where

import Dimacs ( Dimacs )


vnot = negate

atMostOne [] = []
atMostOne [x] = []
atMostOne (x:xs) = [ [vnot x, vnot y] | y <- xs ] 
                   ++ atMostOne xs


onlyone [] = []
onlyone [_]  = []
onlyone vars = vars : atMostOne vars

nqueensDimacs :: Int -> Dimacs
nqueensDimacs n =
  concat $
  -- columns
  [ onlyone [ var (x,y) | y <- [1..n]] | x <- [1..n]]
  -- rows
  ++ [ onlyone [ var (x,y) | x <- [1..n]] | y <- [1..n]]
  -- don't forget diagonals, up
  ++ [ atMostOne [ var (x,y) | x <- [1..n]
                             , let y = d + x
                             , y >= 1 , y <= n ]
     | d <- [(-n+1)..(n-1)]]
  -- don't forget diagonals II, down
  ++ [ atMostOne [ var (x,n-y+1) | x <- [1..n]
                                 , let y = d + x
                                 , y >= 1 , y <= n ]
     | d <- [(-n+1)..(n-1)]]
  where var (x,y) =  100*x + y
