module Random 
       ( randomizeList
       , randomListElement
       , randomListElements
       , randomListIndex
       , randomInt
       , randomInts
       , randomFloat
       , randomDouble
       , randomBoolean
       , randomChoice
       , randomVectorElements
       , randomPartition
       ) 
where

import Control.Monad ( liftM )
import Data.Ord ( comparing )
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified System.Random as R


randomizeList :: [a] -> IO [a]
randomizeList xs = 
  do ys <- mapM prefixRandomInt xs
     return $ map snd $ L.sortBy (comparing fst) ys
  where prefixRandomInt x = do v <- R.getStdRandom (R.randomR (1::Int,1000))
                               return (v, x)

randomListElement :: [a] -> IO a
randomListElement [] = error "empty list"
randomListElement list = 
  do i <- R.getStdRandom (R.randomR (0, length list-1))
     return $ list !! i


{-
bah
randomListElements :: Eq a => Int -> [a] -> IO [a]
randomListElements n list = r n list []
  where r 0 list res = return res
        r n list res = do el <- randomListElement list
                          r (pred n) (L.delete el list) (el:res)
-}

randomListElements :: Int -> [a] -> IO [a]
randomListElements n l = liftM (take n) $ randomizeList l

randomListIndex :: [a] -> IO Int
randomListIndex list = R.getStdRandom $ R.randomR (0, length list - 1)

randomInt :: Int -> Int -> IO Int
randomInt lower upper = R.getStdRandom $ R.randomR (lower, upper)


randomInts :: Int -> Int -> Int -> IO [Int]
randomInts num lower upper = r S.empty num []
  where r _ 0 res = return res
        r seen num res =
          do x <- randomInt lower upper
             if S.member x seen
               then r seen num res
               else r (S.insert x seen) (pred num) (x:res)


randomFloat :: Float -> Float -> IO Float
randomFloat from to = R.getStdRandom $ R.randomR (from, to)

randomDouble :: Double -> Double -> IO Double
randomDouble from to = R.getStdRandom $ R.randomR (from, to)

randomBoolean = R.getStdRandom (R.randomR (False, True))

randomChoice a b = do r <- randomBoolean
                      if r then a else b


randomVectorElements :: Int -> V.Vector a -> IO [a]
randomVectorElements n v =
  do xs <- randomInts n 0 (pred $ V.length v)
     return $ [ v V.! i | i <- xs ]

-- choose every list element with probability p
randomPartition :: Double -> [a] -> IO ([a], [a])
randomPartition p xs = recur xs [] []
  where recur [] ls rs     = return (reverse ls, reverse rs)
        recur (x:xs) ls rs =
          do r <- randomDouble 0 1
             if r < p
               then recur xs (x:ls) rs
               else recur xs ls (x:rs)


{-
import Timing ( time )
main =
  do tl <- time "1e4" $ randomizeList [1..10000]
     print $ take 20 tl
     tl <- time "1e4" $ randomizeList [1..10000]
     tl <- time "2e4" $ randomizeList [1..20000]
     tl <- time "4e4" $ randomizeList [1..40000]
     tl <- time "8e4" $ randomizeList [1..80000]
     tl <- time "16e4" $ randomizeList [1..160000]
     tl <- time "32e4" $ randomizeList [1..320000]
-}
