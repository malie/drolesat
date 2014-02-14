module Timed ( timed , timedDeepseq ) where

import System.CPUTime ( getCPUTime )
import Control.DeepSeq

timed :: String -> IO a -> IO a
timed msg action =
  do start <- getCPUTime
     res <- action
     res `seq` return res
     end <- getCPUTime
     let ms = fromIntegral (end-start)/10^9::Double
     putStrLn $ msg ++ " took " ++ show ms ++ " ms"
     return res

timedDeepseq :: (NFData a) => String -> IO a -> IO a
timedDeepseq msg action =
  do start <- getCPUTime
     res <- action
     res `deepseq` return res
     end <- getCPUTime
     let ms = fromIntegral (end-start)/10^9::Double
     putStrLn $ msg ++ " took " ++ show ms ++ " ms"
     return res

