module Main (main) where

import qualified Data.List as L
import qualified Data.Set as S
import Control.Monad ( liftM )
import Data.Ord ( comparing )

import RandomCNF ( threeCNF , fourCNF )
import MostOverlapping ( mostOverlapping )



main =
  do cnf <- liftM (map $ L.sortBy (comparing abs)) $
            fourCNF 7 13
     mapM_ print cnf
     mapM_ print $
       mostOverlapping (S.fromList . map abs) cnf

         
