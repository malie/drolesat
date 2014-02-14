module Main (main) where

import qualified Data.Set as S

import RandomCNF ( threeCNF , fourCNF )
import MostOverlapping ( mostOverlapping )



main =
  do cnf <- fourCNF 7 13
     mapM_ print cnf
     mapM_ print $
       mostOverlapping (S.fromList . map abs) cnf

         
