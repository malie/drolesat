module Testing (main)
       where

import qualified Data.List as L
import qualified Data.Set as S
import Control.Monad ( foldM )
import Data.Maybe ( catMaybes )

import Dimacs ( Dimacs , Clause , Literal )
import RandomCNF ( nCNF , threeCNF , fourCNF )

import NQueens

import OBDD ( mkObdd , printPretty )

type Model = S.Set Literal

main = undefined

-- Enumerate models directly from a cnf. No attempt is made
-- at finding a good clause order.
enumerateModels :: Dimacs -> [Model]
enumerateModels = L.foldl' recur [S.empty]
  where
    recur xs clause =
      catMaybes [ fuse x m | m <- clauseModels clause, x <- xs ]

clauseModels :: Clause -> [Model]
clauseModels clause =
  L.delete (S.fromList $ map negate clause)
     (L.foldl' ca [S.empty] clause)
  where ca xs c =
          let l = abs c
          in concat [ [S.insert l x, S.insert (negate l) x]
                    | x <- xs ]

fuse :: Model -> Model -> Maybe Model
fuse a b | S.size b > S.size a = fuse b a
fuse a b = foldM recur a (S.toList b)
  where recur a c
          | S.member (negate c) a = Nothing
          | otherwise             = Just $ S.insert c a
