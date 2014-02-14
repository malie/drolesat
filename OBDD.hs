module OBDD ( obddTest
            , mkObdd
            , mostOverlapping
            ) where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad ( foldM )
import Data.Ord ( comparing )

-- package 'prettyclass'
import Text.PrettyPrint.HughesPJClass
  (text, fsep, sep, hang, parens, Pretty , pPrint , prettyShow )


import Dimacs ( Dimacs , VarId , Clause )
import IndexedCNF ( IndexedCNF , toDimacs , numClauses
                  , clausesWithVar , clauseLiterals
                  , assign , Sign(..) , fromDimacs )
import MostOverlapping ( mostOverlapping , SP(Single, Pair))


data Element = Dis { evars :: S.Set VarId,
                     eclause :: Clause }
             | Obdd { evars :: S.Set VarId,
                      eobdd :: OBDD }

-- obdd's should keep the variable order
-- obdd's should be reduced
data OBDD = IF VarId OBDD OBDD
          | T
          | F

obddTest :: IndexedCNF -> IO ()
obddTest ic = mapM_ printPretty $ build $ map mkDis $ toDimacs ic
  where mkDis clause = Dis (S.fromList $ map abs clause) clause

build :: [Element] -> [Element]
build es =
  concat
  [ case ov of
       Single a -> [a]
       Pair a b -> combine a b
  | ov <- mostOverlapping evars es ]
  where combine (Dis as a) (Dis bs b) =
          [ Dis as a, Dis bs b
          , Obdd (S.union as bs) (mkObdd [a, b])]



mkIf _ T T = T
mkIf _ F F = F
mkIf v l r = IF v l r

mkObdd :: [Clause] -> OBDD
mkObdd cs = mk (fromDimacs cs) order
  where order =
          reverse $ map fst $ L.sortBy (comparing snd) $
          M.toList $ M.unionsWith (+)
          [ M.singleton (abs lit) 1 | cl <- cs , lit <- cl ]
        mk _ [] = T
        mk ic (v:vs) =
          let l = recur vs $ assign v Positive ic
              r = recur vs $ assign v Negative ic
          in mkIf v l r
        recur _ Nothing = F
        recur vs (Just ic) = mk ic vs


printPretty = putStrLn . prettyShow

instance Pretty OBDD where
  pPrint (IF v l r) = parens $ hang (text $ "if " ++ show v) 2 $
                      sep [pPrint l, pPrint r]
  pPrint T = text "t"
  pPrint F = text "f"


instance Pretty Element where
  pPrint (Dis _ cl) = parens $ fsep $ map (text . show) cl
  pPrint (Obdd _ d) = pPrint d
