module OBDD ( obddTest
            , mkObdd
            , mostOverlapping
            , printPretty
            ) where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad ( foldM )
import Data.Ord ( comparing )
import Data.Char ( chr )

-- package 'prettyclass'
import Text.PrettyPrint.HughesPJClass
  (text, fsep, sep, hang, parens, Pretty , pPrint , prettyShow )


import Dimacs ( Dimacs , VarId , Clause )
import IndexedCNF ( IndexedCNF , toDimacs , numClauses
                  , clausesWithVar , clauseLiterals
                  , assign , Sign(..) , fromDimacs , noClauses )
import MostOverlapping ( mostOverlapping , SP(Single, Pair))


data Element = Dis { evars :: S.Set VarId,
                     eclause :: Clause }
             | Obdd { evars :: S.Set VarId,
                      eobdd :: OBDD }

-- obdd's should keep the variable order
data OBDD =
  OBDD { nodes :: M.Map NodeId (VarId, NodeId, NodeId)
       , entry :: NodeId }


type NodeId = Int
-- 0 reserved for false
-- 1 reserved for true

obddTest :: IndexedCNF -> IO ()
obddTest ic = mapM_ printPretty $ build $ map mkDis $ toDimacs ic
  where mkDis clause = Dis (S.fromList $ map abs clause) clause

build :: [Element] -> [Element]
build es = undefined
{-
  concat
  [ case ov of
       Single a -> [a]
       Pair a b -> combine a b
  | ov <- mostOverlapping evars es ]
  where combine (Dis as a) (Dis bs b) =
          [ Dis as a, Dis bs b
          , Obdd (S.union as bs) (mkObdd [a, b])]
-}

type Mkstate = (Int, M.Map (VarId,NodeId,NodeId) NodeId)

-- mkObdd :: [Clause] -> OBDD
mkObdd cs =
  let ((_,rnodes), entry) =
        mk (2, M.empty) (Just $ fromDimacs cs) order
  in OBDD { entry = entry
          , nodes = M.fromList [ (id, node)
                               | (node,id) <- M.toList rnodes ]}
  where order =
          reverse $ map fst $ L.sortBy (comparing snd) $
          M.toList $ M.unionsWith (+)
          [ M.singleton (abs lit) 1 | cl <- cs , lit <- cl ]
        mk :: Mkstate -> Maybe IndexedCNF -> [VarId]
              -> (Mkstate, NodeId)
        mk st Nothing _                  = (st,0)
        mk st (Just ic) _ | noClauses ic = (st,1)
        mk st1 (Just ic) (v:vs) =
          let desc st sign = mk st (assign v sign ic) vs
              (st2,l) = desc st1 Positive
              (st3,r) = desc st2 Negative
          in mkNode st3 v l r
        mkNode :: Mkstate -> VarId -> NodeId -> NodeId -> (Mkstate, Int)
        mkNode st1@(nextId, allocated) v l r =
          let nd = (v, l, r) :: (VarId, NodeId, NodeId)
          in case M.lookup nd allocated of
            Just id -> (st1, id)
            Nothing ->
              let st2 = (succ nextId, M.insert nd nextId allocated)
              in (st2, nextId)
          


printPretty :: Pretty a => a -> IO ()
printPretty = putStrLn . prettyShow

instance Pretty OBDD where
  pPrint (OBDD nodes entry) =
    let nd 0 = "0"
        nd 1 = "1"
        nd x = [chr $ 97 + x - 2]
        desc [] = []
        desc ns =
          (fsep
           [ text $
             nd node ++ "="
             ++ show var ++ "->" ++ nd l ++ ";" ++ nd r
           | node <- ns , node >= 2
           , let (var, l, r) = nodes M.! node ])
          : (desc $ S.toList $ S.fromList $ concat
             [ [l,r]
             | node <- ns , node >= 2
             , let (_, l, r) = nodes M.! node ])
    in parens $ sep $ desc [entry]


instance Pretty Element where
  pPrint (Dis _ cl) = parens $ fsep $ map (text . show) cl
  pPrint (Obdd _ d) = pPrint d
