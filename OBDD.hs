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


data Element = Dis { evars :: S.Set VarId
                   , eclause :: Clause }
             | Clauses { evars :: S.Set VarId
                       , eclauses :: [Clause] }
             | Obdd { evars :: S.Set VarId
                    , eobdd :: OBDD }

-- obdd's should keep the variable order
data OBDD =
  OBDD { nodes :: M.Map NodeId (VarId, NodeId, NodeId)
       , entry :: NodeId
       , order :: [VarId]}


type NodeId = Int
-- 0 reserved for false
-- 1 reserved for true

obddTest :: IndexedCNF -> IO ()
obddTest ic = mapM_ printPretty $ build $
              map mkClauses $ toDimacs ic
  where mkDis clause = Dis (S.fromList $ map abs clause) clause
        mkClauses clause =
          Clauses (S.fromList $ map abs clause) [clause]

build :: [Element] -> [Element]
build es = recur 4 es
  where combine1 (Clauses as a) (Clauses bs b) =
          Clauses (S.union as bs) (a ++ b)
        recur 0 es =
          concat
          [ [c, Obdd as (mkObdd a)]
          | c@(Clauses as a) <- es]
        recur n es = recur (pred n) $
                     concat
                     [ case ov of
                          Single a -> [a]
                          Pair a b -> [combine1 a b]
                     | ov <- mostOverlapping evars es ]


type Mkstate = (Int, M.Map (VarId,NodeId,NodeId) NodeId)

mkObdd :: [Clause] -> OBDD
mkObdd cs = mkObddWithOrder cs order
  where
    order =
      reverse $ map fst $ L.sortBy (comparing snd) $
      M.toList $ M.unionsWith (+)
      [ M.singleton (abs lit) 1 | cl <- cs , lit <- cl ]
        

mkObddWithOrder :: [Clause] -> [VarId] -> OBDD
mkObddWithOrder cs order =
  let ((_,rnodes), entry) =
        mk (2, M.empty) (Just $ fromDimacs cs) order
  in OBDD { entry = entry
          , order = order
          , nodes = M.fromList [ (id, node)
                               | (node,id) <- M.toList rnodes ]}
  where mk :: Mkstate -> Maybe IndexedCNF -> [VarId]
              -> (Mkstate, NodeId)
        mk st Nothing _                  = (st,0)
        mk st (Just ic) _ | noClauses ic = (st,1)
        mk st1 (Just ic) (v:vs) =
          let desc st sign = mk st (assign v sign ic) vs
              (st2,l) = desc st1 Positive
              (st3,r) = desc st2 Negative
          in mkNode st3 v l r
        mkNode st _ l r | l == r = (st, l)
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
  pPrint (OBDD nodes entry order) =
    let nd 0 = "0"
        nd 1 = "1"
        nd x = [chr $ 97 + x - 2]
        desc [] = []
        desc (v:vs) =
          (fsep
           [ text $
             nd node ++ "="
             ++ show var ++ "->" ++ nd l ++ ";" ++ nd r
           | (node, (var, l, r)) <- M.toList nodes
           , var == v ])
          : desc vs
    in parens $ sep $ desc order


instance Pretty Element where
  pPrint (Dis _ cl) = parens $ fsep $ map (text . show) cl
  pPrint (Clauses _ cls) =
    parens (sep $ map (parens . fsep . map (text . show)) cls)
  pPrint (Obdd _ d) = pPrint d
