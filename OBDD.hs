module OBDD ( obddTest
            , mkObdd
            , mkObddWithOrder
            , mkObddWithOrderX
            , mostOverlapping
            , mostOccuringVarsFirstHeuristic
            , mostOccuringPlusNeighboursHeuristic
            , printPretty
            ) where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad ( foldM )
import Data.Ord ( comparing )
import Data.Char ( chr )
import Debug.Trace ( trace )

-- package 'prettyclass'
import Text.PrettyPrint.HughesPJClass
  ( text, fsep, sep, hang, parens, brackets
  , Pretty , pPrint , prettyShow , Doc , comma , (<>))


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
obddTest ic =
  mapM_ printPretty $
  build1 mkObdd $
  -- build1 (mkObddWithOrderX globalOrder) $
  map mkClauses $ toDimacs ic
  where mkDis clause = Dis (S.fromList $ map abs clause) clause
        mkClauses clause =
          Clauses (S.fromList $ map abs clause) [clause]
        globalOrder = -- mostOccuringVarsFirstHeuristic $
                      mostOccuringPlusNeighboursHeuristic $
                      toDimacs ic

-- use mostOverlapping to first combine those clauses that are
-- most overlapping in the sense of using the same variables.
build1 :: ([Clause] -> OBDD) -> [Element] -> [Element]
build1 mkobdd es = recur 3 es
  where combine1 (Clauses as a) (Clauses bs b) =
          Clauses (S.union as bs) (a ++ b)
        recur 0 es =
          concat
          [ [c, Obdd as (mkobdd a)]
          | c@(Clauses as a) <- es]
        recur n es = recur (pred n) $
                     concat
                     [ case ov of
                          Single a -> [a]
                          Pair a b -> [combine1 a b]
                     | ov <- mov ]
                     where mov = let x = mostOverlapping evarsL es
                                 in -- trace (prettyShow x)
                                    x
                           evarsL =
                             S.fromList . take 8 . S.toList . evars


-- try to build clusters starting with mid-frequent variables...
build2 :: ([Clause] -> OBDD) -> [Element] -> [Element]
build2 mkobdd es = concat $ map make $ part seeds es []
  where
    n = 200
    seeds = take n $ midOutwards $ byVarFrequence es
    len = max 1 $ length es `div` n
    part _ [] res = res
    part [] ys res = res ++ map (\x -> [x]) ys
    part (x:xs) es res =
      let (ys0, zs0) =
            L.partition (\e-> S.member x (evars e)
                              && S.size (evars e) < 7)
            es
          ys = take len ys0
          zs = drop len ys0 ++ zs0
      in part xs zs (ys:res)
    make [Clauses vs [e]] = [Clauses vs [e]]
    make es =
      let clauses = concat [ case el of
                                Clauses _ cs -> cs
                           | el <- es ]
      in [ Clauses S.empty clauses
         , Obdd
           (S.fromList $ map abs $ concat clauses)
           (mkobdd clauses)]
      

byVarFrequence es =
  map fst $ L.sortBy (comparing snd) $ M.toList $
  M.unionsWith (+)
  [ M.fromList $
    case e of
      Clauses _ cs -> [ (abs l, 1)
                      | c <- cs , l <- c]
      x -> [ (v, 1)
           | v <- S.toList $ evars x ]
  | e <- es ]

midOutwards xs = merge (reverse $ take len xs) (drop len xs)
  where len = length xs `div` 2
        merge [] bs = bs
        merge as [] = as
        merge (a:as) (b:bs) = a : b : merge as bs


type Mkstate = (Int, M.Map (VarId,NodeId,NodeId) NodeId)

mkObdd :: [Clause] -> OBDD
mkObdd cs = mkObddWithOrder
            -- (mostOccuringVarsFirstHeuristic cs)
            (mostOccuringPlusNeighboursHeuristic cs)
            cs
            
mostOccuringVarsFirstHeuristic :: [Clause] -> [VarId]
mostOccuringVarsFirstHeuristic clauses =
  reverse $ map fst $ L.sortBy (comparing snd) $
  M.toList $ M.unionsWith (+)
  [ M.singleton (abs lit) 1 | cl <- clauses , lit <- cl ]

mostOccuringPlusNeighboursHeuristic :: [Clause] -> [VarId]
mostOccuringPlusNeighboursHeuristic clauses =
  recur S.empty [] 1
  where m cs seen =
          reverse $ map fst $ L.sortBy (comparing snd) $
          M.toList $ M.unionsWith (+)
          [ M.singleton var score
          | cl <- clauses
          , let vars = map abs cl
                unseenVars = filter (flip S.notMember seen) vars
                numSeenVars = length vars - length unseenVars
                score = 1.0 / fromIntegral (length unseenVars)
                        + 1 * fromIntegral numSeenVars
          , var <- unseenVars ]
        all = S.unions $ [ S.fromList $ map abs c | c <- clauses ]
        recur seen res _ | all == seen = res
        recur seen res n =
          let mo = take (ceiling $ fromIntegral n / 4) $ m clauses seen
          in recur (S.union seen $ S.fromList mo) (res ++ mo) (succ n)


-- build a obdd with a given var order,
-- but first drop the unneeded variables
mkObddWithOrderX :: [VarId] -> [Clause] -> OBDD
mkObddWithOrderX order cs =
  trace ("using order " ++ show orderC) $
  mkObddWithOrder orderC cs
  where mentioned = S.fromList $ map abs $ concat cs
        orderC = filter (flip S.member mentioned) order

mkObddWithOrder :: [VarId] -> [Clause] -> OBDD
mkObddWithOrder order cs =
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
        nd x = recur $ x - 2
        recur x
          | x > 25    = recur (x `div` 26)
                        ++ [letter $ x `mod` 26]
          | otherwise = [letter x]
        letter x = chr $ 97 + x
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
    flist (flist $ text . show) cls
  pPrint (Obdd _ d) = pPrint d

instance Show Element where
  show = prettyShow

flist :: (a -> Doc) -> [a] -> Doc
flist f xs = brackets $ fsep $ recur xs
  where recur []     = []
        recur [x]    = [f x]
        recur (x:xs) = f x <> comma : recur xs

