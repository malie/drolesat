module OBDD ( OBDD(..)
            , obddTest
            , mkObdd
            , mkObddWithOrder
            , mkObddWithOrderX
            , mostOverlapping
            , mostOccuringVarsFirstHeuristic
            , mostOccuringPlusNeighboursHeuristic
            , conjoin
            , disjoin
            , restrict
            , quantifyVariable
            , variableOrderComparer
            , buildObddByConjoiningClauses
            , obddEnumerateModels
            , isTrue
            , isFalse
            , obddVars
            , NodeId
            , nodeIdToName
            ) where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad ( foldM )
import Data.Maybe ( fromMaybe , catMaybes )
import Data.Ord ( comparing )
import Data.Char ( chr )
import Debug.Trace ( trace )

-- package 'prettyclass'
import Text.PrettyPrint.HughesPJClass
  ( text, fsep, sep, hang, parens, brackets
  , Pretty , pPrint , prettyShow , Doc , comma , (<>))
import PrettyClassExt ( flist , printPretty )

import Dimacs ( Dimacs , VarId , Clause , Model )
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

obddTest1 :: IndexedCNF -> IO ()
obddTest1 ic =
  mapM_ printPretty $
  -- build1 mkObdd $
  build1 (mkObddWithOrderX globalOrder) $
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

mkstateInitial :: Mkstate
mkstateInitial = (2, M.empty)

mkNode :: Mkstate -> VarId -> NodeId -> NodeId -> (Mkstate, NodeId)
mkNode st _ l r | l == r = (st, l)
mkNode st1@(nextId, allocated) v l r =
  let nd = (v, l, r) :: (VarId, NodeId, NodeId)
  in case M.lookup nd allocated of
    Just id -> (st1, id)
    Nothing ->
      let st2 = (succ nextId, M.insert nd nextId allocated)
      in (st2, nextId)

consObddWithNodes :: NodeId
                     -> [VarId]
                     -> M.Map (VarId,NodeId,NodeId) NodeId
                     -> OBDD
consObddWithNodes entry order rnodes =
   OBDD { entry = entry
          , order = order
          , nodes = M.fromList [ (id, node)
                               | (node,id) <- M.toList rnodes ]}



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
  -- trace ("using order " ++ show orderC) $
  mkObddWithOrder orderC cs
  where mentioned = S.fromList $ map abs $ concat cs
        orderC = filter (flip S.member mentioned) order

mkObddWithOrder :: [VarId] -> [Clause] -> OBDD
mkObddWithOrder order cs =
  let ((_,rnodes), entry) =
        mk mkstateInitial (Just $ fromDimacs cs) order
  in consObddWithNodes entry order rnodes
  where mk :: Mkstate -> Maybe IndexedCNF -> [VarId]
              -> (Mkstate, NodeId)
        mk st Nothing _                  = (st,0)
        mk st (Just ic) _ | noClauses ic = (st,1)
        mk st1 (Just ic) (v:vs) =
          let desc st sign = mk st (assign v sign ic) vs
              (st2,l) = desc st1 Positive
              (st3,r) = desc st2 Negative
          in mkNode st3 v l r

obddTest :: IndexedCNF -> IO ()
obddTest ic =
  printPretty $ buildObddByConjoiningClauses $ toDimacs ic

buildObddByConjoiningClauses :: Dimacs -> OBDD
buildObddByConjoiningClauses cnf = recur initial
  where mkDis clause = Dis (S.fromList $ map abs clause) clause
        obddForSingleClause cl = mkObddWithOrderX globalOrder [cl]
        globalOrder = -- mostOccuringVarsFirstHeuristic $
                      mostOccuringPlusNeighboursHeuristic
                      cnf
        initial = map (mkElement . obddForSingleClause) cnf
        mkElement o@(OBDD _ _ ord) = Obdd (S.fromList ord) o
        varcmp = variableOrderComparer globalOrder
        join es = [ case ov of
                       Single a -> a
                       Pair a b ->
                         Obdd (S.union (evars a) (evars b)) $
                         conjoin varcmp globalOrder
                         (eobdd a)
                         (eobdd b)
                  | ov <- mostOverlapping evars es ]
        recur [e] = eobdd e
        recur es = recur $ join es


type VariableOrderComparer = VarId -> VarId -> Ordering

variableOrderComparer :: [VarId] -> VariableOrderComparer
variableOrderComparer variables = cmp
  where vs = M.fromList $ zip variables [1..]
        cmp a b = compare (vs M.! a) (vs M.! b)


{-
conjoin :: OBDD -> OBDD -> IO OBDD
conjoin a b =
  do ord <- commonVariableOrder a b
     conjoinWithOrder ord a b

commonVariableOrder a b
  | order a == order b = return $ order a
  | otherwise = error "commonVariableOrder..."
-}

conjoin' :: VariableOrderComparer -> [VarId] -> OBDD -> OBDD -> OBDD
conjoin' cmp order a b =
  let res = conjoin cmp order a b
  in
   trace ("conjoin\n " ++ prettyShow a ++ "\n " ++ prettyShow b
          ++ "\n=>\n " ++ prettyShow res)
   res

conjoin cmp order a b =
  let ((_,rnodes), xentry) = join mkstateInitial (entry a) (entry b)
  in consObddWithNodes xentry order rnodes
  where vnn x nid = nodes x M.! nid
        recur st1 an bn =
          let (av,al,ar) = vnn a an
              (bv,bl,br) = vnn b bn
          in case cmp av bv of
            EQ -> let (st2, xan) = join st1 al bl
                      (st3, xbn) = join st2 ar br
                  in mkNode st3 av xan xbn
            LT -> let (st2, xan) = join st1 al bn
                      (st3, xbn) = join st2 ar bn
                  in mkNode st3 av xan xbn
            GT -> let (st2, xan) = join st1 an bl
                      (st3, xbn) = join st2 an br
                  in mkNode st3 bv xan xbn
        join st 0 _ = (st, 0)
        join st _ 0 = (st, 0)
        join st 1 1 = (st, 1)
        join st1 1 n = copy st1 b n
        join st1 n 1 = copy st1 a n
        join st1 an bn = recur st1 an bn
        copy st1 _ 0 = (st1, 0)
        copy st1 _ 1 = (st1, 1)
        copy st1 o n = -- todo: only copy once???
          let (v,l,r) = vnn o n
              (st2,ll) = copy st1 o l
              (st3,rr) = copy st2 o r
          in mkNode st3 v ll rr

{-
disjoin :: VariableOrderComparer -> [VarId] -> OBDD -> OBDD -> OBDD
disjoin cmp order a b =
  let res = disjoin1 cmp order a b
  in
   trace ("disjoin\n " ++ prettyShow a ++ "\n " ++ prettyShow b
          ++ "\n=>\n " ++ prettyShow res)
   res -}

disjoin cmp order a b =
  let ((_,rnodes), xentry) = join mkstateInitial (entry a) (entry b)
  in consObddWithNodes xentry order rnodes
  where vnn x nid = nodes x M.! nid
        recur st1 an bn =
          let (av,al,ar) = vnn a an
              (bv,bl,br) = vnn b bn
          in case cmp av bv of
            EQ -> let (st2, xan) = join st1 al bl
                      (st3, xbn) = join st2 ar br
                  in mkNode st3 av xan xbn
            LT -> let (st2, xan) = join st1 al bn
                      (st3, xbn) = join st2 ar bn
                  in mkNode st3 av xan xbn
            GT -> let (st2, xan) = join st1 an bl
                      (st3, xbn) = join st2 an br
                  in mkNode st3 bv xan xbn
        join st 1 _ = (st, 1)
        join st _ 1 = (st, 1)
        join st 0 0 = (st, 0)
        join st1 0 n = copy st1 b n
        join st1 n 0 = copy st1 a n
        join st1 an bn = recur st1 an bn
        copy st1 _ 0 = (st1, 0)
        copy st1 _ 1 = (st1, 1)
        copy st1 o n = -- todo: only copy once???
          let (v,l,r) = vnn o n
              (st2,ll) = copy st1 o l
              (st3,rr) = copy st2 o r
          in mkNode st3 v ll rr


-- how to most easily clean up unreferenced nodes after restrict?
restrict :: VarId -> Bool -> OBDD -> OBDD
restrict var value o =
  minimize
  OBDD { order = L.delete var $ order o
       , entry = replaceMaybe $ entry o
       , nodes = M.fromList
                 [ (n, (v, replaceMaybe l, replaceMaybe r))
                 | (n, (v,l,r)) <- M.toList $ nodes o
                 , v /= var ]}
  where repl = M.fromList $ catMaybes
               [ case (v == var, value) of
                    (True, True) -> Just (n, l)
                    (True, False) -> Just (n, r)
                    (False, _) -> Nothing
               | (n, (v,l,r)) <- M.toList $ nodes o ]
        replaceMaybe n = fromMaybe n $ M.lookup n repl

minimize o =
  let (ns, e) = recur (entry o) (M.toList $ nodes o)
      used = keepUsed (M.fromList ns) e $ S.empty
      ns2 = [ (n, (v,l,r))
            | (n, (v,l,r)) <- ns
            , S.member n used ]
  in
   o { nodes = M.fromList ns2
     , entry = e }
  where recur e ns =
          let mods =
                M.fromList
                [ (n, l)
                | (n, (v,l,r)) <- ns
                , l == r ]
              replaceMaybe n = fromMaybe n $ M.lookup n mods
          in if M.null mods
             then (ns, e)
             else recur (replaceMaybe e)
                    [ (n, (v, replaceMaybe l, replaceMaybe r))
                    | (n, (v,l,r)) <- ns
                    , not $ M.member n mods ]
        keepUsed ns n used
          | n == 0 || n == 1 = used
          | S.member n used = used
          | otherwise = let (v,l,r) = ns M.! n
                            u1 = S.insert n used
                            u2 = keepUsed ns l u1
                            u3 = keepUsed ns r u2
                        in u3

        



quantifyVariable :: VariableOrderComparer -> [VarId]
                    -> VarId -> OBDD -> OBDD
quantifyVariable cmp order var o =
  disjoin cmp order
          (restrict var False o)
          (restrict var True o)


-- composition

isTrue, isFalse :: OBDD -> Bool
isTrue o  = entry o == 1
isFalse o = entry o == 0

obddVars :: OBDD -> S.Set VarId
obddVars o =
  fst $ recur (entry o) S.empty S.empty
  where
    recur n usedVars seenNodes
          | n == 0 || n == 1 || S.member n seenNodes =
            (usedVars, seenNodes)
          | otherwise =
            let (v,l,r) = nodes o M.! n
                (u1,s1) = (S.insert v usedVars, S.insert n seenNodes)
                (u2,s2) = recur l u1 s1
            in recur r u2 s2

obddEnumerateModels :: OBDD -> [Model]
obddEnumerateModels o = recur S.empty (entry o)
  where
    recur _ 0 = []
    recur model 1 = [model]
    recur model node =
      let (v,l,r) = nodes o M.! node
      in recur (S.insert v model) l
         ++ recur (S.insert (negate v) model) r
  

nodeIdToName 0 = "0"
nodeIdToName 1 = "1"
nodeIdToName x = recur $ x - 2
  where
    recur x
      | x > 25    = recur (x `div` 26)
                    ++ [letter $ x `mod` 26]
      | otherwise = [letter x]
    letter x = chr $ 97 + x
        

instance Pretty OBDD where
  pPrint (OBDD nodes entry order) =
    let desc [] = []
        desc (v:vs) =
          (fsep
           [ text $
             nodeIdToName node ++ "="
             ++ show var ++ "->"
             ++ nodeIdToName l ++ ";"
             ++ nodeIdToName r
           | (node, (var, l, r)) <- M.toList nodes
           , var == v ])
          : desc vs
    in parens $ case entry of
                  0 -> text "0"
                  1 -> text "1"
                  _ -> sep $ desc order


instance Pretty Element where
  pPrint (Dis _ cl) = parens $ fsep $ map (text . show) cl
  pPrint (Clauses _ cls) = flist (flist $ text . show) cls
  pPrint (Obdd _ d) = pPrint d

instance Show Element where
  show = prettyShow
