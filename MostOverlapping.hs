module MostOverlapping ( mostOverlapping , SP(..) ) where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Ord ( comparing )

data SP a = Single a | Pair a a
          deriving (Show)

-- | Find pairs of most overlapping sets in a given list of sets.
-- A list of objects 'a' plus a function to extract a set 'b'
-- from an object have to be provided.
-- Pairs are held by a 'Pair', the rest is enclosed in 'Single'.
mostOverlapping :: (Ord b)
                   => (a -> S.Set b) -> [a] -> [SP a]
mostOverlapping ex xs =
  [ Pair a b | ((_, a), (_, b)) <- pairs ]
  ++ [ Single a | (aid, a) <- ys, S.notMember aid used ]
  where ys = zip [1..] xs
        pairs = map snd $ recur ex 1 ys (const True)
        used = S.fromList $ concat
               [ [aid,bid] | ((aid, _), (bid, _)) <- pairs]

type Level = Int
type SetId = Int

-- | At each level the sets are 'partitioned' by every occuring
-- element. This is repeated recursively. At the deepest level
-- occuring pairs are tagged with their level. On the way back up,
-- the pairs occuring deepest, the one with the largest overlap,
-- are kept, and it is taken care to not use any set more often than
-- once.
recur :: (Ord b) =>
     (a -> S.Set b)
     -> Int
     -> [(SetId, a)]
     -> (b -> Bool)
     -> [(Level, ((SetId, a), (SetId, a)))]
recur ex level ys elpred =
  let partitions =
        M.unionsWith (++)
        [ M.fromList
          [ (element, [(id,x)])
          | element <- S.toList $ ex x
          , elpred element ]
        | (id,x) <- ys ]
  in keepLargestPairs $ concat $ map (pairs level) $
     M.toList partitions
  where
    pairs _ (_, [_]) = []
    pairs _ (_, [(aid, a),(bid, b)]) =
      [( S.size (S.intersection (ex a) (ex b))
       , ((aid, a), (bid, b)))]
    pairs level (el, partition) =
      let pairsWithLevel = recur ex (succ level) partition (>el)
          usedIds =
            S.fromList $ concat
            [ [aid,bid]
            | (_, ((aid, _), (bid, _))) <- pairsWithLevel]
          morePairs =
            [ (level, (a,b))
            | (a,b) <- pairSequence
                       [ x
                       | x@(id,_) <- partition
                       , S.notMember id usedIds]]
      in morePairs ++ pairsWithLevel

keepLargestPairs :: [(Level, ((SetId, a), (SetId, b)))]
                    -> [(Level, ((SetId, a), (SetId, b)))]
keepLargestPairs ps =
  keep S.empty (reverse $ L.sortBy (comparing fst) ps) []
  where
    keep _ [] res = res
    keep seen (p@(_, ((aid, _), (bid, _))):ps) res =
      if S.member aid seen || S.member bid seen
      then keep seen ps res
      else keep (S.union seen (S.fromList [aid, bid]))
             ps
             (p:res)
          
pairSequence :: [a] -> [(a,a)]
pairSequence (a:b:xs) = (a,b) : pairSequence xs
pairSequence _ = []
