{-# LANGUAGE BangPatterns #-}
module IndexedCNF
       ( IndexedCNF
       , ClauseId
       , ClauseSize
       , Sign (Positive, Negative)
       , empty
       , addClause
       , fromDimacs
       , toDimacs
       , clausesWithVar
       , clauseBodiesWithVar
       , allVariables
       , clauseLiterals
       , assign
       , Assignment
       , unitPropagate
       , noClauses
       , numClauses
       ) where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe ( catMaybes , fromMaybe )
import Text.PrettyPrint ( text , sep , fsep , hang ,
                          (<>), vcat, parens)
import Debug.Trace ( trace )
import Control.DeepSeq ( NFData , rnf )

import Dimacs ( VarId, Literal, Clause, Dimacs )

type ClauseId = Int
type ClauseSize = Int

data Sign = Positive | Negative
          deriving (Eq, Ord, Show)

data IndexedCNF =
  IndexedCNF
  { icClauses :: M.Map ClauseId Clause
  , icClausesBySize :: M.Map ClauseSize (S.Set ClauseId)
  , icOccurrence :: M.Map VarId (S.Set ClauseId)
  , icNextClauseId :: ClauseId
  }

instance NFData IndexedCNF where
  rnf (IndexedCNF c cs o n) =
    rnf c `seq` rnf cs `seq` rnf o `seq` rnf n

pp ic =
  hang (text "IndexedCNF")
    2 $ sep $
      [ hang (text "icClauses")
          2 $ vcat [ text (show cid ++ ": ")
                     <> sep (map (text.show) cl)
                   | (cid, cl) <- M.toList (icClauses ic)]
      , hang (text "icClausesBySize")
          2 $ vcat [ hang (text (size sz ++ ":"))
                       2 $ sepClauseSet cset
                   | (sz, cset) <- M.toList (icClausesBySize ic)]
      , hang (text "icOccurence") 2 $
        vcat
        [ hang (text $ show var ++ ":") 3 $ sepClauseSet cset
        | (var, cset) <- M.toList (icOccurrence ic) ]]
  where sepClauseSet cset =
          fsep [ text (show clid) | clid <- S.toList cset]
        size sz = "len" ++ show sz

instance Show IndexedCNF where
  show = show . pp

litSign x = if x > 0 then Positive else Negative

fromDimacs :: Dimacs -> IndexedCNF
fromDimacs = L.foldl' (flip addClause) empty 

toDimacs = M.elems . icClauses

-- instance Show IndexedCNF where
--   show = show . toDimacs

empty :: IndexedCNF
empty = IndexedCNF { icClauses = M.empty
                   , icClausesBySize = M.empty
                   , icOccurrence = M.empty
                   , icNextClauseId = 1000 }

addClause :: Clause -> IndexedCNF -> IndexedCNF
addClause clause ic =
  IndexedCNF { icClauses = M.insert clid clause (icClauses ic)
             , icClausesBySize = M.unionWith S.union
                                 (icClausesBySize ic)
                                 (M.singleton size (S.singleton clid))
             , icOccurrence =
                   M.unionWith S.union
                     (icOccurrence ic) $
                     M.fromList [ ( abs lit
                                  , S.singleton clid)
                                | lit <- clause ]
             , icNextClauseId = succ clid }
  where clid = icNextClauseId ic
        size = length clause

        

clausesWithVar :: VarId -> IndexedCNF -> S.Set ClauseId
clausesWithVar v ic =
  fromMaybe S.empty $ M.lookup v $ icOccurrence ic

clauseBodiesWithVar :: VarId -> IndexedCNF -> [Clause]
clauseBodiesWithVar v ic =
  map (icClauses ic M.!) $ S.toList $ clausesWithVar v ic

allVariables :: IndexedCNF -> [VarId]
allVariables = M.keys . icOccurrence

clauseLiterals :: ClauseId -> IndexedCNF -> Clause
clauseLiterals clid ic =
  case M.lookup clid $ icClauses ic of
    Nothing   -> error $ "no such clause id " ++ show clid
    Just lits -> lits


deleteAll :: (Ord k) => [k] -> M.Map k v -> M.Map k v
deleteAll ks m = L.foldl' (flip M.delete) m ks

mapAdjustMany :: (Ord k) => (v -> v) -> [k] -> M.Map k v -> M.Map k v
mapAdjustMany f keys m =
  L.foldl' (\m k -> M.adjust f k m) m keys


assign :: VarId -> Sign -> IndexedCNF -> Maybe IndexedCNF
assign var sign ic
  | var < 0 = error "vars must be positive."
  | contradicting = Nothing
  | otherwise =
    -- trace ("satisfiedClauses:" ++ show satisfiedClauses)
    -- trace ("varsInSatisfiedClauses:" ++ show varsInSatisfiedClauses)
    Just
    IndexedCNF
      { icClauses =
          M.union (M.fromList reducedClauses) $
          deleteAll touchedClauses $
          icClauses ic
      , icClausesBySize =
          M.unionWith (S.union) reducedClausesBySize $
          M.map (flip S.difference touchedClausesSet) $
          icClausesBySize ic
      , icOccurrence = updatedOccurences
      , icNextClauseId = icNextClauseId ic }
  where lit = if sign == Positive then var else negate var
        touchedClausesSet = clausesWithVar var ic
        touchedClauses = S.toList touchedClausesSet
        reducedClauses =
          catMaybes
          [ if isSatisfied
            then Nothing
            else Just ( clid
                      , L.filter ((/=var) . abs) lits)
          | clid <- touchedClauses
          , let lits = clauseLiterals clid ic
                isSatisfied = L.elem lit lits ]
        contradicting = any null $ map snd reducedClauses
        reducedClausesBySize =
          M.fromListWith S.union
          [ (length cl, S.singleton clid)
          | (clid, cl) <- reducedClauses ]
        satisfiedClauses = 
          catMaybes
          [ if isSatisfied
            then Just clid
            else Nothing
          | clid <- touchedClauses
          , let lits = clauseLiterals clid ic
                isSatisfied = L.elem lit lits ]
        varsInSatisfiedClauses =
          S.unions
          [ S.fromList (map abs $ icClauses ic M.! clid)
          | clid <- satisfiedClauses ]
        updatedOccurences =
          mapAdjustMany dropSatisfiedClauses
          (S.toList varsInSatisfiedClauses) $
          M.delete var $ icOccurrence ic
        satisfiedClausesSet = S.fromList satisfiedClauses
        dropSatisfiedClauses cset =
          S.difference cset satisfiedClausesSet

-- | Try to find a unit clause, i.e. one that contains one single
-- variable, and return the literal.
someUnitClauseLiteral :: IndexedCNF -> Maybe Literal
someUnitClauseLiteral ic =
  case M.lookup 1 $ icClausesBySize ic of
    Nothing -> Nothing
    Just unitClauses1 ->
      if S.null unitClauses1
      then Nothing
      else let (clause, unitClauses2) = S.deleteFindMin unitClauses1
               [literal] = icClauses ic M.! clause
           in Just literal


-- | A complete or partial assignment of boolean values to variables.
-- positive VarId means assign true, negative VarId means false
type Assignment = [Literal]

-- | Propagate unit clauses. Variables appearing in clauses of size one
-- must be assigned the value indicated by the literal.
-- Maybe generate Nothing to indicate contradiction.
unitPropagate :: IndexedCNF -> Maybe (IndexedCNF, Assignment)
unitPropagate !ic = recur ic []
  where recur ic as =
          case someUnitClauseLiteral ic of
            Nothing -> Just (ic, as)
            Just literal ->
              case unitPropagateLiteral literal ic of
                Just ic2 -> recur ic2 (literal:as)
                Nothing  -> Nothing


unitPropagateLiteral :: Literal -> IndexedCNF -> Maybe IndexedCNF
unitPropagateLiteral !lit !ic = assign (abs lit) (litSign lit) ic

noClauses :: IndexedCNF -> Bool
noClauses = M.null . icClauses

numClauses :: IndexedCNF -> Int
numClauses = M.size . icClauses
