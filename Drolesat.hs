{-# LANGUAGE BangPatterns, TupleSections #-}
module Main where

import Control.Monad.State

import qualified Data.List as L
import qualified Data.Map as M
import qualified System.Random as R
import qualified Data.Set as S

import Data.Ord ( comparing )

import Dimacs ( Dimacs , VarId , Literal , readDimacsFile )
import RandomCNF ( threeCNF )
import Random ( randomizeList )
import IndexedCNF ( IndexedCNF , ClauseId , empty , addClause
                  , fromDimacs , unitPropagate , toDimacs )
import DPLL ( dpll , dpllTimed
            , randomHeuristic , mostOftenUsedVarHeuristic
            , momsHeuristic )
import Timed ( timed , timedDeepseq )

import OBDD ( obddTest )
import Testing ( printCNFStatsShort , printCNFStats, histogram )

data P = Clause [Literal] [[Literal]]
       | Assignments [S.Set Literal]

data S = 
  S { clauses :: M.Map ClauseId P
    , varOccurence :: M.Map VarId [ClauseId]
    }



xmain = 
  do d <- readDimacsFile 
          -- "../Downloads/E04F19.cnf" -- ???
          -- "../Downloads/gss-17-s100.cnf"
          "ueb11.cnf"
     let diid = dimacsWithIds d
     -- mapM_ print $ M.toList diid
     {-mapM_ print $ take 1000 $ reverse $ L.sortBy (comparing snd) $
       M.toList $ varcounts d-}
     {-mapM_ print $ reverse $ L.sortBy (comparing snd) $
       M.toList $ overlaps diid-}
     return ()
     
     -- let numseeds = max 3 $ sqrt numvars
     -- partition numseeds diid

-- partition n 


varcounts :: Dimacs -> M.Map VarId Int
varcounts di =
  M.fromListWith (+) $
  [ (abs lit, 1)
  | lits <- di
  , lit <- lits ]

type DimacsId = M.Map ClauseId [Literal]

dimacsWithIds :: Dimacs -> DimacsId
dimacsWithIds ds = M.fromList $ zip [1000000..] ds


overlaps :: DimacsId -> M.Map (ClauseId, ClauseId) Int
overlaps diid =
  M.fromListWith (+)
  [ ((ca, cb), 1)
  | (!ca, !lits) <- M.toList diid
  , length lits < 4
  , !lit <- lits
  , !cb <- varocc M.! (abs lit) 
  , ca < cb ]
  where !varocc = M.fromListWith (++)
                  [ (abs lit, [clid])
                  | (!clid, !lits) <- M.toList diid
                  , !lit <- lits ]


setSeed :: Int -> IO ()
setSeed seed = R.setStdGen $ R.mkStdGen seed



main =
  mapM_ p $
  words $
  "out.cnf"
  -- "E04F19.cnf"
  -- "gss-17-s100.cnf"
  -- "grieu-vmpc-31.cnf"
  -- ++ "ueb11.cnf"
  -- "vmpc_29.cnf"
  -- "partial-10-11-s.cnf"
  -- "../sat-2002-beta/submitted/"
  -- "prestwich/mediator/med11.shuffled.cnf"
  -- "prestwich/mediator/med19.shuffled.cnf"
  -- "prestwich/mediator/med30.shuffled.cnf"
  -- "pyhala/pyhala-braun-sat-4/pyhala-braun-sat-30-4-01.shuffled.cnf"
  -- "goldberg/bmc1/4.shuffled.cnf"
  -- ++ "goldberg/fpga_routing/term1_gr_rcs_w4.shuffled.cnf"
  where 
    p = -- printSomeStats
        -- printSomeStats2
        -- solve
        runOBDDTestFile

runOBDDTestFile fn =
  do putStrLn "\n"
     print fn
     d <- readDimacsFile fn
     runOBDDTest d
     
runOBDDTest :: Dimacs -> IO ()
runOBDDTest d = 
  do putStrLn "initial unit clause resolution..."
     let ic1 = fromDimacs d
     timedDeepseq "ic" $ return ic1
     let Just (ic2, as) = unitPropagate ic1
     timedDeepseq "up" $ return ic2
     printCNFStatsShort $ toDimacs ic2
     obddTest ic2

printSomeStats fn =
  do putStrLn "\n"
     print fn
     d <- readDimacsFile fn
     printCNFStats d

printSomeStats2 fn =
  do putStrLn "\n"
     print fn
     d <- readDimacsFile fn
     printCNFStats d
     putStrLn "after unit clause resolution:"
     let Just (ic, as) = unitPropagate (fromDimacs d)
     putStrLn "assignments:"
     mapM_ print as
     printCNFStats $ toDimacs ic

solve fn =
  do d <- readDimacsFile fn
     let ic1 = fromDimacs d
     Just (ic2, as) <- timed "initial unit resolution" $
                         return $ unitPropagate ic1
     res <- dpllTimed momsHeuristic ic2
     mapM_ print $ map (filter (\x-> x > 0)) $ map ((++as).snd) res


{-
assign clause-ids to clauses
index clauses by vars
group clauses if vars are subset

then solve N smallest clauses, build set of assignments

"order clauses by number of overlaps"

then combine those clauses with most overlap


-}
