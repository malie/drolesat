import DTreeOT ( dtreeTest , buildOccurenceTree
               , reportOccurenceTree , testOccurenceTree
               , randomInitialAssignment , reportAssignment
               , improveAssignment )


setSeed :: Int -> IO ()
setSeed seed = R.setStdGen $ R.mkStdGen seed

xxmain =
  do setSeed 123
     cnf <- threeCNF 100 300
     runDTreeTest cnf


runDTreeTestFile fn =
  do putStrLn "\n"
     print fn
     d <- readDimacsFile fn
     runDTreeTest d
     
runDTreeTest :: Dimacs -> IO ()
runDTreeTest d = 
  do if True
       then do putStrLn "initial unit clause resolution..."
               let ic1 = fromDimacs d
               timedDeepseq "ic" $ return ic1
               let Just (ic2, as) = unitPropagate ic1
               timedDeepseq "up" $ return ic2
               printCNFStatsShort $ toDimacs ic2
               -- dtreeTest ic2
               d2 <- randomizeList $ toDimacs ic2
               let t = buildOccurenceTree d2
               reportOccurenceTree t
               testOccurenceTree t $ toDimacs ic2
               as <- randomInitialAssignment t
               reportAssignment 3 as
               void $ improveAssignment 1000 as
       else do printCNFStatsShort d
               let t = buildOccurenceTree d
               reportOccurenceTree t
               testOccurenceTree t d

