module Main where

import Data.Maybe ( isJust , fromJust )
import qualified Data.Set as S

type Var = Int

var x y d = d + ((x-1)+(y-1)*9) * 9
vnot = negate

type CNF = [[Var]]

onlyone vars = vars : excl vars
  where excl [x] = []
        excl (x:xs) = [ [vnot x, vnot y] | y <- xs ] 
                      ++ excl xs
        

ds :: [Int]
ds = [1..9]

cellDigits :: Int -> Int -> CNF
cellDigits x y = onlyone [ var x y d | d <- ds]

row :: Int -> Int -> CNF
row y d = onlyone [ var x y d | x <- ds ]

column :: Int -> Int -> CNF
column x d = onlyone [ var x y d | y <- ds ]

block :: Int -> Int -> CNF
block b d = onlyone [ var (xo+x) (yo+y) d | (x,y) <- es ]
  where xo = 3*((b-1) `div` 3)
        yo = 3*((b-1) `mod` 3)
        es = [(x,y) | x <- [1..3], y <- [1..3]]

type Digit = Int -- 1..9
type SudokuSource = [[Maybe Digit]]

parseSudoku :: [String] -> SudokuSource
parseSudoku = map p
  where p line = [ readField w
                 | w <- words line]

readField w = if w == "_"
              then Nothing
              else Just $ read w

sudokuBase :: CNF
sudokuBase = concat
             [ cellDigits a b ++ row a b ++ column a b ++ block a b
             | a <- [1..9]
             , b <- [1..9]]

sudoku :: SudokuSource -> CNF
sudoku src = unique $ sudokuBase ++ givens
  where givens = 
          [ [var x y (fromJust c)] -- singleton disjunctions!
          | (y,r) <- zip [1..9] src
          , (x,c) <- zip [1..9] r
          , isJust c ]

unique :: CNF -> CNF
unique = map S.toList . S.toList . S.fromList . map S.fromList

s1 :: [String]
s1 =
  [ "1 _ _ _ _ 5 3 _ 7"
  , "_ 9 2 1 4 _ _ _ _"
  , "_ _ _ 8 _ _ _ _ _"
  , "_ _ _ _ 7 _ 5 _ 1"
  , "_ 4 _ _ _ _ _ 7 _"
  , "5 _ 6 _ 8 _ _ _ _"
  , "_ _ _ _ _ 6 _ _ _"
  , "_ _ _ _ 3 9 2 5 _"
  , "2 _ 9 4 _ _ _ _ 3" ]

printDimacs :: String -> CNF -> IO ()
printDimacs filename cnf = writeFile filename text
  where text = unlines ls
        numVariables = S.size $ S.fromList $ concatMap (map abs) cnf
        numClauses = length cnf
        header = [unwords ["p cnf", show numVariables, show numClauses]]
        ls = header ++ map (\clause -> unwords $ map show $ clause ++ [0]) cnf


main = printDimacs "out.cnf" $ sudoku $ parseSudoku s1
