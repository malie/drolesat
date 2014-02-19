module PrettyClassExt ( flist , printPretty ) where

import Text.PrettyPrint.HughesPJClass
  ( Pretty , pPrint , hang , text , fsep , comma , (<>) , Doc
  , brackets , prettyShow )

import qualified Data.Set as S
import qualified Data.Map as M

flist :: (a -> Doc) -> [a] -> Doc
flist f xs = brackets $ fsep $ recur xs
  where recur []     = []
        recur [x]    = [f x]
        recur (x:xs) = f x <> comma : recur xs


printPretty :: Pretty a => a -> IO ()
printPretty = putStrLn . prettyShow

instance Pretty a => Pretty (S.Set a) where
  pPrint set = hang (text "S.fromList") 2 $
               flist pPrint $ S.toList set

instance (Pretty a, Pretty b) =>
         Pretty (M.Map a b) where
  pPrint set = hang (text "M.fromList") 2 $
               flist pPrint $ M.toList set
