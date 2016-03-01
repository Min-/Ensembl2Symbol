{-#LANGUAGE OverloadedStrings#-}

{-
Project name: Gtf2Table
Min Zhang
Date: March 1, 2016
Version: v0.1.0
README: Fix some genes have errors in annotation 
-}

module Main
where

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TextIO
import Control.Applicative
import qualified Data.List as L
import qualified Data.Set as Set
import Control.Monad (fmap)
import Data.Ord (comparing)
import Data.Function (on)
import qualified Safe as S
import qualified Data.HashMap.Lazy as M
import qualified Data.Maybe as Maybe
import qualified Data.Foldable as F (all)
import qualified System.IO as IO
import System.Environment

main = do
  intro
  args <- getArgs
  let inputpath = S.headNote "Please put two arguments. gtf2Tab inputpath outputpath." args
  let outputpath = S.lastNote "Please put two arguments. gtf2Tab inputpath outputpath." args
  input <- TextIO.readFile inputpath
  let output = (makeTable . shrinkLines . filter (\x->length x == 2) . map (map last . filter (\x->head x == "gene_id" || head x == "gene_name") . map (T.splitOn " ")) . map (T.splitOn ";") . T.lines . stripQuotes) input
  TextIO.writeFile outputpath output

intro = TextIO.putStrLn "Gtf2Table: inputpath outputpath"

stripQuotes t = T.replace "\"" "" t

shrinkLines = Set.toList . Set.fromList

makeTable x = T.unlines $ map (T.intercalate "\t") x
