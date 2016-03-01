{-#LANGUAGE OverloadedStrings#-}

{-
Project name: Annotate ensembl genes to symbol using v24 gencode human gtf
Min Zhang
Date: Feb 26, 2016
Version: v0.1.0
README: 
-}

module Lib
    ( annotateEnsembl
    ) where

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TextIO
import Control.Applicative
import qualified Data.List as L
import Control.Monad (fmap)
import Data.Ord (comparing)
import Data.Function (on)
import qualified Safe as S
import qualified Data.HashMap.Lazy as M
import qualified Data.Maybe as Maybe
import System.Environment (getArgs)

ref filepath = TextIO.readFile filepath >>= return . M.fromList . removeDup . map (T.splitOn "\t") . T.lines

refMouse = ref "data/unique.vM8.annotation.pairs.txt" 
refHuman = ref "data/unique.v24.annotation.pairs.txt"

toPair [x, y] = (x, y)

readInt :: Int -> T.Text
readInt x = T.pack $ show x

counter [] = []
counter xs = concat $ map toIndex groupedListLen
             where groupedListLen = map length $ L.group xs
                   toIndex x = map readInt [1..x]


changeGeneName xs = (take 1 xs) ++ [zipWith addCounter nameCol (counter nameCol)]
                    where nameCol = head $ drop 1 xs
                          addCounter a b = if b == "1" then a else T.concat [a, "_", b] 

removeDup xs = map toPair $ L.transpose $ changeGeneName $ L.transpose $ L.sortBy (comparing last) xs

annotateEnsembl = do
  -- assume id is the first column
   intro
   args <- getArgs
   let species = S.headNote "Please choose a genome version [hg38|mm10]" args
   let inputpath = S.lastNote "Please put an input file path." args
   refmap <- (case species of 
                    "mm10" -> refMouse
                    "hg38" -> refHuman
                    otherwise -> refHuman)
   result <- T.unlines . map ((\(ensembl, rest) -> T.concat [M.lookupDefault ensembl ensembl refmap, rest]) . (\x->T.breakOn "\t" x)) . T.lines . T.replace " " "\t" . T.replace "," "\t" <$> TextIO.readFile inputpath
   TextIO.putStr result
   
intro = do
  TextIO.putStrLn "Ensembl2Symbol v0.1"
  TextIO.putStrLn "Min Zhang (mz1 at bcm dot edu)"
  TextIO.putStrLn "current gtf annotation: human gencode v24; mouse gencode vM8"
  TextIO.putStrLn "Note: Only ensembl IDs in the first column will be annoated to gene symbol.\n"
  TextIO.putStrLn "Note: White space and comma delimiters will be converted to tabs"
  TextIO.putStrLn "Usage: Ensembl2Symbol [hg38|mm10] inputpath > outputpath"

