module CorpusParsing (
    getSimpleCorpus,
    cleanCorpusFiltering
    ) where

import Data.Char (isLetter, isDigit)
import Data.List (sortBy)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Ord (comparing, Down(..))
import Text.XML.Light
import Debug.Trace (trace)

getProblematicWords :: [String] -> [String]
getProblematicWords = filter (\s -> not $ length s == 1 || all isLetter s || all isDigit s)

getSimpleCorpus :: String -> IO [String]
getSimpleCorpus = fmap (cleanSimpleCorpus . words) . readFile

cleanSimpleCorpus :: [String] -> [String]
cleanSimpleCorpus = -- Removed guillemets may have uncovered one more layer of punctuation
                    foldr (separatePunctuation id) []
                    -- Separate punctuation and remove guillemets in the same pass
                  . foldr (separatePunctuation removeGuillemets) []
    where separatePunctuation finfunc w acc
            | w == "..." = "…" : acc
            | otherwise = if length w > 1 && last w `elem` ['.', ',', '!', '?', ':', ';']
                          then if length w > 3 && take 3 (reverse w) == "..."
                               then finfunc (init . init . init $ w) : "…" : acc
                               else finfunc (init w) : [last w] : acc
                          else finfunc w : acc
          removeGuillemets w = let nopre  = if head w `elem` ['»', '„', '"']
                                            then (try tail) w else w
                                   nopost = if last nopre `elem` ['«', '“', '"']
                                            then (try init) nopre else nopre
                                   try f l = if length l > 1 then f l else l
                               in nopost

cleanCorpusFiltering :: (Element -> Bool) -> String -> [String]
cleanCorpusFiltering p = cleanSimpleCorpus
                       -- Splice linebreak-separated words
                       . foldr (\w acc -> if length w > 1 && last w == '-'
                                          then (init w ++ head acc) : tail acc
                                          else w : acc) []
                       . words
                       . concatMap stringFromElement
                       . filter p
                       . onlyElems
                       . parseXML
    where stringFromElement = foldr go "" . elContent
              where go :: Content -> String -> String
                    go (Elem el) acc = stringFromElement el ++ acc
                    go (Text cd) acc = cdData cd ++ acc
                    go (CRef _) acc = acc
