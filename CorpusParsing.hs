module CorpusParsing (
    getCorpora,
    ppOccTable
    ) where

import Data.Char (isLetter, isDigit)
import Data.List (sortBy)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Ord (comparing, Down(..))
import Text.Printf
import Text.XML.Light
import Debug.Trace (trace)

getCorpora :: IO ([String], [String])
getCorpora = do aCorpus <- fmap (cleanCorpusFiltering (\(Element n _ _ _) -> n == unqual "p"))
                         $ readFile "mueller_clean_orig_input.html"
                bCorpus <- fmap (cleanCorpusFiltering (\(Element _ as _ _) -> as == [Attr (unqual "align") "justify"]))
                         $ readFile "schmid_clean_orig_input.html"
                let getProblems = unlines . filter (\s -> not $ length s == 1 || all isLetter s || all isDigit s)

                -- putStrLn $ "A / mueller\n" ++ ppOccTable (countWords aCorpus)
                -- putStrLn $ "B / schmid\n" ++ ppOccTable (countWords bCorpus)
                putStrLn $ ("Problems in A: "++) $ show . length $ getProblems aCorpus
                putStrLn $ ("Problems in B: "++) $ show . length $ getProblems bCorpus

                return (aCorpus, bCorpus)

cleanCorpusFiltering :: (Element -> Bool) -> String -> [String]
cleanCorpusFiltering p = -- Removed guillemets may have uncovered one more layer of punctuation
                         foldr (separatePunctuation id) []
                       -- Separate punctuation and remove guillemets in the same pass
                       . foldr (separatePunctuation removeGuillemets) []
                       -- Splice linebreak-separated words
                       . foldr (\w acc -> if length w > 1 && last w == '-'
                                          then (init w ++ head acc) : tail acc
                                          else w : acc) []
                       . words
                       . concatMap stringFromElement
                       . filter p
                       . onlyElems
                       . parseXML
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
          stringFromElement = foldr go "" . elContent
              where go :: Content -> String -> String
                    go (Elem el) acc = stringFromElement el ++ acc
                    go (Text cd) acc = cdData cd ++ acc
                    go (CRef _) acc = acc

countWords :: [String] -> M.Map String Int
countWords = foldl (\m w -> M.insertWith (+) w 1 m) (M.empty :: M.Map String Int)

ppOccTable :: M.Map String Int -> String
ppOccTable inmap =  hr
                 ++ printf ("|%" ++ show (wwidth + nwidth + 2) ++ "d")
                           allWords
                 ++ " words|\n"
                 ++ hr
                 ++ concatMap (\(w, n) -> printf ("|%" ++ show wwidth ++ "s") w
                                       ++ printf ("|%" ++ show nwidth ++ "d") n
                                       ++ printf ("|%6.4f|\n") (n `percentOf` allWords))
                              table
                 ++ hr
                 ++ printf ("|%6.3f") (fromIntegral allWords / fromIntegral punctuation :: Double)
                 ++ replicate (wwidth + nwidth - 7) ' ' ++ " wo./sen.|\n"
                 ++ hr
                 ++ "| . ! ?" ++ replicate (wwidth + nwidth + 2) ' ' ++ "|\n"
                 ++ printf ("|%5.2f") ((fromJust $ M.lookup "." inmap) `percentOf` sentenceEnds)
                 ++ printf (" %5.2f") ((fromJust $ M.lookup "!" inmap) `percentOf` sentenceEnds)
                 ++ printf (" %5.2f") ((fromJust $ M.lookup "?" inmap) `percentOf` sentenceEnds)
                 ++ replicate (wwidth + nwidth - 9) ' ' ++ "|\n"
                 ++ hr
    where table = take 30
                $ sortBy (comparing (Down . snd))
                $ M.toList inmap
          wwidth = maximum $ map (length . fst) table
          nwidth = maximum $ map (length . show . snd) table
          hr = "+" ++ replicate wwidth '-' ++ "+" ++ replicate nwidth '-' ++ "+------+\n"
          allWords = sum $ M.elems inmap
          a `percentOf` b = 100 * fromIntegral a / fromIntegral b :: Double
          sumWords ws = sum $ M.elems $ M.filterWithKey (\x _ -> head x `elem` ws) inmap
          punctuation = sumWords ['.', '!', '?', ':', ';']
          sentenceEnds = sumWords ['.', '!', '?']
