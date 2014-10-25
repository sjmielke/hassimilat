import Data.Char (isLetter)
import Data.List (intersperse, sort, group, sortBy)
import Data.Maybe (fromJust)
import Data.Ord (comparing, Down(..))
import Text.Printf
import Text.XML.Light
import Debug.Trace (trace)

main = do aCorpus <- fmap (cleanCorpusFiltering (\(Element n _ _ _) -> n == unqual "p"))
                   $ readFile "mueller_clean_orig_input.html"
          bCorpus <- fmap (cleanCorpusFiltering (\(Element _ as _ _) -> as == [Attr (unqual "align") "justify"]))
                   $ readFile "schmid_clean_orig_input.html"
          let getProblems = concat
                          . intersperse "\n"
                          . filter (\s -> length s > 1 && (not $ isLetter (last s) && isLetter (head s)))
                       
          putStrLn $ "A / mueller\n" ++ ppOccTable aCorpus
          putStrLn $ "B / schmid\n" ++ ppOccTable bCorpus

cleanCorpusFiltering :: (Element -> Bool) -> String -> [String]
cleanCorpusFiltering pred = cleanWordlistString
                          . concatMap stringFromElement
                          . filter pred
                          . onlyElems
                          . parseXML


cleanWordlistString :: String -> [String]
cleanWordlistString = spliceWords
                    . concatMap separatePunctuation
                    . map removeGuillemets
                    . concatMap separatePunctuation
                    . map (\x -> if x == "..." then "…" else x)
                    . words
    where separatePunctuation w = if length w > 1 && last w `elem` ['.', ',', '!', '?', ':', ';']
                                  then [init w, [last w]]
                                  else [w]
          removeGuillemets w = let nopre  = if head w `elem` ['»', '„', '"']
                                            then (try tail) w else w
                                   nopost = if last nopre `elem` ['«', '“', '"']
                                            then (try init) nopre else nopre
                                   try f w = if length w > 1 then f w else w
                               in nopost
          spliceWords [w] = [w]
          spliceWords (w:wn:ws) = if last w == '-'
                                  then spliceWords ((init w ++ wn) : ws)
                                  else w : spliceWords (wn:ws)

stringFromElement :: Element -> String
stringFromElement = foldr go "" . elContent
    where go :: Content -> String -> String
          go (Elem el) acc = stringFromElement el ++ acc
          go (Text cd) acc = cdData cd ++ acc

ppOccTable :: [String] -> String
ppOccTable corpus =  hr
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
                  ++ printf ("|%5.2f") ((fromJust $ lookup "." intable) `percentOf` sentenceEnds)
                  ++ printf (" %5.2f") ((fromJust $ lookup "!" intable) `percentOf` sentenceEnds)
                  ++ printf (" %5.2f") ((fromJust $ lookup "?" intable) `percentOf` sentenceEnds)
                  ++ replicate (wwidth + nwidth - 9) ' ' ++ "|\n"
                  ++ hr
    where intable = map (\n -> (head n, length n)) . group . sort $ corpus
          table = take 30
                $ sortBy (comparing (Down . snd))
                $ intable
          wwidth = maximum $ map (length . fst) table
          nwidth = maximum $ map (length . show . snd) table
          hr = "+" ++ replicate wwidth '-' ++ "+" ++ replicate nwidth '-' ++ "+------+\n"
          allWords = sum $ map snd intable
          a `percentOf` b = 100 * fromIntegral a / fromIntegral b :: Double
          sumWords ws = sum $ map snd $ filter (\(x,n) -> head x `elem` ws) intable
          punctuation = sumWords ['.', '!', '?', ':', ';']
          sentenceEnds = sumWords ['.', '!', '?']
