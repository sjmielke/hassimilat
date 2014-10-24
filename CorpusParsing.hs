import Data.Char (isLetter)
import Data.List (intersperse, sort, group, sortBy)
import Data.Ord (comparing, Down(..))
import Text.Printf
import Text.XML.Light
import Debug.Trace (trace)

main = do bCorpus <- fmap cleanBCorpus $ readFile "schmid_clean_orig_input.html"
          let bProblems = concat
                        $ intersperse "\n"
                        $ filter (\s -> length s > 1 && (not $ isLetter (last s) && isLetter (head s)))
                        $ bCorpus
          putStrLn $ ppOccTable
                   $ map (\n -> (head n, length n)) . group . sort
                   $ bCorpus

cleanBCorpus :: String -> [String]
cleanBCorpus = spliceWords
             . concatMap separatePunctuation
             . map removeGuillemets
             . concatMap separatePunctuation
             . map (\x -> if x == "..." then "…" else x)
             . words
             . concatMap strContent
             . filter (\(Element _ as _ _) -> as == [Attr (unqual "align") "justify"])
             . onlyElems
             . parseXML
    where separatePunctuation w = if length w > 1 && last w `elem` ['.', ',', '!', '?', ':', ';']
                                  then [init w, [last w]]
                                  else [w]
          removeGuillemets w = let nopre = if head w == '»' then tail w else w
                                   nopost = if last nopre == '«' then init nopre else nopre
                               in nopost
          spliceWords [w] = [w]
          spliceWords (w:wn:ws) = if last w == '-'
                                  then spliceWords ((init w ++ wn) : ws)
                                  else w : spliceWords (wn:ws)

ppOccTable :: [(String, Int)] -> String
ppOccTable intable =  hr
                   ++ printf ("|%" ++ show (wwidth + nwidth - 5) ++ "d")
                             allwords
                   ++ " words|\n"
                   ++ hr
                   ++ concatMap (\(w, n) -> "|"
                                         ++ printf ("%" ++ show wwidth ++ "s") w
                                         ++ "|"
                                         ++ printf ("%" ++ show nwidth ++ "d") n
                                         ++ "|\n")
                                table
                   ++ hr
                   ++ printf ("|%" ++ show (wwidth + nwidth - 6) ++ "." ++ show (wwidth + nwidth - 9) ++ "f")
                             (fromIntegral allwords / fromIntegral punctuation :: Double)
                   ++ " per s.|\n"
                   ++ hr
    where table = take 30
                $ sortBy (comparing (Down . snd))
                $ intable
          wwidth = maximum $ map (length . fst) table
          nwidth = maximum $ map (length . show . snd) table
          hr = "+" ++ replicate wwidth '-' ++ "+" ++ replicate nwidth '-' ++ "+\n"
          allwords = sum $ map snd intable
          punctuation = sum $ map snd $ filter (\(x,n) -> head x `elem` ['.', '!', '?', ':', ';']) intable
