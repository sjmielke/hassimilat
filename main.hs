import NGrams
import MyCorpusData

import TreeParsing (flattenTree, formatInfoTree)

main :: IO ()
main = do {- aCorpus <- getWordListCorpus Mueller
          bCorpus <- getWordListCorpus Schmid
          let aBigrams = getBigrams aCorpus
          let bBigrams = getBigrams bCorpus
          
          putStrLn "Sample A"
          putStrLn $ getText 200 aBigrams
          putStrLn "Sample B"
          putStrLn $ getText 200 bBigrams
          -- -}
          
          {- fsrCorpus <- getWordListCorpus FSR
          richard <- getWordListCorpus Richard
          let combinedCorpus = take 1000 fsrCorpus ++ richard
          putStrLn $ ppOccTable $ countWords $ combinedCorpus
          putStrLn $ getText 200 $ getBigrams combinedCorpus
          -- -}
          
          {- japterCorpus <- fmap (map (map toUpper)) $ getWordListCorpus JapTer
          putStrLn $ ppOccTable $ countWords $ japterCorpus
          putStrLn $ getText 1000 $ getBigrams $ japterCorpus
          -- -}
          
          tiger <- getTreeCorpus Tiger
          putStrLn $ unlines . map formatInfoTree
                   -- $ take 100 
                   $ (:[]) . (!!42)
                   $ tiger -- About 50k sentences. Nice.
