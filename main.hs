import NGrams
import TreeParsing (SNode(NT, T))
import MyCorpusData

import qualified Data.Foldable as F
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Data.Tree (drawTree)

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
          -- putStrLn $ drawTree . fmap show $ tiger !! 42
          
          let addIntoMap oldmap (T p w) = M.insertWith (\_ -> M.insertWith (+) w 1)
                                                       p
                                                       (M.singleton w 1)
                                                       oldmap
              addIntoMap oldmap _ = oldmap
          let posChoices = F.foldl' (F.foldl' addIntoMap)
                                    (M.empty :: M.Map String (M.Map String Int))
                                    tiger
          putStrLn $ ppOccTable $ fromJust $ M.lookup "NN" posChoices
