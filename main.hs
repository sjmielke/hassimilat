import NGrams
import TreeParsing (SNode(NT, T), tag)
import MyCorpusData

import Control.DeepSeq (deepseq)
import qualified Data.Foldable as F
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Data.Tree (Tree(Node), rootLabel, subForest, drawTree)

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
          -- putStrLn $ ppOccTable $ fromJust $ M.lookup "ART" posChoices
          
          -- putStrLn $ unlines $ map show $ filter (fst . snd) $ M.toAscList $ getUsedTags tiger
          
          let rules = getRules tiger
          
          print $ S.size rules
          putStrLn $ unlines . map show $ filter (\(x,_) -> x == "ROOT") $ S.toList rules

getUsedTags :: [Tree SNode] -> M.Map String (Bool, Bool) -- name, used in inner node (NT), used in leaf (T)
getUsedTags = F.foldl' (F.foldl' ins)
                       (M.empty :: M.Map String (Bool, Bool))
    where ins tagmap (NT tag) = M.insertWith (\_ (nt,t) -> (True,t)) tag (True, False) tagmap
          ins tagmap (T tag _) = M.insertWith (\_ (nt,t) -> (nt,True)) tag (False, True) tagmap

type Rule = (String, [String])

getRules :: [Tree SNode] -> S.Set Rule
getRules = F.foldl' (flip readoff)
                    (S.empty :: S.Set Rule)
    where readoff (Node (NT rootTag) children) =
                let newrule = (rootTag, map (tag . rootLabel) children)
                in newrule `deepseq` S.insert newrule
          readoff _ = id
