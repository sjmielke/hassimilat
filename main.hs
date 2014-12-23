import NGrams
import TreeParsing
import MyCorpusData

import Control.Monad.State.Strict (State, runState, evalState, get, put)
import qualified Data.Map.Strict as M
import Data.List (sortBy, nub, group, sort)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Data.Tree (Tree(Node), rootLabel, subForest, drawTree)
import System.Random (StdGen, randomR, getStdGen)

main :: IO ()
main = do tiger <- getTreeCorpus Tiger
          -- putStrLn $ drawTree . fmap show $ tiger !! 42
          
          {- let addIntoMap oldmap (T p w) = M.insertWith (\_ -> M.insertWith (+) w 1)
                                                       p
                                                       (M.singleton w 1)
                                                       oldmap
              addIntoMap oldmap _ = oldmap
          let posChoices = F.foldl' (F.foldl' addIntoMap)
                                    (M.empty :: M.Map String (M.Map String Int))
                                    tiger
          putStrLn $ ppOccTable $ fromJust $ M.lookup "ART" posChoices
          -- -}
          
          -- putStrLn $ unlines $ map show $ filter (fst . snd) $ M.toAscList $ getUsedTags tiger
          
          -- let rules = getRules tiger
          
          -- print $ S.size rules
          -- putStrLn $ unlines . map show $ filter (\(x,_) -> x == "ROOT") $ S.toList rules
          
          let rulesOcc = countRules tiger
          
          {-
          let printNBest n tag = putStrLn
                               $ unlines . map show
                               $ take n
                               $ reverse
                               $ sortBy (comparing snd)
                               $ filter (\((x,_),_) -> x == tag)
                               $ M.toList rulesOcc
          -- -}
          
          -- print $ M.size rulesOcc
          
          -- let usedTags = map (\l -> (head l, length l)) . group $ map fst $ M.keys rulesOcc
          -- putStrLn $ unlines . map show $ usedTags
          
          -- mapM_ (printNBest 1 . fst) usedTags
          
          -- print $ countCallers "VVFIN" rulesOcc
          
          -- print $ M.foldl (+) 0 $ M.map length $ toConstructiveForm rulesOcc
          
          let ruleBook = toConstructiveForm rulesOcc
          
          print $ bestRule "S" ruleBook
          
          print   $ derivationStep ruleBook
                =<< derivationStep ruleBook
                =<< return "S"
          
          putStrLn ""
          rndGen <- getStdGen
          
          print $ sort $ evalState (sequence $ replicate 100 (decentRandomRule "S" ruleBook)) rndGen

-- Note: I stay with the simple Int scores for now, choosing
-- best options works just as well there (as we have seen
-- in the other generation mechanisms before).
-- The values (the rhs lists) are sorted in descending order.
toConstructiveForm :: M.Map (String, [String]) Int -> M.Map String [([String], Int)]
toConstructiveForm = M.map (reverse . sortBy (comparing snd))
                   . M.foldlWithKey' ( \ oldresmap (lhs, rhs) occ
                                      -> M.insertWith (\new -> ((head new):))
                                                      lhs
                                                      [(rhs, occ)]
                                                      oldresmap )
                                     (M.empty :: M.Map String [([String], Int)])

bestRule :: String -> M.Map String [([String], Int)] -> [String]
bestRule lhs = fst
             . head
             . fromJust
             . M.lookup lhs

decentRandomRule :: String -> M.Map String [([String], Int)] -> State StdGen [String]
decentRandomRule lhs = takeSomeRandomOne
                     . fromJust
                     . M.lookup lhs
    where takeSomeRandomOne l = do randomGen <- get
                                   let (n, newGen) = randomR (-3.0, 0.0 :: Double) randomGen
                                   put newGen
                                   return $ fst $ l !! (floor $ (fromIntegral $ min 10 (length l)) * exp n)

derivationStep :: M.Map String [([String], Int)] -> String -> [String]
derivationStep m lhs = if isNT lhs m
                       then bestRule lhs m
                       else [lhs]

isNT :: String -> M.Map String [([String], Int)] -> Bool
isNT x = elem x . M.keys
