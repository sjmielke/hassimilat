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


import qualified Data.Text.Lazy.IO as TIO

main :: IO ()
main = do --{-
          tiger <- getTreeCorpus Tiger
          let rulesOcc = countRules tiger
          let ruleBook = eliminateChainRules $ toConstructiveForm rulesOcc
          
          let wordOcc = countWordRules tiger
          let wordBook = toConstructiveForm wordOcc
          
          rndGen <- getStdGen
          
          let dStep :: [String] -> State StdGen [String]
              dStep l = mapM (decentRandomDerivationStep ruleBook wordBook) l >>= return . concat
          
          let fullyDerive :: [String] -> State StdGen [String]
              fullyDerive xs = do xs' <- dStep xs
                                  if xs == xs' then return xs else fullyDerive xs'
          
          let generateSentence :: State StdGen [String]
              -- generateSentence = decentRandomLookup "S" ruleBook
              generateSentence = fullyDerive ["ROOT"]
          mapM_ (putStrLn . (++"\n") . beautifulUnwords) $ evalState (sequence $ replicate 100 generateSentence) rndGen
          
          -- -}
          
          {-
          print $ M.lookup "$./--" wordBook
          print $ evalState (sequence $ replicate 5 generateSentence) $ read "foo"
          print $ evalState (sequence $ replicate 5 generateSentence) $ read "bar"
          print $ evalState (sequence $ replicate 5 generateSentence) $ read "baz"
          print $ evalState (sequence $ replicate 5 generateSentence) $ read "bat"
          print $ evalState (sequence $ replicate 5 generateSentence) $ read "val"
          -- -}
          
          {-
          sentence <- fmap (head . simpleParseNegra) $ TIO.readFile "data/overlappingsentence.export"
          putStrLn $ drawTree $ fmap show $ sentence
          putStrLn $ drawTree $ fmap show $ simplifySentenceTree sentence
          -- -}

-- Note: I stay with the simple Int scores for now, choosing
-- best options works just as well there (as we have seen
-- in the other generation mechanisms before).
-- The values (the rhs lists) are sorted in descending order.
toConstructiveForm :: M.Map (String, rhs) Int -> M.Map String [(rhs, Int)]
toConstructiveForm = M.map (reverse . sortBy (comparing snd))
                   . M.foldlWithKey' ( \ oldresmap (lhs, rhs) occ
                                      -> M.insertWith (\new -> ((head new):))
                                                      lhs
                                                      [(rhs, occ)]
                                                      oldresmap )
                                     (M.empty :: M.Map String [(rhs, Int)])

-- | Eliminating chain rules doesn't affect my results,
-- but it works better for the current derivation mechanism.
eliminateChainRules :: M.Map String [([String], Int)] -> M.Map String [([String], Int)]
eliminateChainRules = M.mapWithKey (\lhs rhss -> filter (\(rhs,_) -> [lhs] /= rhs) rhss)

bestLookup :: String -> M.Map String [(a, Int)] -> a
bestLookup lhs = fst
               . head
               . fromJust
               . M.lookup lhs

decentRandomLookup :: String -> M.Map String [(a, Int)] -> State StdGen a
decentRandomLookup lhs = takeSomeRandomOne
                       . fromJust
                       . M.lookup lhs
    where takeSomeRandomOne l = do randomGen <- get
                                   let (n, newGen) = randomR (-4.0, 0.0 :: Double) randomGen
                                   put newGen
                                   let index = (fromIntegral $ min 30 (length l)) * exp n
                                   return $ fst $ l !! (floor index)

bestDerivationStep :: M.Map String [([String], Int)]
                   -> M.Map String [(String, Int)]
                   -> String
                   -> [String]
bestDerivationStep rules words lhs = if isNT lhs rules
                                     then bestLookup lhs rules
                                     else if isT lhs words
                                          then [bestLookup lhs words]
                                          else [lhs]

decentRandomDerivationStep :: M.Map String [([String], Int)]
                           -> M.Map String [(String, Int)]
                           -> String
                           -> State StdGen [String]
decentRandomDerivationStep rules words lhs = if isNT lhs rules
                                             then decentRandomLookup lhs rules
                                             else if isT lhs words
                                                  then fmap (:[]) $ decentRandomLookup lhs words
                                                  else return [lhs]

isNT :: String -> M.Map String [([String], Int)] -> Bool
isNT x = elem x . M.keys

isT  :: String -> M.Map String [(String, Int)] -> Bool
isT x = elem x . M.keys
