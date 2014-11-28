-- module NGrams where

import CorpusParsing

import Control.Monad.State.Lazy (evalState, get, put)
import Data.Char (toUpper)
import Data.List (sortBy)
import Data.Ord (comparing, Down(..))
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomR, getStdGen)

type BigramMap = M.Map String (M.Map String Int)

main :: IO ()
main = do {-(aCorpus, bCorpus) <- getCorpora
          let aBigrams = getBigrams aCorpus
          let bBigrams = getBigrams bCorpus
          
          putStrLn "Sample A"
          putStrLn $ unwords $ take 100 $ buildText aBigrams
          putStrLn "Sample B"
          putStrLn $ unwords $ take 100 $ buildText bBigrams
          -}
          
          {-fsrCorpus <- getSimpleCorpus "fsrtexte"
          richard <- getSimpleCorpus "richardscorpus"
          let combinedCorpus = take  0 fsrCorpus ++ richard
          putStrLn $ ppOccTable $ countWords $ combinedCorpus
          let combinedBigrams = getBigrams combinedCorpus
          putStrLn $ unwords $ take 1000 $ buildText combinedBigrams
          -}
          
          japterCorpus <- fmap (map (map toUpper)) $ getSimpleCorpus "japter_plain"
          putStrLn $ ppOccTable $ countWords $ japterCorpus
          putStrLn $ getText 1000 $ getBigrams $ japterCorpus

-- getBigrams ignores the data for the first and last word, why bother,
-- last one is likely just a dot or something similar anyway.
getBigrams :: [String] -> BigramMap
getBigrams = go M.empty
    where go m (wp:w:ws) = go (M.insertWith (\_ -> M.insertWith (+) w 1) wp (M.singleton w 1) m) (w:ws)
          go m _ = m

getText :: Int -> BigramMap -> String
getText n = tail . foldr accOp "" . take n . buildText
    where accOp el acc = if (el `elem` [",", ".", "!", "?", ")"]) || (not (null acc) && last acc == '(')
                         then acc ++ el
                         else acc ++ ' ':el -- TODO Use a difference-list-based approach or packaged builders for concat

buildText :: BigramMap -> [String]
buildText bigrams = evalState (next ".") (unsafePerformIO getStdGen)
    where next wp = do randomGen <- get
                       let (n, newGen) = randomR (-3.0, 0.0 :: Double) randomGen
                       put newGen
                       let w = fst
                             $ (\l ->  l !! (floor $ (fromIntegral $ min 10 (length l)) * exp n))
                             $ sortBy (comparing (Down . snd))
                             $ M.toList
                             $ fromJust
                             $ M.lookup wp bigrams
                       following <- next w
                       return (w : following)
