-- module NGrams where

import CorpusParsing

import Control.Monad.State.Lazy (evalState, get, put)
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
          
          fsrCorpus <- getSimpleCorpus
          putStrLn $ ppOccTable $ countWords $ fsrCorpus
          let fsrBigrams = getBigrams fsrCorpus
          putStrLn "Sample FSR"
          putStrLn $ unwords $ take 1000 $ buildText fsrBigrams

-- getBigrams ignores the data for the first and last word, why bother,
-- last one is likely just a dot or something similar anyway.
getBigrams :: [String] -> BigramMap
getBigrams = go M.empty
    where go m (wp:w:ws) = go (M.insertWith (\_ -> M.insertWith (+) w 1) wp (M.singleton w 1) m) (w:ws)
          go m _ = m

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
