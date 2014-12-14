module NGrams (
    countWords,
    ppOccTable,
    getBigrams,
    getText
    ) where

import Control.Monad.State.Lazy (evalState, get, put)
import Data.Char (toUpper)
import Data.List (sortBy)
import Data.Ord (comparing, Down(..))
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomR, getStdGen)
import Text.Printf

type BigramMap = M.Map String (M.Map String Int)

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
