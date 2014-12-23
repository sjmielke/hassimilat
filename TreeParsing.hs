module TreeParsing ( SNode(NT, T)
                   , tag
                   , countRules
                   , countCallers
                   , simplifySentenceTree
                   , simpleParseNegra
                   ) where

import NGrams (beautifulUnwords)

import Vanda.Corpus.Negra
import Vanda.Corpus.Negra.Text (parseNegra)

import Data.Text.Lazy (Text)
import Data.Tree (Tree(Node), rootLabel, drawTree, flatten)

import Control.DeepSeq (deepseq)
import qualified Data.Foldable as F
import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- | Heavily simplified sentence tree with non-/terminals,
-- where a terminal carries both the POS tag and specific word.
data SNode = NT {tag::String} | T {tag::String, word::String} deriving Show

simplifySentenceTree :: Tree (Maybe SentenceData) -> Tree SNode
simplifySentenceTree = fmap fnc
    where fnc Nothing = NT "ROOT"
          fnc (Just (SentenceWord w p m _ _ _)) = T p w
          fnc (Just (SentenceNode _ p m _ _ _)) = NT p

simpleParseNegra :: Text -> [Tree (Maybe SentenceData)]
simpleParseNegra = map getTree . sentences . parseNegra

getTree :: Sentence -> Tree (Maybe SentenceData)
getTree = -- Fun fact: in the corpus I use the splitting never seems to
          -- take place since the returned forest always only contains
          -- one element! Awesome!
          -- Or (more likely) I don't get how it actually works.
          fmap (fst . fst)
        . head
        . negraToForest
        . sData

flattenTree :: Tree (Maybe SentenceData) -> String
flattenTree = beautifulUnwords . foldr fnc [] . flatten
    where fnc Nothing = id
          fnc (Just (SentenceWord w p m _ _ _)) = (++[w])
          fnc (Just (SentenceNode _ p m _ _ _)) = id

getUsedTags :: [Tree SNode] -> M.Map String (Bool, Bool) -- name, used in inner node (NT), used in leaf (T)
getUsedTags = F.foldl' (F.foldl' ins)
                       (M.empty :: M.Map String (Bool, Bool))
    where ins tagmap (NT tag) = M.insertWith (\_ (nt,t) -> (True,t)) tag (True, False) tagmap
          ins tagmap (T tag _) = M.insertWith (\_ (nt,t) -> (nt,True)) tag (False, True) tagmap

-- CFGish shenanigans.

type Rule = (String, [String])

getRules :: [Tree SNode] -> S.Set Rule
getRules = F.foldl' readoff (S.empty :: S.Set Rule)
    where readoff ruleset (Node (NT rootTag) children) =
                let newrule = (rootTag, map (tag . rootLabel) children)
                in newrule `deepseq`
                   F.foldl' readoff (S.insert newrule ruleset) children
          readoff ruleset _ = ruleset

countRules :: [Tree SNode] -> M.Map Rule Int
countRules = F.foldl' readoff (M.empty :: M.Map Rule Int)
    where readoff rulemap (Node (NT rootTag) children) =
                let newrule = (rootTag, map (tag . rootLabel) children)
                in newrule `deepseq`
                   F.foldl' readoff (M.insertWith (+) newrule 1 rulemap) children
          readoff rulemap _ = rulemap

countCallers :: String -> M.Map Rule Int -> M.Map String Int
countCallers callee = M.foldlWithKey' ( \ oldresmap rule occ
                                       -> M.insertWith (+)
                                                       (fst rule)
                                                       occ
                                                       oldresmap )
                               (M.empty :: M.Map String Int)
                    . M.filterWithKey (\rule _ -> callee `elem` snd rule)
