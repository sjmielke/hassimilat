module TreeParsing where

import NGrams (beautifulUnwords)

import Vanda.Corpus.Negra
import Vanda.Corpus.Negra.Text (parseNegra)
import Data.Text.Lazy (Text)
import Data.Tree (Tree, drawTree, flatten)
import Debug.Trace (trace)

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
