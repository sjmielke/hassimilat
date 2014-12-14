module MyCorpusData where

import CorpusParsing
import TreeParsing

import Vanda.Corpus.Negra (SentenceData)

import qualified Data.Text.Lazy.IO as TIO
import Data.Tree (Tree)
import Text.XML.Light (Element(..), Attr(..), unqual)

data WordListCorpusName = FSR | Richard | JapTer | Mueller | Schmid
data TreeCorpusName = Tiger

getWordListCorpus :: WordListCorpusName -> IO [String]
getWordListCorpus FSR = getSimpleCorpus "data/fsrtexte"
getWordListCorpus Richard = getSimpleCorpus "data/richardscorpus"
getWordListCorpus Mueller = fmap ( cleanCorpusFiltering
                                   (\(Element n _ _ _) -> n == unqual "p") )
                          $ readFile "data/mueller_clean_orig_input.html"
getWordListCorpus Schmid = fmap ( cleanCorpusFiltering
                                  (\(Element _ as _ _) -> as == [Attr (unqual "align") "justify"]) )
                         $ readFile "data/schmid_clean_orig_input.html"
getWordListCorpus JapTer = getSimpleCorpus "data/japter_plain"

getTreeCorpus :: TreeCorpusName -> IO [Tree (Maybe SentenceData)]
getTreeCorpus Tiger = fmap simpleParseNegra $ TIO.readFile "data/tiger_release_aug07.export"
