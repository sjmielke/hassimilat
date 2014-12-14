module MyCorpusData where

import CorpusParsing

import Text.XML.Light (Element(..), Attr(..), unqual)

data WordListCorpusName = FSR | Richard | JapTer | Mueller | Schmid

getWordListCorpus :: WordListCorpusName -> IO [String]
getWordListCorpus FSR = getSimpleCorpus "data/fsrtexte"
getWordListCorpus Richard = getSimpleCorpus "data/richardscorpus"
getWordListCorpus Mueller = fmap (cleanCorpusFiltering (\(Element n _ _ _) -> n == unqual "p"))
                          $ readFile "data/mueller_clean_orig_input.html"
getWordListCorpus Schmid = fmap (cleanCorpusFiltering (\(Element _ as _ _) -> as == [Attr (unqual "align") "justify"]))
                         $ readFile "data/schmid_clean_orig_input.html"
getWordListCorpus JapTer = getSimpleCorpus "data/japter_plain"
