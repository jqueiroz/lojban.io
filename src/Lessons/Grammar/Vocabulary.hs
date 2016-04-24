{-# LANGUAGE OverloadedStrings #-}
module Lessons.Grammar.Vocabulary
( Vocabulary
, WordList
, vocabularyWords
, vocabularyCategorizedSumti
, vocabularyCategorizedSelbri
, gismuList
, cmavoList
, cmevlaList
, getVocabularySelbri
, getVocabularySumti
, buildVocabulary
) where

import Core
import qualified Data.Text as T
import qualified Data.Map as M

--TODO: write function to validate vocabulary
data Vocabulary = Vocabulary
    { vocabularyWords :: WordList
    , vocabularyCategorizedSelbri :: M.Map T.Text [T.Text]
    , vocabularyCategorizedSumti :: M.Map T.Text [T.Text]
    }

data WordList = WordList
    { gismuList :: [Gismu]
    , cmavoList :: [Cmavo]
    , cmevlaList :: [T.Text]
    } deriving (Show)

getVocabularySelbri :: Vocabulary -> T.Text -> [T.Text]
getVocabularySelbri vocabulary key = M.findWithDefault [] key $ vocabularyCategorizedSelbri vocabulary

getVocabularySumti :: Vocabulary -> T.Text -> [T.Text]
getVocabularySumti vocabulary key = M.findWithDefault [] key $ vocabularyCategorizedSumti vocabulary

buildVocabulary :: Dictionary -> [T.Text] -> [T.Text] -> [T.Text] -> [(T.Text, [T.Text])] -> [(T.Text, [T.Text])] -> Vocabulary
buildVocabulary dictionary gismu cmavo cmevla selbri sumti = Vocabulary (WordList gismu' cmavo' cmevla) selbriMap sumtiMap where
    gismu' = map ((dictGismu dictionary) M.!) gismu
    cmavo' = map ((dictCmavo dictionary) M.!) cmavo
    selbriMap = M.fromList selbri
    sumtiMap = M.fromList sumti
