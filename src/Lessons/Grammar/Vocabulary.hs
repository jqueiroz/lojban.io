{-# LANGUAGE OverloadedStrings #-}
module Lessons.Grammar.Vocabulary
( Vocabulary
, VocabularyBuilder
, WordList
, vocabularyWords
, vocabularyCategorizedSumti
, vocabularyCategorizedSelbri
, gismuList
, cmavoList
, cmevlaList
, getVocabularySelbri
, getVocabularySumti
, createVocabularyBuilder
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

type VocabularyBuilder = Dictionary -> Vocabulary

data WordList = WordList
    { gismuList :: [Gismu]
    , cmavoList :: [Cmavo]
    , cmevlaList :: [T.Text]
    } deriving (Show)

getVocabularySelbri :: Vocabulary -> T.Text -> [T.Text]
getVocabularySelbri vocabulary key = M.findWithDefault [] key $ vocabularyCategorizedSelbri vocabulary

getVocabularySumti :: Vocabulary -> T.Text -> [T.Text]
getVocabularySumti vocabulary key = M.findWithDefault [] key $ vocabularyCategorizedSumti vocabulary

createVocabularyBuilder :: [T.Text] -> [T.Text] -> [T.Text] -> [(T.Text, [T.Text])] -> [(T.Text, [T.Text])] -> VocabularyBuilder
createVocabularyBuilder gismu cmavo cmevla selbri sumti dictionary = Vocabulary (WordList gismu' cmavo' cmevla) selbriMap sumtiMap where
    gismu' = map ((dictGismu dictionary) M.!) gismu
    cmavo' = map ((dictCmavo dictionary) M.!) cmavo
    selbriMap = M.fromList selbri
    sumtiMap = M.fromList sumti
