{-# LANGUAGE OverloadedStrings #-}
module Courses.Util.Vocabulary
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
import Util (sortUniq, (?:))
import qualified Data.Text as T
import qualified Data.Map as M

--TODO: write function to validate vocabulary

-- Parameter aliases
type CategoryName = T.Text
type Selbri = T.Text
type Sumti = T.Text
type CategorizedSelbri = [(CategoryName, [Selbri])]
type CategorizedSumti = [(CategoryName, [Sumti])]

-- Data types
type VocabularyBuilder = Dictionary -> Vocabulary
data Vocabulary = Vocabulary
    { vocabularyWords :: WordList
    , vocabularyCategorizedSelbri :: M.Map CategoryName [Selbri]
    , vocabularyCategorizedSumti :: M.Map CategoryName [Sumti]
    }

data WordList = WordList
    { gismuList :: [Gismu]
    , cmavoList :: [Cmavo]
    , cmevlaList :: [T.Text]
    } deriving (Show)

-- Auxiliary functions
getVocabularySelbri :: Vocabulary -> CategoryName -> [Selbri]
getVocabularySelbri vocabulary key = M.findWithDefault [] key $ vocabularyCategorizedSelbri vocabulary

getVocabularySumti :: Vocabulary -> CategoryName -> [Sumti]
getVocabularySumti vocabulary key = M.findWithDefault [] key $ vocabularyCategorizedSumti vocabulary

createVocabularyBuilder :: CategorizedSelbri -> CategorizedSumti -> VocabularyBuilder
createVocabularyBuilder selbri sumti dictionary = Vocabulary (WordList gismu' cmavo' []) selbriMap sumtiMap where
    selbriGismu = sortUniq . M.foldr (++) [] $ selbriMap
    sumtiWords = sortUniq . concat . map T.words . M.foldr (++) [] $ sumtiMap
    sumtiGismu = filter (`M.member` (dictGismu dictionary)) sumtiWords
    sumtiCmavo = filter (`M.member` (dictCmavo dictionary)) sumtiWords
    gismu = sortUniq $ selbriGismu ++ sumtiGismu
    cmavo = "zo'e" ?: sumtiCmavo
    gismu' = map ((dictGismu dictionary) M.!) gismu
    cmavo' = map ((dictCmavo dictionary) M.!) cmavo
    selbriMap = M.fromList selbri
    sumtiMap = M.fromList sumti
