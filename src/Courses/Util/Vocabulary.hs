{-# LANGUAGE OverloadedStrings #-}

module Courses.Util.Vocabulary
( Vocabulary
, VocabularyBuilder
, WordList
, Selbri
, Sumti
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

import Language.Lojban.Core
import Util (sortUniq, (?:))
import qualified Data.Text as T
import qualified Data.Map as M

--TODO: write function to validate vocabulary

-- Parameter aliases
type CategoryName = T.Text
type Selbri = T.Text
type Sumti = T.Text
type CategorizedSelbri = [(CategoryName, [(Int, Selbri)])]
type CategorizedSumti = [(CategoryName, [(Int, Sumti)])]

-- Data types
type VocabularyBuilder = Dictionary -> Vocabulary
data Vocabulary = Vocabulary
    { vocabularyWords :: WordList
    , vocabularyCategorizedSelbri :: M.Map CategoryName [(Int, Selbri)]
    , vocabularyCategorizedSumti :: M.Map CategoryName [(Int, Sumti)]
    }

data WordList = WordList
    { gismuList :: [Gismu]
    , cmavoList :: [Cmavo]
    , cmevlaList :: [T.Text]
    } deriving (Show)

-- Auxiliary functions
getVocabularySelbri :: Vocabulary -> CategoryName -> [(Int, Selbri)]
getVocabularySelbri vocabulary key = M.findWithDefault [] key $ vocabularyCategorizedSelbri vocabulary

getVocabularySumti :: Vocabulary -> CategoryName -> [(Int, Sumti)]
getVocabularySumti vocabulary key = M.findWithDefault [] key $ vocabularyCategorizedSumti vocabulary

createVocabularyBuilder :: CategorizedSelbri -> CategorizedSumti -> VocabularyBuilder
createVocabularyBuilder selbri sumti dictionary = Vocabulary (WordList gismu' cmavo' []) selbriMap sumtiMap where
    selbriGismu = sortUniq . map snd . M.foldr (++) [] $ selbriMap
    sumtiWords = sortUniq . concatMap (T.words . snd) . M.foldr (++) [] $ sumtiMap
    sumtiGismu = filter (`M.member` (dictGismu dictionary)) sumtiWords
    sumtiCmavo = filter (`M.member` (dictCmavo dictionary)) sumtiWords
    gismu = sortUniq $ selbriGismu ++ sumtiGismu
    cmavo = "zo'e" ?: sumtiCmavo
    gismu' = map ((dictGismu dictionary) M.!) gismu
    cmavo' = map ((dictCmavo dictionary) M.!) cmavo
    selbriMap = M.fromList selbri
    sumtiMap = M.fromList sumti
