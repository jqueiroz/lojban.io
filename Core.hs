{-# LANGUAGE OverloadedStrings #-}
module Core where

import qualified Data.Text as T
import qualified Data.Map as M

-- Vocabulary
data Vocabulary = Vocabulary
    { vocabularyWords :: WordList
    , vocabularyCategorizedSelbri :: M.Map T.Text [T.Text]
    , vocabularyCategorizedSumti :: M.Map T.Text [T.Text]
    } --TODO: write function to validate vocabulary
    --TODO: replace functions with map

getVocabularySelbri :: Vocabulary -> T.Text -> [T.Text]
getVocabularySelbri vocabulary key = M.findWithDefault [] key $ vocabularyCategorizedSelbri vocabulary

getVocabularySumti :: Vocabulary -> T.Text -> [T.Text]
getVocabularySumti vocabulary key = M.findWithDefault [] key $ vocabularyCategorizedSumti vocabulary

data WordList = WordList
    { gismuList :: [Gismu]
    , cmavoList :: [Cmavo]
    , cmevlaList :: [T.Text]
    } deriving (Show)

data Dictionary = Dictionary
    { dictGismu :: M.Map T.Text Gismu
    , dictCmavo :: M.Map T.Text Cmavo
    } deriving (Show)

data Gismu = Gismu
    { gismuText :: T.Text
    , gismuRafsi :: [T.Text]
    , gismuEnglishPlaces :: [T.Text]
    , gismuEnglishKeywords :: [T.Text]
    , gismuEnglishDefinition :: T.Text
    , gismuEnglishNotes :: T.Text
    , gismuConfer :: [T.Text]
    } deriving (Show)

data Cmavo = Cmavo
    { cmavoText :: T.Text
    , cmavoEnglishClassification :: T.Text
    , cmavoEnglishKeyword :: T.Text
    , cmavoEnglishDefinition :: T.Text
    , cmavoEnglishNotes :: T.Text
    , cmavoEnglishConfer :: [T.Text]
    } deriving (Show)

instance Eq Gismu where
    x == y = (gismuText x) == (gismuText y)

instance Eq Cmavo where
    x == y = (cmavoText x) == (cmavoText y)

buildVocabulary :: Dictionary -> [T.Text] -> [T.Text] -> [T.Text] -> [(T.Text, [T.Text])] -> [(T.Text, [T.Text])] -> Vocabulary
buildVocabulary dictionary gismu cmavo cmevla selbri sumti = Vocabulary (WordList gismu' cmavo' cmevla) selbriMap sumtiMap where
    gismu' = map ((dictGismu dictionary) M.!) gismu
    cmavo' = map ((dictCmavo dictionary) M.!) cmavo
    selbriMap = M.fromList selbri
    sumtiMap = M.fromList sumti

-- Exercises
data Exercise = 
    MultipleChoiceExercise
        { mceText :: T.Text
        , mceCorrectAlternatives :: [T.Text]
        , mceIncorrectAlternatives :: [T.Text]
        , mceFixedOrdering :: Bool
        } |
    SingleChoiceExercise
        { sceText :: T.Text
        , sceCorrectAlternative :: T.Text
        , sceIncorrectAlternatives :: [T.Text]
        , sceFixedOrdering :: Bool
        } |
    MatchingExercise
        { mteText :: T.Text
        , mteItems :: [(T.Text, T.Text)]
        } |
    TypingExercise
        { tpeText :: T.Text
        , tpeValidate :: T.Text -> Bool
        }

instance Show Exercise where
    show (MultipleChoiceExercise text correctAlternatives incorrectAlternatives fixedOrdering) = "MultipleChoiceExercise { mceText = " ++ (show text) ++ ", mceCorrectAlternatives = " ++ (show correctAlternatives) ++ ", mceIncorrectAlternatives = " ++ (show incorrectAlternatives) ++ ", fixedOrdering = " ++ (show fixedOrdering) ++ "}"
    show (SingleChoiceExercise text correctAlternative incorrectAlternatives fixedOrdering) = "SingleChoiceExercise { sceText = " ++ (show text) ++ ", sceCorrectAlternatives = " ++ (show correctAlternative) ++ ", sceIncorrectAlternatives = " ++ (show incorrectAlternatives) ++ ", fixedOrdering = " ++ (show fixedOrdering) ++ "}"
    show (MatchingExercise text items) = "MatchingExercise { mteText = " ++ (show text) ++ ", mteItems = " ++ (show items) ++ "}"
    show (TypingExercise text _) = "TypingExercise {tpeText = " ++ (show text) ++ "}"
