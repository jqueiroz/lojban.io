{-# LANGUAGE OverloadedStrings #-}
module Core where

import qualified Data.Text as T
import qualified Data.Map as M

-- Dictionary
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
    , gismuTeachingCode :: T.Text
    , gismuOldFrequencyCount :: Int
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

-- Exercises
data ExerciseSentence = ExerciseSentence
    { esLojbanic :: Bool
    , esText :: T.Text
    } deriving (Show)

data Exercise = 
    MultipleChoiceExercise
        { mceTitle :: T.Text
        , mceSentence :: Maybe ExerciseSentence
        , mceCorrectAlternatives :: [T.Text]
        , mceIncorrectAlternatives :: [T.Text]
        , mceFixedOrdering :: Bool
        } |
    SingleChoiceExercise
        { sceTitle :: T.Text
        , sceSentence :: Maybe ExerciseSentence
        , sceCorrectAlternative :: T.Text
        , sceIncorrectAlternatives :: [T.Text]
        , sceFixedOrdering :: Bool
        } |
    MatchingExercise
        { mteTitle :: T.Text
        , mteSentence :: Maybe ExerciseSentence
        , mteItems :: [(T.Text, T.Text)]
        } |
    TypingExercise
        { tpeTitle :: T.Text
        , tpeSentence :: Maybe ExerciseSentence
        , tpeValidate :: T.Text -> Bool
        }

instance Show Exercise where
    show (MultipleChoiceExercise title sentence correctAlternatives incorrectAlternatives fixedOrdering) = "MultipleChoiceExercise { mceTitle = " ++ (show title) ++ ", mceSentence = " ++ (show sentence) ++ ", mceCorrectAlternatives = " ++ (show correctAlternatives) ++ ", mceIncorrectAlternatives = " ++ (show incorrectAlternatives) ++ ", fixedOrdering = " ++ (show fixedOrdering) ++ "}"
    show (SingleChoiceExercise title sentence correctAlternative incorrectAlternatives fixedOrdering) = "SingleChoiceExercise { sceTitle = " ++ (show title) ++ ", sceSentence = " ++ (show sentence) ++ ", sceCorrectAlternatives = " ++ (show correctAlternative) ++ ", sceIncorrectAlternatives = " ++ (show incorrectAlternatives) ++ ", fixedOrdering = " ++ (show fixedOrdering) ++ "}"
    show (MatchingExercise title sentence items) = "MatchingExercise { mteTitle = " ++ (show title) ++ ", mteSentence = " ++ (show sentence) ++ ", mteItems = " ++ (show items) ++ "}"
    show (TypingExercise title sentence _) = "TypingExercise {tpeTitle = " ++ (show title) ++ "tpeSentence = " ++ (show sentence) ++ "}"
