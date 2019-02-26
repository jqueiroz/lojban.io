{-# LANGUAGE OverloadedStrings #-}
module Core where

import System.Random (StdGen)
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Text.Pandoc as P

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
    , gismuIRCFrequencyCount :: Int
    } deriving (Show)

data Cmavo = Cmavo
    { cmavoText :: T.Text
    , cmavoEnglishClassification :: T.Text
    , cmavoEnglishKeyword :: T.Text
    , cmavoEnglishDefinition :: T.Text
    , cmavoEnglishNotes :: T.Text
    , cmavoEnglishConfer :: [T.Text]
    , cmavoIRCFrequencyCount :: Int
    } deriving (Show)

instance Eq Gismu where
    x == y = (gismuText x) == (gismuText y)

instance Eq Cmavo where
    x == y = (cmavoText x) == (cmavoText y)

-- Course
type CourseBuilder = Dictionary -> Course
data Course = Course
    { courseTitle :: String
    , courseLessons :: [Lesson]
    } deriving (Show)

type LessonBuilder = Dictionary -> Lesson
data Lesson = Lesson
    { lessonTitle :: String
    , lessonExercises :: ExerciseGenerator
    , lessonPlan :: P.Pandoc
    }

instance Show Lesson where
    show lesson = "Lesson { title = " ++ show (lessonTitle lesson) ++ " }"

createCourseBuilder :: String -> [LessonBuilder] -> CourseBuilder
createCourseBuilder title lessons dictionary = Course title (lessons <*> [dictionary])

-- Translations
type Translation = ([LojbanSentence], [EnglishSentence])
type TranslationGenerator = StdGen -> (Translation, StdGen)
type EnglishSentence = T.Text
type LojbanSentence = T.Text

type TranslationsByExpression = [(T.Text, [Translation])]
type TranslationGeneratorByExpression = [(T.Text, TranslationGenerator)]

-- Exercises
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
        , tpeCanonicalAnswer :: T.Text
        }

type ExerciseGenerator = StdGen -> Exercise

instance Show Exercise where
    show (MultipleChoiceExercise title sentence correctAlternatives incorrectAlternatives fixedOrdering) = "MultipleChoiceExercise { mceTitle = " ++ (show title) ++ ", mceSentence = " ++ (show sentence) ++ ", mceCorrectAlternatives = " ++ (show correctAlternatives) ++ ", mceIncorrectAlternatives = " ++ (show incorrectAlternatives) ++ ", fixedOrdering = " ++ (show fixedOrdering) ++ "}"
    show (SingleChoiceExercise title sentence correctAlternative incorrectAlternatives fixedOrdering) = "SingleChoiceExercise { sceTitle = " ++ (show title) ++ ", sceSentence = " ++ (show sentence) ++ ", sceCorrectAlternatives = " ++ (show correctAlternative) ++ ", sceIncorrectAlternatives = " ++ (show incorrectAlternatives) ++ ", fixedOrdering = " ++ (show fixedOrdering) ++ "}"
    show (MatchingExercise title sentence items) = "MatchingExercise { mteTitle = " ++ (show title) ++ ", mteSentence = " ++ (show sentence) ++ ", mteItems = " ++ (show items) ++ "}"
    show (TypingExercise title sentence _ canonicalAnswer) = "TypingExercise {tpeTitle = " ++ (show title) ++ ", tpeSentence = " ++ (show sentence) ++ ", canonicalAnswer = " ++ (show canonicalAnswer) ++ "}"

data ExerciseSentence = ExerciseSentence
    { esLojbanic :: Bool
    , esText :: T.Text
    } deriving (Show)
