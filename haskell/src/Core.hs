{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Core where

import System.Random (StdGen)
import Language.Lojban.Core (Dictionary)
import qualified Data.Text as T
import qualified Text.Pandoc as P
import qualified Data.Map as M

-- * Dictionary
type WordGenerator = StdGen -> (T.Text, StdGen)

-- * Courses
data CourseStore = CourseStore
    { courseStoreCollections :: M.Map T.Text CourseCollection
    , courseStoreCourses :: M.Map T.Text Course
    }

data CourseCollection = CourseCollection
    { courseCollectionId :: T.Text
    , courseCollectionCourses :: [Course]
    }

data Course = Course
    { courseId :: T.Text
    , courseTitle :: T.Text
    , courseShortDescription :: T.Text
    , courseLongDescription :: Maybe P.Pandoc
    , courseCredits :: Maybe P.Pandoc
    , courseStyle :: CourseStyle
    , courseDictionary :: Dictionary
    , courseLessons :: [Lesson]
    } deriving (Show)

data CourseStyle = CourseStyle
    { courseStyleColor1 :: Maybe String
    , courseStyleIconUrl :: Maybe String
    } deriving (Show)

data Lesson = Lesson
    { lessonTitle :: T.Text
    , lessonExercises :: ExerciseGenerator
    , lessonLecture :: Maybe P.Pandoc
    , lessonPlan :: Maybe P.Pandoc
    , lessonVocabulary :: Maybe Vocabulary
    }

data Vocabulary = Vocabulary
    { vocabularyBrivlaList :: [T.Text]
    , vocabularyCmavoList :: [T.Text]
    , vocabularyCmevlaList :: [T.Text]
    } deriving (Show)

instance Semigroup Vocabulary where
    (<>) vocabulary1 vocabulary2 = Vocabulary brivla cmavo cmevla where
        brivla = (vocabularyBrivlaList vocabulary1) ++ (vocabularyBrivlaList vocabulary2)
        cmavo = (vocabularyCmavoList vocabulary1) ++ (vocabularyCmavoList vocabulary2)
        cmevla = (vocabularyCmevlaList vocabulary1) ++ (vocabularyCmevlaList vocabulary2)

instance Show Lesson where
    show lesson = "Lesson { title = " ++ show (lessonTitle lesson) ++ " }"

-- * Decks
data DeckStore = DeckStore
    { deckStoreDecks :: M.Map T.Text Deck
    }

data Deck = Deck
    { deckId :: T.Text
    , deckTitle :: T.Text
    , deckShortDescription :: T.Text
    , deckLongDescription :: Maybe P.Pandoc
    , deckCredits :: Maybe P.Pandoc
    , deckDictionary :: Dictionary
    , deckCards :: [Card]
    }

data Card = Card
    { cardTitle :: T.Text
    , cardShortDescription :: T.Text
    , cardExercises :: ExerciseGenerator
    }

-- * Translations
type Translation = ([LojbanSentence], [EnglishSentence])
type TranslationGenerator = StdGen -> (Translation, StdGen)
type EnglishSentence = T.Text
type LojbanSentence = T.Text

type TranslationsByExpression = [(T.Text, [Translation])]
type TranslationGeneratorByExpression = [(T.Text, TranslationGenerator)]

type SentenceComparer = LojbanSentence -> LojbanSentence -> Bool

-- * Exercises
data Exercise =
    MultipleChoiceExercise
        { mceTitle :: T.Text
        , mceSentences :: [ExerciseSentence]
        , mceCorrectAlternatives :: [T.Text]
        , mceIncorrectAlternatives :: [T.Text]
        , mceFixedOrdering :: Bool
        } |
    SingleChoiceExercise
        { sceTitle :: T.Text
        , sceSentences :: [ExerciseSentence]
        , sceCorrectAlternative :: T.Text
        , sceIncorrectAlternatives :: [T.Text]
        , sceFixedOrdering :: Bool
        } |
    MatchingExercise
        { mteTitle :: T.Text
        , mteSentences :: [ExerciseSentence]
        , mteItems :: [(T.Text, T.Text)]
        } |
    TypingExercise
        { tpeTitle :: T.Text
        , tpeSentences :: [ExerciseSentence]
        , tpeValidate :: T.Text -> Bool
        , tpeCanonicalAnswer :: T.Text
        }

type ExerciseGenerator = StdGen -> Exercise
type MaybeExerciseGenerator = StdGen -> (Maybe Exercise, StdGen)

instance Show Exercise where
    show (MultipleChoiceExercise title sentences correctAlternatives incorrectAlternatives fixedOrdering) = "MultipleChoiceExercise { mceTitle = " ++ (show title) ++ ", mceSentences = " ++ (show sentences) ++ ", mceCorrectAlternatives = " ++ (show correctAlternatives) ++ ", mceIncorrectAlternatives = " ++ (show incorrectAlternatives) ++ ", fixedOrdering = " ++ (show fixedOrdering) ++ "}"
    show (SingleChoiceExercise title sentences correctAlternative incorrectAlternatives fixedOrdering) = "SingleChoiceExercise { sceTitle = " ++ (show title) ++ ", sceSentences = " ++ (show sentences) ++ ", sceCorrectAlternatives = " ++ (show correctAlternative) ++ ", sceIncorrectAlternatives = " ++ (show incorrectAlternatives) ++ ", fixedOrdering = " ++ (show fixedOrdering) ++ "}"
    show (MatchingExercise title sentences items) = "MatchingExercise { mteTitle = " ++ (show title) ++ ", mteSentences = " ++ (show sentences) ++ ", mteItems = " ++ (show items) ++ "}"
    show (TypingExercise title sentences _ canonicalAnswer) = "TypingExercise {tpeTitle = " ++ (show title) ++ ", tpeSentences = " ++ (show sentences) ++ ", canonicalAnswer = " ++ (show canonicalAnswer) ++ "}"

data ExerciseSentence = ExerciseSentence
    { esLojbanic :: Bool
    , esText :: T.Text
    } deriving (Show)
