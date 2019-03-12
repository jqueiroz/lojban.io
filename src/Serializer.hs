{-# LANGUAGE OverloadedStrings #-}
module Serializer
( exerciseToJSON
, validateExerciseAnswer
) where

import Core
import Util (shuffleList)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text as T
import qualified Data.Aeson as A
import Data.List (sort)
import System.Random (StdGen)

-- Serialization of exercises
exerciseToJSON :: Exercise -> StdGen -> A.Value

exerciseToJSON (MultipleChoiceExercise title sentences correctAlternatives incorrectAlternatives fixedOrdering) r0 = A.object
    [ "type" A..= ("multiple-choice" :: T.Text)
    , "title" A..= title
    , "sentences" A..= (map exerciseSentenceToJSON sentences)
    , "alternatives" A..= (if fixedOrdering then sort else shuffleList r0) (correctAlternatives ++ incorrectAlternatives)
    ]

exerciseToJSON (SingleChoiceExercise title sentences correctAlternative incorrectAlternatives fixedOrdering) r0 = A.object
    [ "type" A..= ("single-choice" :: T.Text)
    , "title" A..= title
    , "sentences" A..= (map exerciseSentenceToJSON sentences)
    , "alternatives" A..= (if fixedOrdering then sort else shuffleList r0) (correctAlternative : incorrectAlternatives)
    ]

exerciseToJSON (MatchingExercise title sentences items) r0 = A.object
    [ "type" A..= ("matching" :: T.Text)
    , "title" A..= title
    , "sentences" A..= (map exerciseSentenceToJSON sentences)
    , "left_items" A..= map fst items
    , "right_items" A..= shuffleList r0 (map snd items)
    ]

exerciseToJSON (TypingExercise title sentences _ _) r0 = A.object
    [ "type" A..= ("typing" :: T.Text)
    , "title" A..= title
    , "sentences" A..= (map exerciseSentenceToJSON sentences)
    ]

exerciseSentenceToJSON :: ExerciseSentence -> A.Value
exerciseSentenceToJSON (ExerciseSentence lojbanic text) = A.object
    [ "text" A..= text
    , "lojbanic" A..= lojbanic
    ]

-- Deserialization of answers
newtype MultipleChoiceExerciseAnswer = MultipleChoiceExerciseAnswer {
    mceaCorrectAlternatives :: [T.Text]
}

newtype SingleChoiceExerciseAnswer = SingleChoiceExerciseAnswer {
    sceaCorrectAlternative :: T.Text
}

newtype MatchingExerciseAnswer = MatchingExerciseAnswer {
    meaOrderedAlternatives :: [T.Text]
}

newtype TypingExerciseAnswer = TypingExerciseAnswer {
    teaText :: T.Text
}

instance A.FromJSON MultipleChoiceExerciseAnswer where
    parseJSON (A.Object v) = MultipleChoiceExerciseAnswer
        <$> v A..: "correctAlternatives"

instance A.FromJSON SingleChoiceExerciseAnswer where
    parseJSON (A.Object v) = SingleChoiceExerciseAnswer
        <$> v A..: "correctAlternative"

instance A.FromJSON MatchingExerciseAnswer where
    parseJSON (A.Object v) = MatchingExerciseAnswer
         <$> v A..: "orderedAlternatives"

instance A.FromJSON TypingExerciseAnswer where
    parseJSON (A.Object v) = TypingExerciseAnswer
        <$> v A..: "text"

-- Validation of answers
validateExerciseAnswer :: Exercise -> BS.ByteString -> Maybe A.Value

{-validateExerciseAnswer (MultipleChoiceExercise text correctAlternatives incorrectAlternatives)  s = do-}
    {-answer <- A.decode s-}
    {-return $ (sort $ mceaCorrectAlternatives answer) == (sort correctAlternatives)-}

validateExerciseAnswer (SingleChoiceExercise title sentences correctAlternative incorrectAlternatives fixedOrdering) s = do
    answer <- A.decode s
    return $ A.object
        [ ("correct", A.Bool $ sceaCorrectAlternative answer == correctAlternative)
        , ("correctAlternative", A.String correctAlternative)
        ]

validateExerciseAnswer (MatchingExercise title sentences items) s = do
    answer <- A.decode s
    return $ A.object
        [ ("correct", A.Bool $ meaOrderedAlternatives answer == map snd items)
        ]

validateExerciseAnswer (TypingExercise title sentences validate canonicalAnswer) s = do
    answer <- A.decode s
    return $ A.object
        [ ("correct", A.Bool $ validate (teaText answer))
        , ("canonicalAnswer", A.String $ canonicalAnswer)
        ]
