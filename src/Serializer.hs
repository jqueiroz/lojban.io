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
exerciseToJSON :: StdGen -> Exercise -> A.Value

exerciseToJSON r0 (MultipleChoiceExercise title sentence correctAlternatives incorrectAlternatives fixedOrdering) = A.object
    [ "type" A..= ("multiple-choice" :: T.Text)
    , "title" A..= title
    , "sentence" A..= (exerciseSentenceToJSON sentence)
    , "alternatives" A..= (if fixedOrdering then sort else shuffleList r0) (correctAlternatives ++ incorrectAlternatives)
    ]

exerciseToJSON r0 (SingleChoiceExercise title sentence correctAlternative incorrectAlternatives fixedOrdering) = A.object
    [ "type" A..= ("single-choice" :: T.Text)
    , "title" A..= title
    , "sentence" A..= (exerciseSentenceToJSON sentence)
    , "alternatives" A..= (if fixedOrdering then sort else shuffleList r0) (correctAlternative : incorrectAlternatives)
    ]

exerciseToJSON r0 (MatchingExercise title sentence items) = A.object
    [ "type" A..= ("matching" :: T.Text)
    , "title" A..= title
    , "sentence" A..= (exerciseSentenceToJSON sentence)
    , "left_items" A..= map fst items
    , "right_items" A..= shuffleList r0 (map snd items)
    ]

exerciseToJSON r0 (TypingExercise title sentence _ _) = A.object
    [ "type" A..= ("typing" :: T.Text)
    , "title" A..= title
    , "sentence" A..= (exerciseSentenceToJSON sentence)
    ]

exerciseSentenceToJSON :: Maybe ExerciseSentence -> A.Value
exerciseSentenceToJSON Nothing = A.Null
exerciseSentenceToJSON (Just (ExerciseSentence lojbanic text)) = A.object
    [ "text" A..= text
    , "lojbanic" A..= lojbanic
    ]

-- Deserialization of answers
data MultipleChoiceExerciseAnswer = MultipleChoiceExerciseAnswer {
    mceaCorrectAlternatives :: [T.Text]
}

data SingleChoiceExerciseAnswer = SingleChoiceExerciseAnswer {
    sceaCorrectAlternative :: T.Text
}

data MatchingExerciseAnswer = MatchingExerciseAnswer {
    meaOrderedAlternatives :: [T.Text]
}

data TypingExerciseAnswer = TypingExerciseAnswer {
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

validateExerciseAnswer (SingleChoiceExercise title sentence correctAlternative incorrectAlternatives fixedOrdering) s = do
    answer <- A.decode s
    return $ A.object
        [ ("correct", A.Bool $ sceaCorrectAlternative answer == correctAlternative)
        , ("correctAlternative", A.String correctAlternative)
        ]

validateExerciseAnswer (MatchingExercise title sentence items) s = do
    answer <- A.decode s
    return $ A.object
        [ ("correct", A.Bool $ meaOrderedAlternatives answer == map snd items)
        ]

validateExerciseAnswer (TypingExercise text sentence validate cannonicalAnswer) s = do
    answer <- A.decode s
    return $ A.object
        [ ("correct", A.Bool $ validate (teaText answer))
        , ("cannonicalAnswer", A.String $ cannonicalAnswer)
        ]
