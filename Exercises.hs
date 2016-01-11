{-# LANGUAGE OverloadedStrings #-}
module Exercises where

import Core
import Util (shuffleList)
import Control.Applicative ((<$>), (<*>))
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text as T
import qualified Data.Aeson as A
import Data.List (sort)
import System.Random (StdGen)

exerciseToJSON :: StdGen -> Exercise -> A.Value

exerciseToJSON r0 (MultipleChoiceExercise text correctAlternatives incorrectAlternatives fixedOrdering) = A.object
    [ "type" A..= ("multiple-choice" :: T.Text)
    , "text" A..= text
    , "alternatives" A..= (if fixedOrdering then id else shuffleList r0) (correctAlternatives ++ incorrectAlternatives)
    ]

exerciseToJSON r0 (SingleChoiceExercise text correctAlternative incorrectAlternatives fixedOrdering) = A.object
    [ "type" A..= ("single-choice" :: T.Text)
    , "text" A..= text
    , "alternatives" A..= (if fixedOrdering then id else shuffleList r0) (correctAlternative : incorrectAlternatives)
    ]

exerciseToJSON r0 (MatchingExercise text items) = A.object
    [ "type" A..= ("matching" :: T.Text)
    , "text" A..= text
    , "left_items" A..= map fst items
    , "right_items" A..= shuffleList r0 (map snd items)
    ]

exerciseToJSON r0 (TypingExercise text _) = A.object
    [ "type" A..= ("typing" :: T.Text)
    , "text" A..= text
    ]

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

validateAnswer :: Exercise -> BS.ByteString -> Maybe A.Value

{-validateAnswer (MultipleChoiceExercise text correctAlternatives incorrectAlternatives)  s = do-}
    {-answer <- A.decode s-}
    {-return $ (sort $ mceaCorrectAlternatives answer) == (sort correctAlternatives)-}

validateAnswer (SingleChoiceExercise text correctAlternative incorrectAlternatives fixedOrdering) s = do
    answer <- A.decode s
    return $ A.object
        [ ("correct", A.Bool $ sceaCorrectAlternative answer == correctAlternative)
        , ("correctAlternative", A.String correctAlternative)
        ]

validateAnswer (MatchingExercise text items) s = do
    answer <- A.decode s
    return $ A.object
        [ ("correct", A.Bool $ meaOrderedAlternatives answer == map snd items)
        ]

{-validateAnswer (TypingExercise text validate) s = do-}
    {-answer <- A.decode s-}
    {-return $ validate (teaText answer)-}
