{-# LANGUAGE OverloadedStrings #-}

-- | This module defines the course exercuses.
module Courses.English.Vocabulary.Attitudinals.Exercises where

import Core
import Util (chooseItemUniformly, combineFunctionsUniformly)
import Courses.English.Vocabulary.Attitudinals.Model
import Courses.English.Vocabulary.Attitudinals.Vocabulary
import System.Random.Shuffle (shuffle')
import qualified Data.Text as T

generatePositiveAttitudinalBackwardMeaningExercise :: [Attitudinal] -> ExerciseGenerator
generatePositiveAttitudinalBackwardMeaningExercise attitudinals r0 = TypingExercise title [] (== expectedAttitudinal) (expectedAttitudinal) where
    (attitudinal, r1) = chooseItemUniformly r0 attitudinals
    title = "Provide the attitudinal for <b>" `T.append` (attitudinalPositiveMeaning attitudinal) `T.append` "</b>"
    expectedAttitudinal = attitudinalWord attitudinal

generatePositiveAttitudinalForwardMeaningExercise :: [Attitudinal] -> ExerciseGenerator
generatePositiveAttitudinalForwardMeaningExercise attitudinals r0 = SingleChoiceExercise title [] correctMeaning incorrectMeanings False where
    (correctAttitudinal, r1) = chooseItemUniformly r0 attitudinals
    incorrectAttitudinals = filter (/= correctAttitudinal) attitudinals
    shuffledIncorrectAttitudinals = shuffle' incorrectAttitudinals (length incorrectAttitudinals) r1
    title = "Select the meaning of <b>" `T.append` (attitudinalWord correctAttitudinal) `T.append` "</b>"
    correctMeaning = attitudinalPositiveMeaning correctAttitudinal
    incorrectMeanings = map attitudinalPositiveMeaning (take 4 incorrectAttitudinals)

-- | Exercises for the first lesson.
exercises1 :: ExerciseGenerator
exercises1 = combineFunctionsUniformly
    [ generatePositiveAttitudinalBackwardMeaningExercise attitudinals1
    , generatePositiveAttitudinalForwardMeaningExercise attitudinals1
    ]

-- | Exercises for the second lesson.
exercises2 :: ExerciseGenerator
exercises2 = combineFunctionsUniformly
    [ generatePositiveAttitudinalBackwardMeaningExercise attitudinals2
    , generatePositiveAttitudinalForwardMeaningExercise attitudinals2
    ]
