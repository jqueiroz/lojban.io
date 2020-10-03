{-# LANGUAGE OverloadedStrings #-}

-- | This module defines the course exercuses.
module Study.Courses.English.Vocabulary.Attitudinals.Exercises where

import Core
import Util (shuffle, chooseItemUniformly, combineGeneratorsUniformly, mapRandom)
import Study.Courses.English.Vocabulary.Attitudinals.Model
import Study.Courses.English.Vocabulary.Attitudinals.Vocabulary
import Study.Courses.English.Vocabulary.Attitudinals.Util
import qualified Data.Text as T

-- * Exercise templates
generatePositiveAttitudinalBackwardMeaningExercise :: [Attitudinal] -> ExerciseGenerator
generatePositiveAttitudinalBackwardMeaningExercise attitudinals r0 = TypingExercise title [] (== expectedAttitudinal) (expectedAttitudinal) where
    (attitudinal, r1) = chooseItemUniformly r0 attitudinals
    title = "Provide the attitudinal for <b>" `T.append` (attitudinalPositiveMeaning attitudinal) `T.append` "</b>"
    expectedAttitudinal = attitudinalWord attitudinal

generatePositiveAttitudinalForwardMeaningExercise :: [Attitudinal] -> ExerciseGenerator
generatePositiveAttitudinalForwardMeaningExercise attitudinals r0 = SingleChoiceExercise title [] correctMeaning incorrectMeanings False where
    (correctAttitudinal, r1) = chooseItemUniformly r0 attitudinals
    incorrectAttitudinals = filter (/= correctAttitudinal) attitudinals
    (shuffledIncorrectAttitudinals, r2) = shuffle r1 incorrectAttitudinals
    title = "Select the meaning of <b>" `T.append` (attitudinalWord correctAttitudinal) `T.append` "</b>"
    correctMeaning = attitudinalPositiveMeaning correctAttitudinal
    incorrectMeanings = map attitudinalPositiveMeaning (take 4 incorrectAttitudinals)

generateAttitudinalBackwardMeaningExercise :: [Attitudinal] -> ExerciseGenerator
generateAttitudinalBackwardMeaningExercise attitudinals r0 = TypingExercise title [] (== expectedAttitudinalExpression) (expectedAttitudinalExpression) where
    (attitudinal, r1) = chooseItemUniformly r0 attitudinals
    (attitudinalModifier, meaningOfModifiedAttitudinal, r2) = randomlyPickAttitudinalModifier r1 attitudinal
    expectedAttitudinalExpression = (attitudinalWord attitudinal) `T.append` attitudinalModifier
    title = "Provide the attitudinal expression for <b>" `T.append` meaningOfModifiedAttitudinal `T.append` "</b>"

generateAttitudinalForwardMeaningExercise :: [Attitudinal] -> ExerciseGenerator
generateAttitudinalForwardMeaningExercise attitudinals r0 = SingleChoiceExercise title [] correctMeaning incorrectMeanings False where
    title = "Select the meaning of <b>" `T.append` correctAttitudinalExpression `T.append` "</b>"
    -- Correct attitudinal
    (correctAttitudinal, r1) = chooseItemUniformly r0 attitudinals
    (correctAttitudinalModifier, meaningOfModifiedCorrectAttitudinal, r2) = randomlyPickAttitudinalModifier r1 correctAttitudinal
    correctAttitudinalExpression = (attitudinalWord correctAttitudinal) `T.append` correctAttitudinalModifier
    correctMeaning = meaningOfModifiedCorrectAttitudinal
    -- Incorrect attitudinals
    incorrectAttitudinals = filter (/= correctAttitudinal) attitudinals
    (shuffledIncorrectAttitudinals, r3) = shuffle r2 incorrectAttitudinals
    (incorrectMeanings, r4) = mapRandom r3 randomlyPickAttitudinalMeaning (take 4 incorrectAttitudinals)

generateAttitudinalClassificationExercise :: [Attitudinal] -> ExerciseGenerator
generateAttitudinalClassificationExercise attitudinals r0 = SingleChoiceExercise title [] correctClassification [incorrectClassification] True where
    title = "Classify the attitudinal <b>" `T.append` (attitudinalWord attitudinal) `T.append` "</b>"
    (attitudinal, r1) = chooseItemUniformly r0 attitudinals
    (correctClassification, incorrectClassification)
        | (attitudinalType attitudinal) == PureEmotion = ("Pure", "Propositional")
        | otherwise                                    = ("Propositional", "Pure")

-- * Exercises per lesson
-- | Exercises for the first lesson.
exercises1 :: ExerciseGenerator
exercises1 = combineGeneratorsUniformly
    [ generatePositiveAttitudinalBackwardMeaningExercise attitudinals1
    , generatePositiveAttitudinalForwardMeaningExercise attitudinals1
    ]

-- | Exercises for the second lesson.
exercises2 :: ExerciseGenerator
exercises2 = combineGeneratorsUniformly
    [ generatePositiveAttitudinalBackwardMeaningExercise attitudinals2
    , generatePositiveAttitudinalForwardMeaningExercise attitudinals2
    , generateAttitudinalClassificationExercise attitudinals2_cumulative
    ]

-- | Exercises for the third lesson.
exercises3 :: ExerciseGenerator
exercises3 = combineGeneratorsUniformly
    [ generateAttitudinalBackwardMeaningExercise attitudinals3_cumulative
    , generateAttitudinalForwardMeaningExercise attitudinals3_cumulative
    , generateAttitudinalClassificationExercise attitudinals3_cumulative
    ]
