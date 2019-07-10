{-# LANGUAGE OverloadedStrings #-}

-- | This module defines the course exercuses.
module Courses.English.Vocabulary.Attitudinals.Exercises where

import Core
import Util (generatorFromList)
import Courses.English.Vocabulary.Attitudinals.Model
import Courses.English.Vocabulary.Attitudinals.Vocabulary
import qualified Data.Text as T

generatePositiveAttitudinalMeaningExercise :: AttitudinalGenerator -> ExerciseGenerator
generatePositiveAttitudinalMeaningExercise attitudinalGenerator r0 = TypingExercise title [] (== expectedAttitudinal) (expectedAttitudinal) where
    (attitudinal, r1) = attitudinalGenerator r0
    title = "Provide the attitudinal for <b>" `T.append` (attitudinalPositiveMeaning attitudinal) `T.append` "</b>"
    expectedAttitudinal = attitudinalWord attitudinal

-- | Exercises for the first lesson.
exercises1 :: ExerciseGenerator
exercises1 = generatePositiveAttitudinalMeaningExercise $ generatorFromList attitudinals1

-- | Exercises for the second lesson.
exercises2 :: ExerciseGenerator
exercises2 = generatePositiveAttitudinalMeaningExercise $ generatorFromList attitudinals2
