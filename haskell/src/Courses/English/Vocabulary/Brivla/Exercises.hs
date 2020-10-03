{-# LANGUAGE OverloadedStrings #-}

-- | This module defines the course exercises.
module Courses.English.Vocabulary.Brivla.Exercises where

import Core
import Util (generatorFromList, combineGenerators)
import Language.Lojban.Dictionaries (englishDictionary)
import Control.Arrow (second)
import Study.Framework.ExerciseGenerators
import Courses.English.Vocabulary.Brivla.Translations

-- * Auxiliar functions
buildBrivlaExerciseGenerator :: TranslationsByExpression -> ExerciseGenerator
buildBrivlaExerciseGenerator translationsByExpression = combineGenerators [(12, translationExercises), (2, brivlaPlacesExercises), (3, brivlaProvidingExercises)] where
    dictionary = englishDictionary
    brivla = map fst translationsByExpression
    translationExercises = generateFillingBlanksExerciseByExpression translationGeneratorByExpression
    translationGeneratorByExpression = map (second generatorFromList) translationsByExpression
    brivlaPlacesExercises = generateIsolatedBrivlaPlacesExercise dictionary brivla
    brivlaProvidingExercises = generateLexiconProvidingExercise "brivla" dictionary $ generatorFromList brivla

-- * Exercises

-- | Exercises for the corresponding lesson.
exercises01 :: ExerciseGenerator
exercises01 = buildBrivlaExerciseGenerator translationsByExpression01

-- | Exercises for the corresponding lesson.
exercises02 :: ExerciseGenerator
exercises02 = buildBrivlaExerciseGenerator translationsByExpression02

-- | Exercises for the corresponding lesson.
exercises03 :: ExerciseGenerator
exercises03 = buildBrivlaExerciseGenerator translationsByExpression03

-- | Exercises for the corresponding lesson.
exercises04 :: ExerciseGenerator
exercises04 = buildBrivlaExerciseGenerator translationsByExpression04

-- | Exercises for the corresponding lesson.
exercises05 :: ExerciseGenerator
exercises05 = buildBrivlaExerciseGenerator translationsByExpression05
