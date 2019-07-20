{-# LANGUAGE OverloadedStrings #-}

-- | This module defines the course exercises.
module Courses.English.Vocabulary.Brivla.Exercises where

import Core
import Util (generatorFromList, combineFunctions)
import Control.Arrow (second)
import Courses.Util.ExerciseGenerators
import Courses.English.Vocabulary.Brivla.Translations

-- * Auxiliar functions
buildBrivlaExerciseGenerator :: Dictionary -> TranslationsByExpression -> ExerciseGenerator
buildBrivlaExerciseGenerator dictionary translationsByExpression = combineFunctions [(6, translationExercises), (3, brivlaPlacesExercises), (3, brivlaProvidingExercises)] where
    brivla = map fst translationsByExpression
    brivlaWithAtLeastTwoPlaces = filter ((>= 2) . length . retrieveBrivlaPlaces dictionary) brivla
    translationExercises = generateBroadFillingBlanksExerciseByExpression translationGeneratorByExpression
    translationGeneratorByExpression = map (second generatorFromList) translationsByExpression
    brivlaPlacesExercises = generateIsolatedBrivlaPlacesExercise dictionary $ generatorFromList brivlaWithAtLeastTwoPlaces
    brivlaProvidingExercises = generateLexiconProvidingExercise "brivla" dictionary $ generatorFromList brivla

-- * Exercises

-- | Exercises for the corresponding lesson.
exercises01 :: Dictionary -> ExerciseGenerator
exercises01 dictionary = buildBrivlaExerciseGenerator dictionary translations01

-- | Exercises for the corresponding lesson.
exercises02 :: Dictionary -> ExerciseGenerator
exercises02 dictionary = buildBrivlaExerciseGenerator dictionary translations02

-- | Exercises for the corresponding lesson.
exercises03 :: Dictionary -> ExerciseGenerator
exercises03 dictionary = buildBrivlaExerciseGenerator dictionary translations03
