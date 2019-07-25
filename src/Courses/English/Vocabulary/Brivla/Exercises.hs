{-# LANGUAGE OverloadedStrings #-}

-- | This module defines the course exercises.
module Courses.English.Vocabulary.Brivla.Exercises where

import Core
import Util (generatorFromList, combineFunctions)
import Language.Lojban.Core
import Language.Lojban.Dictionaries (englishDictionary)
import Control.Arrow (second)
import Courses.Util.ExerciseGenerators
import Courses.English.Vocabulary.Brivla.Translations

-- * Auxiliar functions
buildBrivlaExerciseGenerator :: TranslationsByExpression -> ExerciseGenerator
buildBrivlaExerciseGenerator translationsByExpression = combineFunctions [(6, translationExercises), (3, brivlaPlacesExercises), (3, brivlaProvidingExercises)] where
    dictionary = englishDictionary
    brivla = map fst translationsByExpression
    brivlaWithAtLeastTwoPlaces = filter ((>= 2) . length . retrieveBrivlaPlaces dictionary) brivla
    translationExercises = generateBroadFillingBlanksExerciseByExpression translationGeneratorByExpression
    translationGeneratorByExpression = map (second generatorFromList) translationsByExpression
    brivlaPlacesExercises = generateIsolatedBrivlaPlacesExercise dictionary $ generatorFromList brivlaWithAtLeastTwoPlaces
    brivlaProvidingExercises = generateLexiconProvidingExercise "brivla" dictionary $ generatorFromList brivla

-- * Exercises

-- | Exercises for the corresponding lesson.
exercises01 :: ExerciseGenerator
exercises01 = buildBrivlaExerciseGenerator translations01

-- | Exercises for the corresponding lesson.
exercises02 :: ExerciseGenerator
exercises02 = buildBrivlaExerciseGenerator translations02

-- | Exercises for the corresponding lesson.
exercises03 :: ExerciseGenerator
exercises03 = buildBrivlaExerciseGenerator translations03

-- | Exercises for the corresponding lesson.
exercises04 :: ExerciseGenerator
exercises04 = buildBrivlaExerciseGenerator translations04

-- | Exercises for the corresponding lesson.
exercises05 :: ExerciseGenerator
exercises05 = buildBrivlaExerciseGenerator translations05
