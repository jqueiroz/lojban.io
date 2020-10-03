-- | This module defines exercises for each of the course lessons.
module Courses.English.Grammar.Crash.Exercises where

import Core
import Study.Framework.ExerciseGenerators
import Courses.English.Grammar.Crash.Translations
import Courses.English.Grammar.Crash.Vocabulary
import Util (combineGenerators)

-- For now, let's just reuse the strategies from the "Introduction" course.
import Courses.English.Grammar.Introduction.Strategies (sentenceCanonicalizer, sentenceComparer)

-- * Exercises
-- | Exercises for the first lesson.
exercises01 :: ExerciseGenerator
exercises01 =
    combineGenerators
        [ (10, translationExercises)
        ]
    where
        vocabulary = vocabulary01_cumulative
        translationExercises = generateTranslationExercise sentenceCanonicalizer sentenceComparer translations01

-- * Exercises
-- | Exercises for the second lesson.
exercises02 :: ExerciseGenerator
exercises02 =
    combineGenerators
        [ (10, translationExercises)
        ]
    where
        vocabulary = vocabulary02_cumulative
        translationExercises = generateTranslationExercise sentenceCanonicalizer sentenceComparer translations02

-- * Exercises
-- | Exercises for the third lesson.
exercises03 :: ExerciseGenerator
exercises03 =
    combineGenerators
        [ (10, translationExercises)
        , (10, generateBasicNumberExercise 999)
        ]
    where
        vocabulary = vocabulary03_cumulative
        translationExercises = generateTranslationExercise sentenceCanonicalizer sentenceComparer translations03
