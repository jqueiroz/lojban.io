-- | This module defines exercises for each of the course lessons.
module Courses.English.Grammar.Crash.Exercises where

import Core
import Courses.Framework.ExerciseGenerators
import Courses.English.Grammar.Crash.Translations
import Courses.English.Grammar.Crash.Vocabulary
import Util (combineFunctions)

-- For now, let's just reuse the strategies from the "Introduction" course.
import Courses.English.Grammar.Introduction.Strategies (sentenceCanonicalizer, sentenceComparer)

-- * Exercises
-- | Exercises for the first lesson.
exercises01 :: ExerciseGenerator
exercises01 =
    combineFunctions
        [ (10, translationExercises)
        ]
    where
        vocabulary = vocabulary01_cumulative
        translationExercises = generateTranslationExercise sentenceCanonicalizer sentenceComparer translations01

-- * Exercises
-- | Exercises for the second lesson.
exercises02 :: ExerciseGenerator
exercises02 =
    combineFunctions
        [ (10, translationExercises)
        ]
    where
        vocabulary = vocabulary01_cumulative
        translationExercises = generateTranslationExercise sentenceCanonicalizer sentenceComparer translations02
