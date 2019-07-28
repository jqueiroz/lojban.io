-- | This module defines exercises for each of the course lessons.
module Courses.English.Grammar.Introduction.Exercises where

import Core
import Courses.Framework.SentenceGenerators (generateNonbridi)
import Courses.Framework.Extractors (extractSimpleBridiFromTranslationGenerator)
import Courses.Framework.ExerciseGenerators
import Language.Lojban.Core
import Language.Lojban.Dictionaries (englishDictionary)
import Language.Lojban.Presentation (displayStandardSimpleBridi, displayVariantSimpleBridi, displayReorderedStandardSimpleBridi)
import Language.Lojban.Refinement (simplifyTerminatorsInBridiDisplayer)
import Courses.English.Grammar.Introduction.Translations
import Courses.English.Grammar.Introduction.Vocabulary
import Util (combineFunctions)

-- * Resources
-- | Dictionary for the exercises.
dictionary :: Dictionary
dictionary = englishDictionary

-- * Exercises
-- | Exercises for the first lesson.
exercises1 :: ExerciseGenerator
exercises1 =
    combineFunctions
        [ (25, generateBridiJufraExercise bridiGenerator nonbridiGenerator displayBridi)
        , (20, generateSelbriIdentificationExercise bridiGenerator displayBridi)
        , (10, generateContextualizedGismuPlacePositionExercise dictionary bridiGenerator displayBridi)
        , (20, generateContextualizedGismuPlaceMeaningExercise dictionary bridiGenerator displayBridi)
        , (40, translationExercises1)
        ]
    where
        vocabulary = vocabulary1_cumulative
        nonbridiGenerator = generateNonbridi vocabulary
        bridiGenerator = extractSimpleBridiFromTranslationGenerator translations1
        displayBridi = displayStandardSimpleBridi

-- | Exercises for the second lesson.
exercises2 :: ExerciseGenerator
exercises2 =
    combineFunctions
        [ (20, generateGrammaticalClassExercise vocabulary)
        , (15, generateBridiJufraExercise bridiGenerator nonbridiGenerator displayBridi)
        , (20, generateSelbriIdentificationExercise bridiGenerator displayBridi)
        , (10, generateContextualizedGismuPlacePositionExercise dictionary bridiGenerator displayBridi)
        , (20, generateContextualizedGismuPlaceMeaningExercise dictionary bridiGenerator displayBridi)
        , (40, translationExercises2)
        ]
    where
        vocabulary = vocabulary2_cumulative
        nonbridiGenerator = generateNonbridi vocabulary
        bridiGenerator = extractSimpleBridiFromTranslationGenerator translations2
        displayBridi = combineFunctions [(7, displayStandardSimpleBridi), (3, displayVariantSimpleBridi)]

-- | Exercises for the third lesson.
exercises3 :: ExerciseGenerator
exercises3 =
    combineFunctions
        [ (10, generateGrammaticalClassExercise vocabulary)
        , (10, generateBridiJufraExercise bridiGenerator nonbridiGenerator displayBridi)
        , (10, generateSelbriIdentificationExercise bridiGenerator displayBridi)
        , (20, generateContextualizedGismuPlacePositionExercise dictionary bridiGenerator displayBridi)
        , (20, generateContextualizedGismuPlaceMeaningExercise dictionary bridiGenerator displayBridi)
        , (30, generateIsolatedBrivlaPlacesExercise dictionary $ vocabularyBrivlaList vocabulary)
        , (60, translationExercises3)
        ]
    where
        vocabulary = vocabulary3_cumulative
        nonbridiGenerator = generateNonbridi vocabulary
        bridiGenerator = extractSimpleBridiFromTranslationGenerator translations3
        displayBridi = combineFunctions [(7, displayStandardSimpleBridi), (2, displayVariantSimpleBridi), (1, displayReorderedStandardSimpleBridi)]

-- | Exercises for the fourth lesson.
exercises4 :: ExerciseGenerator
exercises4 =
    combineFunctions
        [ (20, generateIsolatedBrivlaPlacesExercise dictionary $ vocabularyBrivlaList vocabulary)
        , (20, generateContextualizedGismuPlaceMeaningExercise dictionary bridiGenerator displayBridi)
        , (20, questionExercises4)
        , (80, translationExercises4)
        ]
    where
        vocabulary = vocabulary4_cumulative
        bridiGenerator = extractSimpleBridiFromTranslationGenerator translations4
        displayBridi = combineFunctions [(7, displayStandardSimpleBridi), (2, displayVariantSimpleBridi), (1, displayReorderedStandardSimpleBridi)]

-- | Exercises for the fifth lesson.
exercises5 :: ExerciseGenerator
exercises5 =
    combineFunctions
        [ (30, generateIsolatedBrivlaPlacesExercise dictionary $ vocabularyBrivlaList vocabulary)
        , (10, generateContextualizedGismuPlaceMeaningExercise dictionary bridiGenerator displayBridi)
        , (40, abstractionExercises5)
        , (70, translationExercises5)
        ]
    where
        vocabulary = vocabulary5_cumulative
        bridiGenerator = extractSimpleBridiFromTranslationGenerator translations5
        displayBridi = combineFunctions [(7, displayStandardSimpleBridi), (2, displayVariantSimpleBridi), (1, displayReorderedStandardSimpleBridi)]

-- | Exercises for the sixth lesson.
exercises6 :: ExerciseGenerator
exercises6 =
    combineFunctions
        [ (20, generateIsolatedBrivlaPlacesExercise dictionary $ vocabularyBrivlaList vocabulary)
        , (10, generateContextualizedGismuPlaceMeaningExercise dictionary bridiGenerator displayBridi)
        , (70, translationExercises6_restricted)
        ]
    where
        vocabulary = vocabulary6_cumulative
        bridiGenerator = extractSimpleBridiFromTranslationGenerator translations6
        displayBridi = simplifyTerminatorsInBridiDisplayer $ (combineFunctions [(7, displayStandardSimpleBridi), (2, displayVariantSimpleBridi), (1, displayReorderedStandardSimpleBridi)])

-- | Exercises for the seventh lesson.
exercises1to6 :: ExerciseGenerator
exercises1to6 =
    combineFunctions
        [ (5, generateGrammaticalClassExercise vocabulary)
        , (5, generateBridiJufraExercise bridiGenerator nonbridiGenerator displayBridi)
        , (5, generateSelbriIdentificationExercise bridiGenerator displayBridi)
        , (5, generateContextualizedGismuPlacePositionExercise dictionary bridiGenerator displayBridi)
        , (15, generateContextualizedGismuPlaceMeaningExercise dictionary bridiGenerator displayBridi)
        , (15, generateIsolatedBrivlaPlacesExercise dictionary $ vocabularyBrivlaList vocabulary)
        , (60, translationExercises1to6_simplified)
        , (12, questionExercises4_simplified)
        , (12, abstractionExercises5_simplified)
        ]
    where
        vocabulary = vocabulary6_cumulative
        bridiGenerator = extractSimpleBridiFromTranslationGenerator translations1to6
        nonbridiGenerator = generateNonbridi vocabulary
        displayBridi = simplifyTerminatorsInBridiDisplayer $ (combineFunctions [(7, displayStandardSimpleBridi), (2, displayVariantSimpleBridi), (1, displayReorderedStandardSimpleBridi)])

-- | Exercises for the eighth lesson.
exercises8 :: ExerciseGenerator
exercises8 =
    combineFunctions
        [ (20, generateIsolatedBrivlaPlacesExercise dictionary $ vocabularyBrivlaList vocabulary)
        , (70, translationExercises8)
        , (15, fillingBlanksExercises8)
        ]
    where
        vocabulary = vocabulary8_cumulative

-- | Exercises for the nineth lesson.
exercises9 :: ExerciseGenerator
exercises9 =
    combineFunctions
        [ (20, generateIsolatedBrivlaPlacesExercise dictionary $ vocabularyBrivlaList vocabulary)
        , (70, translationExercises9)
        ]
    where
        vocabulary = vocabulary9_cumulative

-- | Exercises for the tenth lesson.
exercises10 :: ExerciseGenerator
exercises10 =
    combineFunctions
        [ (20, generateIsolatedBrivlaPlacesExercise dictionary $ vocabularyBrivlaList vocabulary)
        , (80, translationExercises10)
        ]
    where
        vocabulary = vocabulary10_cumulative

-- | Exercises for the eleventh lesson.
exercises11 :: ExerciseGenerator
exercises11 =
    combineFunctions
        [ (10, generateIsolatedBrivlaPlacesExercise dictionary $ vocabularyBrivlaList vocabulary)
        , (70, translationExercises11_restricted)
        , (15, translationExercises11_unrestricted)
        , (20, fillingBlanksExercises11)
        ]
    where
        vocabulary = vocabulary11_cumulative

-- | Exercises for the twelveth lesson.
exercises12 :: ExerciseGenerator
exercises12 =
    combineFunctions
        [ (10, generateIsolatedBrivlaPlacesExercise dictionary $ vocabularyBrivlaList vocabulary)
        , (70, translationExercises12)
        ]
    where
        vocabulary = vocabulary12_cumulative

-- | Exercises for the thirteenth lesson.
exercises8to12 :: ExerciseGenerator
exercises8to12 =
    combineFunctions
        [ (10, generateIsolatedBrivlaPlacesExercise dictionary $ vocabularyBrivlaList vocabulary)
        , (70, translationExercises8to12)
        ]
    where
        vocabulary = vocabulary12_cumulative

-- | Exercises for the fourteenth lesson.
exercises14 :: ExerciseGenerator
exercises14 =
    combineFunctions
        [ (70, translationExercises14)
        ]

-- | Exercises for the fifteenth lesson.
exercises15 :: ExerciseGenerator
exercises15 =
    combineFunctions
        [ (70, translationExercises15)
        ]

-- | Exercises for the sixteenth lesson.
exercises16 :: ExerciseGenerator
exercises16 =
    combineFunctions
        [ (70, translationExercises16)
        ]

-- | Exercises for the seventeenth lesson.
exercises17 :: ExerciseGenerator
exercises17 =
    combineFunctions
        [ (70, translationExercises17)
        ]
