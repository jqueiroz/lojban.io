{-# LANGUAGE OverloadedStrings #-}

-- | This module defines exercises for each of the course lessons.
module Courses.English.Grammar.Introduction.Exercises where

import Core
import Courses.Util.Vocabulary
import Courses.Util.Sentences (generateSimpleBridi, generateActionBridi, generateNonbridi)
import Courses.Util.Extractors (extractSimpleBridiFromTranslationGenerator)
import Courses.Util.ExerciseGenerators
import Language.Lojban.Core
import Language.Lojban.Dictionaries (englishDictionary)
import Language.Lojban.Presentation (displayStandardSimpleBridi, displayVariantSimpleBridi, displayReorderedStandardSimpleBridi)
import Language.Lojban.Refinement (simplifyTerminatorsInBridiDisplayer)
import Courses.English.Grammar.Introduction.Translations
import Courses.English.Grammar.Introduction.Vocabulary
import Util (combineFunctions, generatorFromWeightedList)

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
        vocabulary = vocabularyGenerator1 dictionary
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
        vocabulary = vocabularyGenerator2 dictionary
        nonbridiGenerator = generateNonbridi vocabulary
        bridiGenerator = extractSimpleBridiFromTranslationGenerator translations2
        displayBridi = combineFunctions [(7, displayStandardSimpleBridi), (3, displayVariantSimpleBridi)]

-- | Exercises for the third lesson.
exercises3 :: ExerciseGenerator
exercises3 =
    combineFunctions
        [ (10, generateGrammaticalClassExercise vocabulary)
        , (10, generateBridiJufraExercise generalBridiGenerator nonbridiGenerator displayBridi)
        , (10, generateSelbriIdentificationExercise generalBridiGenerator displayBridi)
        , (20, generateContextualizedGismuPlacePositionExercise dictionary generalBridiGenerator displayBridi)
        , (20, generateContextualizedGismuPlaceMeaningExercise dictionary actionBridiGenerator displayBridi)
        , (30, generateIsolatedBrivlaPlacesExercise dictionary $ generatorFromWeightedList $ getVocabularySelbri vocabulary "actions")
        , (60, translationExercises3)
        ]
    where
        vocabulary = vocabularyGenerator3 dictionary
        nonbridiGenerator = generateNonbridi vocabulary
        generalBridiGenerator = generateSimpleBridi vocabulary
        actionBridiGenerator = generateActionBridi vocabulary
        displayBridi = combineFunctions [(7, displayStandardSimpleBridi), (2, displayVariantSimpleBridi), (1, displayReorderedStandardSimpleBridi)]

-- | Exercises for the fourth lesson.
exercises4 :: ExerciseGenerator
exercises4 =
    combineFunctions
        [ (20, generateIsolatedBrivlaPlacesExercise dictionary $ generatorFromWeightedList $ getVocabularySelbri vocabulary "actions")
        , (20, generateContextualizedGismuPlaceMeaningExercise dictionary actionBridiGenerator displayBridi)
        , (20, questionExercises4)
        , (80, translationExercises4)
        ]
    where
        vocabulary = vocabularyGenerator4 dictionary
        actionBridiGenerator = generateActionBridi vocabulary
        displayBridi = combineFunctions [(7, displayStandardSimpleBridi), (2, displayVariantSimpleBridi), (1, displayReorderedStandardSimpleBridi)]

-- | Exercises for the fifth lesson.
exercises5 :: ExerciseGenerator
exercises5 =
    combineFunctions
        [ (30, generateIsolatedBrivlaPlacesExercise dictionary $ generatorFromWeightedList $ getVocabularySelbri vocabulary "actions")
        , (10, generateContextualizedGismuPlaceMeaningExercise dictionary actionBridiGenerator displayBridi)
        , (40, abstractionExercises5)
        , (70, translationExercises5)
        ]
    where
        vocabulary = vocabularyGenerator5 dictionary
        actionBridiGenerator = generateActionBridi vocabulary
        displayBridi = combineFunctions [(7, displayStandardSimpleBridi), (2, displayVariantSimpleBridi), (1, displayReorderedStandardSimpleBridi)]

-- | Exercises for the sixth lesson.
exercises6 :: ExerciseGenerator
exercises6 =
    combineFunctions
        [ (20, generateIsolatedBrivlaPlacesExercise dictionary $ generatorFromWeightedList $ getVocabularySelbri vocabulary "actions")
        , (10, generateContextualizedGismuPlaceMeaningExercise dictionary actionBridiGenerator displayBridi)
        , (70, translationExercises6_restricted)
        ]
    where
        vocabulary = vocabularyGenerator6 dictionary
        actionBridiGenerator = generateActionBridi vocabulary
        displayBridi = simplifyTerminatorsInBridiDisplayer $ (combineFunctions [(7, displayStandardSimpleBridi), (2, displayVariantSimpleBridi), (1, displayReorderedStandardSimpleBridi)])

-- | Exercises for the seventh lesson.
exercises1to6 :: ExerciseGenerator
exercises1to6 =
    combineFunctions
        [ (5, generateGrammaticalClassExercise vocabulary)
        , (5, generateBridiJufraExercise generalBridiGenerator nonbridiGenerator displayBridi)
        , (5, generateSelbriIdentificationExercise generalBridiGenerator displayBridi)
        , (5, generateContextualizedGismuPlacePositionExercise dictionary generalBridiGenerator displayBridi)
        , (15, generateContextualizedGismuPlaceMeaningExercise dictionary actionBridiGenerator displayBridi)
        , (15, generateIsolatedBrivlaPlacesExercise dictionary $ generatorFromWeightedList $ getVocabularySelbri vocabulary "actions")
        , (60, translationExercises1to6_simplified)
        , (12, questionExercises4_simplified)
        , (12, abstractionExercises5_simplified)
        ]
    where
        vocabulary = vocabularyGenerator6 dictionary
        generalBridiGenerator = generateSimpleBridi vocabulary
        actionBridiGenerator = generateActionBridi vocabulary
        nonbridiGenerator = generateNonbridi vocabulary
        displayBridi = simplifyTerminatorsInBridiDisplayer $ (combineFunctions [(7, displayStandardSimpleBridi), (2, displayVariantSimpleBridi), (1, displayReorderedStandardSimpleBridi)])

-- | Exercises for the eighth lesson.
exercises8 :: ExerciseGenerator
exercises8 =
    combineFunctions
        [ (20, generateIsolatedBrivlaPlacesExercise dictionary $ generatorFromWeightedList $ getVocabularySelbri vocabulary "actions")
        , (70, translationExercises8)
        , (15, fillingBlanksExercises8)
        ]
    where
        vocabulary = vocabularyGenerator8 dictionary

-- | Exercises for the nineth lesson.
exercises9 :: ExerciseGenerator
exercises9 =
    combineFunctions
        [ (20, generateIsolatedBrivlaPlacesExercise dictionary $ generatorFromWeightedList $ getVocabularySelbri vocabulary "actions" ++ getVocabularySelbri vocabulary "relations")
        , (70, translationExercises9)
        ]
    where
        vocabulary = vocabularyGenerator9 dictionary

-- | Exercises for the tenth lesson.
exercises10 :: ExerciseGenerator
exercises10 =
    combineFunctions
        [ (20, generateIsolatedBrivlaPlacesExercise dictionary $ generatorFromWeightedList $ getVocabularySelbri vocabulary "actions" ++ getVocabularySelbri vocabulary "relations")
        , (80, translationExercises10)
        ]
    where
        vocabulary = vocabularyGenerator10 dictionary

-- | Exercises for the eleventh lesson.
exercises11 :: ExerciseGenerator
exercises11 =
    combineFunctions
        [ (10, generateIsolatedBrivlaPlacesExercise dictionary $ generatorFromWeightedList $ getVocabularySelbri vocabulary "actions" ++ getVocabularySelbri vocabulary "relations")
        , (70, translationExercises11_restricted)
        , (15, translationExercises11_unrestricted)
        , (20, fillingBlanksExercises11)
        ]
    where
        vocabulary = vocabularyGenerator11 dictionary

-- | Exercises for the twelveth lesson.
exercises12 :: ExerciseGenerator
exercises12 =
    combineFunctions
        [ (10, generateIsolatedBrivlaPlacesExercise dictionary $ generatorFromWeightedList $ getVocabularySelbri vocabulary "actions" ++ getVocabularySelbri vocabulary "relations")
        , (70, translationExercises12)
        ]
    where
        vocabulary = vocabularyGenerator12 dictionary

-- | Exercises for the thirteenth lesson.
exercises8to12 :: ExerciseGenerator
exercises8to12 =
    combineFunctions
        [ (10, generateIsolatedBrivlaPlacesExercise dictionary $ generatorFromWeightedList $ getVocabularySelbri vocabulary "actions" ++ getVocabularySelbri vocabulary "relations")
        , (70, translationExercises8to12)
        ]
    where
        vocabulary = vocabularyGenerator12 dictionary

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
