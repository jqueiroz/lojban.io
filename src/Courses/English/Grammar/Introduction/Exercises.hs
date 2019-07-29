{-# LANGUAGE OverloadedStrings #-}

-- | This module defines exercises for each of the course lessons.
module Courses.English.Grammar.Introduction.Exercises where

import Core
import Courses.Framework.SentenceGenerators (generateNonbridi)
import Courses.Framework.Extractors (extractSimpleBridiGeneratorFromTranslationGenerator)
import Courses.Framework.ExerciseGenerators
import Courses.Framework.ExerciseUtils (simplifyTerminatorsInCanonicalAnswer)
import Courses.Framework.TranslationUtils (simplifyTerminatorsInTranslationGenerator)
import Language.Lojban.Core
import Language.Lojban.Dictionaries (englishDictionary)
import Language.Lojban.Presentation (displayStandardSimpleBridi, displayVariantSimpleBridi, displayReorderedStandardSimpleBridi)
import Language.Lojban.Refinement (simplifyTerminatorsInBridiDisplayer)
import Courses.English.Grammar.Introduction.Translations
import Courses.English.Grammar.Introduction.Vocabulary
import Courses.English.Grammar.Introduction.Strategies
import Util (combineGenerators, combineGeneratorsUniformly)

-- * Resources
-- | Dictionary for the exercises.
dictionary :: Dictionary
dictionary = englishDictionary

-- * Lesson 1: Basics 1
-- | Exercises for the first lesson.
exercises1 :: ExerciseGenerator
exercises1 =
    combineGenerators
        [ (25, generateEnglishOrLojbanBridiJufraExercise bridiGenerator nonbridiGenerator displayBridi)
        , (20, generateSelbriIdentificationExercise bridiGenerator displayBridi)
        , (10, generateContextualizedGismuPlacePositionExercise dictionary bridiGenerator displayBridi)
        , (20, generateContextualizedGismuPlaceMeaningExercise dictionary bridiGenerator displayBridi)
        , (40, translationExercises1)
        ]
    where
        vocabulary = vocabulary1_cumulative
        nonbridiGenerator = generateNonbridi vocabulary
        bridiGenerator = extractSimpleBridiGeneratorFromTranslationGenerator translations1
        displayBridi = displayStandardSimpleBridi

-- | Translation exercises for the first lesson.
translationExercises1 :: ExerciseGenerator
translationExercises1 = generateTranslationExercise sentenceCanonicalizer sentenceComparer translations1

-- * Lesson 2: Basics 2
-- | Exercises for the second lesson.
exercises2 :: ExerciseGenerator
exercises2 =
    combineGenerators
        [ (20, generateGrammaticalClassExercise vocabulary)
        , (15, generateEnglishOrLojbanBridiJufraExercise bridiGenerator nonbridiGenerator displayBridi)
        , (20, generateSelbriIdentificationExercise bridiGenerator displayBridi)
        , (10, generateContextualizedGismuPlacePositionExercise dictionary bridiGenerator displayBridi)
        , (20, generateContextualizedGismuPlaceMeaningExercise dictionary bridiGenerator displayBridi)
        , (40, translationExercises2)
        ]
    where
        vocabulary = vocabulary2_cumulative
        nonbridiGenerator = generateNonbridi vocabulary
        bridiGenerator = extractSimpleBridiGeneratorFromTranslationGenerator translations2
        displayBridi = combineGenerators [(7, displayStandardSimpleBridi), (3, displayVariantSimpleBridi)]

-- | Translation exercises for the second lesson.
translationExercises2 :: ExerciseGenerator
translationExercises2 = generateTranslationExercise sentenceCanonicalizer sentenceComparer translations2

-- * Lesson 3: Basics 3

-- | Exercises for the third lesson.
exercises3 :: ExerciseGenerator
exercises3 =
    combineGenerators
        [ (10, generateGrammaticalClassExercise vocabulary)
        , (10, generateEnglishOrLojbanBridiJufraExercise bridiGenerator nonbridiGenerator displayBridi)
        , (10, generateSelbriIdentificationExercise bridiGenerator displayBridi)
        , (20, generateContextualizedGismuPlacePositionExercise dictionary bridiGenerator displayBridi)
        , (20, generateContextualizedGismuPlaceMeaningExercise dictionary bridiGenerator displayBridi)
        , (30, generateIsolatedBrivlaPlacesExercise dictionary $ vocabularyBrivlaList vocabulary)
        , (60, translationExercises3)
        ]
    where
        vocabulary = vocabulary3_cumulative
        nonbridiGenerator = generateNonbridi vocabulary
        bridiGenerator = extractSimpleBridiGeneratorFromTranslationGenerator translations3
        displayBridi = combineGenerators [(7, displayStandardSimpleBridi), (2, displayVariantSimpleBridi), (1, displayReorderedStandardSimpleBridi)]

-- | Interesting translation exercises for the third lesson: comprises regular exercises involving interesting translations, as well as "Translate without using zo'e" exercises involving restricted translations.
--
-- Defined separately so that they may be reused in the checkpoint lesson (Lesson 7).
translationExercises3_nice :: ExerciseGenerator
translationExercises3_nice = combineGenerators [(1, restricted), (5, nice)] where
    restricted = generateBlacklistedWordTranslationExercise "zo'e" sentenceCanonicalizer sentenceComparer translations3_restricted
    nice = generateTranslationExercise sentenceCanonicalizer sentenceComparer translations3_nice

-- | Regular translation exercises for the third lesson.
translationExercises3_normal :: ExerciseGenerator
translationExercises3_normal = generateTranslationExercise sentenceCanonicalizer sentenceComparer translations3_normal

-- | Overall translation exercises for the third lesson.
translationExercises3 :: ExerciseGenerator
translationExercises3 =  combineGenerators [(1, restricted), (5, unrestricted)] where
    restricted = generateBlacklistedWordTranslationExercise "zo'e" sentenceCanonicalizer sentenceComparer translations3_restricted
    unrestricted = generateTranslationExercise sentenceCanonicalizer sentenceComparer translations3

-- * Lesson 4: Questions 1
-- | Exercises for the fourth lesson.
exercises4 :: ExerciseGenerator
exercises4 =
    combineGenerators
        [ (20, generateIsolatedBrivlaPlacesExercise dictionary $ vocabularyBrivlaList vocabulary)
        , (20, generateContextualizedGismuPlaceMeaningExercise dictionary bridiGenerator displayBridi)
        , (20, questionExercises4)
        , (80, translationExercises4)
        ]
    where
        vocabulary = vocabulary4_cumulative
        bridiGenerator = extractSimpleBridiGeneratorFromTranslationGenerator translations4
        displayBridi = combineGenerators [(7, displayStandardSimpleBridi), (2, displayVariantSimpleBridi), (1, displayReorderedStandardSimpleBridi)]

translationExercises4 :: ExerciseGenerator
translationExercises4 = combineGenerators [(1, restricted), (5, unrestricted)] where
    restricted = generateBlacklistedWordTranslationExercise "zo'e" sentenceCanonicalizer sentenceComparer translations4_restricted
    unrestricted = generateTranslationExercise sentenceCanonicalizer sentenceComparer translations4

questionExercises4 :: ExerciseGenerator
questionExercises4 = generateNarrowFillingBlanksExerciseByAlternatives ["mo", "ma"] $ combineGeneratorsUniformly [translations4_ma, translations4_mo]

questionExercises4_simplified :: ExerciseGenerator
questionExercises4_simplified = generateNarrowFillingBlanksExerciseByAlternatives ["mo", "ma"] $ simplifyTerminatorsInTranslationGenerator $ combineGeneratorsUniformly [translations4_ma, translations4_mo]

-- * Lesson 5: Abstractions 1
-- | Exercises for the fifth lesson.
exercises5 :: ExerciseGenerator
exercises5 =
    combineGenerators
        [ (30, generateIsolatedBrivlaPlacesExercise dictionary $ vocabularyBrivlaList vocabulary)
        , (10, generateContextualizedGismuPlaceMeaningExercise dictionary bridiGenerator displayBridi)
        , (40, abstractionExercises5)
        , (70, translationExercises5)
        ]
    where
        vocabulary = vocabulary5_cumulative
        bridiGenerator = extractSimpleBridiGeneratorFromTranslationGenerator translations5
        displayBridi = combineGenerators [(7, displayStandardSimpleBridi), (2, displayVariantSimpleBridi), (1, displayReorderedStandardSimpleBridi)]

translationExercises5 :: ExerciseGenerator
translationExercises5 = generateTranslationExercise sentenceCanonicalizer sentenceComparer translations5

-- "narrow" is required to avoid alternative translations using "ko'a"
abstractionExercises5 :: ExerciseGenerator
abstractionExercises5 = generateNarrowFillingBlanksExerciseByAlternatives ["lo nu", "lo du'u", "lo se du'u"] $ combineGeneratorsUniformly [translations5_nu, translations5_du'u, translations5_sedu'u]

abstractionExercises5_simplified :: ExerciseGenerator
abstractionExercises5_simplified = generateNarrowFillingBlanksExerciseByAlternatives ["lo nu", "lo du'u", "lo se du'u"] $ simplifyTerminatorsInTranslationGenerator $ combineGeneratorsUniformly [translations5_nu, translations5_du'u, translations5_sedu'u]

-- * Lesson 6: Terminator elision
-- | Exercises for the sixth lesson.
exercises6 :: ExerciseGenerator
exercises6 =
    combineGenerators
        [ (20, generateIsolatedBrivlaPlacesExercise dictionary $ vocabularyBrivlaList vocabulary)
        , (10, generateContextualizedGismuPlaceMeaningExercise dictionary bridiGenerator displayBridi)
        , (70, translationExercises6_restricted)
        ]
    where
        vocabulary = vocabulary6_cumulative
        bridiGenerator = extractSimpleBridiGeneratorFromTranslationGenerator translations6
        displayBridi = simplifyTerminatorsInBridiDisplayer $ (combineGenerators [(7, displayStandardSimpleBridi), (2, displayVariantSimpleBridi), (1, displayReorderedStandardSimpleBridi)])

translationExercises6_restricted :: ExerciseGenerator
translationExercises6_restricted = generateBlacklistedWordTranslationExercise "ku" sentenceCanonicalizer sentenceComparer translations6_restricted

-- * Lesson 7: Checkpoint -- Lessons 1-6
-- | Exercises for the seventh lesson.
exercises1to6 :: ExerciseGenerator
exercises1to6 =
    combineGenerators
        [ (5, generateGrammaticalClassExercise vocabulary)
        , (5, generateEnglishOrLojbanBridiJufraExercise bridiGenerator nonbridiGenerator displayBridi)
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
        bridiGenerator = extractSimpleBridiGeneratorFromTranslationGenerator translations1to6
        nonbridiGenerator = generateNonbridi vocabulary
        displayBridi = simplifyTerminatorsInBridiDisplayer $ (combineGenerators [(7, displayStandardSimpleBridi), (2, displayVariantSimpleBridi), (1, displayReorderedStandardSimpleBridi)])

translationExercises1to6_simplified :: ExerciseGenerator
translationExercises1to6_simplified = simplifyTerminatorsInCanonicalAnswer . combineGenerators [(4, translationExercises3_nice), (1, translationExercises3_normal), (5, translationExercises4), (6, translationExercises5), (5, translationExercises6_restricted)]

-- * Lesson 8: Relative clauses
-- | Exercises for the eighth lesson.
exercises8 :: ExerciseGenerator
exercises8 =
    combineGenerators
        [ (20, generateIsolatedBrivlaPlacesExercise dictionary $ vocabularyBrivlaList vocabulary)
        , (70, translationExercises8)
        , (15, fillingBlanksExercises8)
        ]
    where
        vocabulary = vocabulary8_cumulative

translationExercises8 :: ExerciseGenerator
translationExercises8 = generateTranslationExercise sentenceCanonicalizer sentenceComparer translations8

fillingBlanksExercises8 :: ExerciseGenerator
fillingBlanksExercises8 = generateContextualizedBroadFillingBlanksExerciseByAlternatives ["poi", "noi"] translations8

-- * Lesson 9: Linked sumti
-- | Exercises for the nineth lesson.
exercises9 :: ExerciseGenerator
exercises9 =
    combineGenerators
        [ (20, generateIsolatedBrivlaPlacesExercise dictionary $ vocabularyBrivlaList vocabulary)
        , (70, translationExercises9)
        ]
    where
        vocabulary = vocabulary9_cumulative

translationExercises9 :: ExerciseGenerator
translationExercises9 = generateTranslationExercise sentenceCanonicalizer sentenceComparer translations9

-- * Lesson 10: Sumtcita
-- | Exercises for the tenth lesson.
exercises10 :: ExerciseGenerator
exercises10 =
    combineGenerators
        [ (20, generateIsolatedBrivlaPlacesExercise dictionary $ vocabularyBrivlaList vocabulary)
        , (80, translationExercises10)
        ]
    where
        vocabulary = vocabulary10_cumulative

translationExercises10 :: ExerciseGenerator
translationExercises10 = generateTranslationExercise sentenceCanonicalizer sentenceComparer translations10

-- * Lesson 11: Tenses 1
-- | Exercises for the eleventh lesson.
exercises11 :: ExerciseGenerator
exercises11 =
    combineGenerators
        [ (10, generateIsolatedBrivlaPlacesExercise dictionary $ vocabularyBrivlaList vocabulary)
        , (70, translationExercises11_restricted)
        , (15, translationExercises11_unrestricted)
        , (20, fillingBlanksExercises11)
        ]
    where
        vocabulary = vocabulary11_cumulative

translationExercises11_restricted :: ExerciseGenerator
translationExercises11_restricted = generateRestrictedTranslationExercise "Translate <b>specifying tenses</b>" (const True) sentenceCanonicalizer sentenceComparer translations11_restricted

translationExercises11_unrestricted :: ExerciseGenerator
translationExercises11_unrestricted = generateTranslationExercise sentenceCanonicalizer sentenceComparer translations11_unrestricted

fillingBlanksExercises11 :: ExerciseGenerator
fillingBlanksExercises11 = generateContextualizedBroadFillingBlanksExerciseByAlternatives ["pu", "ca", "ba"] translations11_restricted

-- * Lesson 12: Tanru
-- | Exercises for the twelveth lesson.
exercises12 :: ExerciseGenerator
exercises12 =
    combineGenerators
        [ (10, generateIsolatedBrivlaPlacesExercise dictionary $ vocabularyBrivlaList vocabulary)
        , (70, translationExercises12)
        ]
    where
        vocabulary = vocabulary12_cumulative

translationExercises12 :: ExerciseGenerator
translationExercises12 = combineGenerators [(1, translationExercises12_expressions), (1, translationExercises12_sentences)] where
    translationExercises12_expressions = generateRestrictedTranslationExercise "Translate this expression as a tanru" (const True) sentenceCanonicalizer sentenceComparer translations12_expressions
    translationExercises12_sentences = generateTranslationExercise sentenceCanonicalizer sentenceComparer translations12_sentences

-- * Lesson 13: Checkpoint -- Lessons 8-12
-- | Exercises for the thirteenth lesson.
exercises8to12 :: ExerciseGenerator
exercises8to12 =
    combineGenerators
        [ (10, generateIsolatedBrivlaPlacesExercise dictionary $ vocabularyBrivlaList vocabulary)
        , (70, translationExercises8to12)
        ]
    where
        vocabulary = vocabulary12_cumulative

translationExercises8to12 :: ExerciseGenerator
translationExercises8to12 = combineGenerators
    [ (5, translationExercises8)
    , (2, fillingBlanksExercises8)
    , (5, translationExercises9)
    , (5, translationExercises10)
    , (5, translationExercises11_restricted)
    , (1, translationExercises11_unrestricted)
    , (2, fillingBlanksExercises11)
    , (5, translationExercises12)
    ]


-- * Lesson 14: Quotations 1
-- | Exercises for the fourteenth lesson.
exercises14 :: ExerciseGenerator
exercises14 =
    combineGenerators
        [ (70, translationExercises14)
        ]

translationExercises14 :: ExerciseGenerator
translationExercises14 = generateTranslationExercise sentenceCanonicalizer sentenceComparer $ combineGeneratorsUniformly [translations14_zo, translations14_lu]

-- * Lesson 15: Relative phrases
-- | Exercises for the fifteenth lesson.
exercises15 :: ExerciseGenerator
exercises15 =
    combineGenerators
        [ (70, translationExercises15)
        ]

translationExercises15 :: ExerciseGenerator
translationExercises15 = generateTranslationExercise sentenceCanonicalizer sentenceComparer $ combineGeneratorsUniformly [translations15_expressions, translations15_sentences]

-- * Lesson 16: Logical connectives 1
-- | Exercises for the sixteenth lesson.
exercises16 :: ExerciseGenerator
exercises16 =
    combineGenerators
        [ (70, translationExercises16)
        ]

translationExercises16 :: ExerciseGenerator
translationExercises16 = generateRestrictedTranslationExercise "Translate using <b>sumti connectives</b>" (const True) sentenceCanonicalizer sentenceComparer $ combineGeneratorsUniformly [translations16_a, translations16_e, translations16_o, translations16_u]

-- * Lesson 17: Negation 1
-- | Exercises for the seventeenth lesson.
exercises17 :: ExerciseGenerator
exercises17 =
    combineGenerators
        [ (70, translationExercises17)
        ]

-- TODO: fix canonicalization of "mi na prenu"
translationExercises17 :: ExerciseGenerator
translationExercises17 = generateTranslationExercise sentenceCanonicalizer sentenceComparer $ combineGeneratorsUniformly [translations17_na, translations17_na'e, translations17_to'e]

