{-# LANGUAGE OverloadedStrings #-}

-- | This module defines exercises for each of the course lessons.
module Courses.English.Grammar.Introduction.Exercises where

import Core
import Courses.Framework.SentenceGenerators (generateNonbridi)
import Courses.Framework.Extractors (extractSimpleBridiGeneratorFromTranslationGenerator, extractTrivialBridiGeneratorFromVocabulary)
import Courses.Framework.ExerciseGenerators
import Courses.Framework.ExerciseUtils (simplifyTerminatorsInCanonicalAnswer)
import Courses.Framework.TranslationUtils (simplifyTerminatorsInTranslationGenerator, narrowTranslationGenerator)
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
-- | Exercises for the lesson.
exercises1 :: ExerciseGenerator
exercises1 =
    combineGenerators
        [ (25, generateEnglishOrLojbanBridiJufraExercise (combineGenerators [(6, bridiGenerator), (3, trivialBridiGenerator)]) nonbridiGenerator bridiDisplayer)
        , (20, generateSelbriIdentificationExercise bridiGenerator bridiDisplayer)
        , (10, generateContextualizedGismuPlacePositionExercise dictionary bridiGenerator bridiDisplayer)
        , (20, generateContextualizedGismuPlaceMeaningExercise dictionary bridiGenerator bridiDisplayer)
        , (40, translationExercises1)
        ]
    where
        vocabulary = vocabulary1_cumulative
        nonbridiGenerator = generateNonbridi vocabulary
        bridiGenerator = extractSimpleBridiGeneratorFromTranslationGenerator translations1
        trivialBridiGenerator = extractTrivialBridiGeneratorFromVocabulary vocabulary
        bridiDisplayer = displayStandardSimpleBridi

-- | Translation exercises for the first lesson.
translationExercises1 :: ExerciseGenerator
translationExercises1 = generateTranslationExercise sentenceCanonicalizer sentenceComparer translations1

-- * Lesson 2: Basics 2
-- | Exercises for the lesson.
exercises2 :: ExerciseGenerator
exercises2 =
    combineGenerators
        [ (15, generateEnglishOrLojbanBridiJufraExercise (combineGenerators [(5, bridiGenerator), (3, trivialBridiGenerator)]) nonbridiGenerator bridiDisplayer)
        , (20, generateSelbriIdentificationExercise bridiGenerator bridiDisplayer)
        , (10, generateContextualizedGismuPlacePositionExercise dictionary bridiGenerator bridiDisplayer)
        , (20, generateContextualizedGismuPlaceMeaningExercise dictionary bridiGenerator bridiDisplayer)
        , (40, translationExercises2)
        ]
    where
        vocabulary = vocabulary2_cumulative
        nonbridiGenerator = generateNonbridi vocabulary
        bridiGenerator = extractSimpleBridiGeneratorFromTranslationGenerator translations2
        trivialBridiGenerator = extractTrivialBridiGeneratorFromVocabulary vocabulary
        bridiDisplayer = combineGenerators [(7, displayStandardSimpleBridi), (3, displayVariantSimpleBridi)]

-- | Translation exercises for the second lesson.
translationExercises2 :: ExerciseGenerator
translationExercises2 = generateTranslationExercise sentenceCanonicalizer sentenceComparer translations2

-- * Lesson 3: Basics 3
-- | Exercises for the lesson.
exercises3 :: ExerciseGenerator
exercises3 =
    combineGenerators
        [ (10, generateEnglishOrLojbanBridiJufraExercise (combineGenerators [(4, bridiGenerator), (3, trivialBridiGenerator)]) nonbridiGenerator bridiDisplayer)
        , (10, generateSelbriIdentificationExercise bridiGenerator bridiDisplayer)
        , (20, generateContextualizedGismuPlacePositionExercise dictionary bridiGenerator bridiDisplayer)
        , (20, generateContextualizedGismuPlaceMeaningExercise dictionary bridiGenerator bridiDisplayer)
        , (30, generateIsolatedBrivlaPlacesExercise dictionary $ vocabularyBrivlaList vocabulary)
        , (60, translationExercises3)
        ]
    where
        vocabulary = vocabulary3_cumulative
        nonbridiGenerator = generateNonbridi vocabulary
        bridiGenerator = extractSimpleBridiGeneratorFromTranslationGenerator translations3
        trivialBridiGenerator = extractTrivialBridiGeneratorFromVocabulary vocabulary
        bridiDisplayer = combineGenerators [(7, displayStandardSimpleBridi), (2, displayVariantSimpleBridi), (1, displayReorderedStandardSimpleBridi)]

-- | Interesting translation exercises for the third lesson: comprises regular exercises involving interesting translations, as well as "Translate without using zo'e" exercises involving restricted translations.
--
-- Defined separately so that they may be reused in the next checkpoint lesson.
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

-- * Lesson 4: Tanru
-- | Exercises for the lesson.
exercises4 :: ExerciseGenerator
exercises4 =
    combineGenerators
        [ (10, generateContextualizedGismuPlacePositionExercise dictionary bridiGenerator bridiDisplayer)
        , (20, generateContextualizedGismuPlaceMeaningExercise dictionary bridiGenerator bridiDisplayer)
        , (20, generateIsolatedBrivlaPlacesExercise dictionary $ vocabularyBrivlaList vocabulary)
        , (100, translationExercises4)
        ]
    where
        vocabulary = vocabulary4_cumulative
        -- Narrowing the 'TranslationGenerator' is required to avoid alternative translations using "poi"
        bridiGenerator = extractSimpleBridiGeneratorFromTranslationGenerator $ narrowTranslationGenerator translations4_sentences
        bridiDisplayer = combineGenerators [(7, displayStandardSimpleBridi), (2, displayVariantSimpleBridi), (1, displayReorderedStandardSimpleBridi)]

translationExercises4 :: ExerciseGenerator
translationExercises4 = combineGenerators [(1, translationExercises4_expressions), (1, translationExercises4_sentences)] where
    translationExercises4_expressions = generateRestrictedTranslationExercise "Translate this expression as a tanru" (const True) sentenceCanonicalizer sentenceComparer translations4_expressions
    translationExercises4_sentences = generateTranslationExercise sentenceCanonicalizer sentenceComparer translations4_sentences

-- * Lesson 5: Questions 1
-- | Exercises for the lesson.
exercises5 :: ExerciseGenerator
exercises5 =
    combineGenerators
        [ (20, generateIsolatedBrivlaPlacesExercise dictionary $ vocabularyBrivlaList vocabulary)
        , (20, generateContextualizedGismuPlaceMeaningExercise dictionary bridiGenerator bridiDisplayer)
        , (20, questionExercises5)
        , (80, translationExercises5)
        ]
    where
        vocabulary = vocabulary5_cumulative
        bridiGenerator = extractSimpleBridiGeneratorFromTranslationGenerator translations5
        bridiDisplayer = combineGenerators [(7, displayStandardSimpleBridi), (2, displayVariantSimpleBridi), (1, displayReorderedStandardSimpleBridi)]

translationExercises5 :: ExerciseGenerator
translationExercises5 = combineGenerators [(1, restricted), (5, unrestricted)] where
    restricted = generateBlacklistedWordTranslationExercise "zo'e" sentenceCanonicalizer sentenceComparer translations5_restricted
    unrestricted = generateTranslationExercise sentenceCanonicalizer sentenceComparer translations5

questionExercises5 :: ExerciseGenerator
questionExercises5 = generateFillingBlanksExerciseByAlternatives ["mo", "ma"] $ narrowTranslationGenerator $ combineGeneratorsUniformly [translations5_ma, translations5_mo]

questionExercises5_simplified :: ExerciseGenerator
questionExercises5_simplified = generateFillingBlanksExerciseByAlternatives ["mo", "ma"] $ simplifyTerminatorsInTranslationGenerator $ narrowTranslationGenerator $ combineGeneratorsUniformly [translations5_ma, translations5_mo]

-- * Lesson 6: Abstractions 1
-- | Exercises for the lesson.
exercises6 :: ExerciseGenerator
exercises6 =
    combineGenerators
        [ (30, generateIsolatedBrivlaPlacesExercise dictionary $ vocabularyBrivlaList vocabulary)
        , (10, generateContextualizedGismuPlaceMeaningExercise dictionary bridiGenerator bridiDisplayer)
        , (40, abstractionExercises6)
        , (70, translationExercises6)
        ]
    where
        vocabulary = vocabulary6_cumulative
        bridiGenerator = extractSimpleBridiGeneratorFromTranslationGenerator translations6
        bridiDisplayer = combineGenerators [(7, displayStandardSimpleBridi), (2, displayVariantSimpleBridi), (1, displayReorderedStandardSimpleBridi)]

translationExercises6 :: ExerciseGenerator
translationExercises6 = generateTranslationExercise sentenceCanonicalizer sentenceComparer translations6

-- Narrowing the 'TranslationGenerator' is required to avoid alternative translations using "ko'a"
abstractionExercises6 :: ExerciseGenerator
abstractionExercises6 = generateFillingBlanksExerciseByAlternatives ["lo nu", "lo du'u", "lo se du'u"] $ narrowTranslationGenerator $ combineGeneratorsUniformly [translations6_nu, translations6_du'u, translations6_sedu'u]

-- Narrowing the 'TranslationGenerator' is required to avoid alternative translations using "ko'a"
abstractionExercises6_simplified :: ExerciseGenerator
abstractionExercises6_simplified = generateFillingBlanksExerciseByAlternatives ["lo nu", "lo du'u", "lo se du'u"] $ simplifyTerminatorsInTranslationGenerator $ narrowTranslationGenerator $ combineGeneratorsUniformly [translations6_nu, translations6_du'u, translations6_sedu'u]

-- * Lesson 7: Terminator elision
-- | Exercises for the lesson.
exercises7 :: ExerciseGenerator
exercises7 =
    combineGenerators
        [ (20, generateIsolatedBrivlaPlacesExercise dictionary $ vocabularyBrivlaList vocabulary)
        , (10, generateContextualizedGismuPlaceMeaningExercise dictionary bridiGenerator bridiDisplayer)
        , (70, translationExercises7_restricted)
        ]
    where
        vocabulary = vocabulary6_cumulative
        bridiGenerator = extractSimpleBridiGeneratorFromTranslationGenerator translations7
        bridiDisplayer = simplifyTerminatorsInBridiDisplayer $ (combineGenerators [(7, displayStandardSimpleBridi), (2, displayVariantSimpleBridi), (1, displayReorderedStandardSimpleBridi)])

translationExercises7_restricted :: ExerciseGenerator
translationExercises7_restricted = generateBlacklistedWordTranslationExercise "ku" sentenceCanonicalizer sentenceComparer translations7_restricted

-- * Lesson 8: Checkpoint -- Lessons 1-7
-- | Exercises for the seventh lesson.
exercises1to7 :: ExerciseGenerator
exercises1to7 =
    combineGenerators
        [ (5, generateEnglishOrLojbanBridiJufraExercise (combineGenerators [(3, bridiGenerator), (3, trivialBridiGenerator)]) nonbridiGenerator bridiDisplayer)
        , (5, generateSelbriIdentificationExercise bridiGenerator bridiDisplayer)
        , (5, generateContextualizedGismuPlacePositionExercise dictionary bridiGenerator bridiDisplayer)
        , (15, generateContextualizedGismuPlaceMeaningExercise dictionary bridiGenerator bridiDisplayer)
        , (15, generateIsolatedBrivlaPlacesExercise dictionary $ vocabularyBrivlaList vocabulary)
        , (60, translationExercises1to7_simplified)
        , (12, questionExercises5_simplified)
        , (12, abstractionExercises6_simplified)
        ]
    where
        vocabulary = vocabulary7_cumulative
        bridiGenerator = extractSimpleBridiGeneratorFromTranslationGenerator translations1to7
        trivialBridiGenerator = extractTrivialBridiGeneratorFromVocabulary vocabulary
        nonbridiGenerator = generateNonbridi vocabulary
        bridiDisplayer = simplifyTerminatorsInBridiDisplayer $ (combineGenerators [(7, displayStandardSimpleBridi), (2, displayVariantSimpleBridi), (1, displayReorderedStandardSimpleBridi)])

translationExercises1to7_simplified :: ExerciseGenerator
translationExercises1to7_simplified = simplifyTerminatorsInCanonicalAnswer . combineGenerators [(4, translationExercises3_nice), (1, translationExercises3_normal), (5, translationExercises4), (5, translationExercises5), (6, translationExercises6), (5, translationExercises7_restricted)]

-- * Lesson 9: Relative clauses
-- | Exercises for the lesson.
exercises9 :: ExerciseGenerator
exercises9 =
    combineGenerators
        [ (20, generateIsolatedBrivlaPlacesExercise dictionary $ vocabularyBrivlaList vocabulary)
        , (70, translationExercises9)
        , (15, fillingBlanksExercises9)
        ]
    where
        vocabulary = vocabulary9_cumulative

translationExercises9 :: ExerciseGenerator
translationExercises9 = generateTranslationExercise sentenceCanonicalizer sentenceComparer translations9

fillingBlanksExercises9 :: ExerciseGenerator
fillingBlanksExercises9 = generateContextualizedFillingBlanksExerciseByAlternatives ["poi", "noi"] (narrowTranslationGenerator translations9)

-- * Lesson 10: Linked sumti
-- | Exercises for the lesson.
exercises10 :: ExerciseGenerator
exercises10 =
    combineGenerators
        [ (20, generateIsolatedBrivlaPlacesExercise dictionary $ vocabularyBrivlaList vocabulary)
        , (70, translationExercises10)
        ]
    where
        vocabulary = vocabulary10_cumulative

translationExercises10 :: ExerciseGenerator
translationExercises10 = generateTranslationExercise sentenceCanonicalizer sentenceComparer translations10

-- * Lesson 11: Sumtcita
-- | Exercises for the lesson.
exercises11 :: ExerciseGenerator
exercises11 =
    combineGenerators
        [ (20, generateIsolatedBrivlaPlacesExercise dictionary $ vocabularyBrivlaList vocabulary)
        , (80, translationExercises11)
        ]
    where
        vocabulary = vocabulary11_cumulative

translationExercises11 :: ExerciseGenerator
translationExercises11 = generateTranslationExercise sentenceCanonicalizer sentenceComparer translations11

-- * Lesson 12: Tenses 1
-- | Exercises for the lesson.
exercises12 :: ExerciseGenerator
exercises12 =
    combineGenerators
        [ (10, generateIsolatedBrivlaPlacesExercise dictionary $ vocabularyBrivlaList vocabulary)
        , (70, translationExercises12_restricted)
        , (15, translationExercises12_unrestricted)
        , (20, fillingBlanksExercises12)
        ]
    where
        vocabulary = vocabulary12_cumulative

translationExercises12_restricted :: ExerciseGenerator
translationExercises12_restricted = generateRestrictedTranslationExercise "Translate <b>specifying tenses</b>" (const True) sentenceCanonicalizer sentenceComparer translations12_restricted

translationExercises12_unrestricted :: ExerciseGenerator
translationExercises12_unrestricted = generateTranslationExercise sentenceCanonicalizer sentenceComparer translations12_unrestricted

fillingBlanksExercises12 :: ExerciseGenerator
fillingBlanksExercises12 = generateContextualizedFillingBlanksExerciseByAlternatives ["pu", "ca", "ba"] translations12_restricted

-- * Lesson 13: Checkpoint -- Lessons 9-12
-- | Exercises for the lesson.
exercises9to12 :: ExerciseGenerator
exercises9to12 =
    combineGenerators
        [ (10, generateIsolatedBrivlaPlacesExercise dictionary $ vocabularyBrivlaList vocabulary)
        , (70, translationExercises9to12)
        ]
    where
        vocabulary = vocabulary12_cumulative

translationExercises9to12 :: ExerciseGenerator
translationExercises9to12 = combineGenerators
    [ (5, translationExercises9)
    , (2, fillingBlanksExercises9)
    , (5, translationExercises10)
    , (5, translationExercises11)
    , (5, translationExercises12_restricted)
    , (1, translationExercises12_unrestricted)
    , (2, fillingBlanksExercises12)
    ]


-- * Lesson 14: Quotations 1
-- | Exercises for the lesson.
exercises14 :: ExerciseGenerator
exercises14 =
    combineGenerators
        [ (70, translationExercises14)
        ]

translationExercises14 :: ExerciseGenerator
translationExercises14 = generateTranslationExercise sentenceCanonicalizer sentenceComparer $ combineGeneratorsUniformly [translations14_zo, translations14_lu]

-- * Lesson 15: Relative phrases
-- | Exercises for the lesson.
exercises15 :: ExerciseGenerator
exercises15 =
    combineGenerators
        [ (70, translationExercises15)
        ]

translationExercises15 :: ExerciseGenerator
translationExercises15 = generateTranslationExercise sentenceCanonicalizer sentenceComparer $ combineGeneratorsUniformly [translations15_expressions, translations15_sentences]

-- * Lesson 16: Logical connectives 1
-- | Exercises for the lesson.
exercises16 :: ExerciseGenerator
exercises16 =
    combineGenerators
        [ (70, translationExercises16)
        ]

translationExercises16 :: ExerciseGenerator
translationExercises16 = generateRestrictedTranslationExercise "Translate using <b>sumti connectives</b>" (const True) sentenceCanonicalizer sentenceComparer $ combineGeneratorsUniformly [translations16_a, translations16_e, translations16_o, translations16_u]

-- * Lesson 17: Negation 1
-- | Exercises for the lesson.
exercises17 :: ExerciseGenerator
exercises17 =
    combineGenerators
        [ (70, translationExercises17)
        , (50, fillingBlanksExercises17)
        ]

translationExercises17 :: ExerciseGenerator
translationExercises17 = generateTranslationExercise sentenceCanonicalizer sentenceComparer translations17

fillingBlanksExercises17 :: ExerciseGenerator
fillingBlanksExercises17 = generateContextualizedFillingBlanksExerciseByAlternatives ["na", "na'e", "no'e", "to'e"] translations17

-- * Lesson 18: Misc 1
-- | Exercises for the lesson.
exercises18 :: ExerciseGenerator
exercises18 =
    combineGenerators
        [ (70, translationExercises18)
        ]

translationExercises18 :: ExerciseGenerator
translationExercises18 = generateTranslationExercise sentenceCanonicalizer sentenceComparer translations18

-- * Lesson 19: Checkpoint -- Lessons 14-18
-- | Exercises for the lesson.
exercises14to18 :: ExerciseGenerator
exercises14to18 =
    combineGenerators
        [ (10, exercises14)
        , (10, exercises15)
        , (10, exercises16)
        , (10, exercises17)
        , (10, exercises18)
        ]

-- * Lesson 20: Morphology
-- | Exercises for the lesson.
exercises20 :: ExerciseGenerator
exercises20 = generateMorphologicalClassExercise vocabulary where
    vocabulary = vocabulary20_cumulative

-- * Lesson 21: Gadri 1
-- | Exercises for the lesson.
exercises21 :: ExerciseGenerator
exercises21 =
    combineGenerators
        [
        ]

-- * Lesson 22: Numbers 1
-- | Exercises for the lesson.
exercises22 :: ExerciseGenerator
exercises22 =
    combineGenerators
        [
        ]

-- * Lesson 23: Tenses 2
-- | Exercises for the lesson.
exercises23 :: ExerciseGenerator
exercises23 =
    combineGenerators
        [
        ]

-- * Lesson 24: Gadri 2
-- | Exercises for the lesson.
exercises24 :: ExerciseGenerator
exercises24 =
    combineGenerators
        [
        ]

-- * Lesson 25: Checkpoint -- Lessons 20-24
-- | Exercises for the lesson.
exercises20to24 :: ExerciseGenerator
exercises20to24 =
    combineGenerators
        [ (10, exercises20)
        , (10, exercises21)
        , (10, exercises22)
        , (10, exercises23)
        , (10, exercises24)
        ]

-- * Lesson 26: Quantifying sumti 1
-- | Exercises for the lesson.
exercises26 :: ExerciseGenerator
exercises26 =
    combineGenerators
        [
        ]

-- * Lesson 27: Tenses 3
-- | Exercises for the lesson.
exercises27 :: ExerciseGenerator
exercises27 =
    combineGenerators
        [
        ]
