-- | This module defines vocabulary for the course.
module Study.Courses.English.Vocabulary.Brivla.Vocabulary where

import Core
import Study.Courses.English.Vocabulary.Brivla.Translations

-- * Auxiliar functions
-- | Extracts vocabulary from 'TranslationsByExpression'.
extractVocabulary :: TranslationsByExpression -> Vocabulary
extractVocabulary translationsByExpression = Vocabulary brivlaList [] [] where
    brivlaList = map fst translationsByExpression

-- * Vocabulary

-- | Vocabulary for the corresponding lesson.
vocabulary01 :: Vocabulary
vocabulary01 = extractVocabulary translationsByExpression01

-- | Vocabulary for the corresponding lesson.
vocabulary02 :: Vocabulary
vocabulary02 = extractVocabulary translationsByExpression02

-- | Vocabulary for the corresponding lesson.
vocabulary03 :: Vocabulary
vocabulary03 = extractVocabulary translationsByExpression03

-- | Vocabulary for the corresponding lesson.
vocabulary04 :: Vocabulary
vocabulary04 = extractVocabulary translationsByExpression04

-- | Vocabulary for the corresponding lesson.
vocabulary05 :: Vocabulary
vocabulary05 = extractVocabulary translationsByExpression05
