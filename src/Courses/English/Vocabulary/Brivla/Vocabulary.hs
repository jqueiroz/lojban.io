-- | This module defines vocabulary for the course.
module Courses.English.Vocabulary.Brivla.Vocabulary where

import Core
import Courses.English.Vocabulary.Brivla.Translations

-- * Auxiliar functions
-- | Extracts vocabulary from 'TranslationsByExpression'.
extractVocabulary :: TranslationsByExpression -> Vocabulary
extractVocabulary translationsByExpression = Vocabulary brivlaList [] [] where
    brivlaList = map fst translationsByExpression

-- * Vocabulary

-- | Vocabulary for the corresponding lesson.
vocabulary01 :: Vocabulary
vocabulary01 = extractVocabulary translations01

-- | Vocabulary for the corresponding lesson.
vocabulary02 :: Vocabulary
vocabulary02 = extractVocabulary translations02

-- | Vocabulary for the corresponding lesson.
vocabulary03 :: Vocabulary
vocabulary03 = extractVocabulary translations03

-- | Vocabulary for the corresponding lesson.
vocabulary04 :: Vocabulary
vocabulary04 = extractVocabulary translations04

-- | Vocabulary for the corresponding lesson.
vocabulary05 :: Vocabulary
vocabulary05 = extractVocabulary translations05
