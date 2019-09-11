{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module defines vocabulary for the course.
module Courses.English.Grammar.Crash.Vocabulary where

import Core
import Courses.Framework.VocabularyLoaders (loadVocabularyFromYamlCode)
import Data.FileEmbed (embedStringFile)

-- | Vocabulary for the first lesson.
vocabulary01 :: Vocabulary
vocabulary01 = loadVocabularyFromYamlCode $(embedStringFile "courses/english/grammar/crash/vocabulary/01.yaml")

-- | Cumulative vocabulary up to the first lesson.
vocabulary01_cumulative :: Vocabulary
vocabulary01_cumulative = vocabulary01

-- | Vocabulary for the second lesson.
vocabulary02 :: Vocabulary
vocabulary02 = loadVocabularyFromYamlCode $(embedStringFile "courses/english/grammar/crash/vocabulary/02.yaml")

-- | Cumulative vocabulary up to the second lesson.
vocabulary02_cumulative :: Vocabulary
vocabulary02_cumulative = vocabulary01_cumulative <> vocabulary02

-- | Vocabulary for the third lesson.
vocabulary03 :: Vocabulary
vocabulary03 = loadVocabularyFromYamlCode $(embedStringFile "courses/english/grammar/crash/vocabulary/03.yaml")

-- | Cumulative vocabulary up to the third lesson.
vocabulary03_cumulative :: Vocabulary
vocabulary03_cumulative = vocabulary02_cumulative <> vocabulary03
