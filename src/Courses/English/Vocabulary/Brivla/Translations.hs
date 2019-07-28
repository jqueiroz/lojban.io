{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module defines translations for the course.
module Courses.English.Vocabulary.Brivla.Translations where

import Core
import Courses.Framework.TranslationLoaders (loadTranslationsByExpressionFromYamlCode)
import Data.FileEmbed (embedStringFile)

-- | Translations for the corresponding lesson.
translations01 :: TranslationsByExpression
translations01 = loadTranslationsByExpressionFromYamlCode $(embedStringFile "courses/english/vocabulary/brivla/01_easy.yaml")

-- | Translations for the corresponding lesson.
translations02 :: TranslationsByExpression
translations02 = loadTranslationsByExpressionFromYamlCode $(embedStringFile "courses/english/vocabulary/brivla/02_easy.yaml")

-- | Translations for the corresponding lesson.
translations03 :: TranslationsByExpression
translations03 = loadTranslationsByExpressionFromYamlCode $(embedStringFile "courses/english/vocabulary/brivla/03_easy.yaml")

-- | Translations for the corresponding lesson.
translations04 :: TranslationsByExpression
translations04 = loadTranslationsByExpressionFromYamlCode $(embedStringFile "courses/english/vocabulary/brivla/04_easy.yaml")

-- | Translations for the corresponding lesson.
translations05 :: TranslationsByExpression
translations05 = loadTranslationsByExpressionFromYamlCode $(embedStringFile "courses/english/vocabulary/brivla/05_easy.yaml")
