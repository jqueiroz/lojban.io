{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module defines translations for the course.
module Courses.English.Vocabulary.Brivla.Translations where

import Core
import Courses.Framework.TranslationLoaders (loadTranslationsByExpressionFromYamlText)
import Data.FileEmbed (embedStringFile)

-- | Translations for the corresponding lesson.
translations01 :: TranslationsByExpression
translations01 = loadTranslationsByExpressionFromYamlText $(embedStringFile "courses/english/vocabulary/brivla/01_easy.yaml")

-- | Translations for the corresponding lesson.
translations02 :: TranslationsByExpression
translations02 = loadTranslationsByExpressionFromYamlText $(embedStringFile "courses/english/vocabulary/brivla/02_easy.yaml")

-- | Translations for the corresponding lesson.
translations03 :: TranslationsByExpression
translations03 = loadTranslationsByExpressionFromYamlText $(embedStringFile "courses/english/vocabulary/brivla/03_easy.yaml")

-- | Translations for the corresponding lesson.
translations04 :: TranslationsByExpression
translations04 = loadTranslationsByExpressionFromYamlText $(embedStringFile "courses/english/vocabulary/brivla/04_easy.yaml")

-- | Translations for the corresponding lesson.
translations05 :: TranslationsByExpression
translations05 = loadTranslationsByExpressionFromYamlText $(embedStringFile "courses/english/vocabulary/brivla/05_easy.yaml")
