{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module defines translations for the course.
module Courses.English.Vocabulary.Brivla.Translations where

import Core
import Courses.Framework.TranslationLoaders (loadTranslationsByExpressionFromYamlCode)
import Data.FileEmbed (embedStringFile)

-- | Translations for the corresponding lesson.
translationsByExpression01 :: TranslationsByExpression
translationsByExpression01 = loadTranslationsByExpressionFromYamlCode $(embedStringFile "resources/courses/english/vocabulary/brivla/01_easy.yaml")

-- | Translations for the corresponding lesson.
translationsByExpression02 :: TranslationsByExpression
translationsByExpression02 = loadTranslationsByExpressionFromYamlCode $(embedStringFile "resources/courses/english/vocabulary/brivla/02_easy.yaml")

-- | Translations for the corresponding lesson.
translationsByExpression03 :: TranslationsByExpression
translationsByExpression03 = loadTranslationsByExpressionFromYamlCode $(embedStringFile "resources/courses/english/vocabulary/brivla/03_easy.yaml")

-- | Translations for the corresponding lesson.
translationsByExpression04 :: TranslationsByExpression
translationsByExpression04 = loadTranslationsByExpressionFromYamlCode $(embedStringFile "resources/courses/english/vocabulary/brivla/04_easy.yaml")

-- | Translations for the corresponding lesson.
translationsByExpression05 :: TranslationsByExpression
translationsByExpression05 = loadTranslationsByExpressionFromYamlCode $(embedStringFile "resources/courses/english/vocabulary/brivla/05_easy.yaml")

-- | All translations.
translationsByExpression :: TranslationsByExpression
translationsByExpression = mconcat [translationsByExpression01, translationsByExpression02, translationsByExpression03, translationsByExpression04, translationsByExpression05]
