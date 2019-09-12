{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module provides translations (and exercises relying on translations) for each of the course lessons.
module Courses.English.Grammar.Crash.Translations where

import Core
import Courses.Framework.TranslationLoaders (loadTranslationGeneratorFromYamlCode)
import Data.FileEmbed (embedStringFile)

-- | Translations for the corresponding lesson.
translations01 :: TranslationGenerator
translations01 = loadTranslationGeneratorFromYamlCode $(embedStringFile "resources/courses/english/grammar/crash/translations/01.yaml")

-- | Translations for the corresponding lesson.
translations02 :: TranslationGenerator
translations02 = loadTranslationGeneratorFromYamlCode $(embedStringFile "resources/courses/english/grammar/crash/translations/02.yaml")

-- | Translations for the corresponding lesson.
translations03 :: TranslationGenerator
translations03 = loadTranslationGeneratorFromYamlCode $(embedStringFile "resources/courses/english/grammar/crash/translations/03.yaml")
