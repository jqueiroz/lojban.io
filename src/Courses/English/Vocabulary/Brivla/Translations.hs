{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module defines translations for the course.
module Courses.English.Vocabulary.Brivla.Translations where

import Core
import Data.List.Split (chunksOf)
import Data.FileEmbed (embedStringFile)
import qualified Data.Text as T

-- * Auxiliar functions
loadTranslationsByExpression :: T.Text -> TranslationsByExpression
loadTranslationsByExpression contents = map loadTranslations $ T.splitOn "\n\n\n" contents where
    loadTranslations :: T.Text -> (T.Text, [Translation])
    loadTranslations contents = (title, translations) where
        title_line:translation_lines = T.lines contents
        title = head $ T.splitOn ":" title_line
        translations = map makeTranslation $ chunksOf 2 translation_lines where
            makeTranslation [lojban_line, english_line] = ([(T.splitOn "\t" lojban_line) !! 1], [(T.splitOn "\t\t" english_line) !! 1])

-- * Translations

-- | Translations for the corresponding lesson.
translations01 :: TranslationsByExpression
translations01 = loadTranslationsByExpression $(embedStringFile "courses/english/vocabulary/brivla/01_easy.txt")

-- | Translations for the corresponding lesson.
translations02 :: TranslationsByExpression
translations02 = loadTranslationsByExpression $(embedStringFile "courses/english/vocabulary/brivla/02_easy.txt")

-- | Translations for the corresponding lesson.
translations03 :: TranslationsByExpression
translations03 = loadTranslationsByExpression $(embedStringFile "courses/english/vocabulary/brivla/03_easy.txt")
