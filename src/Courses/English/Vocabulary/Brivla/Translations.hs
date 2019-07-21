{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module defines translations for the course.
module Courses.English.Vocabulary.Brivla.Translations where

import Core
import Data.List.Split (chunksOf)
import Data.FileEmbed (embedStringFile)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Map as M
import qualified Data.Yaml as Y

-- * Auxiliar functions
loadTranslationsByExpressionFromRawText :: T.Text -> TranslationsByExpression
loadTranslationsByExpressionFromRawText contents = map loadTranslations $ T.splitOn "\n\n\n" contents where
    loadTranslations :: T.Text -> (T.Text, [Translation])
    loadTranslations contents = (title, translations) where
        title_line:translation_lines = T.lines contents
        title = head $ T.splitOn ":" title_line
        translations = map makeTranslation $ chunksOf 2 translation_lines where
            makeTranslation [lojban_line, english_line] = ([(T.splitOn "\t" lojban_line) !! 1], [(T.splitOn "\t\t" english_line) !! 1])

loadTranslationsByExpressionFromYamlText :: T.Text -> TranslationsByExpression
loadTranslationsByExpressionFromYamlText yamlText = M.assocs $ M.map handleExpression yamlData where
    yamlData :: M.Map T.Text [M.Map T.Text [T.Text]]
    Right yamlData = Y.decodeEither $ TE.encodeUtf8 yamlText
    handleExpression :: [M.Map T.Text [T.Text]] -> [Translation]
    handleExpression = map handleTranslation
    handleTranslation :: M.Map T.Text [T.Text] -> Translation
    handleTranslation dict = (dict M.! "lojban_sentences", dict M.! "translated_sentences")

-- * Translations

-- | Translations for the corresponding lesson.
translations01 :: TranslationsByExpression
translations01 = loadTranslationsByExpressionFromRawText $(embedStringFile "courses/english/vocabulary/brivla/01_easy.txt")

-- | Translations for the corresponding lesson.
translations02 :: TranslationsByExpression
translations02 = loadTranslationsByExpressionFromRawText $(embedStringFile "courses/english/vocabulary/brivla/02_easy.txt")

-- | Translations for the corresponding lesson.
translations03 :: TranslationsByExpression
translations03 = loadTranslationsByExpressionFromRawText $(embedStringFile "courses/english/vocabulary/brivla/03_easy.txt")

-- | Translations for the corresponding lesson.
translations04 :: TranslationsByExpression
translations04 = loadTranslationsByExpressionFromYamlText $(embedStringFile "courses/english/vocabulary/brivla/04_easy.yaml")

-- | Translations for the corresponding lesson.
translations05 :: TranslationsByExpression
translations05 = loadTranslationsByExpressionFromYamlText $(embedStringFile "courses/english/vocabulary/brivla/05_easy.yaml")
