{-# LANGUAGE OverloadedStrings #-}

-- | This module provides utilities for loading translations from files.
module Courses.Framework.TranslationLoaders
( loadTranslationsByExpressionFromYamlCode
) where

import Core
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Yaml as Y

-- | Loads 'TranslationsByExpression' from yaml code.
loadTranslationsByExpressionFromYamlCode :: T.Text -> TranslationsByExpression
loadTranslationsByExpressionFromYamlCode yamlCode = M.assocs $ M.map handleExpression yamlData where
    yamlData :: M.Map T.Text [M.Map T.Text [T.Text]]
    Right yamlData = Y.decodeEither $ TE.encodeUtf8 yamlCode
    handleExpression :: [M.Map T.Text [T.Text]] -> [Translation]
    handleExpression = map handleTranslation
    handleTranslation :: M.Map T.Text [T.Text] -> Translation
    handleTranslation dict = (dict M.! "lojban_sentences", dict M.! "translated_sentences")

-- | Saves 'TranslationsByExpression' to yaml code.
saveTranslationsByExpressionToYamlText :: TranslationsByExpression -> T.Text
saveTranslationsByExpressionToYamlText translationsByExpression = TE.decodeUtf8 $ Y.encode yamlData where
    yamlData :: M.Map T.Text [M.Map T.Text [T.Text]]
    yamlData = M.map encodeExpression $ M.fromList translationsByExpression
    encodeExpression :: [Translation] -> [M.Map T.Text [T.Text]]
    encodeExpression = map encodeTranslation
    encodeTranslation :: Translation -> M.Map T.Text [T.Text]
    encodeTranslation (lojban_sentences, english_sentences) = M.fromList $ [("lojban_sentences", lojban_sentences), ("translated_sentences", english_sentences)]
