{-# LANGUAGE PartialTypeSignatures #-}

module Language.Eberban.Parsing
( parseText
) where

import qualified Data.Text as T
import qualified Language.Eberban.Parser.Mercury as MercuryParser

-- | Parses Eberban text using "Language.Eberban.Parser.Mercury".
parseText :: T.Text -> Either String _
parseText = MercuryParser.parseText . T.unpack

parseSentence :: T.Text -> Either String _
parseSentence = MercuryParser.parseSentence . T.unpack
