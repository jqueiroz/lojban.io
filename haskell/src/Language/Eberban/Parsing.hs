{-# LANGUAGE PartialTypeSignatures #-}

module Language.Eberban.Parsing
( parseText
) where

import qualified Data.Text as T
import qualified Language.Eberban.Parser.Experimental as ExperimentalParser

-- | Parses Eberban text using "Language.Eberban.Parser.Experimental".
parseText :: T.Text -> Either String _
parseText = ExperimentalParser.parseText . T.unpack

parseSentence :: T.Text -> Either String _
parseSentence = ExperimentalParser.parseSentence . T.unpack
