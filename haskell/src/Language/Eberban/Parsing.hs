module Language.Eberban.Parsing
( parseText
) where

import qualified Data.Text as T
import qualified Language.Eberban.Parser.Experimental as ExperimentalParser

-- | Parses Eberban text using "Language.Eberban.Parser.Experimental".
parseText :: T.Text -> Either String String
parseText = ExperimentalParser.parse . T.unpack
