module Language.Lojban.Parsing
( parseText
) where

import qualified Data.Text as T
import qualified Language.Lojban.Parser.ZasniGerna as ZG

parseText :: T.Text -> Either String (ZG.Free, ZG.Text, ZG.Terminator)
parseText = ZG.parse . T.unpack
