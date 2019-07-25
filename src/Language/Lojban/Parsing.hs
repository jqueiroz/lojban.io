module Language.Lojban.Parsing
( parseText
) where

import qualified Data.Text as T
import qualified Language.Lojban.Parser.ZasniGerna as ZG

-- | Parses Lojban text using "Language.Lojban.Parser.ZasniGerna".
parseText :: T.Text -> Either String (ZG.Free, ZG.Text, ZG.Terminator)
parseText = ZG.parse . T.unpack
