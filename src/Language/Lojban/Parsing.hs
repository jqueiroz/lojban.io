module Language.Lojban.Parsing
( parse
) where

import qualified Data.Text as T
import qualified Language.Lojban.Parser.ZasniGerna as ZG

parse :: T.Text -> Either String (ZG.Free, ZG.Text, ZG.Terminator)
parse = ZG.parse . T.unpack
