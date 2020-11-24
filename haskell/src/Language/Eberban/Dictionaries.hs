{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Eberban.Dictionaries
( officialDictionary
) where

import Language.Eberban.Core
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Map as M
import qualified Data.Yaml as Y
import Data.FileEmbed (embedFile)

-- | Official Dictionary
officialDictionary :: Dictionary
officialDictionary = Dictionary "official" entryMap where
    entryList = loadEntriesFromYamlText $ TE.decodeUtf8 $(embedFile "resources/language/eberban/words.yaml")
    entryMap = M.fromList $ map (\entry -> (entryText entry, entry)) entryList

-- | Loads entries from yaml-encoded text.
loadEntriesFromYamlText :: T.Text -> [Entry]
loadEntriesFromYamlText yamlText = map handleEntry yamlList where
    yamlData :: M.Map T.Text (M.Map T.Text T.Text)
    yamlData = case Y.decodeEither $ TE.encodeUtf8 yamlText of
        Left msg -> error $ "Failed to load eberban entries: " ++ msg
        Right x -> x
    yamlList :: [(T.Text, M.Map T.Text T.Text)]
    yamlList = M.assocs yamlData
    handleEntry :: (T.Text, M.Map T.Text T.Text) -> Entry
    handleEntry (entryKey, entryData) = Entry entryKey entryFamily entrySignature entryEnglishShort entryEnglishLong entryLojbanSimilar where
        entryFamily = (entryData M.! "_family")
        entrySignature = (entryData M.!? "_signature")
        entryEnglishShort = (entryData M.! "eng_short")
        entryEnglishLong = (entryData M.! "eng_long")
        entryLojbanSimilar = (entryData M.!? "jbo_similar")
