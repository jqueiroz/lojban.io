{-# LANGUAGE OverloadedStrings #-}
module Playground where

import Language.Lojban.Core
import Language.Lojban.Dictionaries (englishDictionary)
import Language.Lojban.Canonicalization (basicSentenceCanonicalizer)
import Control.Applicative ((<$>))
import Data.Maybe (mapMaybe)
import Data.List.Ordered (nubSort)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import qualified Data.Map as M

-- See also: https://mw.lojban.org/papri/N-grams_of_Lojban_corpus

allGismu :: IO [Gismu]
allGismu = return $ map snd . M.toList . dictGismu $ englishDictionary

popularGismu :: IO [Gismu]
popularGismu = filter ((>=200) . gismuIRCFrequencyCount) <$> allGismu

popularGismuWords :: IO [T.Text]
popularGismuWords = map gismuText <$> popularGismu

loadGismuFromText :: Dictionary -> T.Text -> [Gismu]
loadGismuFromText dict t = mapMaybe lookup words where
    gismu = dictGismu dict
    lookup word = M.lookup word gismu
    words = nubSort . filter (/= "") . T.splitOn " " . T.replace "\n" " " $ t

loadGismuFromFile :: FilePath -> IO [Gismu]
loadGismuFromFile filePath = do
    loadGismuFromText englishDictionary <$> TIO.readFile filePath

terry :: IO [Gismu]
terry = loadGismuFromFile "/home/john/Temp/lojban/texts/terry.txt"

terryWords :: IO [T.Text]
terryWords = map gismuText <$> terry

canonicalizeString :: String -> Either String T.Text
canonicalizeString =  basicSentenceCanonicalizer . T.pack
