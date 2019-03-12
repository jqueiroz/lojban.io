{-# LANGUAGE OverloadedStrings #-}
module Playground where

import Core
import Control.Applicative ((<$>), (<*>))
import Data.Maybe (mapMaybe)
import Data.List (nub)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import qualified Data.Map as M

import DictionaryLoader (loadDictionary)

-- See also: https://mw.lojban.org/papri/N-grams_of_Lojban_corpus

allGismu = do
    dict <- loadDictionary
    return $ map snd $ M.toList $ dictGismu dict

popularGismu = filter ((>=200) . gismuIRCFrequencyCount) <$> allGismu
popularGismuWords = map gismuText <$> popularGismu

loadGismuFromText :: Dictionary -> T.Text -> [Gismu]
loadGismuFromText dict t = mapMaybe lookup words where
    gismu = dictGismu dict
    lookup word = M.lookup word gismu
    words = nub . filter (/= "") . T.splitOn " " . T.replace "\n" " " $ t

loadGismuFromFile :: FilePath -> IO [Gismu]
loadGismuFromFile filePath = do
    dict <- loadDictionary
    loadGismuFromText dict <$> TIO.readFile filePath

terry = loadGismuFromFile "/home/john/Temp/lojban/texts/terry.txt"
terryWords = map gismuText <$> terry
