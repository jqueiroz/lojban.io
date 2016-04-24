{-# LANGUAGE OverloadedStrings #-}
module Lessons.LessonOne where

import Core
import Control.Applicative ((<$>), (<*>))
import Data.Maybe (catMaybes)
import Data.List (nub)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import qualified Data.Map as M

import Dictionary (loadDictionary)

allGismu = do
    dict <- loadDictionary
    return $ map snd $ M.toList $ dictGismu dict

popularGismu = filter ((>=200) . gismuIRCFrequencyCount) <$> allGismu
popularGismuWords = map gismuText <$> popularGismu

loadGismuFromText :: Dictionary -> T.Text -> [Gismu]
loadGismuFromText dict t = catMaybes $ map lookup words where
    gismu = dictGismu dict
    lookup word = M.lookup word gismu
    words = nub . filter (/= "") . T.splitOn " " . T.replace "\n" " " $ t

loadGismuFromFile :: FilePath -> IO [Gismu]
loadGismuFromFile filePath = do
    dict <- loadDictionary
    loadGismuFromText dict <$> TIO.readFile filePath

terry = loadGismuFromFile "/home/john/Temp/lojban/texts/terry.txt"
terryWords = map gismuText <$> terry
