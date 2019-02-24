{-# LANGUAGE OverloadedStrings #-}

module DictionaryLoader
( loadDictionary
) where

import Core
import Util (subfield)
import Control.Applicative ((<$>), (<*>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map as M

-- Dictionary
loadDictionary :: IO Dictionary
loadDictionary = do
    -- Frequency map
    frequencyMap <- loadFrequencyMapFromFile
    -- Cmavo
    cmavo <- loadCmavoFromFile frequencyMap
    let cmavoMap = M.fromList $ map (\c -> (cmavoText c, c)) cmavo
    let isReallyGismu gismu = M.lookup (gismuText gismu) cmavoMap == Nothing
    -- Gismu
    gismu <- filter isReallyGismu <$> loadGismuFromFile frequencyMap
    let gismuMap = M.fromList $ map (\g -> (gismuText g, g)) gismu
    return $ Dictionary gismuMap cmavoMap

-- Gismu
loadGismuFromLine :: FrequencyMap -> T.Text -> Gismu
loadGismuFromLine frequencyMap line =
    let text = subfield 1 6 line
        rafsi1 = subfield 7 10 line
        rafsi2 = subfield 11 14 line
        rafsi3 = subfield 15 19 line
        englishSumtiPlaces = retrieveEnglishSumtiPlaces text
        englishKeyword1 = subfield 20 41 line
        englishKeyword2 = T.replace "'" "" $ subfield 41 62 line
        englishDefinition = subfield 62 158 line
        teachingCode = subfield 159 161 line
        oldFrequencyCount = (read . T.unpack $ subfield 161 165 line) :: Int
        englishFullNotes = T.strip $ T.drop 165 line
        (englishNotes, confer) = parseNotes englishFullNotes
    in Gismu text (filter (/=T.empty) [rafsi1, rafsi2, rafsi3]) englishSumtiPlaces (filter (/=T.empty) [englishKeyword1, englishKeyword2]) englishDefinition englishNotes confer teachingCode oldFrequencyCount (M.findWithDefault 0 text frequencyMap)

loadGismuFromText :: FrequencyMap -> T.Text -> [Gismu]
loadGismuFromText frequencyMap = fmap (loadGismuFromLine frequencyMap) . tail . T.lines

loadGismuFromFile :: FrequencyMap -> IO [Gismu]
loadGismuFromFile frequencyMap = loadGismuFromText frequencyMap <$> TIO.readFile "resources/gismu.txt"

-- Cmavo
loadCmavoFromLine :: FrequencyMap -> T.Text -> Cmavo
loadCmavoFromLine frequencyMap line =
    let text = subfield 0 11 line
        englishClassification = subfield 11 20 line
        englishKeyword = subfield 20 62 line
        englishDefinition = subfield 62 168 line
        englishFullNotes = T.strip $ T.drop 168 line
        (englishNotes, confer) = parseNotes englishFullNotes
    in Cmavo text englishClassification englishKeyword englishDefinition englishNotes confer (M.findWithDefault 0 text frequencyMap)

loadCmavoFromText :: FrequencyMap -> T.Text -> [Cmavo]
loadCmavoFromText frequencyMap = fmap (loadCmavoFromLine frequencyMap) . tail . T.lines

loadCmavoFromFile :: FrequencyMap -> IO [Cmavo]
loadCmavoFromFile frequencyMap = loadCmavoFromText frequencyMap <$> TIO.readFile "resources/cmavo.txt"

-- Helper functions
parseNotes :: T.Text -> (T.Text, [T.Text])
parseNotes englishFullNotes =
    case T.splitOn "(cf. " englishFullNotes of
        englishNotes:confer':_ -> (englishNotes, filter isSingleWord . map T.strip . T.splitOn ", " . T.takeWhile (/=')') $ confer')
        englishNotes:_ -> (englishNotes, [])
    where
        isSingleWord :: T.Text -> Bool
        isSingleWord x = length (T.words x) == 1

englishSumtiPlacesBase :: M.Map T.Text [T.Text]
englishSumtiPlacesBase = M.fromList
    [ ("tavla", ["speaker", "listener", "subject", "language"])
    , ("dunda", ["donor", "gift", "recipient"])
    , ("ctuca", ["instructor", "audience/student(s)", "ideas/methods", "subject", "teaching method"])
    , ("citka", ["consumer", "aliment"])
    , ("ciska", ["writer", "text/symbols", "display/storage medium", "writing implement"])
    , ("klama", ["traveler", "destination", "origin", "route", "means/vehicle"])
    , ("bridi", ["predicate relationship", "relation", "arguments"])
    , ("djuno", ["entity", "facts", "subject", "epistemology"])
    , ("nupre", ["agent", "promise", "beneficiary/victim"])
    , ("cusku", ["agent", "message", "audience", "expressive medium"])
    -- TODO: nelci
    -- TODO: pendo
    -- TODO: melbi
    -- TODO: gleki
    -- TODO: prenu
    -- TODO: mlatu
    -- TODO: gerku
    -- TODO: zdani
    ] -- TODO: ask people to build a database

retrieveEnglishSumtiPlaces :: T.Text -> [T.Text]
retrieveEnglishSumtiPlaces sumti =
    let places = M.findWithDefault [] sumti englishSumtiPlacesBase
    in if places == []
        then error $ "Missing sumti places for '" ++ (T.unpack sumti) ++ "'"
        else places

-- Frequency map
type FrequencyMap = M.Map T.Text Int

loadFrequencyPairFromLine :: T.Text -> (T.Text, Int)
loadFrequencyPairFromLine line = (w, read $ T.unpack f) where
    f:w:[] = T.splitOn " " line

loadFrequencyMapFromFile :: IO FrequencyMap
loadFrequencyMapFromFile = M.fromList . map loadFrequencyPairFromLine . map (T.replace "\r" "") . T.lines <$> TIO.readFile "resources/MyFreq-COMB_without_dots.txt"
