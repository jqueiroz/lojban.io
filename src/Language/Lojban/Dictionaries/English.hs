{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Lojban.Dictionaries.English
( englishDictionary
) where

import Language.Lojban.Core
import Util (subfield)
import Control.Applicative ((<$>))
import Control.Arrow (second)
import Data.Maybe (isNothing, catMaybes)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Map as M
import qualified Data.Yaml as Y
import Data.FileEmbed (embedStringFile)

-- Dictionary
englishDictionary :: Dictionary
englishDictionary = Dictionary gismuMap cmavoMap brivlaMap definitionsMap englishBrivlaPlacesMap where
    -- Frequency map
    frequencyMap = loadFrequencyMapFromText $ T.pack $(embedStringFile "resources/MyFreq-COMB_without_dots.txt")
    -- Cmavo
    cmavo = loadCmavoFromText frequencyMap $ T.pack $(embedStringFile "resources/english/cmavo.txt")
    cmavoList = map (\c -> (cmavoText c, c)) cmavo
    cmavoMap = M.fromList cmavoList
    -- Gismu
    isReallyGismu gismu = isNothing $ M.lookup (gismuText gismu) cmavoMap
    gismu = filter isReallyGismu $ loadGismuFromText frequencyMap $ T.pack $(embedStringFile "resources/english/gismu.txt")
    gismuList = map (\g -> (gismuText g, g)) gismu
    gismuMap = M.fromList gismuList
    -- Brivla
    brivlaFromGismu = map (\g -> Brivla (gismuText g) (gismuEnglishDefinition g) (gismuIRCFrequencyCount g)) gismu
    brivlaFromFile = loadBrivlaFromText frequencyMap $ T.pack $(embedStringFile "resources/english/brivla.yaml")
    brivla = brivlaFromGismu ++ brivlaFromFile
    brivlaList = map (\b -> (brivlaText b, b)) brivla
    brivlaMap = M.fromList brivlaList
    -- Definitions
    brivlaDefinitions = (second brivlaDefinition) <$> brivlaList
    cmavoDefinitions = (second cmavoEnglishDefinition) <$> cmavoList
    gismuDefinitions = (second gismuEnglishDefinition) <$> gismuList
    definitionsList = cmavoDefinitions ++ gismuDefinitions ++ brivlaDefinitions
    definitionsMap = M.fromList definitionsList

-- Gismu
loadGismuFromLine :: FrequencyMap -> T.Text -> Gismu
loadGismuFromLine frequencyMap line =
    let text = subfield 1 6 line
        rafsi1 = subfield 7 10 line
        rafsi2 = subfield 11 14 line
        rafsi3 = subfield 15 19 line
        englishBrivlaPlaces = englishBrivlaPlacesMap M.! text
        englishKeyword1 = subfield 20 41 line
        englishKeyword2 = T.replace "'" "" $ subfield 41 62 line
        englishDefinition = subfield 62 158 line
        teachingCode = subfield 159 161 line
        oldFrequencyCount = (read . T.unpack $ subfield 161 165 line) :: Int
        englishFullNotes = T.strip $ T.drop 165 line
        (englishNotes, confer) = parseNotes englishFullNotes
    in Gismu text (filter (/=T.empty) [rafsi1, rafsi2, rafsi3]) englishBrivlaPlaces (filter (/=T.empty) [englishKeyword1, englishKeyword2]) englishDefinition englishNotes confer teachingCode oldFrequencyCount (M.findWithDefault 0 text frequencyMap)

loadGismuFromText :: FrequencyMap -> T.Text -> [Gismu]
loadGismuFromText frequencyMap = fmap (loadGismuFromLine frequencyMap) . tail . T.lines

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

-- Brivla
loadBrivlaFromText :: FrequencyMap -> T.Text -> [Brivla]
loadBrivlaFromText frequencyMap yamlText = map handleBrivla yamlList where
    yamlData :: M.Map T.Text (M.Map T.Text T.Text)
    Right yamlData = Y.decodeEither $ TE.encodeUtf8 yamlText
    yamlList :: [(T.Text, M.Map T.Text T.Text)]
    yamlList = M.assocs yamlData
    handleBrivla :: (T.Text, M.Map T.Text T.Text) -> Brivla
    handleBrivla (brivlaKey, brivlaData) = Brivla brivlaKey (brivlaData M.! "definition") (frequencyMap M.! brivlaKey)

-- Brivla places
englishBrivlaPlacesMap :: M.Map T.Text [T.Text]
englishBrivlaPlacesMap = loadBrivlaPlacesMapFromYaml $ T.pack $(embedStringFile "resources/english/brivla-places.yaml")

loadBrivlaPlacesMapFromYaml :: T.Text -> M.Map T.Text [T.Text]
loadBrivlaPlacesMapFromYaml yamlText = M.map extractPlaces yamlData where
    yamlData :: M.Map T.Text (M.Map T.Text T.Text)
    Right yamlData = Y.decodeEither $ TE.encodeUtf8 yamlText
    extractPlaces :: M.Map T.Text T.Text -> [T.Text]
    extractPlaces dict = catMaybes [ dict M.!? "x1" , dict M.!? "x2", dict M.!? "x3", dict M.!? "x4", dict M.!? "x5" ]

-- Helper functions
parseNotes :: T.Text -> (T.Text, [T.Text])
parseNotes englishFullNotes =
    case T.splitOn "(cf. " englishFullNotes of
        englishNotes:confer':_ -> (englishNotes, filter isSingleWord . map T.strip . T.splitOn ", " . T.takeWhile (/=')') $ confer')
        englishNotes:_ -> (englishNotes, [])
    where
        isSingleWord :: T.Text -> Bool
        isSingleWord x = length (T.words x) == 1

-- Frequency map
type FrequencyMap = M.Map T.Text Int

loadFrequencyPairFromLine :: T.Text -> (T.Text, Int)
loadFrequencyPairFromLine line = (w, read $ T.unpack f) where
    [f, w] = T.splitOn " " line

loadFrequencyMapFromText :: T.Text -> FrequencyMap
loadFrequencyMapFromText = M.fromList . map loadFrequencyPairFromLine . map (T.replace "\r" "") . T.lines
