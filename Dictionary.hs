{-# LANGUAGE OverloadedStrings #-}
module Dictionary where

import Core
import Util (subfield)
import Control.Applicative ((<$>), (<*>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map as M

-- TODO: ask people to build a database
englishSumtiPlacesBase :: M.Map T.Text [T.Text]
englishSumtiPlacesBase = M.fromList
    [ ("tavla", ["speaker", "listener", "subject", "language"])
    , ("dunda", ["donor", "gift", "recipient"])
    , ("klama", ["traveler", "destination", "origin", "route", "means/vehicle"])
    , ("bridi", ["predicate relationship", "relation", "arguments"])
    ]

loadDictionary :: IO Dictionary
loadDictionary = do
    cmavo <- loadCmavoFromFile
    let cmavoMap = M.fromList $ map (\c -> (cmavoText c, c)) cmavo
    let isReallyGismu gismu = M.lookup (gismuText gismu) cmavoMap == Nothing
    gismu <- filter isReallyGismu <$> loadGismuFromFile
    let gismuMap = M.fromList $ map (\g -> (gismuText g, g)) gismu
    return $ Dictionary gismuMap cmavoMap

loadGismuFromLine :: T.Text -> Gismu
loadGismuFromLine line =
    let text = subfield 1 6 line
        rafsi1 = subfield 7 10 line
        rafsi2 = subfield 11 14 line
        rafsi3 = subfield 15 19 line
        englishSumtiPlaces = M.findWithDefault [] text englishSumtiPlacesBase
        englishKeyword1 = subfield 20 41 line
        englishKeyword2 = T.replace "'" "" $ subfield 41 62 line
        englishDefinition = subfield 62 158 line
        teachingCode = subfield 159 161 line
        oldFrequencyCount = (read . T.unpack $ subfield 161 165 line) :: Int
        englishFullNotes = T.strip $ T.drop 165 line
        (englishNotes, confer) = parseNotes englishFullNotes
    in Gismu text (filter (/=T.empty) [rafsi1, rafsi2, rafsi3]) englishSumtiPlaces (filter (/=T.empty) [englishKeyword1, englishKeyword2]) englishDefinition englishNotes confer teachingCode oldFrequencyCount
loadGismuFromText :: T.Text -> [Gismu]
loadGismuFromText = fmap loadGismuFromLine . tail . T.lines
loadGismuFromFile :: IO [Gismu]
loadGismuFromFile = loadGismuFromText <$> TIO.readFile "resources/gismu.txt"

loadCmavoFromLine :: T.Text -> Cmavo
loadCmavoFromLine line =
    let text = subfield 0 11 line
        englishClassification = subfield 11 20 line
        englishKeyword = subfield 20 62 line
        englishDefinition = subfield 62 168 line
        englishFullNotes = T.strip $ T.drop 168 line
        (englishNotes, confer) = parseNotes englishFullNotes
    in Cmavo text englishClassification englishKeyword englishDefinition englishNotes confer
loadCmavoFromText :: T.Text -> [Cmavo]
loadCmavoFromText = fmap loadCmavoFromLine . tail . T.lines
loadCmavoFromFile :: IO [Cmavo]
loadCmavoFromFile = loadCmavoFromText <$> TIO.readFile "resources/cmavo.txt"

parseNotes :: T.Text -> (T.Text, [T.Text])
parseNotes englishFullNotes =
    case T.splitOn "(cf. " englishFullNotes of
        englishNotes:confer':_ -> (englishNotes, filter isSingleWord . map T.strip . T.splitOn ", " . T.takeWhile (/=')') $ confer')
        englishNotes:_ -> (englishNotes, [])
    where
        isSingleWord :: T.Text -> Bool
        isSingleWord x = length (T.words x) == 1
