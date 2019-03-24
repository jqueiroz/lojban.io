{-# LANGUAGE OverloadedStrings #-}

module DictionaryLoader
( loadDictionary
) where

import Core
import Util (subfield)
import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((***), first, second)
import Data.Maybe (isNothing)
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
    let cmavoList = map (\c -> (cmavoText c, c)) cmavo
    let cmavoMap = M.fromList cmavoList
    -- Gismu
    let isReallyGismu gismu = isNothing $ M.lookup (gismuText gismu) cmavoMap
    gismu <- filter isReallyGismu <$> loadGismuFromFile frequencyMap
    let gismuList = map (\g -> (gismuText g, g)) gismu
    let gismuMap = M.fromList gismuList
    -- Definitions
    let cmavoDefinitions = (second cmavoEnglishDefinition) <$> cmavoList
    let gismuDefinitions = (second gismuEnglishDefinition) <$> gismuList
    let definitionsList = cmavoDefinitions ++ gismuDefinitions
    let definitionsMap = M.fromList definitionsList
    -- Return dictionary
    return $ Dictionary gismuMap cmavoMap definitionsMap englishBrivlaPlacesMap

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

-- Brivla places
-- See also: http://www.lojban.org/publications/wordlists/oblique_keywords.txt
englishBrivlaPlacesMap :: M.Map T.Text [T.Text]
englishBrivlaPlacesMap = M.fromList
    [ ("tavla", ["speaker", "listener", "subject", "language"])
    , ("dunda", ["donor", "gift", "recipient"])
    , ("ctuca", ["instructor", "audience/student(s)", "ideas/methods", "subject", "teaching method"])
    , ("citka", ["consumer", "aliment"])
    , ("ciska", ["writer", "text/symbols", "display/storage medium", "writing implement"])
    , ("klama", ["traveler", "destination", "origin", "route", "means/vehicle"])
    , ("bridi", ["predicate relationship", "relation", "arguments"])
    , ("djuno", ["knower", "facts", "subject", "epistemology"])
    , ("nupre", ["promisor", "promise", "beneficiary/victim"])
    , ("cusku", ["expresser", "message", "audience", "expressive medium"])
    , ("cizra", ["strange thing", "viewpoint holder", "property"])
    , ("cmene", ["name/title", "name posessor", "name-giver/name-user"])
    , ("cusku", ["agent", "expressed idea", "audience", "expressive medium"])
    , ("djica", ["desirer", "event/state", "purpose"])
    , ("gleki", ["happy entity", "event/state"])
    , ("jimpe", ["understander", "fact/truth", "subject"])
    , ("klama", ["traveler", "destination", "origin", "route", "means/vehicle"])
    , ("mutce", ["much/extreme thing", "property", "extreme/direction"])
    , ("nelci", ["liker", "object/state"])
    , ("pilno", ["user", "instrument", "purpose"])
    , ("sipna", ["asleep entity"])
    , ("xamgu", ["good object/event", "beneficiary", "standard"])
    , ("zgana", ["observer", "observed", "senses/means", "conditions"])
    , ("bangu", ["language/dialect", "language user", "communicated idea"])
    , ("cliva", ["agent", "point of departure", "route"])
    , ("finti", ["inventor/composer", "invention", "purpose", "existing elements/ideas"])
    , ("gunka", ["worker", "activity", "goal"])
    , ("jundi", ["attentive entity", "object/affair"])
    , ("kakne", ["capable entity", "capability", "conditions"])
    , ("tcidu", ["reader", "text", "reading material"])
    , ("valsi", ["word", "meaning", "language"])
    , ("zvati", ["atendee/event", "location"])
    , ("cinri", ["interesting abstraction", "interested entity"])
    , ("drata", ["entity #1", "entity #2", "standard"])
    , ("simsa", ["entity #1", "entity #2", "property/quantity"])
    , ("klaku", ["crier", "tears", "reason"])
    , ("melbi", ["beautiful entity", "viewpoint holder", "aspect", "aesthetic standard"])
    , ("smuni", ["meaning/interpretation", "expression", "opinion holder"]) -- not very good
    -- TODO: pendo
    -- TODO: smuni
    -- TODO: prenu
    -- TODO: mlatu
    -- TODO: gerku
    -- TODO: zdani
    ] -- TODO: ask people to build a database

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

loadFrequencyMapFromFile :: IO FrequencyMap
loadFrequencyMapFromFile = M.fromList . map loadFrequencyPairFromLine . map (T.replace "\r" "") . T.lines <$> TIO.readFile "resources/MyFreq-COMB_without_dots.txt"
