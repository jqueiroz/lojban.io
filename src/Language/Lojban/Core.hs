module Language.Lojban.Core
( SimpleBridi (..)
, SimpleBridiDisplayer
, SentenceCanonicalizer
, Dictionary (..)
, Gismu (..)
, Cmavo (..)
, Brivla (..)
, retrieveBrivlaPlaces
) where

import System.Random (StdGen)
import qualified Data.Text as T
import qualified Data.Map as M

-- * Syntax
data SimpleBridi = SimpleBridi
    { simpleBridiXu :: Bool
    , simpleBridiSelbri :: T.Text
    , simpleBridiSumti :: [T.Text]
    , simpleBridiExtraSumti :: [T.Text]
    } deriving (Show, Eq)

type SimpleBridiDisplayer = StdGen -> SimpleBridi -> (T.Text, StdGen)

type SentenceCanonicalizer = T.Text -> Either String T.Text

-- * Lexicon
data Dictionary = Dictionary
    { dictGismu :: M.Map T.Text Gismu
    , dictCmavo :: M.Map T.Text Cmavo
    , dictBrivla :: M.Map T.Text Brivla
    , dictValsiDefinition :: M.Map T.Text T.Text
    , dictBrivlaPlaces :: M.Map T.Text [T.Text]
    } deriving (Show)

data Gismu = Gismu
    { gismuText :: T.Text
    , gismuRafsi :: [T.Text]
    , gismuEnglishPlaces :: [T.Text]
    , gismuEnglishKeywords :: [T.Text]
    , gismuEnglishDefinition :: T.Text
    , gismuEnglishNotes :: T.Text
    , gismuConfer :: [T.Text]
    , gismuTeachingCode :: T.Text
    , gismuOldFrequencyCount :: Int
    , gismuIRCFrequencyCount :: Int
    } deriving (Show)

data Cmavo = Cmavo
    { cmavoText :: T.Text
    , cmavoEnglishClassification :: T.Text
    , cmavoEnglishKeyword :: T.Text
    , cmavoEnglishDefinition :: T.Text
    , cmavoEnglishNotes :: T.Text
    , cmavoEnglishConfer :: [T.Text]
    , cmavoIRCFrequencyCount :: Int
    } deriving (Show)

data Brivla = Brivla
    { brivlaText :: T.Text
    , brivlaDefinition :: T.Text
    , brivlaIRCFrequencyCount :: Int
    } deriving (Show)

instance Eq Gismu where
    x == y = (gismuText x) == (gismuText y)

instance Eq Cmavo where
    x == y = (cmavoText x) == (cmavoText y)

retrieveBrivlaPlaces :: Dictionary -> T.Text -> [T.Text]
retrieveBrivlaPlaces dictionary brivla =
    let places = M.findWithDefault [] brivla $ dictBrivlaPlaces dictionary
    in if null places
        then error $ "Missing brivla places for '" ++ (T.unpack brivla) ++ "'"
        else places
