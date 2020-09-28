{-# LANGUAGE OverloadedStrings #-}

module Language.Lojban.Core
( SimpleBridi (..)
, SimpleBridiGenerator
, SimpleBridiDisplayer
, TextGenerator
, SentenceCanonicalizer
, Dictionary (..)
, Gismu (..)
, Cmavo (..)
, Brivla (..)
, dictFindBrivla
, dictFindBrivla'
, dictLookupValsiDefinition
, retrieveBrivlaPlaces
) where

import System.Random (StdGen)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Aeson as A

-- * Syntax
data SimpleBridi = SimpleBridi
    { simpleBridiXu :: Bool
    , simpleBridiSelbri :: T.Text
    , simpleBridiSumti :: [T.Text]
    , simpleBridiExtraSumti :: [T.Text]
    } deriving (Show, Eq)

type SimpleBridiGenerator = StdGen -> (SimpleBridi, StdGen)

type SimpleBridiDisplayer = StdGen -> SimpleBridi -> (T.Text, StdGen)

type TextGenerator = StdGen -> (T.Text, StdGen)

type SentenceCanonicalizer = T.Text -> Either String T.Text

-- * Lexicon
data Dictionary = Dictionary
    { dictIdentifier :: T.Text
    , dictGismu :: M.Map T.Text Gismu
    , dictCmavo :: M.Map T.Text Cmavo
    , dictBrivla :: M.Map T.Text Brivla
    , dictRafsi :: M.Map T.Text Gismu
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

-- * Helper functions

dictFindBrivla :: Dictionary -> T.Text -> Maybe Brivla
dictFindBrivla dictionary key = (dictBrivla dictionary) M.!? key

dictFindBrivla' :: Dictionary -> T.Text -> Brivla
dictFindBrivla' dictionary key = fromMaybe errorResult (dictFindBrivla dictionary key) where
    errorResult = error $ "Brivla not found in dictionary: " ++ (T.unpack key)

dictLookupValsiDefinition :: Dictionary -> T.Text -> T.Text
dictLookupValsiDefinition dictionary key = fromMaybe errorResult ((dictValsiDefinition dictionary) M.!? key) where
    errorResult = error $ "Valsi not found in dictionary: " ++ (T.unpack key)

retrieveBrivlaPlaces :: Dictionary -> T.Text -> [T.Text]
retrieveBrivlaPlaces dictionary brivla =
    let lastSelbriInBrivla = last $ T.splitOn " " brivla
        places = M.findWithDefault [] lastSelbriInBrivla $ dictBrivlaPlaces dictionary
    in if null places
        then error $ "Missing brivla places for '" ++ (T.unpack brivla) ++ "'"
        else places

-- * Serialization
instance A.ToJSON Dictionary where
    toJSON dictionary = A.object
        [ ("valsiDefinition", A.toJSON $ dictValsiDefinition dictionary)
        ]
