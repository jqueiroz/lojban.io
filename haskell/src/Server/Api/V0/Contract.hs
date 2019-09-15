{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Server.Api.V0.Contract where

import GHC.Generics
import qualified Data.Aeson as A
import qualified Data.Map as M
import qualified Data.Text as T

data Course = Course
    { title :: T.Text
    , dictionaryId :: T.Text
    , style :: CourseStyle
    } deriving (Generic, Show)

data CourseStyle = CourseStyle
    { color1 :: Maybe String
    , iconUrl :: Maybe String
    } deriving (Generic, Show)

data Deck = Deck
    { title :: T.Text
    , dictionaryId :: T.Text
    , cards :: [Card]
    , deckPreferences :: Maybe DeckPreferences
    , deckProficiency :: Maybe DeckProficiency
    } deriving (Generic, Show)

data Card = Card
    { title :: T.Text
    , shortDescription :: T.Text
    } deriving (Generic, Show)

data DeckPreferences = DeckPreferences
    { cardPreferences :: M.Map T.Text CardPreferences
    } deriving (Generic, Show)

data CardPreferences = CardPreferences
    { enabled :: Bool
    } deriving (Generic, Show)

data DeckProficiency = DeckProficiency
    { cardProficiencies :: M.Map T.Text CardProficiency
    } deriving (Generic, Show)

data CardProficiency = CardProficiency
    { score :: Double
    } deriving (Generic, Show)

instance A.ToJSON Course where
    toEncoding = A.genericToEncoding A.defaultOptions

instance A.ToJSON CourseStyle where
    toEncoding = A.genericToEncoding A.defaultOptions

instance A.ToJSON Deck where
    toEncoding = A.genericToEncoding A.defaultOptions

instance A.ToJSON Card where
    toEncoding = A.genericToEncoding A.defaultOptions

instance A.ToJSON DeckPreferences where
    toEncoding = A.genericToEncoding A.defaultOptions

instance A.ToJSON CardPreferences where
    toEncoding = A.genericToEncoding A.defaultOptions

instance A.ToJSON DeckProficiency where
    toEncoding = A.genericToEncoding A.defaultOptions

instance A.ToJSON CardProficiency where
    toEncoding = A.genericToEncoding A.defaultOptions
