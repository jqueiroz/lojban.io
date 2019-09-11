{-# LANGUAGE DeriveGeneric #-}

module Server.Core where

import GHC.Generics
import Network.HTTP.Client (Manager)
import qualified Database.Redis as Redis
import qualified Data.Aeson as A
import qualified Data.Map as M
import qualified Data.Text as T

data ServerResources = ServerResources
    { serverResourcesTlsManager :: Manager
    , serverResourcesRedisConnection :: Redis.Connection
    }

data UserIdentifier = UserIdentifier
    { userIdentifierProvider :: T.Text
    , userIdentifierEmail :: T.Text
    }

data UserIdentity = UserIdentity
    { userIdentifier :: UserIdentifier
    , userPictureUrl :: T.Text
    , userGivenName :: T.Text
    , userFamilyName :: T.Text
    }

data DeckPreferences = DeckPreferences
    { cardPreferences :: M.Map T.Text CardPreferences
    } deriving (Generic, Show)

data CardPreferences = CardPreferences
    { cardEnabled :: Bool
    } deriving (Generic, Show)

data DeckProficiency = DeckProficiency
    { cardProficiencies :: M.Map T.Text CardProficiency
    } deriving (Generic, Show)

data CardProficiency = CardProficiency
    { lastAttempts :: [Bool]
    } deriving (Generic, Show)

instance A.FromJSON DeckPreferences where
    parseJSON = A.genericParseJSON A.defaultOptions

instance A.FromJSON CardPreferences where
    parseJSON = A.genericParseJSON A.defaultOptions

instance A.FromJSON DeckProficiency where
    parseJSON = A.genericParseJSON A.defaultOptions

instance A.FromJSON CardProficiency where
    parseJSON = A.genericParseJSON A.defaultOptions

instance A.ToJSON DeckPreferences where
    toEncoding = A.genericToEncoding A.defaultOptions

instance A.ToJSON CardPreferences where
    toEncoding = A.genericToEncoding A.defaultOptions

instance A.ToJSON DeckProficiency where
    toEncoding = A.genericToEncoding A.defaultOptions

instance A.ToJSON CardProficiency where
    toEncoding = A.genericToEncoding A.defaultOptions
