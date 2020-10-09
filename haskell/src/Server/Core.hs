{-# LANGUAGE DeriveGeneric #-}

module Server.Core where

import GHC.Generics
import Network.HTTP.Client (Manager)
import qualified Database.Redis as Redis
import qualified Data.Aeson as A
import qualified Data.Map as M
import qualified Data.Text as T

data EnvironmentType = EnvironmentTypeDev | EnvironmentTypeProd
    deriving (Eq, Show)

data IdentityProvider = IdentityProvider
    { identityProviderIdentifier :: T.Text
    , identityProviderName :: T.Text
    , identityProviderLoginUrl :: T.Text
    }

data ServerConfiguration = ServerConfiguration
    { serverConfigurationEnvironmentType :: EnvironmentType
    , serverConfigurationIdentityProviders :: [IdentityProvider]
    , serverConfigurationRedisHostname :: Maybe String
    , serverConfigurationOpenIdMicrosoftClientId :: Maybe String
    , serverConfigurationOpenIdMicrosoftClientSecret :: Maybe String
    }

data ServerResources = ServerResources
    { serverResourcesTlsManager :: Manager
    , serverResourcesRedisConnection :: Redis.Connection
    }

data UserIdentifier = UserIdentifier
    { userIdentifierProvider :: T.Text
    , userIdentifierSubject :: T.Text -- Must be unique per provider
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

data CardStatus = CardCurrentlyLearning | CardAlreadyMastered | CardNotStarted
    deriving (Eq, Generic, Show)

data CardPreferences = CardPreferences
    { cardStatus :: CardStatus
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

instance A.FromJSON CardStatus where
    parseJSON = A.genericParseJSON A.defaultOptions

instance A.FromJSON DeckProficiency where
    parseJSON = A.genericParseJSON A.defaultOptions

instance A.FromJSON CardProficiency where
    parseJSON = A.genericParseJSON A.defaultOptions

instance A.ToJSON DeckPreferences where
    toEncoding = A.genericToEncoding A.defaultOptions

instance A.ToJSON CardPreferences where
    toEncoding = A.genericToEncoding A.defaultOptions

instance A.ToJSON CardStatus where
    toEncoding = A.genericToEncoding A.defaultOptions

instance A.ToJSON DeckProficiency where
    toEncoding = A.genericToEncoding A.defaultOptions

instance A.ToJSON CardProficiency where
    toEncoding = A.genericToEncoding A.defaultOptions
