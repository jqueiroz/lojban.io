module Server.Core where

import Network.HTTP.Client (Manager)
import qualified Data.Text as T

data ServerResources = ServerResources
    { serverResourcesTlsManager :: Manager
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
