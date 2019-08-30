module Server.OAuth2.Core where

import qualified Data.Text as T

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
