{-# LANGUAGE OverloadedStrings #-}

module Server.OAuth2.Main
( handleRoot
, readUserIdentityFromCookies
) where

import Control.Monad (msum)
import Happstack.Server
import Server.Core
import qualified Server.OAuth2.Google as Google

handleRoot :: ServerResources -> ServerPart Response
handleRoot serverResources = msum
    [ dir "google" $ Google.handleRoot serverResources
    ]

readUserIdentityFromCookies :: ServerResources -> ServerPart (Maybe UserIdentity)
readUserIdentityFromCookies = Google.readUserIdentityFromCookies
