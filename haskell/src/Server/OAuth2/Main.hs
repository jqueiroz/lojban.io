{-# LANGUAGE OverloadedStrings #-}

module Server.OAuth2.Main
( handleRoot
, readUserIdentityFromCookies
) where

import Control.Monad (msum)
import Happstack.Server
import Server.Core
import qualified Server.OAuth2.Google as Google
import qualified Server.OAuth2.Mock as Mock

handleRoot :: ServerResources -> ServerPart Response
handleRoot serverResources = msum
    [ dir "google" $ Google.handleRoot serverResources
    , dir "mock" $ Mock.handleRoot serverResources
    ]

readUserIdentityFromCookies :: ServerResources -> ServerPart (Maybe UserIdentity)
readUserIdentityFromCookies serverResources = msum
    [ Google.readUserIdentityFromCookies serverResources
    , Mock.readUserIdentityFromCookies serverResources
    , return Nothing ]
