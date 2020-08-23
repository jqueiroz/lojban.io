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

handleRoot :: ServerConfiguration -> ServerResources -> ServerPart Response
handleRoot serverConfiguration serverResources = msum
    [ dir "google" $ Google.handleRoot serverConfiguration serverResources
    , dir "mock" $ Mock.handleRoot serverConfiguration serverResources
    ]

readUserIdentityFromCookies :: ServerConfiguration -> ServerResources -> ServerPart (Maybe UserIdentity)
readUserIdentityFromCookies serverConfiguration serverResources = msum
    [ Google.readUserIdentityFromCookies serverConfiguration serverResources
    , Mock.readUserIdentityFromCookies serverConfiguration serverResources
    , return Nothing ]
