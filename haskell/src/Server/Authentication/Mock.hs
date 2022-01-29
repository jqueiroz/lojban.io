{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Authentication.Mock
( handleRoot
, handleLogout
, readUserIdentityFromCookies
) where

import Server.Authentication.Utils (redirectToCurrentRefererIfAllowed)
import Server.Core
import Happstack.Server
import Control.Monad (msum, mzero)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)

handleRoot :: ServerConfiguration -> ServerResources -> ServerPart Response
handleRoot serverConfiguration serverResources = msum
    [ dir "login" $ handleLogin
    ]

signedInCookieName :: String
signedInCookieName = "mock_isSignedIn"

handleLogin :: ServerPart Response
handleLogin = do
    addCookie Session $ mkCookie signedInCookieName "true"
    redirectToCurrentRefererIfAllowed

handleLogout :: ServerConfiguration -> ServerResources -> ServerPart ()
handleLogout serverConfiguration serverResources = do
    expireCookie signedInCookieName

readUserIdentityFromCookies :: ServerConfiguration -> ServerResources -> ServerPart (Maybe UserIdentity)
readUserIdentityFromCookies serverConfiguration serverResources = runMaybeT $ do
    isSignedIn <- lift $ lookCookieValue signedInCookieName
    if isSignedIn == "true" then do
        let userIdentifier = UserIdentifier "mock" "testuser1"
        let userPictureUrl = "https://lojban.io/favicon.ico"
        let userName = "Arthur Dent"
        return $ UserIdentity userIdentifier userPictureUrl userName
    else
        mzero

