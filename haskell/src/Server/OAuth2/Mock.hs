{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.OAuth2.Mock
( handleRoot
, readUserIdentityFromCookies
) where

import Server.OAuth2.Utils (redirectToCurrentRefererIfAllowed)
import Server.Core
import Happstack.Server
import Control.Monad (msum, mzero)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)

handleRoot :: ServerConfiguration -> ServerResources -> ServerPart Response
handleRoot serverConfiguration serverResources = msum
    [ dir "login" $ handleLogin
    , dir "logout" $ handleLogout
    ]

signedInCookieName :: String
signedInCookieName = "mock_isSignedIn"

handleLogin :: ServerPart Response
handleLogin = do
    addCookie Session $ mkCookie signedInCookieName "true"
    redirectToCurrentRefererIfAllowed

handleLogout :: ServerPart Response
handleLogout = do
    expireCookie signedInCookieName
    redirectToCurrentRefererIfAllowed

readUserIdentityFromCookies :: ServerConfiguration -> ServerResources -> ServerPart (Maybe UserIdentity)
readUserIdentityFromCookies serverConfiguration serverResources = runMaybeT $ do
    isSignedIn <- lift $ lookCookieValue signedInCookieName
    if isSignedIn == "true" then do
        let userIdentifier = UserIdentifier "mock" "testuser1"
        let userPictureUrl = "https://lojban.io/favicon.ico"
        let userGivenName = "Arthur"
        let userFamilyName = "Dent"
        return $ UserIdentity userIdentifier userPictureUrl userGivenName userFamilyName
    else
        mzero

