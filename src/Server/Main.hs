{-# LANGUAGE OverloadedStrings #-}

module Server.Main (runServer, acquireServerResources) where

import Server.Core
import Server.Website.Main (handleHome, handleGrammar, handleVocabulary, handleResources)
import Server.Util (forceSlash)
import Control.Monad (msum)
import Happstack.Server
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Server.Api.Main as Api
import qualified Server.OAuth2.Main as OAuth2

-- TODO: consider adding breadcrumbs (https://getbootstrap.com/docs/4.0/components/breadcrumb/)

runServer :: Int -> IO ()
runServer portNumber = do
    serverResources <- acquireServerResources
    simpleHTTP nullConf { port = portNumber } (handleRoot serverResources)

handleRoot :: ServerResources -> ServerPart Response
handleRoot serverResources = msum
    [ forceSlash $ handleHome
    , dir "docs" $ movedPermanently ("doc/lojto-0.1.0.0/index.html" :: String) (toResponse ())
    , dir "doc" $ serveDirectory EnableBrowsing [] "doc"
    , dir "static" $ serveDirectory EnableBrowsing [] "static"
    , dir "grammar" handleGrammar
    , dir "vocabulary" handleVocabulary
    , dir "resources" handleResources
    , dir "api" Api.handleRoot
    , dir "oauth2" $ OAuth2.handleRoot serverResources
    ]

acquireServerResources :: IO ServerResources
acquireServerResources = do
    tlsManager <- newManager tlsManagerSettings
    return $ ServerResources tlsManager
