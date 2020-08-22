{-# LANGUAGE OverloadedStrings #-}

module Server.Main (runServer, acquireServerResources) where

import Server.Core
import Control.Monad (msum)
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import Happstack.Server
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Database.Redis as Redis
import qualified Server.Website.Main as Website
import qualified Server.Api.Main as Api
import qualified Server.OAuth2.Main as OAuth2

-- TODO: consider adding breadcrumbs (https://getbootstrap.com/docs/4.0/components/breadcrumb/)

runServer :: Int -> IO ()
runServer portNumber = do
    serverResources <- acquireServerResources
    simpleHTTP nullConf { port = portNumber } (handleRoot serverResources)

handleRoot :: ServerResources -> ServerPart Response
handleRoot serverResources = msum
    [ dir "docs" $ movedPermanently ("doc/lojto-0.1.0.0/index.html" :: String) (toResponse ())
    , dir "doc" $ serveDirectory EnableBrowsing [] "doc"
    , dir "static" $ serveDirectory EnableBrowsing [] "static"
    , dir "api" $ Api.handleRoot serverResources
    , dir "oauth2" $ OAuth2.handleRoot serverResources
    , dir "favicon.ico" $ serveFile (asContentType "image/png") "static/images/favicon.png"
    , dir "manifest.webmanifest" $ serveFile (asContentType "text/json") "static/pwa/manifest.webmanifest"
    , Website.handleRoot serverResources
    ]

acquireServerResources :: IO ServerResources
acquireServerResources = do
    tlsManager <- newManager tlsManagerSettings
    redisHostname <- fromMaybe "127.0.0.1" <$> lookupEnv "LOJBAN_TOOL_REDIS_HOSTNAME"
    redisConnection <- Redis.checkedConnect Redis.defaultConnectInfo
        { Redis.connectHost = redisHostname
        }
    return $ ServerResources tlsManager redisConnection
