{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Server.Main (runServer, acquireServerResources) where

import Server.Core
import Control.Monad (msum)
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import Happstack.Server
import Happstack.Server.Compression (compressedResponseFilter)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.Text as T
import qualified Database.Redis as Redis
import qualified Server.Website.Main as Website
import qualified Server.Api.Main as Api
import qualified Server.OAuth2.Main as OAuth2

-- TODO: consider adding breadcrumbs (https://getbootstrap.com/docs/4.0/components/breadcrumb/)

runServer :: Int -> IO ()
runServer portNumber = do
    serverConfiguration <- readServerConfiguration
    serverResources <- acquireServerResources
    seq serverConfiguration $ simpleHTTP nullConf { port = portNumber } (handleRoot serverConfiguration serverResources)

handleRoot :: ServerConfiguration -> ServerResources -> ServerPart Response
handleRoot serverConfiguration serverResources = do
    let cacheControlForAssets = fmap $ setHeader "Cache-Control" "max-age=600"
    _ <- compressedResponseFilter
    msum
        [ dir "docs" $ movedPermanently ("documentation/lojto-0.1.0.0/index.html" :: String) (toResponse ())
        , dir "documentation" $ serveDirectory EnableBrowsing [] "documentation"
        , dir "static" $ cacheControlForAssets $ serveDirectory EnableBrowsing [] "static"
        , dir "api" $ Api.handleRoot serverConfiguration serverResources
        , dir "oauth2" $ OAuth2.handleRoot serverConfiguration serverResources
        , dir "favicon.ico" $ cacheControlForAssets $ serveFile (asContentType "image/png") "static/images/favicon.png"
        , dir "manifest.webmanifest" $ cacheControlForAssets $ serveFile (asContentType "text/json") "static/pwa/manifest.webmanifest"
        , dir "pwabuilder-sw.js" $ cacheControlForAssets $ serveFile (asContentType "text/javascript") "static/pwa/pwabuilder-sw.js"
        , Website.handleRoot serverConfiguration serverResources
        ]

acquireServerResources :: IO ServerResources
acquireServerResources = do
    tlsManager <- newManager tlsManagerSettings
    redisHostname <- fromMaybe "127.0.0.1" <$> lookupEnv "LOJBAN_TOOL_REDIS_HOSTNAME"
    redisConnection <- Redis.checkedConnect Redis.defaultConnectInfo
        { Redis.connectHost = redisHostname
        }
    return $ ServerResources tlsManager redisConnection

readServerConfiguration :: IO ServerConfiguration
readServerConfiguration = do
    environmentType <- lookupEnv "LOJBAN_TOOL_ENVIRONMENT" >>= \case
        Just "prod" -> return EnvironmentTypeProd
        Just "dev" -> return EnvironmentTypeDev
        _ -> error "Error: incorrect or unspecified environment type"
    let identityProvider = IdentityProvider ("google" :: T.Text)
    return $ ServerConfiguration environmentType identityProvider
