{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Server.Main (runServer, acquireServerResources) where

import Server.Core
import Control.Monad (msum)
import Control.Exception (SomeException, catch)
import Data.Maybe (fromMaybe, isJust)
import System.Environment (lookupEnv)
import Happstack.Server
import Happstack.Server.Compression (compressedResponseFilter)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.Text as T
import qualified Database.Redis as Redis
import qualified Server.Website.Main as Website
import qualified Server.Api.Main as Api
import qualified Server.Authentication.Main as Authentication

-- TODO: consider adding breadcrumbs (https://getbootstrap.com/docs/4.0/components/breadcrumb/)

runServer :: Int -> IO ()
runServer portNumber = do
    serverConfiguration <- readServerConfiguration
    serverResources <- acquireServerResources serverConfiguration
    seq serverConfiguration $ simpleHTTP nullConf { port = portNumber } (handleRoot serverConfiguration serverResources)

handleRoot :: ServerConfiguration -> ServerResources -> ServerPart Response
handleRoot serverConfiguration serverResources = do
    let cacheControlForAssets = case serverConfigurationEnvironmentType serverConfiguration of
            EnvironmentTypeProd -> fmap $ setHeader "Cache-Control" "max-age=600"
            _ -> id
    _ <- compressedResponseFilter
    msum
        [ dir "docs" $ movedPermanently ("documentation/lojbanios-0.1.0.0/index.html" :: String) (toResponse ())
        , dir "documentation" $ serveDirectory EnableBrowsing [] "documentation"
        , dir "static" $ cacheControlForAssets $ serveDirectory EnableBrowsing [] "static"
        , dir "api" $ Api.handleRoot serverConfiguration serverResources
        , dir "oauth2" $ Authentication.handleRoot serverConfiguration serverResources
        , dir "authentication" $ Authentication.handleRoot serverConfiguration serverResources
        , dir "favicon.ico" $ cacheControlForAssets $ serveFile (asContentType "image/png") "static/images/favicon.png"
        , dir "manifest.webmanifest" $ cacheControlForAssets $ serveFile (asContentType "text/json") "static/pwa/manifest.webmanifest"
        , dir "pwabuilder-sw.js" $ cacheControlForAssets $ serveFile (asContentType "text/javascript") "static/pwa/pwabuilder-sw.js"
        , Website.handleRoot serverConfiguration serverResources
        ]

acquireServerResources :: ServerConfiguration -> IO ServerResources
acquireServerResources serverConfiguration = do
    tlsManager <- newManager tlsManagerSettings
    let connectInfo = case serverConfigurationEnvironmentType serverConfiguration of
            EnvironmentTypeProd ->
                -- For prod, we must always connect over TCP to $LOJBANIOS_REDIS_HOSTNAME
                case serverConfigurationRedisHostname serverConfiguration of
                        Nothing -> error "Running in production. The environment variable LOJBANIOS_REDIS_HOSTNAME must be specified."
                        Just redisHostname -> Redis.defaultConnectInfo
                            { Redis.connectHost = redisHostname
                            }
            EnvironmentTypeDev ->
                -- For dev, if $LOJBANIOS_REDIS_HOSTNAME is unspecified, we fall back to unix sockets
                case serverConfigurationRedisHostname serverConfiguration of
                        Nothing -> Redis.defaultConnectInfo
                            { Redis.connectPort = Redis.UnixSocket "/tmp/lojbanios-redis-dev.sock"
                            }
                        Just redisHostname -> Redis.defaultConnectInfo
                            { Redis.connectHost = redisHostname
                            }
    redisConnection <- catch (Redis.checkedConnect connectInfo) redisExceptionHandler
    return $ ServerResources tlsManager redisConnection
    where
        redisExceptionHandler :: SomeException -> IO Redis.Connection
        redisExceptionHandler ex = error $ "Connection to redis could not be established. If running locally, outside of Docker, please make sure to run './run-redis.sh'.\nException details: " ++ show ex

lookupStringEnvironmentVariable :: String -> IO (Maybe String)
lookupStringEnvironmentVariable environmentVariableName = lookupEnv environmentVariableName >>= \case
    Nothing -> return Nothing
    Just "" -> return Nothing
    Just x -> return $ Just x

readServerConfiguration :: IO ServerConfiguration
readServerConfiguration = do
    environmentType <- lookupEnv "LOJBANIOS_ENVIRONMENT" >>= \case
        Just "prod" -> return EnvironmentTypeProd
        Just "dev" -> return EnvironmentTypeDev
        _ -> error "Error: incorrect or unspecified environment type"
    redisHostname <- lookupStringEnvironmentVariable "LOJBANIOS_REDIS_HOSTNAME"
    openIdMicrosoftClientId <- lookupStringEnvironmentVariable "LOJBANIOS_OPENID_MICROSOFT_CLIENT_ID"
    openIdMicrosoftClientSecret <- lookupStringEnvironmentVariable "LOJBANIOS_OPENID_MICROSOFT_CLIENT_SECRET"
    let identityProviders = concat
            -- Mock
            [ [IdentityProvider "mock" "mock" "/authentication/mock/login/" | environmentType == EnvironmentTypeDev]
            -- Google
            , [IdentityProvider "google" "Google" "/oauth2/google/login/"] -- TODO:only if the environment variables are set
            -- Microsoft
            , [(IdentityProvider "microsoft" "Microsoft" "/authentication/openid/microsoft/login/") | (isJust openIdMicrosoftClientId && isJust openIdMicrosoftClientSecret)]
            ]
    return $ ServerConfiguration environmentType identityProviders redisHostname openIdMicrosoftClientId openIdMicrosoftClientSecret
