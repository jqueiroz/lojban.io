{-# LANGUAGE OverloadedStrings #-}

module Server.Main (runServer) where

import Server.Website.Main (handleHome, handleGrammar, handleVocabulary, handleResources)
import Server.Util (forceSlash)
import Control.Monad (msum)
import Happstack.Server
import qualified Server.Api.Main as Api

-- TODO: consider adding breadcrumbs (https://getbootstrap.com/docs/4.0/components/breadcrumb/)

runServer :: Int -> IO ()
runServer portNumber = simpleHTTP nullConf { port = portNumber } handleRoot

handleRoot :: ServerPart Response
handleRoot = msum
    [ forceSlash $ handleHome
    , dir "docs" $ movedPermanently ("doc/lojto-0.1.0.0/index.html" :: String) (toResponse ())
    , dir "doc" $ serveDirectory EnableBrowsing [] "doc"
    , dir "static" $ serveDirectory EnableBrowsing [] "static"
    , dir "grammar" handleGrammar
    , dir "vocabulary" handleVocabulary
    , dir "resources" handleResources
    , dir "api" Api.handleRoot
    ]
