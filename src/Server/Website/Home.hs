{-# LANGUAGE OverloadedStrings #-}

module Server.Website.Home
( displayHome
) where

import Server.Website.Core
import qualified Text.Blaze as B
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

displayHome :: H.Html
displayHome =
    H.html $ do
        H.head $ do
            H.title $ H.toHtml ("Home" :: String)
            includeUniversalStylesheets
            includeUniversalScripts
        H.body $ do
            displayTopbar TopbarHome
            H.div B.! A.class_ (H.stringValue "main") $ do
                H.h1 $ H.toHtml ("Home" :: String)
