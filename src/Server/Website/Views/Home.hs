{-# LANGUAGE OverloadedStrings #-}

module Server.Website.Views.Home
( displayHome
) where

import Server.Core
import Server.Website.Views.Core
import qualified Text.Blaze as B
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

displayHome :: Maybe UserIdentity -> H.Html
displayHome userIdentityMaybe =
    H.html $ do
        H.head $ do
            H.title $ H.toHtml ("Home" :: String)
            includeUniversalStylesheets
            includeUniversalScripts
        H.body $ do
            displayTopbar userIdentityMaybe TopbarHome
            H.div B.! A.class_ (H.stringValue "main") $ do
                H.h1 $ H.toHtml ("Home" :: String)
