{-# LANGUAGE OverloadedStrings #-}

module Server.Website.Resources
( displayResourcesHome
) where

import Server.Core
import Server.Website.Core
import qualified Text.Blaze as B
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

displayResourcesHome :: Maybe UserIdentity -> H.Html
displayResourcesHome userIdentityMaybe =
    H.html $ do
        H.head $ do
            H.title $ H.toHtml ("Additional resources" :: String)
            includeUniversalStylesheets
            includeUniversalScripts
        H.body $ do
            displayTopbar userIdentityMaybe TopbarResources
            H.div B.! A.class_ (H.stringValue "main") $ do
                H.h1 $ H.toHtml ("Additional resources" :: String)
