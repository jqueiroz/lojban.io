{-# LANGUAGE OverloadedStrings #-}

module Server.Website.Views.Resources
( displayResourcesHome
) where

import Server.Core
import Server.Website.Views.Core
import qualified Data.Text as T
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
            includeInternalStylesheet "resources.css"
        H.body $ do
            displayTopbar userIdentityMaybe TopbarResources
            H.div B.! A.class_ (H.stringValue "main") $ do
                H.div B.! A.class_ (H.textValue "header") $ do
                    H.div B.! A.class_ (H.textValue "header-bg") $ H.toHtml ("" :: T.Text)
                    H.h1 $ H.toHtml ("Additional resources" :: String)
