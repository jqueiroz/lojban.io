{-# LANGUAGE OverloadedStrings #-}

module Server.Website.Views.Offline
( displayOfflineHome
) where

import Server.Core
import Server.Website.Views.Core
import qualified Data.Text as T
import qualified Text.Blaze as B
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

displayOfflineHome :: Maybe UserIdentity -> H.Html
displayOfflineHome userIdentityMaybe =
    H.html B.! A.lang (H.stringValue "en-us") $ do
        H.head $ do
            H.title $ H.toHtml ("Offline :: lojban.io" :: T.Text)
            includeUniversalStylesheets
            includeUniversalScripts
            includeInternalStylesheet "offline.css"
        H.body $ do
            displayTopbar userIdentityMaybe TopbarNone
            H.div B.! A.class_ (H.stringValue "main") $ do
                H.div B.! A.class_ (H.textValue "header") $ do
                    H.div B.! A.class_ (H.textValue "header-bg") $ H.toHtml ("" :: T.Text)
                    H.h1 $ H.toHtml ("Offline" :: T.Text)
                    H.p $ H.toHtml ("Looks like you are offline :(" :: T.Text)
