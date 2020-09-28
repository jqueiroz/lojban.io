{-# LANGUAGE OverloadedStrings #-}

module Server.Website.Views.NotFound
( displayNotFoundHome
) where

import Server.Core
import Server.Website.Views.Core
import qualified Data.Text as T
import qualified Text.Blaze as B
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

displayNotFoundHome :: ServerConfiguration -> Maybe UserIdentity -> H.Html
displayNotFoundHome serverConfiguration userIdentityMaybe = do
    H.docType
    H.html B.! A.lang (H.stringValue "en-us") $ do
        H.head $ do
            H.title $ H.toHtml ("Not found :: lojban.io" :: T.Text)
            includeUniversalStylesheets
            includeUniversalScripts
            includeInternalStylesheet "notfound.css"
        H.body $ do
            displayTopbar serverConfiguration userIdentityMaybe TopbarNone
            H.div B.! A.class_ (H.stringValue "main") $ do
                H.div B.! A.class_ (H.textValue "header") $ do
                    H.div B.! A.class_ (H.textValue "header-bg") $ H.toHtml ("" :: T.Text)
                    H.h1 $ H.toHtml ("Not found" :: T.Text)
                    H.p $ H.toHtml ("Sorry, this page could not be found." :: T.Text)
