{-# LANGUAGE OverloadedStrings #-}

module Server.Website.Views.Home
( displayHome
) where

import Server.Core
import Server.Website.Views.Core
import qualified Data.Text as T
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
            includeInternalStylesheet "home.css"
        H.body $ do
            displayTopbar userIdentityMaybe TopbarHome
            H.div B.! A.class_ (H.textValue "main") $ do
                H.div B.! A.class_ (H.textValue "header") $ do
                    H.h1 $ do
                        H.span B.! A.class_ (H.textValue "learn") $ H.toHtml ("Learn " :: T.Text)
                        H.span B.! A.class_ (H.textValue "logically") $ H.toHtml ("logically" :: T.Text)
                    H.p $ H.toHtml ("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor. Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor." :: String)
                    H.div B.! A.class_ (H.textValue "buttons") $ do
                        H.a (H.toHtml ("Courses" :: T.Text))
                            B.! A.href (H.textValue "/courses")
                        H.a (H.toHtml ("Decks" :: T.Text))
                            B.! A.href (H.textValue "/decks")
                H.div B.! A.class_ (H.textValue "courses") $ do
                    H.h1 $ H.toHtml ("Courses" :: T.Text)
