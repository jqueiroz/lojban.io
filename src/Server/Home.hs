{-# LANGUAGE OverloadedStrings #-}

module Server.Home
( displayHome
) where

import Core
import Server.Core
import qualified Text.Blaze as B
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

displayHome =
    H.html $ do
        H.head $ do
            H.title $ H.toHtml ("Home" :: String)
            universalStylesheets
            universalScripts
        H.body $ do
            displayTopbar TopbarHome
            H.div B.! A.class_ (H.stringValue "main") $ do
                H.h1 $ H.toHtml ("Home" :: String)
