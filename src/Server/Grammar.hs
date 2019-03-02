{-# LANGUAGE OverloadedStrings #-}

module Server.Grammar
( displayGrammarHome
) where

import Core
import Server.Core
import qualified Text.Blaze as B
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

displayGrammarHome :: H.Html
displayGrammarHome =
    H.html $ do
        H.head $ do
            H.title $ H.toHtml ("Grammar" :: String)
            includeUniversalStylesheets
            includeUniversalScripts
        H.body $ do
            displayTopbar TopbarGrammar
            H.div B.! A.class_ (H.stringValue "main") $ do
                H.h1 $ H.toHtml ("Grammar module" :: String)
