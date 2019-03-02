{-# LANGUAGE OverloadedStrings #-}

module Server.Vocabulary
( displayVocabularyHome
) where

import Core
import Server.Core
import qualified Text.Blaze as B
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

displayVocabularyHome :: H.Html
displayVocabularyHome =
    H.html $ do
        H.head $ do
            H.title $ H.toHtml ("Vocabulary" :: String)
            includeUniversalStylesheets
            includeUniversalScripts
        H.body $ do
            displayTopbar TopbarVocabulary
            H.div B.! A.class_ (H.stringValue "main") $ do
                H.h1 $ H.toHtml ("Vocabulary module" :: String)
