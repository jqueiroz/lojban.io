{-# LANGUAGE OverloadedStrings #-}

module Server.Core
( universalStylesheets
, internalStylesheet
, externalStylesheet
, courseStylesheet
, universalScripts
, internalScript
, externalScript
, TopbarCategory (..)
, displayTopbar
) where

import Core
import qualified Text.Blaze as B
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- Stylesheets
universalStylesheets = do
    internalStylesheet "bootstrap.min.css"
    internalStylesheet "main.css"
    externalStylesheet "https://maxcdn.bootstrapcdn.com/font-awesome/4.5.0/css/font-awesome.min.css"
    externalStylesheet "https://fonts.googleapis.com/icon?family=Material+Icons"

internalStylesheet :: String -> H.Html
internalStylesheet = externalStylesheet . ("/static/style/"++)

externalStylesheet :: String -> H.Html
externalStylesheet src =
    H.link
      B.! A.href (H.stringValue src)
      B.! A.rel "stylesheet"

courseStylesheet :: Course -> H.Html
courseStylesheet course =
    case courseStyleFilename (courseStyle course) of
        Nothing -> return ()
        Just filename -> internalStylesheet filename

-- Scripts
universalScripts = do
    internalScript "jquery-2.1.4.min.js"
    internalScript "bootstrap.min.js"

internalScript :: String -> H.Html
internalScript = externalScript . ("/static/scripts/"++)

externalScript :: String -> H.Html
externalScript src =
    H.script ""
      B.! A.type_ "text/javascript"
      B.! A.src (H.stringValue src)


-- Topbar
data TopbarCategory = TopbarHome | TopbarGrammar | TopbarVocabulary | TopbarResources deriving (Enum, Eq)

displayTopbar :: TopbarCategory -> H.Html
displayTopbar topbarCategory = do
    H.div B.! A.class_ (H.stringValue "topbar") $ do
        H.div B.! A.class_ "logo" $ H.toHtml ("lojban" :: String)
        displayTopbarMenu topbarCategory

displayTopbarMenu :: TopbarCategory -> H.Html
displayTopbarMenu topbarCategory = do
    H.ul $ do
        displayTopbarMenuItem (topbarCategory == TopbarHome) "Home" "/"
        displayTopbarMenuItem (topbarCategory == TopbarGrammar) "Grammar" "/grammar/"
        displayTopbarMenuItem (topbarCategory == TopbarVocabulary) "Vocabulary" "/vocabulary/"
        displayTopbarMenuItem (topbarCategory == TopbarResources) "Resources" "/resources/"

displayTopbarMenuItem :: Bool -> String -> String -> H.Html
displayTopbarMenuItem selected text url = do
    let selectedClass = if selected then "selected" else ""
    H.li B.! A.class_ selectedClass $ do
        H.a (H.toHtml text)
            B.! A.href (H.stringValue url)

