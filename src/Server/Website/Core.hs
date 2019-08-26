{-# LANGUAGE OverloadedStrings #-}

module Server.Website.Core
( includeUniversalStylesheets
, includeInternalStylesheet
, includeExternalStylesheet
, includeCourseStylesheet
, includeUniversalScripts
, includeInternalScript
, includeExternalScript
, includeDictionaryScript
, TopbarCategory (..)
, displayTopbar
) where

import Core
import Language.Lojban.Core
import qualified Data.Text as T
import qualified Text.Blaze as B
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- * Stylesheets
includeUniversalStylesheets :: H.Html
includeUniversalStylesheets = do
    includeInternalStylesheet "bootstrap.min.css"
    includeInternalStylesheet "main.css"
    includeExternalStylesheet "https://maxcdn.bootstrapcdn.com/font-awesome/4.5.0/css/font-awesome.min.css"
    includeExternalStylesheet "https://fonts.googleapis.com/icon?family=Material+Icons"

includeInternalStylesheet :: String -> H.Html
includeInternalStylesheet = includeExternalStylesheet . ("/static/style/"++)

includeExternalStylesheet :: String -> H.Html
includeExternalStylesheet src =
    H.link
      B.! A.href (H.stringValue src)
      B.! A.rel "stylesheet"

includeInlineStylesheet :: String -> H.Html
includeInlineStylesheet code =
    H.style B.! A.type_ (H.stringValue "text/css") $
        H.toHtml code

includeCourseStylesheet :: Course -> H.Html
includeCourseStylesheet course = includeInlineStylesheet code where
    style = courseStyle course
    courseColor1 = case (courseStyleColor1 style) of
        Just color ->"--course-color1: " ++ color ++ ";"
        Nothing -> ""
    courseIcon = case (courseStyleIconUrl style) of
        Just url -> "--course-icon: url(" ++ url ++ ");"
        Nothing -> ""
    code = concat
        [ ":root {"
        , courseColor1
        , courseIcon
        , "}"
        ]

-- * Scripts
includeUniversalScripts :: H.Html
includeUniversalScripts = do
    includeInternalScript "jquery-2.1.4.min.js"
    includeInternalScript "bootstrap.min.js"

includeInternalScript :: String -> H.Html
includeInternalScript = includeExternalScript . ("/static/scripts/"++)

includeExternalScript :: String -> H.Html
includeExternalScript src =
    H.script ""
      B.! A.type_ "text/javascript"
      B.! A.src (H.stringValue src)

includeDictionaryScript :: Dictionary -> H.Html
includeDictionaryScript dictionary = includeInternalScript $ T.unpack $ "dictionaries/" `T.append` (dictIdentifier dictionary) `T.append` ".js"

-- * Topbar
data TopbarCategory = TopbarHome | TopbarGrammar | TopbarVocabulary | TopbarResources deriving (Enum, Eq)

displayTopbar :: TopbarCategory -> H.Html
displayTopbar topbarCategory = do
    H.div B.! A.class_ (H.stringValue "topbar") $ do
        H.div B.! A.class_ "logo" $ do
            H.a (H.toHtml ("lojban" :: String))
                B.! A.href (H.stringValue "/")
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
    H.li $ do
        H.a (H.toHtml text)
            B.! A.href (H.stringValue url)
            B.! A.class_ selectedClass

