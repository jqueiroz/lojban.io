{-# LANGUAGE OverloadedStrings #-}

module Server.Website.Vocabulary
( displayVocabularyHome
) where

import Server.Website.Core
import Server.Website.Modules
import qualified Courses.English.Vocabulary.Brivla.Course
import qualified Courses.English.Vocabulary.Attitudinals.Course
import qualified Text.Blaze as B
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

displayVocabularyHome :: H.Html
displayVocabularyHome = do
    let moduleTitle = "Vocabulary"
    let moduleCourses =
            [ ("/vocabulary/brivla", Courses.English.Vocabulary.Brivla.Course.course)
            , ("/vocabulary/attitudinals", Courses.English.Vocabulary.Attitudinals.Course.course)
            ]
    H.html $ do
        H.head $ do
            H.title $ H.toHtml ("Vocabulary" :: String)
            includeUniversalStylesheets
            includeUniversalScripts
            includeInternalStylesheet "module.css"
        H.body $ do
            displayTopbar TopbarVocabulary
            H.div B.! A.class_ (H.stringValue "main") $ do
                displayModule moduleTitle moduleCourses
