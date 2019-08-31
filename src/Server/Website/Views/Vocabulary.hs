{-# LANGUAGE OverloadedStrings #-}

module Server.Website.Views.Vocabulary
( displayVocabularyHome
) where

import Server.Core
import Server.Website.Views.Core
import Server.Website.Views.Modules
import qualified Courses.English.Vocabulary.Brivla.Course
import qualified Courses.English.Vocabulary.Attitudinals.Course
import qualified Text.Blaze as B
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

displayVocabularyHome :: Maybe UserIdentity -> H.Html
displayVocabularyHome userIdentityMaybe = do
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
            displayTopbar userIdentityMaybe TopbarVocabulary
            H.div B.! A.class_ (H.stringValue "main") $ do
                displayModule moduleTitle moduleCourses
