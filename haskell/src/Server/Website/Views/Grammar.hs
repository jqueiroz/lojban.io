{-# LANGUAGE OverloadedStrings #-}

module Server.Website.Views.Grammar
( displayGrammarHome
) where

import Server.Core
import Server.Website.Views.Core
import Server.Website.Views.Modules
import qualified Courses.English.Grammar.Introduction.Course
import qualified Courses.English.Grammar.Crash.Course
import qualified Data.Text as T
import qualified Text.Blaze as B
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

displayGrammarHome :: ServerConfiguration -> Maybe UserIdentity -> H.Html
displayGrammarHome serverConfiguration userIdentityMaybe = do
    let moduleTitle = "Grammar"
    let moduleCourses =
            [ ("/grammar/introduction", Courses.English.Grammar.Introduction.Course.course)
            , ("/grammar/crash", Courses.English.Grammar.Crash.Course.course)
            ]
    H.docType
    H.html B.! A.lang (H.stringValue "en-us") $ do
        H.head $ do
            H.title $ H.toHtml ("Grammar :: lojban.io" :: T.Text)
            includeUniversalStylesheets
            includeUniversalScripts
            includeInternalStylesheet "module.css"
        H.body $ do
            displayTopbar serverConfiguration userIdentityMaybe TopbarCourses
            H.div B.! A.class_ (H.stringValue "main") $ do
                displayModule moduleTitle moduleCourses
