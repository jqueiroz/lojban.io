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

displayGrammarHome :: Maybe UserIdentity -> H.Html
displayGrammarHome userIdentityMaybe = do
    let moduleTitle = "Grammar"
    let moduleCourses =
            [ ("/grammar/introduction", Courses.English.Grammar.Introduction.Course.course)
            , ("/grammar/crash", Courses.English.Grammar.Crash.Course.course)
            ]
    H.html $ do
        H.head $ do
            H.title $ H.toHtml ("lojban :: Grammar" :: T.Text)
            includeUniversalStylesheets
            includeUniversalScripts
            includeInternalStylesheet "module.css"
        H.body $ do
            displayTopbar userIdentityMaybe TopbarCourses
            H.div B.! A.class_ (H.stringValue "main") $ do
                displayModule moduleTitle moduleCourses
