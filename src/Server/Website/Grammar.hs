{-# LANGUAGE OverloadedStrings #-}

module Server.Website.Grammar
( displayGrammarHome
) where

import Server.Website.Core
import Server.Website.Modules
import qualified Courses.English.Grammar.Introduction.Course
import qualified Courses.English.Grammar.Crash.Course
import qualified Text.Blaze as B
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

displayGrammarHome :: H.Html
displayGrammarHome = do
    let moduleTitle = "Grammar"
    let moduleCourses =
            [ ("/grammar/introduction", Courses.English.Grammar.Introduction.Course.course)
            , ("/grammar/crash", Courses.English.Grammar.Crash.Course.course)
            ]
    H.html $ do
        H.head $ do
            H.title $ H.toHtml ("Grammar" :: String)
            includeUniversalStylesheets
            includeUniversalScripts
            includeInternalStylesheet "module.css"
        H.body $ do
            displayTopbar TopbarGrammar
            H.div B.! A.class_ (H.stringValue "main") $ do
                displayModule moduleTitle moduleCourses
