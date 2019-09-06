{-# LANGUAGE OverloadedStrings #-}

module Server.Website.Views.Courses
( displayCoursesHome
) where

import Core
import Server.Core
import Server.Website.Views.Core
import qualified Courses.English.Grammar.Introduction.Course
import qualified Courses.English.Grammar.Crash.Course
import qualified Courses.English.Vocabulary.Attitudinals.Course
import qualified Courses.English.Vocabulary.Brivla.Course
import qualified Data.Text as T
import qualified Text.Blaze as B
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

displayCoursesHome :: Maybe UserIdentity -> H.Html
displayCoursesHome userIdentityMaybe = do
    H.html $ do
        H.head $ do
            H.title $ H.toHtml ("lojban :: Courses" :: T.Text)
            includeUniversalStylesheets
            includeUniversalScripts
            includeInternalStylesheet "courses.css"
        H.body $ do
            displayTopbar userIdentityMaybe TopbarCourses
            H.div B.! A.class_ (H.textValue "main") $ do
                H.div B.! A.class_ (H.textValue "header") $ do
                    H.h1 $ H.toHtml ("Courses" :: T.Text)
                H.div B.! A.class_ (H.textValue "body") $ do
                    H.div B.! A.class_ (H.textValue "grammar") $ do
                        H.h2 $ H.toHtml ("Learn from the beginning" :: T.Text)
                        H.div B.! A.class_ (H.textValue "grid") $ do
                            displayCourse ("/grammar/introduction", Courses.English.Grammar.Introduction.Course.course)
                            displayCourse ("/grammar/crash", Courses.English.Grammar.Crash.Course.course)
                    H.div B.! A.class_ (H.textValue "vocabulary") $ do
                        H.h2 $ H.toHtml ("Learn by subject" :: T.Text)
                        H.div B.! A.class_ (H.textValue "grid") $ do
                            displayCourse ("/vocabulary/attitudinals", Courses.English.Vocabulary.Attitudinals.Course.course)
                            displayCourse ("/vocabulary/brivla", Courses.English.Vocabulary.Brivla.Course.course)
                            displayCourse ("/vocabulary/attitudinals", Courses.English.Vocabulary.Attitudinals.Course.course)
                            displayCourse ("/vocabulary/attitudinals", Courses.English.Vocabulary.Attitudinals.Course.course)
                            displayCourse ("/vocabulary/brivla", Courses.English.Vocabulary.Brivla.Course.course)
                            displayCourse ("/vocabulary/brivla", Courses.English.Vocabulary.Brivla.Course.course)
                            displayCourse ("/vocabulary/brivla", Courses.English.Vocabulary.Brivla.Course.course)
                            displayCourse ("/vocabulary/brivla", Courses.English.Vocabulary.Brivla.Course.course)
                            displayCourse ("/vocabulary/brivla", Courses.English.Vocabulary.Brivla.Course.course)
                            displayCourse ("/vocabulary/brivla", Courses.English.Vocabulary.Brivla.Course.course)
                displayFooter

displayCourse :: (T.Text, Course) -> H.Html
displayCourse (url, course) = do
    let title = courseTitle course
    let shortDescription = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor." :: T.Text
    let linkText = "Learn more" :: T.Text
    H.li B.! A.class_ (H.textValue "course") $ do
        H.div B.! A.class_ (H.textValue "course-title") $ H.toHtml title
        H.div B.! A.class_ (H.textValue "course-description") $ H.toHtml shortDescription
        H.div B.! A.class_ (H.textValue "course-link") $ do
            H.a B.! A.href (H.textValue url) $ H.toHtml linkText
