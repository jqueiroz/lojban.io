{-# LANGUAGE OverloadedStrings #-}

module Server.Website.Views.Courses
( displayCoursesHome
) where

import Core
import Server.Core
import Server.Website.Views.Core
import qualified Study.Courses.English.Grammar.Introduction.Course
import qualified Study.Courses.English.Grammar.Crash.Course
import qualified Study.Courses.English.Vocabulary.Attitudinals.Course
import qualified Study.Courses.English.Vocabulary.Brivla.Course
import qualified Data.Text as T
import qualified Text.Blaze as B
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

displayCoursesHome :: ServerConfiguration -> Maybe UserIdentity -> H.Html
displayCoursesHome serverConfiguration userIdentityMaybe = do
    let descriptionPart1 = ("Learn lojban with carefully designed courses, and practice with entertaining interactive exercises." :: T.Text)
    let descriptionPart2 = ("Learn from the beginning if you are a newcomer, or learn by subject if you are already familiar with the core aspects of the language." :: T.Text)
    let descriptionComplete = descriptionPart1 `T.append` " " `T.append` descriptionPart2
    H.docType
    H.html B.! A.lang (H.stringValue "en-us") $ do
        H.head $ do
            H.title $ H.toHtml ("Courses :: lojban.io" :: T.Text)
            H.meta B.! A.name (H.textValue "description") B.! A.content (H.textValue descriptionComplete)
            includeUniversalStylesheets
            includeUniversalScripts
            includeInternalStylesheet "courses.css"
        H.body $ do
            displayTopbar serverConfiguration userIdentityMaybe TopbarCourses
            H.div B.! A.class_ (H.textValue "main") $ do
                H.div B.! A.class_ (H.textValue "header") $ do
                    H.div B.! A.class_ (H.textValue "header-bg") $ H.toHtml ("" :: T.Text)
                    H.h1 $ H.toHtml ("Courses" :: T.Text)
                    H.p $ H.toHtml descriptionPart1
                    H.p $ H.toHtml descriptionPart2
                H.div B.! A.class_ (H.textValue "body") $ do
                    H.div B.! A.class_ (H.textValue "grammar") $ do
                        H.h2 $ H.toHtml ("Learn from the beginning" :: T.Text)
                        H.div B.! A.class_ (H.textValue "grid") $ do
                            displayCourse ("/courses/introduction", Study.Courses.English.Grammar.Introduction.Course.course)
                            --displayCourse ("/courses/crash", Study.Courses.English.Grammar.Crash.Course.course)
                    H.div B.! A.class_ (H.textValue "vocabulary") $ do
                        H.h2 $ H.toHtml ("Learn by subject" :: T.Text)
                        H.div B.! A.class_ (H.textValue "grid") $ do
                            displayCourse ("/courses/attitudinals", Study.Courses.English.Vocabulary.Attitudinals.Course.course)
                            --displayCourse ("/courses/brivla", Study.Courses.English.Vocabulary.Brivla.Course.course)
                            --displayCourse ("/courses/attitudinals", Study.Courses.English.Vocabulary.Attitudinals.Course.course)
                            --displayCourse ("/courses/attitudinals", Study.Courses.English.Vocabulary.Attitudinals.Course.course)
                            --displayCourse ("/courses/brivla", Study.Courses.English.Vocabulary.Brivla.Course.course)
                            --displayCourse ("/courses/brivla", Study.Courses.English.Vocabulary.Brivla.Course.course)
                            --displayCourse ("/courses/brivla", Study.Courses.English.Vocabulary.Brivla.Course.course)
                            --displayCourse ("/courses/brivla", Study.Courses.English.Vocabulary.Brivla.Course.course)
                            --displayCourse ("/courses/brivla", Study.Courses.English.Vocabulary.Brivla.Course.course)
                            --displayCourse ("/courses/brivla", Study.Courses.English.Vocabulary.Brivla.Course.course)
                    displayFooter

displayCourse :: (T.Text, Course) -> H.Html
displayCourse (url, course) = do
    let title = courseTitle course
    let shortDescription = courseShortDescription course
    let linkText = "Learn more" :: T.Text
    H.div B.! A.class_ (H.textValue "course") $ do
        H.div B.! A.class_ (H.textValue "course-title") $ H.toHtml title
        H.div B.! A.class_ (H.textValue "course-description") $ H.toHtml shortDescription
        H.div B.! A.class_ (H.textValue "course-link") $ do
            H.a B.! A.href (H.textValue url) $ H.toHtml linkText
