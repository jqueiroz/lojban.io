{-# LANGUAGE OverloadedStrings #-}

module Server.Website.Views.Course
( displayCourseHome
) where

import Core
import Server.Core
import Server.Website.Views.Core
import Control.Monad (when, forM_)
import Data.Maybe (isJust, fromJust)
import Data.Either.Unwrap (fromRight)
import qualified Data.Text as T
import qualified Text.Pandoc as P
import qualified Text.Pandoc.Writers.HTML as PWH
import qualified Text.Blaze as B
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- TODO: consider using list groups (https://getbootstrap.com/docs/4.0/components/list-group/)
displayCourseHome :: ServerConfiguration -> Maybe UserIdentity -> TopbarCategory -> Course -> H.Html
displayCourseHome serverConfiguration userIdentityMaybe topbarCategory course = do
    let baseCourseUrl = ""
    let title = courseTitle course
    let shortDescription = courseShortDescription course
    H.docType
    H.html B.! A.lang (H.stringValue "en-us") $ do
        H.head $ do
            H.title $ H.toHtml (title `T.append` " :: lojban.io")
            H.meta B.! A.name (H.textValue "description") B.! A.content (H.textValue shortDescription)
            includeUniversalStylesheets
            includeUniversalScripts
            includeInternalStylesheet "course.css"
            includeCourseStylesheet course
            includeCourseScript course
        H.body $ do
            displayTopbar serverConfiguration userIdentityMaybe topbarCategory
            H.div B.! A.class_ (H.stringValue "main") $ do
                H.div B.! A.class_ (H.textValue "header") $ do
                    H.div B.! A.class_ (H.textValue "header-bg") $ H.toHtml ("" :: T.Text)
                    displayCourseMenu baseCourseUrl course
                H.div B.! A.class_ (H.textValue "body") $ do
                    H.div B.! A.class_ (H.stringValue "course") $ do
                        displayCourseContents baseCourseUrl course
                    displayFooter

displayCourseMenu :: String -> Course -> H.Html
displayCourseMenu baseCourseUrl course = do
    H.div B.! A.class_ (H.stringValue "course-header") $ do
        H.div B.! A.class_ (H.stringValue "course-info") $ do
            H.h1 B.! A.class_ "course-title" $ H.toHtml (courseTitle course)
            H.h1 B.! A.class_ "course-lessons-count" $ H.toHtml (showNumberOfLessons . length . courseLessons $ course)
            H.div B.! A.class_ "course-description" $ H.toHtml ("" :: String)

displayCourseContents :: String -> Course -> H.Html
displayCourseContents baseCourseUrl course = do
    let lessons = courseLessons course
    H.div B.! A.class_ (H.stringValue "course-contents") $ do
        when (isJust $ courseLongDescription course) $ do
            H.div B.! A.class_ (H.stringValue "course-about") $ do
                H.h2 $ H.toHtml ("About this course" :: String)
                H.div $ do
                    fromRight . P.runPure . PWH.writeHtml5 P.def $ fromJust (courseLongDescription course)
        H.div B.! A.class_ (H.stringValue "course-lessons") $ do
            H.h2 $ H.toHtml ("Lessons" :: String)
            H.ol $ forM_ (zip [1..] lessons) displayCourseLessonItem
        when (isJust $ courseCredits course) $ do
            H.div B.! A.class_ (H.stringValue "course-credits") $ do
                H.h2 $ H.toHtml ("Credits" :: String)
                H.div $ do
                    fromRight . P.runPure . PWH.writeHtml5 P.def $ fromJust (courseCredits course)

displayCourseLessonItem :: (Int, Lesson) -> H.Html
displayCourseLessonItem (lessonNumber, lesson) = do
    H.li $ do
        H.a (H.toHtml $ lessonTitle lesson)
            B.! A.href (H.stringValue . (++"/") . show $ lessonNumber)

showNumberOfLessons :: Int -> String
showNumberOfLessons 0 = "No lessons yet..."
showNumberOfLessons 1 = "1 lesson"
showNumberOfLessons x = (show x) ++ " lessons"

