{-# LANGUAGE OverloadedStrings #-}

module Server.Course
( displayCourseHome
) where

import Core
import Server.Core
import Control.Monad (forM_)
import qualified Text.Blaze as B
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- TODO: consider using list groups (https://getbootstrap.com/docs/4.0/components/list-group/)
displayCourseHome :: TopbarCategory -> Course -> H.Html
displayCourseHome topbarCategory course = do
    let title = courseTitle course
    H.html $ do
        H.head $ do
            H.title $ H.toHtml title
            includeUniversalStylesheets
            includeUniversalScripts
            includeInternalStylesheet "course.css"
            includeCourseStylesheet course
        H.body $ do
            displayTopbar topbarCategory
            H.div B.! A.class_ (H.stringValue "main") $ do
                H.div B.! A.class_ (H.stringValue "course") $ do
                    displayCourseMenu "" course
                    displayCourseContents "" course

displayCourseMenu :: String -> Course -> H.Html
displayCourseMenu baseCourseUrl course = do
    H.div B.! A.class_ (H.stringValue "course-header") $ do
        H.div B.! A.class_ (H.stringValue "course-info") $ do
            H.h1 B.! A.class_ "course-title" $ H.toHtml (courseTitle course)
            H.div B.! A.class_ "course-description" $ H.toHtml ("" :: String)

displayCourseContents :: String -> Course -> H.Html
displayCourseContents baseCourseUrl course = do
    let lessons = courseLessons course
    H.div B.! A.class_ (H.stringValue "course-contents") $ do
        H.h2 $ H.toHtml ("Lessons" :: String)
        H.ol $ forM_ (zip [1..] lessons) displayCourseLessonItem

displayCourseLessonItem :: (Int, Lesson) -> H.Html
displayCourseLessonItem (lessonNumber, lesson) = do
    H.li $ do
        H.a (H.toHtml $ lessonTitle lesson)
            B.! A.href (H.stringValue . (++"/") . show $ lessonNumber)

