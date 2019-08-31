{-# LANGUAGE OverloadedStrings #-}

module Server.Website.Lesson
( displayLessonHome
, displayLessonExercise
) where

import Core
import Server.Core
import Server.Website.Core
import Control.Monad (when)
import Data.Maybe (isJust, fromJust)
import Data.Either.Unwrap (fromRight)
import Courses.Framework.DocumentBuilders (buildGlossaryDocument)
import qualified Text.Pandoc as P
import qualified Text.Pandoc.Writers.HTML as PWH
import qualified Text.Blaze as B
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

data LessonSubpage = LessonHome | LessonVocabulary | LessonExercises deriving (Enum, Eq)

displayLessonHome :: Maybe UserIdentity -> TopbarCategory -> Course -> Int -> H.Html
displayLessonHome userIdentityMaybe topbarCategory course lessonNumber = do
    let dictionary = courseDictionary course
    let baseLessonUrl = ""
    let lesson = (courseLessons course) !! (lessonNumber - 1)
    H.html $ do
        H.head $ do
            H.title $ H.toHtml (lessonTitle lesson)
            includeUniversalStylesheets
            includeUniversalScripts
            includeInternalStylesheet "lesson.css"
            includeCourseStylesheet course
        H.body $ do
            displayTopbar userIdentityMaybe topbarCategory
            H.div B.! A.class_ (H.stringValue "main") $ do
                H.div B.! A.class_ (H.stringValue "lesson") $ do
                    displayLessonHeader baseLessonUrl LessonHome course lessonNumber
                    H.div B.! A.class_ (H.stringValue "lesson-body") $ do
                        displayLessonTabs lesson
                        when (isJust $ lessonLecture lesson) $ do
                            H.div B.! A.class_ (H.stringValue "lesson-lecture") $ do
                                H.div $ do
                                    fromRight . P.runPure . PWH.writeHtml5 P.def $ fromJust (lessonLecture lesson)
                        when (isJust $ lessonPlan lesson) $ do
                            H.div B.! A.class_ (H.stringValue "lesson-plan") $ do
                                H.div $ do
                                    fromRight . P.runPure . PWH.writeHtml5 P.def $ fromJust (lessonPlan lesson)
                        when (isJust $ lessonVocabulary lesson) $ do
                            H.div B.! A.class_ (H.stringValue "lesson-vocabulary") $ do
                                H.div $ do
                                    fromRight . P.runPure . PWH.writeHtml5 P.def . buildGlossaryDocument dictionary $ fromJust (lessonVocabulary lesson)

displayLessonTabs :: Lesson -> H.Html
displayLessonTabs lesson = do
    let hasLecture = isJust $ lessonLecture lesson
    let hasPlan = isJust $ lessonPlan lesson
    let hasVocabulary = isJust $ lessonVocabulary lesson
    when hasLecture $ do
        displayLessonTab "lesson-tab-lecture" "Lecture" True
    when hasVocabulary $ do
        displayLessonTab "lesson-tab-vocabulary" "Vocabulary" $ (not hasLecture)
    when hasPlan $ do
        displayLessonTab "lesson-tab-plan" "Plan" $ (not hasLecture) && (not hasVocabulary)

displayLessonTab :: String -> String -> Bool -> H.Html
displayLessonTab id title checked = do
    H.input B.! A.type_ (H.stringValue "radio") B.!? (checked, A.checked "checked") B.! A.name (H.stringValue "lesson-tabgroup") B.! A.id (H.stringValue id) B.! A.class_ (H.stringValue "lesson-tab-input")
    H.label B.! A.for (H.stringValue id) B.! A.class_ ("lesson-tab-label") $ do
        H.toHtml title

-- Embedded dictionary: consider using tooltips (https://getbootstrap.com/docs/4.0/components/tooltips/)
displayLessonExercise :: Maybe UserIdentity -> TopbarCategory -> Course -> Int -> H.Html
displayLessonExercise userIdentityMaybe topbarCategory course lessonNumber = do
    let dictionary = courseDictionary course
    let baseLessonUrl = "../"
    H.html $ do
        H.head $ do
            H.title (H.toHtml (lessonTitle lesson ++ " :: Practice"))
            includeUniversalStylesheets
            includeInternalStylesheet "lesson.css"
            includeInternalStylesheet "funkyradio.css"
            includeInternalStylesheet "list-group-horizontal.css"
            includeInternalStylesheet "exercise.css"
            includeUniversalScripts
            includeDictionaryScript dictionary
            includeInternalScript "exercise.js"
            includeCourseStylesheet course
        H.body $ do
            displayTopbar userIdentityMaybe topbarCategory
            H.div B.! A.class_ (H.stringValue "main") $ do
                H.div B.! A.class_ (H.stringValue "lesson") $ do
                    displayLessonHeader baseLessonUrl LessonExercises course lessonNumber
                    H.div B.! A.id (H.stringValue "exercise-holder") $ H.toHtml ("" :: String)
    where
        lesson = (courseLessons course) !! (lessonNumber - 1)

displayLessonHeader :: String -> LessonSubpage -> Course -> Int -> H.Html
displayLessonHeader baseLessonUrl lessonSubpage course lessonNumber = do
    let baseCourseUrl = baseLessonUrl ++ "../"
    let lessons = courseLessons course
    let lessonsCount = length lessons
    let lesson = lessons !! (lessonNumber - 1)
    H.div B.! A.class_ (H.stringValue "lesson-header") $ do
        H.div B.! A.class_ (H.stringValue "lesson-info") $ do
            H.h1 B.! A.class_ "course-title" $
                H.a B.! A.href (H.stringValue baseCourseUrl) $ H.toHtml (courseTitle course)
            H.h2 B.! A.class_ "lesson-title" $ do
                -- Previous lesson
                when (lessonNumber >= 2) $
                    let
                        url = ("../" ++) . (baseLessonUrl ++) . show $ lessonNumber - 1
                        title = ("Previous lesson: " ++) . lessonTitle $ lessons !! (lessonNumber - 2)
                    in
                        H.a B.! A.href (H.stringValue url) B.! A.title (H.stringValue title) $ H.toHtml ("<" :: String)
                -- Lesson title
                H.span $ H.toHtml ((show lessonNumber) ++ ". " ++ lessonTitle lesson)
                -- Next lesson
                when (lessonNumber < lessonsCount) $
                    let
                        url = ("../"++) . (baseLessonUrl++) . show $ lessonNumber + 1
                        title = ("Next lesson: " ++) . lessonTitle $ lessons !! lessonNumber
                    in
                        H.a B.! A.href (H.stringValue url) B.! A.title (H.stringValue title) $ H.toHtml (">" :: String)
        H.div B.! A.class_ "lesson-buttons" $ do
            when (lessonSubpage /= LessonHome) $ H.a B.! A.class_ (H.stringValue "button") B.! A.href (H.stringValue "../") $ (H.toHtml ("Review Theory" :: String))
            when (lessonSubpage /= LessonExercises) $ H.a B.! A.class_ (H.stringValue "button") B.! A.href (H.stringValue $ baseLessonUrl ++ "exercises") $ (H.toHtml ("Practice" :: String))
            --when (lessonSubpage /= LessonVocabulary) $ H.a B.! A.class_ (H.stringValue "button") B.! A.href (H.stringValue $ baseLessonUrl ++ "vocabulary")$ (H.toHtml ("Vocabulary" :: String))
            --TODO: consider alternative layout: inside theory, there are two tabs: one for the actual theory and another for vocabulary
            -- also consider including the lesson plan in a third tab

-- displayLessonVocabulary: consider using cards (https://getbootstrap.com/docs/4.0/components/card/)
-- probably better: table similar to https://www.memrise.com/course/37344/simplified-gismu/1/
