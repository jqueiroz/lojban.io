{-# LANGUAGE OverloadedStrings #-}

module Server.Website.Views.Lesson
( displayLessonHome
, displayLessonExercise
) where

import Core
import Server.Core
import Server.Website.Views.Core
import Control.Monad (when)
import Data.Maybe (isJust, fromJust)
import Data.Either.Unwrap (fromRight)
import Study.Framework.DocumentBuilders (buildGlossaryDocument)
import qualified Data.Text as T
import qualified Text.Pandoc as P
import qualified Text.Pandoc.Writers.HTML as PWH
import qualified Text.Blaze as B
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

data LessonSubpage = LessonHome | LessonVocabulary | LessonExercises deriving (Enum, Eq)

displayLessonHome :: ServerConfiguration -> Maybe UserIdentity -> TopbarCategory -> Course -> Int -> H.Html
displayLessonHome serverConfiguration userIdentityMaybe topbarCategory course lessonNumber = do
    let dictionary = courseDictionary course
    let baseLessonUrl = ""
    let lesson = (courseLessons course) !! (lessonNumber - 1)
    H.docType
    H.html B.! A.lang (H.stringValue "en-us") $ do
        H.head $ do
            H.title $ H.toHtml ((lessonTitle lesson) `T.append` " :: lojban.io")
            includeUniversalStylesheets
            includeUniversalScripts
            includeInternalStylesheet "lesson.css"
            includeCourseStylesheet course
            includeInternalScript "lesson-min.js"
            includeCourseScript course
            includeLessonScript lessonNumber
        H.body $ do
            displayTopbar serverConfiguration userIdentityMaybe topbarCategory
            H.div B.! A.class_ (H.stringValue "main") $ do
                H.div B.! A.class_ (H.textValue "header") $ do
                    H.div B.! A.class_ (H.textValue "header-bg") $ H.toHtml ("" :: T.Text)
                    displayLessonHeader baseLessonUrl LessonHome course lessonNumber
                H.div B.! A.class_ (H.textValue "body") $ do
                    H.div B.! A.class_ (H.stringValue "lesson") $ do
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
                            when (isJust $ lessonLecture lesson) $ do
                                H.div B.! A.class_ (H.stringValue "lesson-feedback") $ do
                                    H.div $ do
                                        H.h3 $ H.toHtml ("Feedback" :: T.Text)
                                        H.p $ do
                                            H.span "Any feedback about this lesson would be deeply appreciated. "
                                        H.p $ do
                                            H.span "If you believe you discovered an error, or if you have any criticism or suggestions, please consider "
                                            H.a B.! A.href (H.stringValue $ baseLessonUrl ++ "report") $ H.toHtml ("opening an issue" :: T.Text)
                                            H.span " in our GitHub repository."
                    displayFooter

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
    when hasLecture $ do
        displayLessonTab "lesson-tab-feedback" "Feedback" False

displayLessonTab :: String -> String -> Bool -> H.Html
displayLessonTab id title checked = do
    H.input B.! A.type_ (H.stringValue "radio") B.!? (checked, A.checked "checked") B.! A.name (H.stringValue "lesson-tabgroup") B.! A.id (H.stringValue id) B.! A.class_ (H.stringValue "lesson-tab-input")
    H.label B.! A.for (H.stringValue id) B.! A.class_ ("lesson-tab-label") $ do
        H.toHtml title

-- Embedded dictionary: consider using tooltips (https://getbootstrap.com/docs/4.0/components/tooltips/)
displayLessonExercise :: ServerConfiguration -> Maybe UserIdentity -> TopbarCategory -> Course -> Int -> H.Html
displayLessonExercise serverConfiguration userIdentityMaybe topbarCategory course lessonNumber = do
    let dictionary = courseDictionary course
    let baseLessonUrl = "../"
    H.docType
    H.html $ do
        H.head $ do
            H.title (H.toHtml $ (lessonTitle lesson) `T.append` " :: Practice :: lojban.io")
            includeUniversalStylesheets
            includeInternalStylesheet "funkyradio.css"
            includeInternalStylesheet "list-group-horizontal.css"
            includeInternalStylesheet "exercise.css"
            includeUniversalScripts
            includeCourseScript course
            includeLessonScript lessonNumber
            includeDictionaryScript dictionary
            includeInternalScript "exercise-min.js"
            includeCourseStylesheet course
        H.body $ do
            displayTopbar serverConfiguration userIdentityMaybe topbarCategory
            H.div B.! A.class_ (H.stringValue "main") $ do
                H.div B.! A.class_ (H.textValue "body") $ do
                    displayLessonHeader baseLessonUrl LessonExercises course lessonNumber
                    H.div B.! A.class_ (H.stringValue "lesson") $ do
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
                        title = ("Previous lesson: " `T.append`) . lessonTitle $ lessons !! (lessonNumber - 2)
                    in
                        H.a B.! A.href (H.stringValue url) B.! A.title (H.textValue title) $ H.toHtml ("<" :: String)
                -- Lesson title
                H.span $ H.toHtml ((T.pack $ show lessonNumber) `T.append` ". " `T.append` lessonTitle lesson)
                -- Next lesson
                when (lessonNumber < lessonsCount) $
                    let
                        url = ("../"++) . (baseLessonUrl++) . show $ lessonNumber + 1
                        title = ("Next lesson: " `T.append`) . lessonTitle $ lessons !! lessonNumber
                    in
                        H.a B.! A.href (H.stringValue url) B.! A.title (H.textValue title) $ H.toHtml (">" :: String)
        H.div B.! A.class_ "lesson-buttons" $ do
            when (lessonSubpage /= LessonHome) $ H.a B.! A.class_ (H.stringValue "button") B.! A.href (H.stringValue "../") $ (H.toHtml ("Review Theory" :: String))
            when (lessonSubpage /= LessonExercises) $ H.a B.! A.class_ (H.stringValue "button") B.! A.href (H.stringValue $ baseLessonUrl ++ "exercises") $ (H.toHtml ("Practice" :: String))
            --when (lessonSubpage /= LessonVocabulary) $ H.a B.! A.class_ (H.stringValue "button") B.! A.href (H.stringValue $ baseLessonUrl ++ "vocabulary")$ (H.toHtml ("Vocabulary" :: String))
            --TODO: consider alternative layout: inside theory, there are two tabs: one for the actual theory and another for vocabulary
            -- also consider including the lesson plan in a third tab

-- displayLessonVocabulary: consider using cards (https://getbootstrap.com/docs/4.0/components/card/)
-- probably better: table similar to https://www.memrise.com/course/37344/simplified-gismu/1/
