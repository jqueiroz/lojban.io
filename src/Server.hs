{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Server (main) where

import Core
import DictionaryLoader (loadDictionary)
import Serializer (exerciseToJSON, validateExerciseAnswer)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Happstack.Server
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as M
import qualified Data.Aeson as A
import qualified Data.Text as T
import Data.Either.Unwrap (fromRight)
import qualified Courses.English.Grammar.Introduction
import qualified Courses.English.Vocabulary.Brivla
import System.Random (newStdGen, mkStdGen)
import qualified Text.Blaze as B
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Pandoc as P
import qualified Text.Pandoc.Writers.HTML as PWH

-- TODO: consider adding breadcrumbs (https://getbootstrap.com/docs/4.0/components/breadcrumb/)

main :: IO ()
main = do
    dictionary <- loadDictionary
    simpleHTTP nullConf $ handleRoot dictionary

-- Utility functions
internalStylesheet :: String -> H.Html
internalStylesheet = externalStylesheet . ("/static/style/"++)

externalStylesheet :: String -> H.Html
externalStylesheet src =
    H.link
      B.! A.href (H.stringValue src)
      B.! A.rel "stylesheet"

internalScript :: String -> H.Html
internalScript = externalScript . ("/static/scripts/"++)

externalScript :: String -> H.Html
externalScript src =
    H.script ""
      B.! A.type_ "text/javascript"
      B.! A.src (H.stringValue src)

universalStylesheets = do
    internalStylesheet "bootstrap.min.css"
    internalStylesheet "main.css"
    externalStylesheet "https://maxcdn.bootstrapcdn.com/font-awesome/4.5.0/css/font-awesome.min.css"
    externalStylesheet "https://fonts.googleapis.com/icon?family=Material+Icons"

universalScripts = do
    internalScript "jquery-2.1.4.min.js"
    internalScript "bootstrap.min.js"

forceSlash :: ServerPart Response -> ServerPart Response
forceSlash x = nullDir >> msum [trailingSlash >> x, askRq >>= \rq -> seeOther (rqUri rq ++ "/") (toResponse ())]

getBody :: ServerPart BS.ByteString
getBody = askRq >>= liftIO . takeRequestBody >>= \body ->
    case body of
        Just rqbody -> return . unBody $ rqbody
        Nothing     -> return ""

-- Handlers
handleRoot :: Dictionary -> ServerPart Response
handleRoot dictionary = msum
    [ forceSlash . ok . toResponse $ displayHome
    , dir "static" $ serveDirectory EnableBrowsing [] "static"
    , dir "grammar" $ handleGrammar dictionary
    , dir "vocabulary" $ handleVocabulary dictionary
    , dir "resources" $ handleResources dictionary
    ]

handleGrammar :: Dictionary -> ServerPart Response
handleGrammar dictionary = msum
    [ forceSlash . ok . toResponse $ displayGrammarHome
    , dir "introduction" $ handleCourse TopbarGrammar dictionary Courses.English.Grammar.Introduction.course
    ]

handleVocabulary :: Dictionary -> ServerPart Response
handleVocabulary dictionary = msum
    [ forceSlash . ok . toResponse $ displayVocabularyHome
    , dir "brivla" $ handleCourse TopbarVocabulary dictionary Courses.English.Vocabulary.Brivla.course
    ]

handleResources :: Dictionary -> ServerPart Response
handleResources dictionary = msum
    [ forceSlash . ok . toResponse $ displayResourcesHome
    ]

handleCourse :: TopbarCategory -> Dictionary -> CourseBuilder -> ServerPart Response
handleCourse topbarCategory dictionary courseBuilder =
    let course = courseBuilder dictionary
        lessons = courseLessons course
    in msum
        [ forceSlash . ok . toResponse . displayCourseHome topbarCategory $ course
        , path $ \n -> (guard $ 1 <= n && n <= (length lessons)) >> (handleLesson topbarCategory dictionary course n)
        ]

handleLesson :: TopbarCategory -> Dictionary -> Course -> Int -> ServerPart Response
handleLesson topbarCategory dictionary course lessonNumber = msum
    [ forceSlash . ok . toResponse $ displayLessonHome topbarCategory course lessonNumber
    , dir "exercises" $ msum
        [ forceSlash . ok . toResponse $ displayLessonExercise topbarCategory course lessonNumber
        , path $ \n ->
            let
                lesson = (courseLessons course) !! (lessonNumber - 1)
                exercise = lessonExercises lesson (mkStdGen n)
            in msum
                [ dir "get" $ (liftIO $ newStdGen) >>= ok . toResponse . A.encode . exerciseToJSON exercise
                , dir "submit" $ getBody >>= \body -> ok . toResponse . A.encode . A.object $
                    case validateExerciseAnswer exercise body of
                        Nothing -> [("success", A.Bool False)]
                        Just data' -> [("success", A.Bool True), ("data", data')]
                ]
        ]
    ]

-- Home page
displayHome =
    H.html $ do
        H.head $ do
            H.title $ H.toHtml ("Home" :: String)
            universalStylesheets
            universalScripts
        H.body $ do
            displayTopbar TopbarHome
            H.div B.! A.class_ (H.stringValue "main") $ do
                H.h1 $ H.toHtml ("Home" :: String)

-- Grammar home page
displayGrammarHome :: H.Html
displayGrammarHome =
    H.html $ do
        H.head $ do
            H.title $ H.toHtml ("Grammar" :: String)
            universalStylesheets
            universalScripts
        H.body $ do
            displayTopbar TopbarGrammar
            H.div B.! A.class_ (H.stringValue "main") $ do
                H.h1 $ H.toHtml ("Grammar module" :: String)

-- Vocabulary home page
displayVocabularyHome :: H.Html
displayVocabularyHome =
    H.html $ do
        H.head $ do
            H.title $ H.toHtml ("Vocabulary" :: String)
            universalStylesheets
            universalScripts
        H.body $ do
            displayTopbar TopbarVocabulary
            H.div B.! A.class_ (H.stringValue "main") $ do
                H.h1 $ H.toHtml ("Vocabulary module" :: String)

-- Resources home page
displayResourcesHome :: H.Html
displayResourcesHome =
    H.html $ do
        H.head $ do
            H.title $ H.toHtml ("Additional resources" :: String)
            universalStylesheets
            universalScripts
        H.body $ do
            displayTopbar TopbarResources
            H.div B.! A.class_ (H.stringValue "main") $ do
                H.h1 $ H.toHtml ("Additional resources" :: String)

-- Topbar
data TopbarCategory = TopbarHome | TopbarGrammar | TopbarVocabulary | TopbarResources deriving (Enum, Eq)

displayTopbar :: TopbarCategory -> H.Html
displayTopbar topbarCategory = do
    H.div B.! A.class_ (H.stringValue "topbar") $ do
        H.div B.! A.class_ "logo" $ H.toHtml ("lojban" :: String)
        displayTopbarMenu topbarCategory

displayTopbarMenu :: TopbarCategory -> H.Html
displayTopbarMenu topbarCategory = do
    H.ul $ do
        displayTopbarMenuItem (topbarCategory == TopbarHome) "Home" "/"
        displayTopbarMenuItem (topbarCategory == TopbarGrammar) "Grammar" "/grammar/"
        displayTopbarMenuItem (topbarCategory == TopbarVocabulary) "Vocabulary" "/vocabulary/"
        displayTopbarMenuItem (topbarCategory == TopbarResources) "Resources" "/resources/"

displayTopbarMenuItem :: Bool -> String -> String -> H.Html
displayTopbarMenuItem selected text url = do
    let selectedClass = if selected then "selected" else ""
    H.li B.! A.class_ selectedClass $ do
        H.a (H.toHtml text)
            B.! A.href (H.stringValue url)

-- Course page
-- TODO: consider using list groups (https://getbootstrap.com/docs/4.0/components/list-group/)
displayCourseHome :: TopbarCategory -> Course -> H.Html
displayCourseHome topbarCategory course = do
    let title = courseTitle course
    H.html $ do
        H.head $ do
            H.title $ H.toHtml title
            universalStylesheets
            universalScripts
            internalStylesheet "course.css"
            courseStylesheet course
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

courseStylesheet :: Course -> H.Html
courseStylesheet course =
    case courseStyleFilename (courseStyle course) of
        Nothing -> return ()
        Just filename -> internalStylesheet filename

-- Lesson pages
data LessonSubpage = LessonHome | LessonVocabulary | LessonExercises deriving (Enum, Eq)

displayLessonHome :: TopbarCategory -> Course -> Int -> H.Html
displayLessonHome topbarCategory course lessonNumber = do
    let lesson = (courseLessons course) !! (lessonNumber - 1)
    H.html $ do
        H.head $ do
            H.title $ H.toHtml (lessonTitle lesson)
            universalStylesheets
            universalScripts
            internalStylesheet "lesson.css"
            courseStylesheet course
        H.body $ do
            displayTopbar topbarCategory
            H.div B.! A.class_ (H.stringValue "main") $ do
                H.div B.! A.class_ (H.stringValue "lesson") $ do
                    displayLessonHeader "" LessonHome course lessonNumber
                    H.div B.! A.class_ (H.stringValue "lesson-contents") $ do
                        H.div $ do
                            H.h3 $ H.toHtml ("Lesson plan" :: String)
                            fromRight $ P.runPure $ PWH.writeHtml5 P.def (lessonPlan lesson)

-- Embedded dictionary: consider using tooltips (https://getbootstrap.com/docs/4.0/components/tooltips/)
displayLessonExercise :: TopbarCategory -> Course -> Int -> H.Html
displayLessonExercise topbarCategory course lessonNumber =
    H.html $ do
        H.head $ do
            H.title (H.toHtml (lessonTitle lesson ++ " :: Practice"))
            universalStylesheets
            internalStylesheet "lesson.css"
            internalStylesheet "funkyradio.css"
            internalStylesheet "list-group-horizontal.css"
            internalStylesheet "exercise.css"
            universalScripts
            internalScript "exercise.js"
            courseStylesheet course
        H.body $ do
            displayTopbar topbarCategory
            H.div B.! A.class_ (H.stringValue "main") $ do
                H.div B.! A.class_ (H.stringValue "lesson") $ do
                    displayLessonHeader "../" LessonExercises course lessonNumber
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
                when (lessonNumber >= 2) $
                    let
                        url = ("../" ++) . (baseLessonUrl ++) . show $ lessonNumber - 1
                        title = ("Previous lesson: " ++) . lessonTitle $ lessons !! (lessonNumber - 2)
                    in
                        H.a B.! A.href (H.stringValue url) B.! A.title (H.stringValue title) $ H.toHtml ("<" :: String)
                H.span $ H.toHtml ((show lessonNumber) ++ ". " ++ lessonTitle lesson)
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
