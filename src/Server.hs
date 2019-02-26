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
import System.Random (newStdGen, mkStdGen)
import qualified Text.Blaze as B
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Pandoc as P
import qualified Text.Pandoc.Writers.HTML as PWH

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
    ]

handleGrammar :: Dictionary -> ServerPart Response
handleGrammar dictionary = msum
    [ forceSlash . ok . toResponse $ displayGrammarHome
    , dir "introduction" $ handleCourse dictionary Courses.English.Grammar.Introduction.course
    ]

handleCourse :: Dictionary -> CourseBuilder -> ServerPart Response
handleCourse dictionary courseBuilder =
    let course = courseBuilder dictionary
        lessons = courseLessons course
    in msum
        [ forceSlash . ok . toResponse . displayCourseHome $ course
        , path $ \n -> (guard $ 1 <= n && n <= (length lessons)) >> (handleLesson dictionary course n)
        ]

handleLesson :: Dictionary -> Course -> Int -> ServerPart Response
handleLesson dictionary course lessonNumber = msum
    [ forceSlash . ok . toResponse $ displayLessonHome course lessonNumber
    , dir "exercises" $ msum
        [ forceSlash . ok . toResponse $ displayExercise course lessonNumber
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
    H.html ""

-- Grammar home page
displayGrammarHome =
    H.html ""

-- Course page
displayCourseHome :: Course -> H.Html
displayCourseHome course = do
    let title = courseTitle course
    let lessons = courseLessons course
    H.html $ do
        H.head $ do
            H.title $ H.toHtml title
            universalStylesheets
            universalScripts
        H.body $ do
            H.h1 $ H.toHtml title
            H.ul $ forM_ (zip [1..] lessons) displayLessonItem

displayLessonItem :: (Int, Lesson) -> H.Html
displayLessonItem (lessonNumber, lesson) = do
    H.li $ do
        H.a (H.toHtml $ lessonTitle lesson)
            B.! A.href (H.stringValue . (++"/") . show $ lessonNumber)

-- Lesson menu
displayLessonMenuItems :: String -> Course -> Int -> H.Html
displayLessonMenuItems baseUrl course lessonNumber = do
    let lessonsCount = length $ courseLessons course
    H.li $ H.a B.! A.href (H.stringValue $ "../" ++ baseUrl) $ (H.toHtml ("Course index" :: String))
    if lessonNumber <= 1 || lessonNumber <= lessonsCount
        then H.ul $ do
            if lessonNumber >= 2
                then H.li $ H.a B.! A.href (H.stringValue . ("../"++) . (baseUrl++) . show $ lessonNumber - 1) $ (H.toHtml ("Previous lesson" :: String))
                else return ()
            if lessonNumber < lessonsCount
                then H.li $ H.a B.! A.href (H.stringValue . ("../"++) . (baseUrl++) . show $ lessonNumber + 1) $ (H.toHtml ("Next lesson" :: String))
                else return ()
        else return ()
    H.li $ H.a B.! A.href (H.stringValue "pending") $ (H.toHtml ("Vocabulary" :: String))

-- Lesson page
displayTopbar :: Course -> H.Html
displayTopbar course = do
    H.div B.! A.class_ (H.stringValue "topbar") $ do
        H.h1 $ H.toHtml (courseTitle course)

displayLessonHome :: Course -> Int -> H.Html
displayLessonHome course lessonNumber = do
    let lesson = (courseLessons course) !! (lessonNumber - 1)
    H.html $ do
        H.head $ do
            H.title $ H.toHtml (lessonTitle lesson)
            universalStylesheets
            universalScripts
            internalStylesheet "course.css"
        H.body $ do
            displayTopbar course
            H.div B.! A.class_ (H.stringValue "lesson") $ do
                H.div B.! A.class_ (H.stringValue "lesson-contents") $ do
                    H.h3 $ H.toHtml (lessonTitle lesson)
                    H.div $ do
                        H.h4 $ H.toHtml ("Lesson plan" :: String)
                        fromRight $ P.runPure $ PWH.writeHtml5 P.def (lessonPlan lesson)
                H.div B.! A.class_ (H.stringValue "lesson-menu") $ do
                    H.h4 $ H.toHtml ("Menu" :: String)
                    H.ul $ displayLessonMenuItems "" course lessonNumber
                    H.a B.! A.href (H.stringValue "exercises") B.! A.class_ (H.stringValue "button") $ (H.toHtml ("Practice" :: String))

-- Exercise page
displayExercise :: Course -> Int -> H.Html
displayExercise course lessonNumber =
    H.html $ do
        H.head $ do
            H.title (H.toHtml ("Practice" :: T.Text))
            universalStylesheets
            internalStylesheet "course.css"
            internalStylesheet "funkyradio.css"
            internalStylesheet "list-group-horizontal.css"
            internalStylesheet "exercise.css"
            universalScripts
            internalScript "exercise.js"
        H.body $ do
            displayTopbar course
            H.div B.! A.class_ (H.stringValue "lesson") $ do
                H.div B.! A.id (H.stringValue "exercise-holder") $ H.toHtml ("" :: String)
                H.div B.! A.class_ (H.stringValue "lesson-menu") $ do
                    H.h4 $ H.toHtml ("Menu" :: String)
                    H.ul $ displayLessonMenuItems "../" course lessonNumber
                    H.a B.! A.href (H.stringValue "../") B.! A.class_ (H.stringValue "button") $ (H.toHtml ("Back to lesson" :: String))
    where
        lesson = (courseLessons course) !! (lessonNumber - 1)
