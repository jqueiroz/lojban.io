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
import qualified Lessons.Grammar.Lesson as GrammarLesson
import System.Random (newStdGen, mkStdGen)
import qualified Text.Blaze as B
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

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
    , dir "course" $ handleCourse dictionary GrammarLesson.course
    ]

handleCourse :: Dictionary -> CourseBuilder -> ServerPart Response
handleCourse dictionary courseBuilder =
    let course = courseBuilder dictionary
        lessons = courseLessons course
    in msum
        [ forceSlash . ok . toResponse . displayCourseHome $ course
        , path $ \n -> (guard $ 1 <= n && n <= (length lessons)) >> (handleLesson dictionary $ lessons !! (n-1))
        ]

handleLesson :: Dictionary -> Lesson -> ServerPart Response
handleLesson dictionary lesson = msum
    [ forceSlash . ok . toResponse . displayLessonHome $ lesson
    , dir "exercises" $ msum
        [ forceSlash . ok . toResponse $ displayExercise
        , path $ \n -> let exercise = lessonExercises lesson (mkStdGen n) in msum
            [ dir "get" $ do
                gen <- liftIO $ newStdGen
                ok $ toResponse (A.encode $ exerciseToJSON gen exercise)
            , dir "submit" $ do
                body <- getBody
                ok . toResponse . A.encode . A.object $ case validateExerciseAnswer exercise body of
                    Nothing -> [("success", A.Bool False)]
                    Just data' -> [("success", A.Bool True), ("data", data')]
            ]
        ]
    ]

-- Home page
displayHome =
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

displayLessonItem :: (Integer, Lesson) -> H.Html
displayLessonItem (lessonNumber, lesson) = do
    H.li $ do
        H.a (H.toHtml $ lessonTitle lesson)
            B.! A.href (H.stringValue . (++"/") . show $ lessonNumber)

-- Lesson page
displayLessonHome :: Lesson -> H.Html
displayLessonHome lesson = do
    let title = lessonTitle lesson
    H.html $ do
        H.head $ do
            H.title $ H.toHtml title
            universalStylesheets
            universalScripts
        H.body $ do
            H.h1 $ H.toHtml title

-- Exercise page
displayExercise =
    H.html $ do
        H.head $ do
            H.title (H.toHtml ("Lojto" :: T.Text))
            universalStylesheets
            internalStylesheet "funkyradio.css"
            internalStylesheet "list-group-horizontal.css"
            internalStylesheet "exercise.css"
            universalScripts
            internalScript "exercise.js"
        H.body $ do
          H.div ""
            B.! A.id "exercise-holder"
