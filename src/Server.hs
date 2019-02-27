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
        [ forceSlash . ok . toResponse $ displayExercise topbarCategory course lessonNumber
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
displayCourseHome :: TopbarCategory -> Course -> H.Html
displayCourseHome topbarCategory course = do
    let title = courseTitle course
    let lessons = courseLessons course
    H.html $ do
        H.head $ do
            H.title $ H.toHtml title
            universalStylesheets
            universalScripts
        H.body $ do
            displayTopbar topbarCategory
            H.div B.! A.class_ (H.stringValue "main") $ do
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
displayLessonHome :: TopbarCategory -> Course -> Int -> H.Html
displayLessonHome topbarCategory course lessonNumber = do
    let lesson = (courseLessons course) !! (lessonNumber - 1)
    H.html $ do
        H.head $ do
            H.title $ H.toHtml (lessonTitle lesson)
            universalStylesheets
            universalScripts
            internalStylesheet "course.css"
        H.body $ do
            displayTopbar topbarCategory
            H.div B.! A.class_ (H.stringValue "main") $ do
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
displayExercise :: TopbarCategory -> Course -> Int -> H.Html
displayExercise topbarCategory course lessonNumber =
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
            displayTopbar topbarCategory
            H.div B.! A.class_ (H.stringValue "main") $ do
                H.div B.! A.class_ (H.stringValue "lesson") $ do
                    H.div B.! A.id (H.stringValue "exercise-holder") $ H.toHtml ("" :: String)
                    H.div B.! A.class_ (H.stringValue "lesson-menu") $ do
                        H.h4 $ H.toHtml ("Menu" :: String)
                        H.ul $ displayLessonMenuItems "../" course lessonNumber
                        H.a B.! A.href (H.stringValue "../") B.! A.class_ (H.stringValue "button") $ (H.toHtml ("Back to lesson" :: String))
    where
        lesson = (courseLessons course) !! (lessonNumber - 1)
