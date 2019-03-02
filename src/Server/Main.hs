{-# LANGUAGE OverloadedStrings #-}

module Server.Main (main) where

import Core
import DictionaryLoader (loadDictionary)
import Serializer (exerciseToJSON, validateExerciseAnswer)
import qualified Courses.English.Grammar.Introduction
import qualified Courses.English.Vocabulary.Brivla
import Server.Core
import Server.Home (displayHome)
import Server.Grammar (displayGrammarHome)
import Server.Vocabulary (displayVocabularyHome)
import Server.Resources (displayResourcesHome)
import Server.Course (displayCourseHome)
import Server.Lesson (displayLessonHome, displayLessonExercise)
import Control.Monad (msum, guard)
import Control.Monad.IO.Class (liftIO)
import System.Random (newStdGen, mkStdGen)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Aeson as A
import Happstack.Server

-- TODO: consider adding breadcrumbs (https://getbootstrap.com/docs/4.0/components/breadcrumb/)

main :: IO ()
main = do
    dictionary <- loadDictionary
    simpleHTTP nullConf $ handleRoot dictionary

-- Utility functions
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
