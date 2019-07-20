{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Server.Main (main) where

import Core
import Serializer (exerciseToJSON, validateExerciseAnswer)
import qualified Courses.English.Grammar.Introduction.Course
import qualified Courses.English.Vocabulary.Attitudinals.Course
import qualified Courses.English.Vocabulary.Brivla.Course
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
main = simpleHTTP nullConf handleRoot

-- Utility functions
forceSlash :: ServerPart Response -> ServerPart Response
forceSlash x = nullDir >> msum [trailingSlash >> x, askRq >>= \rq -> seeOther (rqUri rq ++ "/") (toResponse ())]

getBody :: ServerPart BS.ByteString
getBody = askRq >>= liftIO . takeRequestBody >>=
    \case
        Just rqbody -> return . unBody $ rqbody
        Nothing     -> return ""

-- Handlers
handleRoot :: ServerPart Response
handleRoot = msum
    [ forceSlash . ok . toResponse $ displayHome
    , dir "static" $ serveDirectory EnableBrowsing [] "static"
    , dir "grammar" handleGrammar
    , dir "vocabulary" handleVocabulary
    , dir "resources" handleResources
    ]

handleGrammar :: ServerPart Response
handleGrammar = msum
    [ forceSlash . ok . toResponse $ displayGrammarHome
    , dir "introduction" $ handleCourse TopbarGrammar Courses.English.Grammar.Introduction.Course.course
    ]

handleVocabulary :: ServerPart Response
handleVocabulary = msum
    [ forceSlash . ok . toResponse $ displayVocabularyHome
    , dir "attitudinals" $ handleCourse TopbarVocabulary Courses.English.Vocabulary.Attitudinals.Course.course
    , dir "brivla" $ handleCourse TopbarVocabulary Courses.English.Vocabulary.Brivla.Course.course
    ]

handleResources :: ServerPart Response
handleResources = msum
    [ forceSlash . ok . toResponse $ displayResourcesHome
    ]

handleCourse :: TopbarCategory -> Course -> ServerPart Response
handleCourse topbarCategory course =
    let lessons = courseLessons course
    in msum
        [ forceSlash . ok . toResponse . displayCourseHome topbarCategory $ course
        , path $ \n -> (guard $ 1 <= n && n <= (length lessons)) >> (handleLesson topbarCategory course n)
        ]

handleLesson :: TopbarCategory -> Course -> Int -> ServerPart Response
handleLesson topbarCategory course lessonNumber = msum
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
