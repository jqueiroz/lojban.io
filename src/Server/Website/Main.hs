{-# LANGUAGE OverloadedStrings #-}

module Server.Website.Main
( handleRoot
) where

import Core
import Serializer (exerciseToJSON, validateExerciseAnswer)
import qualified Courses.English.Grammar.Introduction.Course
import qualified Courses.English.Grammar.Crash.Course
import qualified Courses.English.Vocabulary.Attitudinals.Course
import qualified Courses.English.Vocabulary.Brivla.Course
import Server.Core
import Server.Util (forceSlash, getBody)
import Server.Website.Views.Core
import Server.Website.Views.Home (displayHome)
import Server.Website.Views.Grammar (displayGrammarHome)
import Server.Website.Views.Vocabulary (displayVocabularyHome)
import Server.Website.Views.Resources (displayResourcesHome)
import Server.Website.Views.Course (displayCourseHome)
import Server.Website.Views.Lesson (displayLessonHome, displayLessonExercise)
import Control.Monad (msum, guard)
import Control.Monad.IO.Class (liftIO)
import System.Random (newStdGen, mkStdGen)
import qualified Server.OAuth2.Main as OAuth2
import qualified Data.Aeson as A
import Happstack.Server

-- TODO: consider adding breadcrumbs (https://getbootstrap.com/docs/4.0/components/breadcrumb/)

-- * Handlers
handleRoot :: ServerResources -> ServerPart Response
handleRoot serverResources = do
    userIdentityMaybe <- OAuth2.readUserIdentityFromCookies serverResources
    msum
        [ forceSlash $ handleHome userIdentityMaybe
        , dir "grammar" $ handleGrammar userIdentityMaybe
        , dir "vocabulary" $ handleVocabulary userIdentityMaybe
        , dir "resources" $ handleResources userIdentityMaybe
        ]

handleHome :: Maybe UserIdentity -> ServerPart Response
handleHome userIdentityMaybe = ok . toResponse $ displayHome userIdentityMaybe

handleGrammar :: Maybe UserIdentity -> ServerPart Response
handleGrammar userIdentityMaybe = msum
    [ forceSlash . ok . toResponse $ displayGrammarHome userIdentityMaybe
    , dir "introduction" $ handleCourse userIdentityMaybe TopbarGrammar Courses.English.Grammar.Introduction.Course.course
    , dir "crash" $ handleCourse userIdentityMaybe TopbarGrammar Courses.English.Grammar.Crash.Course.course
    ]

handleVocabulary :: Maybe UserIdentity -> ServerPart Response
handleVocabulary userIdentityMaybe = msum
    [ forceSlash . ok . toResponse $ displayVocabularyHome userIdentityMaybe
    , dir "attitudinals" $ handleCourse userIdentityMaybe TopbarVocabulary Courses.English.Vocabulary.Attitudinals.Course.course
    , dir "brivla" $ handleCourse userIdentityMaybe TopbarVocabulary Courses.English.Vocabulary.Brivla.Course.course
    ]

handleResources :: Maybe UserIdentity -> ServerPart Response
handleResources userIdentityMaybe = msum
    [ forceSlash . ok . toResponse $ displayResourcesHome userIdentityMaybe
    ]

handleCourse :: Maybe UserIdentity -> TopbarCategory -> Course -> ServerPart Response
handleCourse userIdentityMaybe topbarCategory course =
    let lessons = courseLessons course
    in msum
        [ forceSlash . ok . toResponse . displayCourseHome userIdentityMaybe topbarCategory $ course
        , path $ \n -> (guard $ 1 <= n && n <= (length lessons)) >> (handleLesson userIdentityMaybe topbarCategory course n)
        ]

handleLesson :: Maybe UserIdentity -> TopbarCategory -> Course -> Int -> ServerPart Response
handleLesson userIdentityMaybe topbarCategory course lessonNumber = msum
    [ forceSlash . ok . toResponse $ displayLessonHome userIdentityMaybe topbarCategory course lessonNumber
    , dir "exercises" $ msum
        [ forceSlash . ok . toResponse $ displayLessonExercise userIdentityMaybe topbarCategory course lessonNumber
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
