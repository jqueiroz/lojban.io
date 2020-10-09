{-# LANGUAGE OverloadedStrings #-}

module Server.Website.Main
( handleRoot
) where

import Core
import Serializer (exerciseToJSON, validateExerciseAnswer)
import qualified Study.Courses.English.Grammar.Introduction.Course
import qualified Study.Courses.English.Grammar.Crash.Course
import qualified Study.Courses.English.Vocabulary.Attitudinals.Course
import qualified Study.Courses.English.Vocabulary.Brivla.Course
import qualified Study.Decks.English.ContextualizedBrivla
import Server.Core
import Server.Util (forceSlash, getBody)
import Server.Website.Views.Core
import Server.Website.Views.Home (displayHome)
import Server.Website.Views.Courses (displayCoursesHome)
import Server.Website.Views.Decks (displayDecksHome)
import Server.Website.Views.Deck (displayDeckHome, displayDeckExercise)
import Server.Website.Views.Resources (displayResourcesHome)
import Server.Website.Views.Offline (displayOfflineHome)
import Server.Website.Views.NotFound (displayNotFoundHome)
import Server.Website.Views.Course (displayCourseHome)
import Server.Website.Views.Lesson (displayLessonHome, displayLessonExercise)
import Control.Monad (msum, guard)
import Control.Monad.IO.Class (liftIO)
import System.Random (newStdGen, mkStdGen)
import Data.ByteString.Builder (toLazyByteString)
import qualified Server.Authentication.Main as Authentication
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BS
import qualified Network.HTTP.Types.URI as URI
import Happstack.Server

-- TODO: consider adding breadcrumbs (https://getbootstrap.com/docs/4.0/components/breadcrumb/)

-- * Handlers
handleRoot :: ServerConfiguration -> ServerResources -> ServerPart Response
handleRoot serverConfiguration serverResources = do
    userIdentityMaybe <- Authentication.readUserIdentityFromCookies serverConfiguration serverResources
    msum
        [ forceSlash $ handleHome serverConfiguration userIdentityMaybe
        , dir "courses" $ handleCourses serverConfiguration userIdentityMaybe
        , dir "decks" $ handleDecks serverConfiguration serverResources userIdentityMaybe
        , dir "resources" $ handleResources serverConfiguration userIdentityMaybe
        , dir "offline" $ handleOffline serverConfiguration userIdentityMaybe
        , anyPath $ handleNotFound serverConfiguration userIdentityMaybe
        ]

handleHome :: ServerConfiguration -> Maybe UserIdentity -> ServerPart Response
handleHome serverConfiguration userIdentityMaybe = ok . toResponse $ displayHome serverConfiguration userIdentityMaybe

handleCourses :: ServerConfiguration -> Maybe UserIdentity -> ServerPart Response
handleCourses serverConfiguration userIdentityMaybe = msum
    [ forceSlash . ok . toResponse $ displayCoursesHome serverConfiguration userIdentityMaybe
    , dir "introduction" $ handleCourse serverConfiguration userIdentityMaybe TopbarCourses Study.Courses.English.Grammar.Introduction.Course.course
    , dir "crash" $ handleCourse serverConfiguration userIdentityMaybe TopbarCourses Study.Courses.English.Grammar.Crash.Course.course
    , dir "attitudinals" $ handleCourse serverConfiguration userIdentityMaybe TopbarCourses Study.Courses.English.Vocabulary.Attitudinals.Course.course
    , dir "brivla" $ handleCourse serverConfiguration userIdentityMaybe TopbarCourses Study.Courses.English.Vocabulary.Brivla.Course.course
    , anyPath $ handleNotFound serverConfiguration userIdentityMaybe
    ]

handleDecks :: ServerConfiguration -> ServerResources -> Maybe UserIdentity -> ServerPart Response
handleDecks serverConfiguration serverResources userIdentityMaybe = msum
    [ forceSlash . ok . toResponse $ displayDecksHome serverConfiguration userIdentityMaybe
    , dir "contextualized-brivla" $ handleDeck serverConfiguration serverResources userIdentityMaybe Study.Decks.English.ContextualizedBrivla.deck
    , anyPath $ handleNotFound serverConfiguration userIdentityMaybe
    ]

handleResources :: ServerConfiguration -> Maybe UserIdentity -> ServerPart Response
handleResources serverConfiguration userIdentityMaybe = msum
    [ forceSlash . ok . toResponse $ displayResourcesHome serverConfiguration userIdentityMaybe
    , anyPath $ handleNotFound serverConfiguration userIdentityMaybe
    ]

handleOffline :: ServerConfiguration -> Maybe UserIdentity -> ServerPart Response
handleOffline serverConfiguration userIdentityMaybe = msum
    [ forceSlash . ok . toResponse $ displayOfflineHome serverConfiguration userIdentityMaybe
    , anyPath $ handleNotFound serverConfiguration userIdentityMaybe
    ]

handleNotFound :: ServerConfiguration -> Maybe UserIdentity -> ServerPart Response
handleNotFound serverConfiguration userIdentityMaybe = msum
    [ forceSlash . notFound . toResponse $ displayNotFoundHome serverConfiguration userIdentityMaybe
    ]

handleCourse :: ServerConfiguration -> Maybe UserIdentity -> TopbarCategory -> Course -> ServerPart Response
handleCourse serverConfiguration userIdentityMaybe topbarCategory course =
    let lessons = courseLessons course
    in msum
        [ forceSlash . ok . toResponse . displayCourseHome serverConfiguration userIdentityMaybe topbarCategory $ course
        , path $ \n -> (guard $ 1 <= n && n <= (length lessons)) >> (handleLesson serverConfiguration userIdentityMaybe topbarCategory course n)
        , anyPath $ handleNotFound serverConfiguration userIdentityMaybe
        ]

handleDeck :: ServerConfiguration -> ServerResources -> Maybe UserIdentity -> Deck -> ServerPart Response
handleDeck serverConfiguration serverResources userIdentityMaybe deck = msum
    [ forceSlash . ok . toResponse $ displayDeckHome serverConfiguration userIdentityMaybe deck
    , dir "exercises" $ do
        identityMaybe <- Authentication.readUserIdentityFromCookies serverConfiguration serverResources
        case identityMaybe of
            Nothing -> do
                --tempRedirect ("./" :: T.Text) . toResponse $ ("You must be signed in." :: T.Text)
                ok . toResponse $ includeInlineScript ("alert('To practice with decks, you need to sign in.'); window.location.href='./';" :: T.Text)
            Just identity -> ok . toResponse $ displayDeckExercise serverConfiguration userIdentityMaybe deck
    , anyPath $ handleNotFound serverConfiguration userIdentityMaybe
    ]

handleLesson :: ServerConfiguration -> Maybe UserIdentity -> TopbarCategory -> Course -> Int -> ServerPart Response
handleLesson serverConfiguration userIdentityMaybe topbarCategory course lessonNumber = msum
    [ forceSlash . ok . toResponse $ displayLessonHome serverConfiguration userIdentityMaybe topbarCategory course lessonNumber
    , dir "report" $ handleLessonReport course lessonNumber
    , dir "exercises" $ msum
        [ forceSlash . ok . toResponse $ displayLessonExercise serverConfiguration userIdentityMaybe topbarCategory course lessonNumber
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
    , anyPath $ handleNotFound serverConfiguration userIdentityMaybe
    ]

handleLessonReport :: Course -> Int -> ServerPart Response
handleLessonReport course lessonNumber =
    -- The only reason for using `tempRedirect` is that we may want to change the target url in the future
    forceSlash . tempRedirect url . toResponse $ ("To report an issue, please visit our GitHub repository." :: T.Text) where
        lesson :: Lesson
        lesson = (courseLessons course) !! (lessonNumber - 1)
        url :: T.Text
        url = "https://github.com/jqueiroz/lojban.io/issues/new" `T.append` queryString
        queryString :: T.Text
        queryString = TE.decodeUtf8 . BS.toStrict . toLazyByteString $ URI.renderQueryText True
            [ ("labels", Just "reported-lesson")
            , ("title", Just $ "Feedback regarding lesson: " `T.append` (lessonTitle lesson))
            , ("body", Just $ "Please provide your feedback here...\n\n### Context\nFor context, this feedback refers to the lesson \"" `T.append` (lessonTitle lesson) `T.append` "\" in the course \"" `T.append` (courseTitle course) `T.append` "\".")
            ]
