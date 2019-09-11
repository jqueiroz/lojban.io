{-# LANGUAGE OverloadedStrings #-}

module Server.Api.V0.Main (handleRoot) where

import Core
import Server.Core
import Courses.CourseStore (courseStore)
import Decks.DeckStore (deckStore)
import Server.Logic.Redis (runRedis)
import Server.Logic.Decks (retrieveDeckPreferences, retrieveDeckProficiency)
import Control.Monad (msum)
import Server.Util (forceSlash)
import Server.Api.V0.Serializers (serializeCourse, serializeDeck)
import Happstack.Server
import Control.Monad.Trans (liftIO)
import qualified Server.OAuth2.Main as OAuth2
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Aeson as A

handleRoot :: ServerResources -> ServerPart Response
handleRoot serverResources = msum
    [ forceSlash $ ok . toResponse $ A.encode $ A.object [("success", A.Bool True)]
    , dir "course" $ path handleCourse
    , dir "deck" $ path (handleDeck serverResources)
    ]

handleCourse :: T.Text -> ServerPart Response
handleCourse courseId =
    let courseLookup = M.lookup courseId (courseStoreCourses courseStore)
    in case courseLookup of
        Nothing -> notFound . toResponse $ ("" :: T.Text)
        Just course -> ok . toResponse . A.encode $ serializeCourse course

handleDeck :: ServerResources -> T.Text -> ServerPart Response
handleDeck serverResources deckId =
    let deckLookup = M.lookup deckId (deckStoreDecks deckStore)
    in case deckLookup of
        Nothing -> notFound . toResponse $ ("" :: T.Text)
        Just deck -> do
            identityMaybe <- OAuth2.readUserIdentityFromCookies serverResources
            let identifierMaybe = userIdentifier <$> identityMaybe
            deckPreferencesMaybe <- case identifierMaybe of
                Nothing -> return Nothing
                Just identifier -> liftIO $ runRedis serverResources $ Just <$> retrieveDeckPreferences identifier deck
            deckProficiencyMaybe <- case identifierMaybe of
                Nothing -> return Nothing
                Just identifier -> liftIO $ runRedis serverResources $ Just <$> retrieveDeckProficiency identifier deck
            ok . toResponse . A.encode $ serializeDeck deck deckPreferencesMaybe deckProficiencyMaybe
