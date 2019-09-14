{-# LANGUAGE OverloadedStrings #-}

module Server.Api.V0.Main (handleRoot) where

import Core
import Server.Core
import Courses.CourseStore (courseStore)
import Decks.DeckStore (deckStore)
import Server.Logic.Redis (runRedis)
import Server.Logic.Decks (retrieveDeckPreferences, retrieveDeckProficiency, updateDeckPreferencesByTogglingCard)
import Control.Monad (msum)
import Server.Util (forceSlash)
import Server.Api.V0.Serializers (serializeCourse, serializeDeck)
import Happstack.Server
import Control.Monad.Trans (liftIO)
import qualified Server.OAuth2.Main as OAuth2
import qualified Database.Redis as Redis
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
        Just deck -> msum
            [ forceSlash $ handleDeckRetrieve serverResources deck
            , dir "setCardStatus" $ path (handleDeckSetCardStatus serverResources deck)
            ]

handleDeckRetrieve :: ServerResources -> Deck -> ServerPart Response
handleDeckRetrieve serverResources deck = do
    identityMaybe <- OAuth2.readUserIdentityFromCookies serverResources
    let identifierMaybe = userIdentifier <$> identityMaybe
    deckPreferencesMaybe <- case identifierMaybe of
        Nothing -> return Nothing
        Just identifier -> liftIO $ runRedis serverResources $ Just <$> retrieveDeckPreferences identifier deck
    deckProficiencyMaybe <- case identifierMaybe of
        Nothing -> return Nothing
        Just identifier -> liftIO $ runRedis serverResources $ Just <$> retrieveDeckProficiency identifier deck
    ok . toResponse . A.encode $ serializeDeck deck deckPreferencesMaybe deckProficiencyMaybe

handleDeckSetCardStatus :: ServerResources -> Deck -> T.Text -> ServerPart Response
handleDeckSetCardStatus serverResources deck cardTitle = msum
    [ dir "enabled" $ handleDeckSetCardStatus' serverResources deck cardTitle True
    , dir "disabled" $ handleDeckSetCardStatus' serverResources deck cardTitle False
    ]

handleDeckSetCardStatus' :: ServerResources -> Deck -> T.Text -> Bool -> ServerPart Response
handleDeckSetCardStatus' serverResources deck cardTitle cardNewStatus = do
    identityMaybe <- OAuth2.readUserIdentityFromCookies serverResources
    case identityMaybe of
        Nothing -> unauthorized . toResponse . A.encode $ ("You must be signed in." :: T.Text)
        Just identity -> do
            redisResponse <- liftIO $ runRedis serverResources $ updateDeckPreferencesByTogglingCard (userIdentifier identity) deck cardTitle cardNewStatus
            case redisResponse of
                Right Redis.Ok -> ok . toResponse $ ("Successfully updated card status." :: T.Text)
                _ -> internalServerError . toResponse $ ("Failed to update card status." :: T.Text)
