{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Api.V0.Main (handleRoot) where

import Core
import Server.Core
import Study.Courses.CourseStore (courseStore)
import Study.Decks.DeckStore (deckStore)
import Server.Logic.Redis (runRedis)
import Server.Logic.Decks (retrieveDeckPreferences, retrieveDeckProficiency, retrieveDeckActiveCards, updateDeckPreferencesByTogglingCard, updateDeckProficiencyByRegisteringExerciseAttempt)
import Control.Monad (msum)
import Server.Util (forceSlash, getBody)
import Server.Api.V0.Serializers (serializeCourse, serializeDeck)
import Happstack.Server
import Control.Monad.Trans (liftIO)
import System.Random (StdGen, mkStdGen, newStdGen)
import Serializer (personalizedExerciseToJSON, validateExerciseAnswer)
import Util (chooseItem, combineGenerators)
import qualified Server.Authentication.Main as Authentication
import qualified Database.Redis as Redis
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson as A

handleRoot :: ServerConfiguration -> ServerResources -> ServerPart Response
handleRoot serverConfiguration serverResources = msum
    [ forceSlash $ ok . toResponse $ A.encode $ A.object [("success", A.Bool True)]
    , dir "course" $ path handleCourse
    , dir "deck" $ path (handleDeck serverConfiguration serverResources)
    ]

handleCourse :: T.Text -> ServerPart Response
handleCourse courseId =
    let courseLookup = M.lookup courseId (courseStoreCourses courseStore)
    in case courseLookup of
        Nothing -> notFound . toResponse $ ("" :: T.Text)
        Just course -> ok . toResponse . A.encode $ serializeCourse course

handleDeck :: ServerConfiguration -> ServerResources -> T.Text -> ServerPart Response
handleDeck serverConfiguration serverResources deckId =
    let deckLookup = M.lookup deckId (deckStoreDecks deckStore)
    in case deckLookup of
        Nothing -> notFound . toResponse $ ("" :: T.Text)
        Just deck -> msum
            [ forceSlash $ handleDeckRetrieve serverConfiguration serverResources deck
            , dir "setCardStatus" $ path (handleDeckSetCardStatus serverConfiguration serverResources deck)
            , dir "exercises" $ path $ handleDeckExercises serverConfiguration serverResources deck
            ]

handleDeckRetrieve :: ServerConfiguration -> ServerResources -> Deck -> ServerPart Response
handleDeckRetrieve serverConfiguration serverResources deck = do
    identityMaybe <- Authentication.readUserIdentityFromCookies serverConfiguration serverResources
    let identifierMaybe = userIdentifier <$> identityMaybe
    deckPreferencesMaybe <- case identifierMaybe of
        Nothing -> return Nothing
        Just identifier -> liftIO $ runRedis serverConfiguration serverResources $ Just <$> retrieveDeckPreferences identifier deck
    deckProficiencyMaybe <- case identifierMaybe of
        Nothing -> return Nothing
        Just identifier -> liftIO $ runRedis serverConfiguration serverResources $ Just <$> retrieveDeckProficiency identifier deck
    ok . toResponse . A.encode $ serializeDeck deck deckPreferencesMaybe deckProficiencyMaybe

handleDeckSetCardStatus :: ServerConfiguration -> ServerResources -> Deck -> T.Text -> ServerPart Response
handleDeckSetCardStatus serverConfiguration serverResources deck cardTitle = msum
    [ dir "AlreadyMastered" $ handleDeckSetCardStatus' serverConfiguration serverResources deck cardTitle CardAlreadyMastered
    , dir "CurrentlyLearning" $ handleDeckSetCardStatus' serverConfiguration serverResources deck cardTitle CardCurrentlyLearning
    , dir "NotStarted" $ handleDeckSetCardStatus' serverConfiguration serverResources deck cardTitle CardNotStarted
    ]

handleDeckSetCardStatus' :: ServerConfiguration -> ServerResources -> Deck -> T.Text -> CardStatus -> ServerPart Response
handleDeckSetCardStatus' serverConfiguration serverResources deck cardTitle cardNewStatus = do
    identityMaybe <- Authentication.readUserIdentityFromCookies serverConfiguration serverResources
    case identityMaybe of
        Nothing -> unauthorized . toResponse . A.encode $ ("You must be signed in." :: T.Text)
        Just identity -> do
            redisResponse <- liftIO $ runRedis serverConfiguration serverResources $ updateDeckPreferencesByTogglingCard (userIdentifier identity) deck cardTitle cardNewStatus
            case redisResponse of
                Right Redis.Ok -> ok . toResponse $ ("Successfully updated card status." :: T.Text)
                _ -> internalServerError . toResponse $ ("Failed to update card status." :: T.Text)

handleDeckExercises :: ServerConfiguration -> ServerResources -> Deck -> Int -> ServerPart Response
handleDeckExercises serverConfiguration serverResources deck exerciseId = do
    identityMaybe <- Authentication.readUserIdentityFromCookies serverConfiguration serverResources
    case identityMaybe of
        Nothing -> unauthorized . toResponse . A.encode $ ("You must be signed in." :: T.Text)
        Just identity -> do
            cardsWithUserFeatures <- liftIO $ runRedis serverConfiguration serverResources $ retrieveDeckActiveCards (userIdentifier identity) deck
            let r0 = mkStdGen exerciseId
            let (selectedCardWithUserFeatures, r1) = selectCardWithBiasTowardsLowScoreOnes cardsWithUserFeatures r0
            let selectedCard = card selectedCardWithUserFeatures
            let selectedExercise = (cardExercises selectedCard) r1
            let personalizedExercise = PersonalizedExercise selectedExercise (cardShouldDisplayHint selectedCardWithUserFeatures)
            msum
                [ dir "get" $ (liftIO $ newStdGen) >>= ok . toResponse . A.encode . personalizedExerciseToJSON personalizedExercise
                , dir "submit" $ getBody >>= \body -> do
                    let errorResponse = ok . toResponse . A.encode . A.object $ [("success", A.Bool False)]
                    case validateExerciseAnswer selectedExercise body of
                        Nothing -> errorResponse
                        Just responseData -> do
                            let extractCorrect :: A.Value -> Maybe Bool = \case
                                    A.Object responseObject ->
                                        case HM.lookup "correct" responseObject of
                                            Just (A.Bool correct) -> Just correct
                                            _ -> Nothing
                                    _ -> Nothing
                            case extractCorrect responseData of
                                Nothing -> errorResponse
                                Just isCorrect -> do
                                    redisStatus <- liftIO $ runRedis serverConfiguration serverResources $ updateDeckProficiencyByRegisteringExerciseAttempt (userIdentifier identity) deck (cardTitle selectedCard) isCorrect
                                    ok . toResponse . A.encode . A.object $ [("success", A.Bool True), ("data", responseData)]
                ]

selectCardWithBiasTowardsLowScoreOnes :: [CardWithUserFeatures] -> StdGen -> (CardWithUserFeatures, StdGen)
selectCardWithBiasTowardsLowScoreOnes cards = combineGenerators [(probabilityOfSelectingLowScoreCards, selectLowScoreCard), (100-probabilityOfSelectingLowScoreCards, selectGeneralCard)] where
    -- | Low-score cards.
    lowScoreCards = filter hasLowScore cards
    -- | Probability of selecting low-score cards.
    probabilityOfSelectingLowScoreCards = decideProbabilityOfSelectingLowScoreCardsGivenTheirCount (length lowScoreCards)
    -- | Decides the probability of selecting low-score cards.
    decideProbabilityOfSelectingLowScoreCardsGivenTheirCount :: Int -> Int
    decideProbabilityOfSelectingLowScoreCardsGivenTheirCount 0 = 0
    decideProbabilityOfSelectingLowScoreCardsGivenTheirCount 1 = 10
    decideProbabilityOfSelectingLowScoreCardsGivenTheirCount 2 = 15
    decideProbabilityOfSelectingLowScoreCardsGivenTheirCount 3 = 20
    decideProbabilityOfSelectingLowScoreCardsGivenTheirCount 4 = 25
    decideProbabilityOfSelectingLowScoreCardsGivenTheirCount 5 = 30
    decideProbabilityOfSelectingLowScoreCardsGivenTheirCount 6 = 35
    decideProbabilityOfSelectingLowScoreCardsGivenTheirCount 7 = 40
    decideProbabilityOfSelectingLowScoreCardsGivenTheirCount 8 = 45
    decideProbabilityOfSelectingLowScoreCardsGivenTheirCount _ = 50
    -- | Decides whether the given card has low score.
    hasLowScore :: CardWithUserFeatures -> Bool
    hasLowScore cardWithUserFeatures = (cardProficiencyScore cardWithUserFeatures) <= 0.4
    -- | Randomly selects a card among those with low score.
    selectLowScoreCard = selectCard lowScoreCards
    -- | Randomly selects a card among all of them.
    selectGeneralCard = selectCard cards

selectCard :: [CardWithUserFeatures] -> StdGen -> (CardWithUserFeatures, StdGen)
selectCard cardsWithUserFeatures r0 = chooseItem r0 $ map (\cardWithUserFeatures -> (cardProficiencyWeight cardWithUserFeatures, cardWithUserFeatures)) cardsWithUserFeatures
