{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Logic.Decks
( computeCardProficiencyScore
, retrieveDeckProficiency
, updateDeckProficiencyByRegisteringExerciseAttempt
, retrieveDeckPreferences
, updateDeckPreferencesByTogglingCard
, retrieveDeckActiveCards
) where

import Core
import Server.Core
import Server.Logic.Redis (encodeRedisKey)
import Data.Maybe (fromMaybe)
import Data.Either (fromRight)
import Control.Exception (assert)
import qualified Database.Redis as Redis
import qualified Data.Aeson as A
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString as BSS

-- * Logic
computeCardProficiencyScore :: CardProficiency -> Double
computeCardProficiencyScore cardProficiency = min 1 $ (fromIntegral $ length successfulAttempts) / (fromIntegral minimumSuccessfulAttemptsForPerfectProficiencyScore) where
    attempts = take numberOfAttemptsUsedInProficiencyScoreCalculation $ lastAttempts cardProficiency ++ repeat False
    successfulAttempts = filter (== True) attempts

-- TODO: implement logic here
computeCardProficiencyWeight :: CardProficiency -> Int
computeCardProficiencyWeight cardProficiency = 1 + totalWeightOfFailedAttempts where
    baseWeightOfMostRecentAttempt = 20
    attempts = lastAttempts cardProficiency
    weightedAttempts = assert (baseWeightOfMostRecentAttempt >= numberOfAttemptsTracked) $ zip [baseWeightOfMostRecentAttempt,baseWeightOfMostRecentAttempt-1..] attempts
    weightedFailedAttempts = filter (not . snd) weightedAttempts
    totalWeightOfFailedAttempts = sum $ map (\x -> x*x*x) $ map fst weightedFailedAttempts

-- * Configuration

-- | Last X attempts will be saved into the database.
numberOfAttemptsTracked :: Int
numberOfAttemptsTracked = 20

-- | Last Y (Y <= X) attempts will be used to compute the user's proficiency score.
numberOfAttemptsUsedInProficiencyScoreCalculation :: Int
numberOfAttemptsUsedInProficiencyScoreCalculation = 10

minimumSuccessfulAttemptsForPerfectProficiencyScore :: Int
minimumSuccessfulAttemptsForPerfectProficiencyScore = 8

-- * Redis bindings

deckPreferencesKey :: UserIdentifier -> Deck -> T.Text
deckPreferencesKey userIdentifier deck = "DeckPreferences" `T.append` deckKey userIdentifier deck

deckProficiencyKey :: UserIdentifier -> Deck -> T.Text
deckProficiencyKey userIdentifier deck = "DeckProficiency" `T.append` deckKey userIdentifier deck

deckKey :: UserIdentifier -> Deck -> T.Text
deckKey userIdentifier deck = encodeRedisKey
    [ ("provider", userIdentifierProvider userIdentifier)
    , ("subject", userIdentifierSubject userIdentifier)
    , ("deck", deckId deck)
    ]

retrieveDeckPreferences :: UserIdentifier -> Deck -> Redis.Redis DeckPreferences
retrieveDeckPreferences userIdentifier deck = do
    let key = deckPreferencesKey userIdentifier deck
    let defaultDeckPreferences = DeckPreferences M.empty
    let defaultCardPreferences = CardPreferences CardNotStarted
    originalDeckPreferences :: DeckPreferences <- fromMaybe defaultDeckPreferences . A.decode . BS.fromStrict . fromMaybe BSS.empty . fromRight Nothing <$> Redis.get (TE.encodeUtf8 key)
    let originalCardPreferences = cardPreferences originalDeckPreferences
    let adjustedCardPreferences = M.fromList $ (flip map) (map cardTitle $ deckCards deck) $ \title -> (title, M.findWithDefault defaultCardPreferences title originalCardPreferences)
    let adjustedCardPreferences' = if any isCardEnabled (M.elems adjustedCardPreferences)
        then adjustedCardPreferences
        else M.insert (cardTitle $ head $ deckCards deck) (CardPreferences CardCurrentlyLearning) adjustedCardPreferences
    let adjustedDeckPreferences = DeckPreferences adjustedCardPreferences'
    return adjustedDeckPreferences

saveDeckPreferences :: UserIdentifier -> Deck -> DeckPreferences -> Redis.Redis (Either Redis.Reply Redis.Status)
saveDeckPreferences userIdentifier deck deckPreferences = do
    let key = deckPreferencesKey userIdentifier deck
    Redis.set (TE.encodeUtf8 key) . BS.toStrict . A.encode $ deckPreferences

retrieveDeckProficiency :: UserIdentifier -> Deck -> Redis.Redis DeckProficiency
retrieveDeckProficiency userIdentifier deck = do
    let key = deckProficiencyKey userIdentifier deck
    let defaultDeckProficiency = DeckProficiency M.empty
    let defaultCardProficiency = CardProficiency []
    originalDeckProficiency :: DeckProficiency <- fromMaybe defaultDeckProficiency . A.decode . BS.fromStrict . fromMaybe BSS.empty . fromRight Nothing <$> Redis.get (TE.encodeUtf8 key)
    let originalCardProficiencies = cardProficiencies originalDeckProficiency
    let adjustedCardProficiencies = M.fromList $ (flip map) (map cardTitle $ deckCards deck) $ \title -> (title, M.findWithDefault defaultCardProficiency title originalCardProficiencies)
    let adjustedDeckProficiency = DeckProficiency adjustedCardProficiencies
    return adjustedDeckProficiency

saveDeckProficiency :: UserIdentifier -> Deck -> DeckProficiency -> Redis.Redis (Either Redis.Reply Redis.Status)
saveDeckProficiency userIdentifier deck deckProficiency = do
    let key = deckProficiencyKey userIdentifier deck
    Redis.set (TE.encodeUtf8 key) . BS.toStrict . A.encode $ deckProficiency

updateDeckProficiencyByRegisteringExerciseAttempt :: UserIdentifier -> Deck -> T.Text -> Bool -> Redis.Redis (Either Redis.Reply Redis.Status)
updateDeckProficiencyByRegisteringExerciseAttempt userIdentifier deck cardTitle success =
    let
        updateCardProficiency :: CardProficiency -> CardProficiency
        updateCardProficiency oldCardProficiency = CardProficiency newLastAttempts where
            oldLastAttempts = lastAttempts oldCardProficiency
            newLastAttempts = take numberOfAttemptsTracked $ success : oldLastAttempts
    in do
        oldDeckProficiency <- retrieveDeckProficiency userIdentifier deck
        let oldCardProficiencies = cardProficiencies oldDeckProficiency
        let newCardProficiencies = M.adjust updateCardProficiency cardTitle oldCardProficiencies
        let newDeckProficiency = DeckProficiency newCardProficiencies
        saveDeckProficiency userIdentifier deck newDeckProficiency

updateDeckPreferencesByTogglingCard :: UserIdentifier -> Deck -> T.Text -> CardStatus -> Redis.Redis (Either Redis.Reply Redis.Status)
updateDeckPreferencesByTogglingCard userIdentifier deck cardTitle cardNewStatus = do
        oldDeckPreferences <- retrieveDeckPreferences userIdentifier deck
        let oldCardPreferences = cardPreferences oldDeckPreferences
        let newCardPreferences = M.insert cardTitle (CardPreferences cardNewStatus) oldCardPreferences
        let newDeckPreferences = DeckPreferences newCardPreferences
        saveDeckPreferences userIdentifier deck newDeckPreferences

retrieveDeckActiveCards :: UserIdentifier -> Deck -> Redis.Redis [(Card, Int, Double)]
retrieveDeckActiveCards userIdentifier deck = do
    -- Retrieve enabled cards
    preferencesMap <- cardPreferences <$> retrieveDeckPreferences userIdentifier deck
    let allCards = deckCards deck
    let enabledCards = (flip filter) allCards $ \card ->
            let
                title = cardTitle card
                preferences = M.lookup title preferencesMap
            in
                maybe False isCardEnabled preferences
    -- Retrieve weights for cards
    proficiencyMap <- cardProficiencies <$> retrieveDeckProficiency userIdentifier deck
    return $ (flip map) enabledCards $ \card ->
            let
                title = cardTitle card
                proficiency = M.lookup title proficiencyMap
                proficiencyWeight = maybe 0 computeCardProficiencyWeight proficiency
                proficiencyScore = maybe 0 computeCardProficiencyScore proficiency
            in
                (card, proficiencyWeight, proficiencyScore)

isCardEnabled :: CardPreferences -> Bool
isCardEnabled = (== CardCurrentlyLearning) . cardStatus
