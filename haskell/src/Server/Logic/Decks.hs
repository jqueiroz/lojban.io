{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Logic.Decks
( retrieveDeckProficiency
, updateDeckProficiency
, computeCardProficiencyScore
, retrieveDeckPreferences
, updateDeckPreferencesByTogglingCard
) where

import Core
import Server.Core
import Server.Logic.Redis (encodeRedisKey)
import Data.Maybe (fromMaybe)
import Data.Either (fromRight)
import qualified Database.Redis as Redis
import qualified Data.Aeson as A
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString as BSS
import Decks.English.ContextualizedBrivla (deck)

-- * Logic
computeCardProficiencyScore :: CardProficiency -> Double
computeCardProficiencyScore cardProficiency = min 1 $ (fromIntegral $ length successfulAttempts) / (fromIntegral minimumSuccessfulAttemptsForPerfectScore) where
    attempts = take numberOfAttemptsTracked $ lastAttempts cardProficiency ++ repeat False
    successfulAttempts = filter (== True) attempts

-- * Configuration

numberOfAttemptsTracked :: Int
numberOfAttemptsTracked = 20

minimumSuccessfulAttemptsForPerfectScore :: Int
minimumSuccessfulAttemptsForPerfectScore = 16

-- * Redis bindings

deckPreferencesKey :: UserIdentifier -> Deck -> T.Text
deckPreferencesKey userIdentifier deck = "DeckPreferences" `T.append` deckKey userIdentifier deck

deckProficiencyKey :: UserIdentifier -> Deck -> T.Text
deckProficiencyKey userIdentifier deck = "DeckProficiency" `T.append` deckKey userIdentifier deck

deckKey :: UserIdentifier -> Deck -> T.Text
deckKey userIdentifier deck = encodeRedisKey
    [ ("provider", userIdentifierProvider userIdentifier)
    , ("email", userIdentifierEmail userIdentifier)
    , ("deck", deckId deck)
    ]

retrieveDeckPreferences :: UserIdentifier -> Deck -> Redis.Redis DeckPreferences
retrieveDeckPreferences userIdentifier deck = do
    let key = deckPreferencesKey userIdentifier deck
    let defaultDeckPreferences = DeckPreferences M.empty
    let defaultCardPreferences = CardPreferences False
    originalDeckPreferences :: DeckPreferences <- fromMaybe defaultDeckPreferences . A.decode . BS.fromStrict . fromMaybe BSS.empty . fromRight Nothing <$> Redis.get (TE.encodeUtf8 key)
    let originalCardPreferences = cardPreferences originalDeckPreferences
    let adjustedCardPreferences = M.fromList $ (flip map) (map cardTitle $ deckCards deck) $ \title -> (title, M.findWithDefault defaultCardPreferences title originalCardPreferences)
    let adjustedCardPreferences' = if any cardEnabled (M.elems adjustedCardPreferences)
        then adjustedCardPreferences
        else M.insert (cardTitle $ head $ deckCards deck) (CardPreferences $ True) adjustedCardPreferences
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

updateDeckProficiency :: UserIdentifier -> Deck -> T.Text -> Bool -> Redis.Redis (Either Redis.Reply Redis.Status)
updateDeckProficiency userIdentifier deck cardTitle success =
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

updateDeckPreferencesByTogglingCard :: UserIdentifier -> Deck -> T.Text -> Bool -> Redis.Redis (Either Redis.Reply Redis.Status)
updateDeckPreferencesByTogglingCard userIdentifier deck cardTitle cardNewState = do
        oldDeckPreferences <- retrieveDeckPreferences userIdentifier deck
        let oldCardPreferences = cardPreferences oldDeckPreferences
        let newCardPreferences = M.insert cardTitle (CardPreferences cardNewState) oldCardPreferences
        let newDeckPreferences = DeckPreferences newCardPreferences
        saveDeckPreferences userIdentifier deck newDeckPreferences
