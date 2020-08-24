module Server.Api.V0.Serializers
( serializeCourse
, serializeDeck
) where

import Core
import Server.Core
import Language.Lojban.Core
import Server.Logic.Decks (computeCardProficiencyScore)
import qualified Server.Api.V0.Contract as Contract
import qualified Data.Map as M

serializeCourse :: Course -> Contract.Course
serializeCourse course = Contract.Course title dictionaryId style where
    title = courseTitle course
    dictionaryId = dictIdentifier (courseDictionary course)
    style = serializeCourseStyle (courseStyle course)

serializeCourseStyle :: CourseStyle -> Contract.CourseStyle
serializeCourseStyle courseStyle = Contract.CourseStyle color1 iconUrl where
    color1 = courseStyleColor1 courseStyle
    iconUrl = courseStyleIconUrl courseStyle

serializeDeck :: Deck -> Maybe DeckPreferences -> Maybe DeckProficiency -> Contract.Deck
serializeDeck deck deckPreferences deckProficiency = Contract.Deck title dictionaryId cards serializedDeckPreferences serializedDeckProficiency where
    title = deckTitle deck
    dictionaryId = dictIdentifier (deckDictionary deck)
    cards = map serializeCard (deckCards deck)
    serializedDeckPreferences = serializeDeckPreferences <$> deckPreferences
    serializedDeckProficiency = serializeDeckProficiency <$> deckProficiency

serializeCard :: Card -> Contract.Card
serializeCard card = Contract.Card title shortDescription where
    title = cardTitle card
    shortDescription = cardShortDescription card

serializeDeckPreferences :: DeckPreferences -> Contract.DeckPreferences
serializeDeckPreferences deckPreferences = Contract.DeckPreferences serializedCardPreferences where
    serializedCardPreferences = M.map serializeCardPreferences $ cardPreferences deckPreferences

serializeCardPreferences :: CardPreferences -> Contract.CardPreferences
serializeCardPreferences cardPreferences = Contract.CardPreferences status where
    status = cardStatus cardPreferences

serializeDeckProficiency :: DeckProficiency -> Contract.DeckProficiency
serializeDeckProficiency deckProficiency = Contract.DeckProficiency serializedCardProficiency where
    serializedCardProficiency = M.map serializeCardProficiency $ cardProficiencies deckProficiency

serializeCardProficiency :: CardProficiency -> Contract.CardProficiency
serializeCardProficiency cardProficiency = Contract.CardProficiency score where
    score = computeCardProficiencyScore cardProficiency
