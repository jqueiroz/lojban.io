{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module defines the deck.
module Decks.English.ContextualizedBrivla
( deck
) where

import Core
import Language.Lojban.Core
import Language.Lojban.Dictionaries (englishDictionary)
import Study.Framework.ExerciseGenerators (generateFillingBlanksExerciseByExpression, generateIsolatedBrivlaPlacesExercise, generateLexiconProvidingExercise)
import Courses.English.Vocabulary.Brivla.Translations (translationsByExpression)
import Study.Framework.DocumentBuilders (buildDocumentFromMarkdownCode)
import Data.FileEmbed (embedStringFile)
import Util (combineGenerators, generatorFromList)
import Control.Arrow (second)
import qualified Data.Text as T
import qualified Text.Pandoc as P

-- | Deck description.
longDescription :: P.Pandoc
Right longDescription = buildDocumentFromMarkdownCode $(embedStringFile "resources/decks/english/brivla/description.md")

-- | Deck credits.
credits :: P.Pandoc
Right credits = buildDocumentFromMarkdownCode $(embedStringFile "resources/decks/english/brivla/credits.md")

-- | Deck: Contextualized Brivla.
deck :: Deck
deck = Deck id title shortDescription (Just longDescription) (Just credits) dictionary cards where
    id = "eng_contextualized-brivla"
    title = "Contextualized brivla"
    shortDescription = "Learn the most commonly used brivla, while also developing your comprehension skills."

-- | Dictionary for the deck.
dictionary :: Dictionary
dictionary = englishDictionary

-- | Cards for the deck
cards :: [Card]
cards = map buildCard brivlaList where
    buildCard brivla = Card brivla (dictLookupValsiDefinition dictionary brivla) (buildBrivlaExerciseGenerator brivla)

-- | List of brivla in the deck.
brivlaList :: [T.Text]
brivlaList = map fst translationsByExpression

-- * Auxiliar functions
buildBrivlaExerciseGenerator :: T.Text -> ExerciseGenerator
buildBrivlaExerciseGenerator brivla = combineGenerators chosenGenerators where
    translationExercises = generateFillingBlanksExerciseByExpression $ filter ((== brivla) . fst) translationGeneratorByExpression
    translationGeneratorByExpression = map (second generatorFromList) translationsByExpression
    brivlaPlacesExercises = generateIsolatedBrivlaPlacesExercise dictionary [brivla]
    brivlaProvidingExercises = generateLexiconProvidingExercise "brivla" dictionary $ generatorFromList [brivla]
    brivlaHasAtLeastTwoPlaces = (length $ retrieveBrivlaPlaces dictionary brivla) >= 2
    chosenGenerators =
        if brivlaHasAtLeastTwoPlaces then
            [(12, translationExercises), (2, brivlaPlacesExercises), (3, brivlaProvidingExercises)]
        else
            [(12, translationExercises), (4, brivlaProvidingExercises)]
