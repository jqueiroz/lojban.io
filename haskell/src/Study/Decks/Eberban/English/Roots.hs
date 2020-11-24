{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module defines the deck.
module Study.Decks.Eberban.English.Roots
( deck
) where

import Core
import Language.Eberban.Core
import Language.Eberban.Dictionaries (officialDictionary)
import Study.Framework.Eberban.ExerciseGenerators (generateLexiconProvidingExercise)
import Study.Framework.DocumentBuilders (buildDocumentFromMarkdownCode)
import Data.FileEmbed (embedStringFile)
import Util (generatorFromList)
import qualified Data.Map as M
import qualified Text.Pandoc as P
import qualified Language.Lojban.Dictionaries as LojbanDictionaries

-- | Deck description.
longDescription :: P.Pandoc
Right longDescription = buildDocumentFromMarkdownCode $(embedStringFile "resources/decks/eberban/english/roots/description.md")

-- | Deck: Contextualized Brivla.
deck :: Deck
deck = Deck id title shortDescription (Just longDescription) credits LojbanDictionaries.englishDictionary cards where
    -- TODO: remove reference to LojbanDictionaries.englishDictionary
    id = "eberban-eng_roots"
    title = "Eberban roots"
    shortDescription = "Learn the officially registered Eberban roots."
    credits = Nothing

-- | Cards for the deck
cards :: [Card]
cards = map buildCard rootList where
    entryList :: [Entry]
    entryList = map snd $ M.toList (dictEntries officialDictionary)
    rootList :: [Entry]
    rootList = filter ((== "R") . entryFamily) entryList
    buildCard :: Entry -> Card
    buildCard root = Card (entryText root) (entryEnglishLong root) (buildRootExerciseGenerator root)

-- * Auxiliar functions
buildRootExerciseGenerator :: Entry -> ExerciseGenerator
buildRootExerciseGenerator root = generateLexiconProvidingExercise "root" $ generatorFromList [root]
