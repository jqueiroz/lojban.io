-- | This modules exposes the 'DeckStore'.
module Decks.DeckStore (deckStore) where

import Core
import qualified Decks.English.ContextualizedBrivla as ContextualizedBrivla
import qualified Data.Map as M

-- | Deck store.
deckStore :: DeckStore
deckStore = DeckStore decksMap where
    decksMap = M.fromList $ map (\deck -> (deckId deck, deck)) decks
    decks = [ContextualizedBrivla.deck]
