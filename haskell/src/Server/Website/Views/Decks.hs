{-# LANGUAGE OverloadedStrings #-}

module Server.Website.Views.Decks
( displayDecksHome
) where

import Core
import Server.Core
import Server.Website.Views.Core
import qualified Decks.English.ContextualizedBrivla
import qualified Data.Text as T
import qualified Text.Blaze as B
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

displayDecksHome :: Maybe UserIdentity -> H.Html
displayDecksHome userIdentityMaybe = do
    H.html $ do
        H.head $ do
            H.title $ H.toHtml ("lojban :: Decks" :: T.Text)
            includeUniversalStylesheets
            includeUniversalScripts
            includeInternalStylesheet "decks.css"
        H.body $ do
            displayTopbar userIdentityMaybe TopbarDecks
            H.div B.! A.class_ (H.textValue "main") $ do
                H.div B.! A.class_ (H.textValue "header") $ do
                    H.div B.! A.class_ (H.textValue "header-bg") $ H.toHtml ("" :: T.Text)
                    H.h1 $ H.toHtml ("Decks" :: T.Text)
                    H.p $ H.toHtml ("Learn vocabulary at your own pace, boosted by spaced repetition." :: T.Text)
                    H.p $ H.toHtml ("Start practicing with a deck that picks your interest, and enable new cards as you progress." :: T.Text)
                    --H.p $ H.toHtml ("You have full control over your learning experience." :: T.Text)
                H.div B.! A.class_ (H.textValue "body") $ do
                    H.div B.! A.class_ (H.textValue "decks") $ do
                        H.h2 $ H.toHtml ("Learn from decks" :: T.Text)
                        H.div B.! A.class_ (H.textValue "grid") $ do
                            displayDeck ("/decks/contextualized-brivla", Decks.English.ContextualizedBrivla.deck)
                            --displayDeck ("/decks/contextualized-brivla", Decks.English.ContextualizedBrivla.deck)
                            --displayDeck ("/decks/contextualized-brivla", Decks.English.ContextualizedBrivla.deck)
                            --displayDeck ("/decks/contextualized-brivla", Decks.English.ContextualizedBrivla.deck)
                            --displayDeck ("/decks/contextualized-brivla", Decks.English.ContextualizedBrivla.deck)
                            --displayDeck ("/decks/contextualized-brivla", Decks.English.ContextualizedBrivla.deck)
                    displayFooter

displayDeck :: (T.Text, Deck) -> H.Html
displayDeck (url, deck) = do
    let title = deckTitle deck
    let shortDescription = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor." :: T.Text
    let linkText = "Learn more" :: T.Text
    H.li B.! A.class_ (H.textValue "deck") $ do
        H.div B.! A.class_ (H.textValue "deck-title") $ H.toHtml title
        H.div B.! A.class_ (H.textValue "deck-description") $ H.toHtml shortDescription
        H.div B.! A.class_ (H.textValue "deck-link") $ do
            H.a B.! A.href (H.textValue url) $ H.toHtml linkText
