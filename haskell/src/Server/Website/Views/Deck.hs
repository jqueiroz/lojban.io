{-# LANGUAGE OverloadedStrings #-}

module Server.Website.Views.Deck
( displayDeckHome
, displayDeckExercise
) where

import Core
import Server.Core
import Server.Website.Views.Core
import qualified Data.Text as T
import qualified Text.Blaze as B
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- TODO: consider using list groups (https://getbootstrap.com/docs/4.0/components/list-group/)
displayDeckHome :: Maybe UserIdentity -> Deck -> H.Html
displayDeckHome userIdentityMaybe deck = do
    let baseDeckUrl = ""
    let title = deckTitle deck
    H.html $ do
        H.head $ do
            H.title $ H.toHtml ("lojban :: " `T.append` title)
            includeUniversalStylesheets
            includeUniversalScripts
            includeInternalStylesheet "deck.css"
            includeDeckScript deck
            includeInternalScript "deck.js"
        H.body $ do
            displayTopbar userIdentityMaybe TopbarDecks
            H.div B.! A.class_ (H.stringValue "main") $ do
                H.div B.! A.class_ (H.textValue "header") $ do
                    H.div B.! A.class_ (H.textValue "header-bg") $ H.toHtml ("" :: T.Text)
                    displayDeckHomeHeader baseDeckUrl deck
                H.div B.! A.class_ (H.textValue "body") $ do
                    -- TOOD: Section: "Manage your cards"
                    H.h2 $ H.toHtml ("Manage your cards" :: T.Text)
                    -- TOOD: Some hints as well (eg, "generally you do not need to disable cards that you already know. If we think you have mastered a card, we will display it less frequently so that you can focus on...")
                    H.div B.! A.class_ (H.stringValue "deck") $ H.toHtml ("" :: T.Text)
                    displayFooter

displayDeckExercise :: Maybe UserIdentity -> Deck -> H.Html
displayDeckExercise userIdentityMaybe deck = do
    let dictionary = deckDictionary deck
    let baseDeckUrl = "./"
    H.html $ do
        H.head $ do
            H.title (H.toHtml $ "lojban :: " `T.append` (deckTitle deck) `T.append` " :: Practice")
            includeUniversalStylesheets
            includeInternalStylesheet "funkyradio.css"
            includeInternalStylesheet "list-group-horizontal.css"
            includeInternalStylesheet "exercise.css"
            includeUniversalScripts
            includeDictionaryScript dictionary
            includeDeckScript deck
            includeInternalScript "exercise.js"
        H.body $ do
            displayTopbar userIdentityMaybe TopbarDecks
            H.div B.! A.class_ (H.stringValue "main") $ do
                H.div B.! A.class_ (H.textValue "body") $ do
                    displayDeckExerciseHeader baseDeckUrl deck
                    H.div B.! A.class_ (H.stringValue "deck") $ do
                        H.div B.! A.id (H.stringValue "exercise-holder") $ H.toHtml ("" :: T.Text)

displayDeckHomeHeader :: T.Text -> Deck -> H.Html
displayDeckHomeHeader baseDeckUrl deck = do
    let title = deckTitle deck
    let shortDescription = deckShortDescription deck
    H.div B.! A.class_ (H.stringValue "deck-header") $ do
        H.div B.! A.class_ (H.stringValue "deck-info") $ do
            H.h1 B.! A.class_ "deck-title" $ H.toHtml title
            H.h1 B.! A.class_ "deck-cards-count" $ H.toHtml (showNumberOfCards . length . deckCards $ deck)
            H.p B.! A.class_ "deck-description" $ H.toHtml shortDescription
        H.div B.! A.class_ "deck-buttons" $ do
            H.a B.! A.class_ (H.textValue "button") B.! A.href (H.textValue $ baseDeckUrl `T.append` "./exercises") $ (H.toHtml ("Practice" :: T.Text))

displayDeckExerciseHeader :: T.Text -> Deck -> H.Html
displayDeckExerciseHeader baseDeckUrl deck = do
    -- TODO: consider displaying: "x active cards out of y"
    let cardsCount = length (deckCards deck)
    H.div B.! A.class_ (H.stringValue "deck-header") $ do
        H.div B.! A.class_ (H.stringValue "deck-info") $ do
            H.h1 B.! A.class_ "deck-title" $
                H.a B.! A.href (H.textValue baseDeckUrl) $ H.toHtml (deckTitle deck)
        H.div B.! A.class_ "deck-buttons" $ do
            H.a B.! A.class_ (H.textValue "button") B.! A.href (H.textValue baseDeckUrl) $ (H.toHtml ("Review Deck" :: T.Text))

showNumberOfCards :: Int -> String
showNumberOfCards 0 = "No cards yet..."
showNumberOfCards 1 = "1 card"
showNumberOfCards x = (show x) ++ " cards"
