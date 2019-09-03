{-# LANGUAGE OverloadedStrings #-}

module Server.Website.Views.Home
( displayHome
) where

import Core
import Server.Core
import Server.Website.Views.Core
import qualified Data.Text as T
import qualified Text.Blaze as B
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Courses.English.Grammar.Introduction.Course
import qualified Courses.English.Grammar.Crash.Course
import qualified Courses.English.Vocabulary.Attitudinals.Course
import qualified Courses.English.Vocabulary.Brivla.Course
import qualified Decks.English.ContextualizedBrivla

displayHome :: Maybe UserIdentity -> H.Html
displayHome userIdentityMaybe =
    H.html $ do
        H.head $ do
            H.title $ H.toHtml ("Home" :: String)
            includeUniversalStylesheets
            includeUniversalScripts
            includeInternalStylesheet "home.css"
        H.body $ do
            displayTopbar userIdentityMaybe TopbarHome
            H.div B.! A.class_ (H.textValue "main") $ do
                H.div B.! A.class_ (H.textValue "header") $ do
                    H.h1 $ do
                        H.span B.! A.class_ (H.textValue "learn") $ H.toHtml ("Learn " :: T.Text)
                        H.span B.! A.class_ (H.textValue "logically") $ H.toHtml ("logically" :: T.Text)
                    H.p $ H.toHtml ("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor. Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor." :: String)
                    H.div B.! A.class_ (H.textValue "buttons") $ do
                        H.a (H.toHtml ("Courses" :: T.Text))
                            B.! A.href (H.textValue "/courses")
                        H.a (H.toHtml ("Decks" :: T.Text))
                            B.! A.href (H.textValue "/decks")
                H.div B.! A.class_ (H.textValue "courses") $ do
                    H.h2 $ H.toHtml ("Courses" :: T.Text)
                    H.div B.! A.class_ (H.stringValue "course-list") $ do
                        displayCourse ("/grammar/introduction", Courses.English.Grammar.Introduction.Course.course)
                        displayCourse ("/grammar/crash", Courses.English.Grammar.Crash.Course.course)
                        displayCourse ("/vocabulary/attitudinals", Courses.English.Vocabulary.Attitudinals.Course.course)
                        displayCourse ("/vocabulary/brivla", Courses.English.Vocabulary.Brivla.Course.course)
                H.div B.! A.class_ (H.textValue "decks") $ do
                    H.h2 $ H.toHtml ("Decks" :: T.Text)
                    H.div B.! A.class_ (H.stringValue "deck-list") $ do
                        displayDeck ("/decks/contextualized-brivla", Decks.English.ContextualizedBrivla.deck)
                        displayDeck ("/decks/contextualized-brivla", Decks.English.ContextualizedBrivla.deck)
                        displayDeck ("/decks/contextualized-brivla", Decks.English.ContextualizedBrivla.deck)
                        displayDeck ("/decks/contextualized-brivla", Decks.English.ContextualizedBrivla.deck)
                displayFooter

displayCourse :: (T.Text, Course) -> H.Html
displayCourse (url, course) = do
    let title = courseTitle course
    let shortDescription = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor." :: T.Text
    let linkText = "Learn more" :: T.Text
    H.div B.! A.class_ (H.textValue "course") $ do
        H.div B.! A.class_ (H.textValue "course-title") $ H.toHtml title
        H.div B.! A.class_ (H.textValue "course-description") $ H.toHtml shortDescription
        H.div B.! A.class_ (H.textValue "course-link") $ do
            H.a B.! A.href (H.textValue url) $ H.toHtml linkText

displayDeck :: (T.Text, Deck) -> H.Html
displayDeck (url, course) = do
    let title = deckTitle course
    let shortDescription = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor." :: T.Text
    let linkText = "Learn more" :: T.Text
    H.div B.! A.class_ (H.textValue "deck") $ do
        H.div B.! A.class_ (H.textValue "deck-title") $ H.toHtml title
        H.div B.! A.class_ (H.textValue "deck-description") $ H.toHtml shortDescription
        H.div B.! A.class_ (H.textValue "deck-link") $ do
            H.a B.! A.href (H.textValue url) $ H.toHtml linkText
