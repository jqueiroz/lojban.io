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
--import qualified Courses.English.Vocabulary.Brivla.Course
import qualified Decks.English.ContextualizedBrivla

-- TODO: link to official lojban wiki
-- TODO: take more info from this brochure: https://mw.lojban.org/images/c/c5/lojbanbrochure.pdf
-- TODO: also link to https://mw.lojban.org/papri/Presenting_Lojban
displayHome :: Maybe UserIdentity -> H.Html
displayHome userIdentityMaybe = do
    let shortDescription = "A free and open-source platform for studying the artificial language Lojban."
    H.html B.! A.lang (H.stringValue "en-us") $ do
        H.head $ do
            H.title $ H.toHtml ("lojban.io" :: T.Text)
            H.meta B.! A.name (H.textValue "description") B.! A.content (H.textValue shortDescription)
            includeUniversalStylesheets
            includeUniversalScripts
            includeInternalStylesheet "home.css"
            includeInternalScript "home.js"
        H.body $ do
            displayTopbar userIdentityMaybe TopbarHome
            H.div B.! A.class_ (H.textValue "main") $ do
                H.div B.! A.class_ (H.textValue "body") $ do
                    H.div B.! A.class_ (H.textValue "welcome") $ do
                        displayWelcome2
                    H.div B.! A.class_ (H.textValue "courses") $ do
                        H.h2 $ H.toHtml ("Courses" :: T.Text)
                        H.div B.! A.class_ (H.textValue "carousel") $ do
                            H.div B.! A.class_ (H.textValue "previous") $ do
                                H.span B.! A.class_ (H.textValue "material-icons") $ H.toHtml ("navigate_before" :: T.Text)
                            H.ul $ do
                                displayCourse ("/courses/introduction", Courses.English.Grammar.Introduction.Course.course)
                                --displayCourse ("/courses/crash", Courses.English.Grammar.Crash.Course.course)
                                --displayCourse ("/courses/attitudinals", Courses.English.Vocabulary.Attitudinals.Course.course)
                                --displayCourse ("/courses/brivla", Courses.English.Vocabulary.Brivla.Course.course)
                            H.div B.! A.class_ (H.textValue "next") $ do
                                H.span B.! A.class_ (H.textValue "material-icons") $ H.toHtml ("navigate_next" :: T.Text)
                    H.div B.! A.class_ (H.textValue "decks") $ do
                        H.h2 $ H.toHtml ("Decks" :: T.Text)
                        H.div B.! A.class_ (H.textValue "carousel") $ do
                            H.div B.! A.class_ (H.textValue "previous") $ do
                                H.span B.! A.class_ (H.textValue "material-icons") $ H.toHtml ("navigate_before" :: T.Text)
                            H.ul $ do
                                displayDeck ("/decks/contextualized-brivla", Decks.English.ContextualizedBrivla.deck)
                                --displayDeck ("/decks/contextualized-brivla", Decks.English.ContextualizedBrivla.deck)
                                --displayDeck ("/decks/contextualized-brivla", Decks.English.ContextualizedBrivla.deck)
                                --displayDeck ("/decks/contextualized-brivla", Decks.English.ContextualizedBrivla.deck)
                            H.div B.! A.class_ (H.textValue "next") $ do
                                H.span B.! A.class_ (H.textValue "material-icons") $ H.toHtml ("navigate_next" :: T.Text)
                    displayFooter

displayWelcome1 :: H.Html
displayWelcome1 = do
    displayWhat
    displayWhy
    displayLearn

displayWelcome2 :: H.Html
displayWelcome2 = do
    displaySpeak
    displayWhy

-- TODO: url to official lojban website (with "external website" icon)
displaySpeak :: H.Html
displaySpeak = do
    H.div B.! A.class_ (H.textValue "speak-lojban") $ do
        H.h1 $ do
            H.span B.! A.class_ (H.textValue "speak") $ H.toHtml ("Speak " :: T.Text)
            H.span B.! A.class_ (H.textValue "logically") $ H.toHtml ("logically" :: T.Text)
        H.p $ H.toHtml ("Lojban is a carefully constructed spoken language. It has been built for over 50 years by dozens of workers and hundreds of supporters." :: T.Text)
        H.p $ H.toHtml ("Lojban's grammar is based on simple rules, and its linguistic features are inspired by predicate logic." :: T.Text)
        H.div B.! A.class_ (H.textValue "buttons") $ do
            H.a (H.toHtml ("Courses" :: T.Text))
                B.! A.href (H.textValue "/courses")
            H.a (H.toHtml ("Decks" :: T.Text))
                B.! A.href (H.textValue "/decks")

displayWhat :: H.Html
displayWhat = do
    H.div B.! A.class_ (H.textValue "what-lojban") $ do
        H.h1 $ H.toHtml ("What is Lojban?" :: T.Text)
        H.p $ H.toHtml ("Lojban is a carefully constructed spoken language. It has been built for over 50 years by dozens of workers and hundreds of supporters." :: T.Text)
        H.p $ H.toHtml ("Lojban's grammar is based on simple rules, and its linguistic features are inspired by predicate logic." :: T.Text)

displayWhy :: H.Html
displayWhy = do
    H.div B.! A.class_ (H.textValue "why-lojban") $ do
        H.h1 $ H.toHtml ("Why lojban?" :: T.Text)
        H.p $ H.toHtml ("Lojban means different things to different people:" :: T.Text)
        H.ul $ do
            H.li $ H.toHtml ("a new perspective on languages;" :: T.Text)
            H.li $ H.toHtml ("a challenging way to expand their minds or discipline their thoughts;" :: T.Text)
            H.li $ H.toHtml ("an entertaining medium to communicate with friends or create art;" :: T.Text)
            H.li $ H.toHtml ("a linguistic curiosity – a test-bed for language experimentation;" :: T.Text)
            H.li $ H.toHtml ("a domain for exploring the intersection of human language and software." :: T.Text)
        --H.p $ H.toHtml ("What will Lojban mean for you?" :: T.Text)
        H.a (H.toHtml ("Find out more" :: T.Text))
            B.! A.href (H.textValue "https://mw.lojban.org/papri/Lojban")
            B.! A.class_ (H.textValue "external")

displayLearn :: H.Html
displayLearn = do
    H.div B.! A.class_ (H.textValue "learn-logically") $ do
        H.h1 $ do
            H.span B.! A.class_ (H.textValue "learn") $ H.toHtml ("Learn " :: T.Text)
            H.span B.! A.class_ (H.textValue "logically") $ H.toHtml ("logically" :: T.Text)
        H.p $ H.toHtml ("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor. Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor." :: String)
        H.div B.! A.class_ (H.textValue "buttons") $ do
            H.a (H.toHtml ("Courses" :: T.Text))
                B.! A.href (H.textValue "/courses")
            H.a (H.toHtml ("Decks" :: T.Text))
                B.! A.href (H.textValue "/decks")


displayCourse :: (T.Text, Course) -> H.Html
displayCourse (url, course) = do
    let title = courseTitle course
    let shortDescription = courseShortDescription course
    let linkText = "Learn more" :: T.Text
    H.li B.! A.class_ (H.textValue "course") $ do
        H.div B.! A.class_ (H.textValue "course-title") $ H.toHtml title
        H.div B.! A.class_ (H.textValue "course-description") $ H.toHtml shortDescription
        H.div B.! A.class_ (H.textValue "course-link") $ do
            H.a B.! A.href (H.textValue url) $ H.toHtml linkText

displayDeck :: (T.Text, Deck) -> H.Html
displayDeck (url, course) = do
    let title = deckTitle course
    let shortDescription = deckShortDescription course
    let linkText = "Learn more" :: T.Text
    H.li B.! A.class_ (H.textValue "deck") $ do
        H.div B.! A.class_ (H.textValue "deck-title") $ H.toHtml title
        H.div B.! A.class_ (H.textValue "deck-description") $ H.toHtml shortDescription
        H.div B.! A.class_ (H.textValue "deck-link") $ do
            H.a B.! A.href (H.textValue url) $ H.toHtml linkText
