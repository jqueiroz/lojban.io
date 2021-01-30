{-# LANGUAGE OverloadedStrings #-}

module Server.Website.Views.Resources
( displayResourcesHome
) where

import Server.Core
import Server.Website.Views.Core
import qualified Data.Text as T
import qualified Text.Blaze as B
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

displayResourcesHome :: ServerConfiguration -> Maybe UserIdentity -> H.Html
displayResourcesHome serverConfiguration userIdentityMaybe = do
    let shortDescription = ("A collection of useful resources for studying Lojban." :: T.Text)
    H.docType
    H.html B.! A.lang (H.stringValue "en-us") $ do
        H.head $ do
            H.title $ H.toHtml ("Resources :: lojban.io" :: T.Text)
            H.meta B.! A.name (H.textValue "description") B.! A.content (H.textValue shortDescription)
            includeUniversalStylesheets
            includeUniversalScripts
            includeInternalStylesheet "resources.css"
        H.body $ do
            displayTopbar serverConfiguration userIdentityMaybe TopbarResources
            H.div B.! A.class_ (H.stringValue "main") $ do
                H.div B.! A.class_ (H.textValue "header") $ do
                    H.div B.! A.class_ (H.textValue "header-bg") $ H.toHtml ("" :: T.Text)
                    H.h1 $ H.toHtml ("Resources" :: T.Text)
                    H.p $ H.toHtml shortDescription
                H.div B.! A.class_ (H.textValue "body") $ do
                    displayResources
                    displayFooter

displayResources :: H.Html
displayResources = do
    H.div B.! A.class_ (H.textValue "resource-category") $ do
        H.h2 $ H.toHtml ("Dictionaries" :: T.Text)
        H.ul $ do
            H.li $ do
                H.a "la sutysisku"
                  B.! A.href "https://la-lojban.github.io/sutysisku/en"
            H.li $ do
                H.a "la vlasisku"
                  B.! A.href "http://vlasisku.lojban.org/"
            H.li $ do
                H.a "la jbovlaste"
                  B.! A.href "http://jbovlaste.lojban.org/"
    H.div B.! A.class_ (H.textValue "resource-category") $ do
        H.h2 $ H.toHtml ("Parsers" :: T.Text)
        H.ul $ do
            H.li $ do
                H.a "la ilmentufa"
                    B.! A.href "https://lojban.github.io/ilmentufa/glosser/glosser.htm"
            H.li $ do
                H.a "visual camxes"
                    B.! A.href "http://camxes.lojban.org/"
    H.div B.! A.class_ (H.textValue "resource-category") $ do
        H.h2 $ H.toHtml ("Courses" :: T.Text)
        H.ul $ do
            H.li $ do
                H.a "The Complete Lojban Language: official version (CLL)"
                    B.! A.href "https://lojban.github.io/cll/"
            H.li $ do
                H.a "The Complete Lojban Language: unofficial version (UnCLL)"
                    B.! A.href "http://lojban.pw/articles/complete-lojban-language/"
            H.li $ do
                H.a "la karda"
                    B.! A.href "https://mw.lojban.org/papri/la_karda"
            H.li $ do
                H.a "The Wave Lessons"
                    B.! A.href "https://mw.lojban.org/papri/Lojban_Wave_Lessons"
            H.li $ do
                H.a "The Crash Course"
                    B.! A.href "https://mw.lojban.org/papri/The_Crash_Course_(a_draft)"
            H.li $ do
                H.a "Lojban for Beginners"
                    B.! A.href "https://mw.lojban.org/papri/Lojban_For_Beginners,_The_Book"
            H.li $ do
                H.a "My First Lojban"
                    B.! A.href "http://foldr.moe/hajiloji/"
            H.li $ do
                H.a "la stika"
                    B.! A.href "https://mw.lojban.org/papri/Learning_with_context"
    H.div B.! A.class_ (H.textValue "resource-category") $ do
        H.h2 $ H.toHtml ("Video lectures" :: T.Text)
        H.ul $ do
            H.li $ do
                H.a "Video lectures by Robert Baruch"
                    B.! A.href "https://www.youtube.com/watch?v=KgxOrTvpWJ4&list=PLEeZWGE3PwbZ7yS5GfDTTT7uR9ESfSKAK"
            H.li $ do
                H.a "Video lectures by la kribacr"
                    B.! A.href "https://www.youtube.com/watch?v=RfdcG5iPJpA"
            H.li $ do
                H.a "Video lectures by the LLG"
                    B.! A.href "https://www.youtube.com/user/LogicalLanguageGroup/videos"
    H.div B.! A.class_ (H.textValue "resource-category") $ do
        H.h2 $ H.toHtml ("Memorization" :: T.Text)
        H.ul $ do
            H.li $ do
                H.a "Memrise"
                    B.! A.href "https://www.memrise.com/courses/english/lojban/"
            H.li $ do
                H.a "Quizlet"
                    B.! A.href "https://quizlet.com/subject/lojban/"
            H.li $ do
                H.a "Anki"
                    B.! A.href "https://ankiweb.net/shared/decks/lojban"
    H.div B.! A.class_ (H.textValue "resource-category") $ do
        H.h2 $ H.toHtml ("Cheatsheets" :: T.Text)
        H.ul $ do
            H.li $ do
                H.a "Mind map of lojban grammar" -- TODO: also link to this in the intro course
                    B.! A.href "https://mw.lojban.org/images/f/f5/759.sip"
            H.li $ do
                H.a "Online messaging cheatsheet"
                    B.! A.href "https://mw.lojban.org/papri/IRC_cheat_sheet"
            H.li $ do
                H.a "Attitudinals, evidentials and discursives"
                    B.! A.href "https://mw.lojban.org/images/0/02/946.sip"
            H.li $ do
                H.a "Table of discourse indicators"
                    B.! A.href "https://mw.lojban.org/images/c/ca/915.sip"
            H.li $ do
                H.a "The periodic table of pro-sumti and pro-bridi"
                    B.! A.href "https://mw.lojban.org/images/a/a8/746.sip"
            H.li $ do
                H.a "Space/time cheatsheet"
                    B.! A.href "https://mw.lojban.org/images/2/20/726.sip"
            H.li $ do
                H.a "Mathematical expressions (mekso)"
                    B.! A.href "https://mw.lojban.org/images/1/1f/734.sip"
    H.div B.! A.class_ (H.textValue "resource-category") $ do
        H.h2 $ H.toHtml ("Dialects" :: T.Text)
        H.ul $ do
            H.li $ do
                H.a "The Lojban I speak (by la lalxu)"
                    B.! A.href "https://gist.github.com/lynn/453a1ccc62aafbc24d2bfbd29bf5cabf"
            H.li $ do
                H.a "Modern lojban concepts"
                    B.! A.href "https://wiki.lojban.io/Modern_Lojban_concepts"
            H.li $ do
                H.a "A simpler connective system"
                    B.! A.href "https://solpahi.wordpress.com/2016/09/20/a-simpler-connective-system/"
            H.li $ do
                H.a "A simpler quantifier logic"
                    B.! A.href "https://solpahi.wordpress.com/2016/09/25/a-simpler-quantifier-logic/"
            H.li $ do
                H.a "xorlo as seen by La Gleki"
                    B.! A.href "https://mw.lojban.org/papri/User:Gleki/xorlo_as_seen_by_La_Gleki"
            H.li $ do
                H.a "zipcpi: Yet another gadri article"
                    B.! A.href "https://mw.lojban.org/papri/zipcpi:_Yet_another_gadri_article"
    H.div B.! A.class_ (H.textValue "resource-category") $ do
        H.h2 $ H.toHtml ("Discussion" :: T.Text)
        H.ul $ do
            H.li $ do
                H.a "Lojban live chat"
                    B.! A.href "https://mw.lojban.org/papri/Lojban_Live_Chat"
            H.li $ do
                H.a "la roljbogu'e"
                    B.! A.href "https://discord.com/invite/dGP5A6Fpj7"
            H.li $ do
                H.a "Lojban Google group"
                    B.! A.href "https://groups.google.com/g/lojban"
            H.li $ do
                H.a "Lojban Facebook group"
                    B.! A.href "https://www.facebook.com/groups/Lojban/"
    H.div B.! A.class_ (H.textValue "resource-category") $ do
        H.h2 $ H.toHtml ("Others" :: T.Text)
        H.ul $ do
            H.li $ do
                H.a "Lojban wiki"
                    B.! A.href "https://mw.lojban.org/papri/Lojban"
            H.li $ do
                H.a "Lojban made easy"
                    B.! A.href "https://lojban.pw"
            H.li $ do
                H.a "Pronunciation guides"
                    B.! A.href "https://mw.lojban.org/papri/pronunciation"
            H.li $ do
                H.a "CLL – the essentials"
                    B.! A.href "https://uacai-uecai.blogspot.com/2021/01/cll-essentials.html"
            H.li $ do
                H.a "More learning materials"
                    B.! A.href "https://mw.lojban.org/papri/Learning_materials._Secondary_links"
            H.li $ do
                H.a "Lojban gotchas"
                    B.! A.href "https://mw.lojban.org/papri/gotchas"
            H.li $ do
                H.a "Word lists"
                    B.! A.href "https://mw.lojban.org/papri/Word_Lists"
            H.li $ do
                H.a "Lojbanic software"
                    B.! A.href "https://mw.lojban.org/papri/Lojbanic_Software"
            H.li $ do
                H.a "Texts in lojban"
                    B.! A.href "https://mw.lojban.org/papri/te_gerna_la_lojban"
            H.li $ do
                H.a "Multimedia"
                    B.! A.href "https://mw.lojban.org/papri/multimedia"
            H.li $ do
                H.a "cniglic"
                    B.! A.href "https://mw.lojban.org/papri/cniglic"
    H.div B.! A.class_ (H.textValue "resource-category") $ do
        H.h2 $ H.toHtml ("Music" :: T.Text)
        H.ul $ do
            H.li $ do
                H.a "Album: za'o (by la selpa'i)"
                    B.! A.href "https://djemynai.bandcamp.com/album/zao"
            H.li $ do
                H.a "Song: tensaia (by la selpa'i)"
                    B.! A.href "https://djemynai.bandcamp.com/album/tensaia"
            H.li $ do
                H.a "Song: sruri fa lo tarci (by Rachel Gardener)"
                    B.! A.href "https://www.youtube.com/watch?v=xZlTA4hbY2o"
            H.li $ do
                H.a "Song: bai do (by Rachel Gardener)"
                    B.! A.href "https://www.youtube.com/watch?v=fR-Z3h1Jsms"
    H.div B.! A.class_ (H.textValue "resource-category") $ do
        H.h2 $ H.toHtml ("Alternative ortographies" :: T.Text)
        H.ul $ do
            H.li $ do
                H.a "la zbalermorna"
                    B.! A.href "https://jackhumbert.github.io/zbalermorna/write-up/"
            H.li $ do
                H.a "la krulermorna"
                    B.! A.href "http://lojban.pw/articles/krulermorna/"
