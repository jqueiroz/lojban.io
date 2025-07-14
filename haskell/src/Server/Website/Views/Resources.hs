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
    let shortDescription = ("Learning Lojban can be challenging, but the right resources make all the difference. This list brings together tools, courses, communities, and more to support you on your journey. Explore the sections below and dive in wherever feels right!" :: T.Text)
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
    H.div B.! A.id "table-of-contents" B.! A.class_ (H.textValue "table-of-contents") $ do
        H.h2 $ H.toHtml ("Table of Contents" :: T.Text)
        H.div B.! A.class_ (H.textValue "table-of-contents-body") $ do
            H.ul B.! A.class_ (H.textValue "listing") $ do
                H.li $ do H.a "Online Communities" B.! A.href "#communities"
            H.ul B.! A.class_ (H.textValue "listing") $ do
                H.li $ do
                    H.a "Courses" B.! A.href "#courses"
                    H.ul B.! A.class_ (H.textValue "sublist") $ do
                        H.li $ do H.a "General Information" B.! A.href "#courses-general"
                        H.li $ do H.a "Individual Courses" B.! A.href "#courses-individual"
                        H.li $ do H.a "Video Lectures" B.! A.href "#courses-video"
            H.ul B.! A.class_ (H.textValue "listing") $ do
                H.li $ do
                    H.a "Grammar" B.! A.href "#grammar"
                    H.ul B.! A.class_ (H.textValue "sublist") $ do
                        H.li $ do H.a "Base Grammar" B.! A.href "#grammar-base"
                        H.li $ do H.a "Proposals and Variations" B.! A.href "#grammar-proposal"
                        H.li $ do H.a "Individual Styles" B.! A.href "#grammar-style"
                        H.li $ do H.a "Parsers" B.! A.href "#grammar-parser"
            H.ul B.! A.class_ (H.textValue "listing") $ do
                H.li $ do
                    H.a "Vocabulary" B.! A.href "#vocab"
                    H.ul B.! A.class_ (H.textValue "sublist") $ do
                        H.li $ do H.a "Dictionaries" B.! A.href "#vocab-dictionary"
                        H.li $ do H.a "Flashcards" B.! A.href "#vocab-flashcard"
                        H.li $ do H.a "Cheatsheets" B.! A.href "#vocab-cheatsheet"
            H.ul B.! A.class_ (H.textValue "listing") $ do
                H.li $ do
                    H.a "Works" B.! A.href "#works"
                    H.ul B.! A.class_ (H.textValue "sublist") $ do
                        H.li $ do H.a "Texts" B.! A.href "#works-text"
                        H.li $ do H.a "Songs" B.! A.href "#works-song"
                        H.li $ do H.a "Other Media" B.! A.href "#works-other"
            H.ul B.! A.class_ (H.textValue "listing") $ do
                H.li $ do
                    H.a "Others" B.! A.href "#other"
                    H.ul B.! A.class_ (H.textValue "sublist") $ do
                        H.li $ do H.a "Alternative Orthographies" B.! A.href "#other-ortho"
                        H.li $ do H.a "Archives" B.! A.href "#other-archive"
    H.div B.! A.id "communities" B.! A.class_ (H.textValue "resource-category") $ do
        H.h2 $ H.toHtml ("Online Communities" :: T.Text)
        H.p $ H.toHtml ("Engaging with fluent speakers and fellow learners is one of the best ways to improve. This section lists forums, social media groups, and other online spaces where you can ask questions, practice, and find support." :: T.Text)
        H.ul $ do
            H.li $ do
                H.a "roljbogu'e"
                  B.! A.href "https://discord.com/invite/dGP5A6Fpj7"
                H.div "The most active group chat for Lojban. Home to most of the active Lojbanists, and where most discussions and activities about the language take place."
            H.li $ do
                H.a "Lojban live chat"
                  B.! A.href "https://mw.lojban.org/papri/Lojban_Live_Chat"
                H.div "An older group chat, now much less active than roljbogu'e."
            H.li $ do
                H.a "r/Lojban"
                  B.! A.href "https://old.reddit.com/r/lojban/"
                H.div "Discussions about Lojban on Reddit."
            H.li $ do
                H.a "Lojban Google Group"
                  B.! A.href "https://groups.google.com/g/lojban"
                H.div "Discussions about Lojban on Google Group."
            H.li $ do
                H.a "Lojban Facebook Group"
                  B.! A.href "https://www.facebook.com/groups/Lojban/"
                H.div "Discussions about Lojban on Facebook."
    H.div B.! A.id "courses" B.! A.class_ (H.textValue "resource-category") $ do
        H.h2 $ H.toHtml ("Courses" :: T.Text)
        H.p $ H.toHtml ("Whether you're a beginner or looking to refine your skills, structured courses can provide guidance and practice. Here, you'll find courses that cover different aspects of the language. Note that all current courses teach grammar only, and you will need to learn the vocabulary separately." :: T.Text)
        H.div B.! A.id "courses-general" B.! A.class_ (H.textValue "resource-category") $ do
            H.h3 $ H.toHtml ("General Information" :: T.Text)
            H.p $ H.toHtml ("This section contains things you might want to look at before jumping into one of the courses. You can use them to figure out which course is the best for you." :: T.Text)
            H.ul $ do
                H.li $ do
                    H.div $ do
                        H.a "Lojban Courses Grammar Point Comparison"
                            B.! A.href "https://docs.google.com/spreadsheets/d/1OphQh-_yyydvZpIgEDdACsrQX5S7x41vKZmT8gDyKCU/edit?pli=1&gid=0#gid=0"
                        " by la nalvai"
                    H.div "A large table showing which grammar points appear where in each course. Good for cross-referencing."
        H.div B.! A.id "courses-individual" B.! A.class_ (H.textValue "resource-category") $ do
            H.h3 $ H.toHtml ("Individual Courses" :: T.Text)
            H.p $ H.toHtml ("These are the courses that will guide a complete beginner through the grammar of Lojban. After completing one of these courses, you should have a firm grasp of Lojban grammar." :: T.Text)
            H.ul $ do
                H.li $ do
                    H.div $ do
                        H.a "Lojban for Beginners"
                            B.! A.href "https://mw.lojban.org/papri/Lojban_For_Beginners,_The_Book"
                        " by Nick Nicholas and Robin Turner"
                    H.div "An old course that teaches CLL Lojban. Although very old, it's not really outdated since most of what it teaches is still valid today, and it contains many insights into the language."
                H.li $ do
                    H.div $ do
                        H.a "The Wave Lessons"
                            B.! A.href "https://mw.lojban.org/papri/Lojban_Wave_Lessons"
                        " by la klaku"
                    H.div "A popular text for learning Lojban. Covers newer grammatical innovations (such as xorlo and dotside) not covered in Lojban for Beginners."
                H.li $ do
                    H.div $ do
                        H.a "Learn Lojban"
                            B.! A.href "https://lojban.pw/en/books/learn-lojban/"
                        " by la gleki"
                    H.div "A newer course that is actively maintained. Features AI-generated illustrations."
                H.li $ do
                    H.div $ do
                        H.a "My First Lojban"
                            B.! A.href "https://cogas.github.io/hajiloji/"
                        " by la .cogas."
                    H.div $ do
                        "A Lojban course in Japanese. Has a "
                        H.a "partial English translation"
                            B.! A.href "https://lynn.github.io/hajiloji/"
                        "."
                H.li $ do
                    H.div $ do
                        H.a "Getting Started with Lojban (alpha)"
                            B.! A.href "https://lojban.io/courses/introduction/"
                    H.div "A modern, gentle introductory course based on The Wave Lessons. Includes Duolingo-style interactive exercises."
                H.li $ do
                    H.div $ do
                        H.a "la karda"
                            B.! A.href "https://mw.lojban.org/papri/la_karda"
                        " by ldlework"
                    H.div "A distilled, fast-paced overview of the grammar of the Lojban language."
        H.div B.! A.id "courses-video" B.! A.class_ (H.textValue "resource-category") $ do
            H.h3 $ H.toHtml ("Video Lectures" :: T.Text)
            H.p $ H.toHtml ("Some Lojbanists have made video lectures on Lojban to appeal to a wider audience. While less comprehensive than the courses listed above, they can serve as an introduction to the language." :: T.Text)
            H.ul $ do
                H.li $ do
                    H.div $ do
                        H.a "Lojban is too easy"
                            B.! A.href "https://www.youtube.com/playlist?list=PLhYRPlAt2ubDm8lBS4-q2vVxXvoyxRryD"
                        " by la lunbe"
                H.li $ do
                    H.div $ do
                        H.a "Video lectures by Robert Baruch"
                            B.! A.href "https://www.youtube.com/watch?v=KgxOrTvpWJ4&list=PLEeZWGE3PwbZ7yS5GfDTTT7uR9ESfSKAK"
                H.li $ do
                    H.div $ do
                        H.a "Video lectures by la .kribacr."
                            B.! A.href "https://www.youtube.com/watch?v=RfdcG5iPJpA"
                H.li $ do
                    H.div $ do
                        H.a "Video lectures by the LLG"
                            B.! A.href "https://www.youtube.com/user/LogicalLanguageGroup/videos"
    H.div B.! A.id "grammar" B.! A.class_ (H.textValue "resource-category") $ do
        H.h2 $ H.toHtml ("Grammar" :: T.Text)
        H.p $ H.toHtml ("Understanding grammar is key to mastering any language. This section includes references, explanations, and tools to help you grasp the rules and structures of the language." :: T.Text)
        H.div B.! A.id "grammar-base" B.! A.class_ (H.textValue "resource-category") $ do
            H.h3 $ H.toHtml ("Base Grammar" :: T.Text)
            H.p $ H.toHtml ("These are the grammars that pretty much every Lojbanist agrees on. Stick to these if you want your Lojban work to be understood by everyone." :: T.Text)
            H.ul $ do
                H.li $ do
                    H.a "The Complete Lojban Language"
                        B.! A.href "https://lojban.org/publications/cll/cll_v1.1_xhtml-section-chunks/"
                    H.div "Often abbreviated as \"the CLL\". Widely regarded as the go-to reference for the language, this book has shaped how many people learn and understand it. While some parts may now be a bit outdated (see the next resource), it remains an essential resource."
                H.li $ do
                    H.a "BPFK Sections"
                        B.! A.href "https://mw.lojban.org/papri/BPFK_Sections"
                    H.div $ do 
                        "These sections, grouped by cmavo classes, expand on the CLL, explaining it more fully and resolving unclear and contradictory parts. Widely accepted by the community, they're considered as part of the base grammar. The most important of these is "
                        H.a "the gadri section"
                            B.! A.href "https://mw.lojban.org/papri/BPFK_Section:_gadri"
                        ", which supersedes the relevant sections in the CLL."
        H.div B.! A.id "grammar-proposal" B.! A.class_ (H.textValue "resource-category") $ do
            H.h3 $ H.toHtml ("Proposals and Variations" :: T.Text)
            H.p $ H.toHtml ("Over time, various Lojban experts have proposed changes to make the grammar more consistent, more expressive, or both. Some of these changes have caught on in the community and are widely used. If you see a Lojban text that seems ungrammatical, check to see if it incorporates some of the changes below!" :: T.Text)
            H.div B.! A.class_ (H.textValue "additional-note") $ do
                H.i "Roughly ordered from most to least common."
            H.ul $ do
                H.li $ do
                    H.a "A Simpler Connective System"
                        B.! A.href "https://solpahi.wordpress.com/2016/09/20/a-simpler-connective-system/"
                    H.div $ do
                        "Go here if you see "
                        H.code "je cu"
                        " or "
                        H.code "je"
                        " followed by a sumti. One of the most common variations of the base grammar."
                H.li $ do
                    H.a "A Simpler Quantifier Logic"
                        B.! A.href "https://solpahi.wordpress.com/2016/09/25/a-simpler-quantifier-logic/"
                    H.div "A proposal to incorporate plural logic into Lojban, as opposed to the singular logic used by the base grammar."
                    H.div B.! A.class_ (H.textValue "additional-note") $ do
                        H.i "This is quite an advanced topic so it's safe to skip this for now if you're a beginner."
                H.li $ do
                    H.a "cmevla-brivla Merger (CBM)"
                        B.! A.href "https://wiki.lojban.io/cmevla%E2%80%93brivla_merger"
                    H.div "Use cmevla more freely by treating them like brivlas."
                H.li $ do
                    H.a "zantufa"
                        B.! A.href "https://mw.lojban.org/papri/zantufa"
                    H.div $ do
                        "A rework of the Lojban grammar, aiming to make it more concise and elegant. "
                        H.a "Has a parser"
                            B.! A.href "https://guskant.github.io/gerna_cipra/zantufa-1.html"
                        "."
                H.li $ do
                    H.a "tcekitau"
                        B.! A.href "https://mw-live.lojban.org/papri/ce_ki_tau_jau"
                    H.div "A \"tcekitau\" dialect swaps some of the more-used two-syllbale cmavo with lesser-used one-syllable cmavo to save syllables in sentences. Once ubiquitous, it sees much less use now, but is important to keep in mind when reading older texts."
                H.li $ do
                    H.a "jboponei"
                        B.! A.href "https://wiki.lojban.io/jboponei"
                    H.div $ do
                        "Have you ever seen a "
                        H.code "po"
                        " or "
                        H.code "peu"
                        " that appears out of place? Maybe it's a substitute for "
                        H.code "lo su'u"
                        "!"
                H.li $ do
                    H.a "zipcpi: Yet another gadri article"
                        B.! A.href "https://mw.lojban.org/papri/zipcpi:_Yet_another_gadri_article"
                    H.div $ do
                        "Part of the gadganzu proposal, which aims to make the gadri system in Lojban more organized. Incompatible with "
                        H.a "the base grammar"
                            B.! A.href "https://mw.lojban.org/papri/BPFK_Section:_gadri"
                        ", however."
                H.li $ do
                    H.a "New Sentence Links (.ajbo)"
                        B.! A.href "https://wiki.lojban.io/New_Sentence_Links"
                    H.div $ do
                        "\"A Simpler Connective System\" frees up the cmavo "
                        H.code ".a"
                        ", "
                        H.code ".e"
                        ", "
                        H.code ".o"
                        ", and "
                        H.code ".u"
                        "... Why not use them like "
                        H.code ".i"
                        "?"
        H.div B.! A.id "grammar-style" B.! A.class_ (H.textValue "resource-category") $ do
            H.h3 $ H.toHtml ("Individual Styles" :: T.Text)
            H.p $ H.toHtml ("Fluent Lojbanists talk about the Lojban grammar they use. Good for seeing how various proposals and base grammar syncretize into one." :: T.Text)
            H.ul $ do
                H.li $ do
                    H.div $ do
                        H.a "The Lojban I speak"
                            B.! A.href "https://gist.github.com/lynn/453a1ccc62aafbc24d2bfbd29bf5cabf"
                        " by la lalxu"
                H.li $ do
                    H.div $ do
                        H.a "The Lojban I speak"
                            B.! A.href "https://gist.github.com/IGJoshua/fcaa1a1d271d30239390d5a98ffdef16"
                        " by la srasu"
        H.div B.! A.id "grammar-parser" B.! A.class_ (H.textValue "resource-category") $ do
            H.h3 $ H.toHtml ("Parsers" :: T.Text)
            H.p $ H.toHtml ("Lojban prides itself on syntactic unambiguity and computer parsability. The tools below will help you check the grammatical structure of your Lojban so that it is parsable by both humans and computers. You can also use them to understand other people's Lojban." :: T.Text)
            H.ul $ do
                H.li $ do
                    H.a "jboski/jbofihe"
                        B.! A.href "https://jboski.lojban.org/"
                    H.div "An old parser that parses text according to the CLL grammar. Although old, many still like to use it because its output is easy to understand and includes glosses for sumti places."
                H.li $ do
                    H.a "ilmentufa"
                        B.! A.href "https://lojban.github.io/ilmentufa/camxes.html"
                    H.div "A more modern parser that has many different modes for parsing text in standard and various experimental grammars."
                H.li $ do
                    H.a "ilmentufa with glosser"
                        B.! A.href "https://lojban.github.io/ilmentufa/glosser/glosser.htm"
                    H.div "A parser based on \"Camxes: standard\" of the above parser, but with syntax highlighting and glosses to make the output more understandable."
                H.li $ do
                    H.a "Visual camxes"
                        B.! A.href "https://camxes.lojban.org/"
                    H.div "Another parser based on \"Camxes: standard\" of ilmentufa which displays the output graphically."
                H.li $ do
                    H.a "zantufa parser"
                        B.! A.href "https://guskant.github.io/gerna_cipra/zantufa-1.html"
                    H.div "A parser for zantufa, a rework of the Lojban grammar."
    H.div B.! A.id "vocab" B.! A.class_ (H.textValue "resource-category") $ do
        H.h2 $ H.toHtml ("Vocabulary" :: T.Text)
        H.p $ H.toHtml ("Expanding your vocabulary makes communication easier and more natural. Here are resources for learning new words, including dictionaries, flashcards, and thematic word lists." :: T.Text)
        H.div B.! A.id "vocab-dictionary" B.! A.class_ (H.textValue "resource-category") $ do
            H.h3 $ H.toHtml ("Dictionaries" :: T.Text)
            H.p $ H.toHtml ("Quick, accessible, and often packed with extra features, online dictionaries are useful tools for looking up words, checking usage, and exploring meanings. This section lists some of the most reliable ones available for the language." :: T.Text)
            H.ul $ do
                H.li $ do
                    H.a "la xlasisku"
                        B.! A.href "https://mi2ebi.github.io/xlasisku/"
                    H.div "A word search engine that supports searching by regex and rhymes, and also finding gismu conflicts."
                H.li $ do
                    H.a "la lidysisku"
                        B.! A.href "https://sisku.org/?en"
                    H.div "A faster alternative to la sutysisku."
                H.li $ do
                    H.a "la sutysisku"
                        B.! A.href "https://la-lojban.github.io/sutysisku/lojban/index.html#&bangu=en"
                    H.div "A popular Lojban word search engine. Has a sentence database so you can search for example sentences too. Works offline."
                H.li $ do
                    H.a "la vlasisku"
                        B.! A.href "https://vlasisku.lojban.org/"
                    H.div "Another popular Lojban word search engine."
                H.li $ do
                    H.a "la jbovlaste"
                        B.! A.href "https://jbovlaste.lojban.org/"
                    H.div "Where all the other dictionaries get their data from. You can register an account there, add words, and vote for words you think are good or bad."
        H.div B.! A.id "vocab-flashcard" B.! A.class_ (H.textValue "resource-category") $ do
            H.h3 $ H.toHtml ("Flashcards" :: T.Text)
            H.p $ H.toHtml ("Flashcards are a handy way to build and reinforce vocabulary. This section includes curated decks that help you practice words and phrases." :: T.Text)
            H.ul $ do
                H.li $ do
                    H.a "Contextualized brivla"
                        B.! A.href "https://lojban.io/decks/contextualized-brivla/"
                H.li $ do
                    H.a "Quizlet"
                        B.! A.href "https://quizlet.com/subject/lojban/"
                H.li $ do
                    H.a "Anki"
                        B.! A.href "https://ankiweb.net/shared/decks/lojban"
        H.div B.! A.id "vocab-cheatsheet" B.! A.class_ (H.textValue "resource-category") $ do
            H.h3 $ H.toHtml ("Cheatsheets" :: T.Text)
            H.p $ H.toHtml ("Various cheatsheets for quick references. Most of them are pretty self-explanatory." :: T.Text)
            H.ul $ do
                H.li $ do
                    H.a "Mind map of lojban grammar"
                        B.! A.href "https://mw.lojban.org/images/f/f5/759.sip"
                    H.div "A map of words commonly used when discussing Lojban grammar. Do you know them all?"
                H.li $ do
                    H.a "Online messaging cheatsheet"
                        B.! A.href "https://mw.lojban.org/papri/IRC_cheat_sheet"
                    H.div "A phrasebook for online messaging."
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
    H.div B.! A.id "works" B.! A.class_ (H.textValue "resource-category") $ do
        H.h2 $ H.toHtml ("Works" :: T.Text)
        H.p $ H.toHtml ("Exposure to real-world content in the language is crucial for immersion. This section contains books, articles, music, and other media to help you engage with the language in context." :: T.Text)
        H.div B.! A.id "works-text" B.! A.class_ (H.textValue "resource-category") $ do
            H.h3 $ H.toHtml ("Texts" :: T.Text)
            H.p $ H.toHtml ("Reading content written in the language is one of the best ways to see how it's used in practice. This section collects stories, articles, and other texts that offer exposure to natural language and a deeper feel for its rhythm and style." :: T.Text)
            H.div B.! A.class_ (H.textValue "additional-note") $ do
                H.i "Roughly ordered from most to least recent."
            H.ul $ do
                H.li $ do
                    H.div $ do
                        H.a "la nu farlu lo plini"
                            B.! A.href "https://liputenpo.org/toki/nanpa-kokosila/la-nu-farlu-lo-plini/"
                        " by la srasu"
                    H.div "A short story in Lojban featured in lipu tenpo, a Toki Pona magazine."
                H.li $ do
                    H.div $ do
                        H.a "la malbi .e le ractu"
                            B.! A.href "https://lynn.github.io/la-melbi-e-le-ractu/"
                        " by la lalxu"
                    H.div "A story that uses the 60 most common gismu and the 60 most common cmavo."
                H.li $ do
                    H.div $ do
                        H.a "lu mi za'o citno li'u"
                            B.! A.href "https://solpahi.wordpress.com/2016/09/26/mi-zaho-citno/"
                        " by la solpahi"
                    H.div $ do
                        "An original story in Lojban. The text is rather heavily tcekitau'd, you can use "
                        H.a "this parser"
                            B.! A.href "https://guskant.github.io/gerna_cipra/maftufa-1.14-cekitaujoizai.html"
                        " if you have difficulty understanding it."
                H.li $ do
                    H.div $ do
                        H.a "Texts by la solpahi"
                            B.! A.href "https://selpahi.de/"
                    H.div $ do
                        "Includes many translations such as Snow White, the Princess and the Pea, Where the Wild Things Are, and also poems and comics. One of these, "
                        H.a "The Wonderful Wizard of Oz"
                            B.! A.href "https://selpahi.de/oz_plain.html"
                        ", doesn't show correctly on the site so its link is provided here."
                H.li $ do
                    H.div $ do
                        H.a "The Little Prince"
                            B.! A.href "https://mw.lojban.org/papri/le_cmalu_noltru"
                        " translated by la .xorxes., edited by la lalxu"
                H.li $ do
                    H.div $ do
                        H.a "Alice in Wonderland"
                            B.! A.href "https://mw.lojban.org/papri/lo_selfri_be_la_.alis._bei_bu%27u_la_selmacygu%27e"
                        " translated by la .xorxes."
                H.li $ do
                    H.div $ do
                        H.a "Metamorphosis"
                            B.! A.href "https://mw.lojban.org/images/f/fc/lonubinxo.pdf"
                        " translated by la .xorxes."
                    H.div "Classic literature translated into Lojban."
        H.div B.! A.id "works-song" B.! A.class_ (H.textValue "resource-category") $ do
            H.h3 $ H.toHtml ("Songs" :: T.Text)
            H.p $ H.toHtml ("Music is a powerful way to connect with a language. This section features songs written in the language - great for getting used to its sounds, rhythms, and cultural context while enjoying something memorable." :: T.Text)
            H.ul $ do
                H.li $ do
                    H.div $ do
                        H.a "Covers by tetsusquared (la .tirprij.)"
                            B.! A.href "https://www.youtube.com/playlist?list=PLPL5rPlw-AHvMtol3V0G8S9wmUdsMjIp9"
                    H.div "Vocaloid song covers in Lojban. Over 20 songs and counting."
                H.li $ do
                    H.div "Songs by Noise and Bells (la janbe)"
                    H.div $ do
                        H.ul B.! A.class_ (H.textValue "sublist") $ do
                            H.li $ do
                                H.a "jbini canlu"
                                    B.! A.href "https://www.youtube.com/watch?v=3aNGqujy-6s"
                            H.li $ do
                                H.a "ko gleki gau la cevni"
                                    B.! A.href "https://www.youtube.com/watch?v=6swu1ESUw0E"
                            H.li $ do
                                H.a "dansu co toldi"
                                    B.! A.href "https://www.youtube.com/watch?v=B156ExvRjzM"
                            H.li $ do
                                H.a "tabgacpi"
                                    B.! A.href "https://www.youtube.com/watch?v=ra4ID_TUJjo"
                            H.li $ do
                                H.a "bai do"
                                    B.! A.href "https://www.youtube.com/watch?v=fR-Z3h1Jsms"
                            H.li $ do
                                H.a "sruri fa lo tarci"
                                    B.! A.href "https://www.youtube.com/watch?v=xZlTA4hbY2o"
                H.li $ do
                    H.div "Songs by la solpahi"
                    H.div $ do
                        H.ul B.! A.class_ (H.textValue "sublist") $ do
                            H.li $ do
                                H.a "ZAhO"
                                    B.! A.href "https://djemynai.bandcamp.com/album/zao"
                                ": A Lojban rap album containing 16 songs."
                            H.li $ do
                                H.a "tensaia"
                                    B.! A.href "https://djemynai.bandcamp.com/album/tensaia"
                            H.li $ do
                                H.a "lo mi jufra"
                                    B.! A.href "https://www.youtube.com/watch?v=sLrmjRIMgAk"
                            H.li $ do
                                H.a "jmive za'o"
                                    B.! A.href "https://www.youtube.com/watch?v=hRrIioMA4w8"
                                ": A cover of \"Still Alive\" from Portal 1."
                H.li $ do
                    H.div "Songs by la .guskant."
                    H.div $ do
                        H.ul B.! A.class_ (H.textValue "sublist") $ do
                            H.li $ do
                                H.a "ro lo ma'a dakta"
                                    B.! A.href "https://vimeo.com/126974807"
                                " (also with la .tijlan.)"
                            H.li $ do
                                H.a "bripre jikca selsa'a"
                                    B.! A.href "https://vimeo.com/113056241"
                            H.li $ do
                                H.a "almapamla"
                                    B.! A.href "https://vimeo.com/233808507"
                                ": A cover of \"Amapola\" by José María Lacalle García."
                            H.li $ do
                                H.a "ro roi za'i re'u ji'a"
                                    B.! A.href "https://vimeo.com/113232481"
                                ": A cover of \"いつも何度でも\", a Japanese song."
                H.li $ do
                    H.div $ do
                        H.a "cicricfoi bartu"
                            B.! A.href "https://www.youtube.com/watch?v=KG-JKVBn6c4"
                        " by Corbin Simpson"
                H.li $ do
                    H.div $ do
                        H.a "galmaumoi"
                            B.! A.href "https://www.youtube.com/watch?v=q9-YzMMp-1A"
                        " by Corbin Simpson"
                H.li $ do
                    H.div $ do
                        H.a "ku'i ba troci"
                            B.! A.href "https://www.youtube.com/watch?v=Y3apKaaBK5c"
                        " by Fyren"
                H.li $ do
                    H.div $ do
                        H.a "le lenrygu'i"
                            B.! A.href "https://www.youtube.com/watch?v=-q0gzt4w-o8"
                        " by la fagri"
                H.li $ do
                    H.div $ do
                        H.a "claxu be do crisa"
                            B.! A.href "https://soundcloud.com/cogas-iuk/sunday-10-38-pm"
                        " by la .cogas."
        H.div B.! A.id "works-other" B.! A.class_ (H.textValue "resource-category") $ do
            H.h3 $ H.toHtml ("Other Media" :: T.Text)
            H.p $ H.toHtml ("Videos, recordings, any Lojban content that is neither text or songs goes here." :: T.Text)
            H.ul $ do
                H.li $ do
                    H.a "Videos by la lunbe"
                        B.! A.href "https://www.youtube.com/@selckiku/videos"
                H.li $ do
                    H.a "Lojban vlogs by Noise and Bells (la janbe)"
                        B.! A.href "https://www.youtube.com/playlist?list=PLENZ6X9jUwQJ_s9utWniafXKr6XloQqyZ"
                H.li $ do
                    H.a "Creating an Island from Scratch"
                        B.! A.href "https://www.youtube.com/playlist?list=PLg1OMpL5Gt_WALpbXFAIWr4Gk6lKicxJj"
                    " by Evar Usar a.k.a. Worldbuilding Notes"
                    H.div "A worldbuilding video series. The first five episodes are in Lojban, with subtitles in English."
                H.li $ do
                    H.a "Lojban Corpus Readings"
                        B.! A.href "https://www.youtube.com/playlist?list=PLBguSndiEtX7BMjHM9zudHAnBPZDN7XbV"
                    " by la solpahi"
                    H.div "la solpahi reading various Lojban texts."
    H.div B.! A.id "other" B.! A.class_ (H.textValue "resource-category") $ do
        H.h2 $ H.toHtml ("Others" :: T.Text)
        H.p $ H.toHtml ("Some useful resources don't fit neatly into the previous categories. This section includes various tools, tips, and references that can enhance your learning experience." :: T.Text)
        H.div B.! A.id "other-ortho" B.! A.class_ (H.textValue "resource-category") $ do
            H.h3 $ H.toHtml ("Alternative Orthographies" :: T.Text)
            H.p $ H.toHtml ("Various Lojbanists have proposed native scripts to Lojban. This section lists the most commonly used ones." :: T.Text)
            H.ul $ do
                H.li $ do
                    H.a "la zbalermorna"
                        B.! A.href "https://jackhumbert.github.io/zbalermorna/write-up/"
                    H.div "The de facto native script of Lojban. You don't need to learn it (most Lojban texts are written in Latin script anyway), but it shows up quite often in Lojban artworks."
        H.div B.! A.id "other-archive" B.! A.class_ (H.textValue "resource-category") $ do
            H.h3 $ H.toHtml ("Archives" :: T.Text)
            H.p $ H.toHtml ("Lojban has a rich history - one of the richest among conlangs, actually. This section lists archives of past discussions in or about the Lojban language. Dive in to feel its history and get insights and tips from people of the past!" :: T.Text)
            H.ul $ do
                H.li $ do
                    H.a "Lojban wiki"
                        B.! A.href "https://mw.lojban.org/papri/Lojban"
                    H.div "Once the central hub for discussion of Lojban, this site now serves primarily as an archive. It preserves years of conversations about the language - covering grammar questions, vocabulary debates, usage notes, and more. It's a valuable resource for deeper insights and historical context."
    H.div B.! A.id "epilogue" B.! A.class_ (H.textValue "resource-category") $ do
        H.p $ H.toHtml ("Languages don't stand still - they grow through the people who use them, explore them, and create with them. This list is just one part of that ongoing story. If you discover or create a resource that helps others learn, please share it with us so we can add it here. Every contribution helps the language - and its community - move forward." :: T.Text)

