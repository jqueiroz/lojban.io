{-# LANGUAGE OverloadedStrings #-}

module Server.Website.Views.FAQ
( displayFAQHome
) where

import Server.Core
import Server.Website.Views.Core
import qualified Data.Text as T
import qualified Text.Blaze as B
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

displayFAQHome :: ServerConfiguration -> Maybe UserIdentity -> H.Html
displayFAQHome serverConfiguration userIdentityMaybe = do
    let shortDescription = ("Lojban has been puzzling and fascinating people for decades. We've gathered some of the most frequent questions — along with honest, community-tested answers — to help you get a clearer picture of what Lojban is and isn't." :: T.Text)
    H.docType
    H.html B.! A.lang (H.stringValue "en-us") $ do
        H.head $ do
            H.title $ H.toHtml ("About Lojban :: lojban.io" :: T.Text)
            H.meta B.! A.name (H.textValue "description") B.! A.content (H.textValue shortDescription)
            includeUniversalStylesheets
            includeUniversalScripts
            includeInternalStylesheet "FAQ.css"
        H.body $ do
            displayTopbar serverConfiguration userIdentityMaybe TopbarFAQ
            H.div B.! A.class_ (H.stringValue "main") $ do
                H.div B.! A.class_ (H.textValue "header") $ do
                    H.div B.! A.class_ (H.textValue "header-bg") $ H.toHtml ("" :: T.Text)
                    H.h1 $ H.toHtml ("About Lojban" :: T.Text)
                    H.p $ H.toHtml shortDescription
                H.div B.! A.class_ (H.textValue "body") $ do
                    displayFAQ
                    displayFooter

displayFAQ :: H.Html
displayFAQ = do
    H.div B.! A.id "table-of-contents" B.! A.class_ (H.textValue "table-of-contents") $ do
        H.h2 $ H.toHtml ("Table of Contents" :: T.Text)
        H.div B.! A.class_ (H.textValue "table-of-contents-body") $ do
            H.ul B.! A.class_ (H.textValue "listing") $ do
                H.li $ do H.a "What is Lojban?" B.! A.href "#what-is-lojban"
                H.li $ do H.a "How can I learn Lojban?" B.! A.href "#how-can-i-learn"
                H.li $ do H.a "Is Lojban hard to learn?" B.! A.href "#is-lojban-hard"
                H.li $ do H.a "Can Generative AIs/LLMs like ChatGPT be my guide/tutor to Lojban?" B.! A.href "#llm"
                H.li $ do H.a "What is the history of Lojban?" B.! A.href "#history"
                H.li $ do H.a "What are some other cool constructed languages I can check out?" B.! A.href "#cool-conlangs"
            H.ul B.! A.class_ (H.textValue "listing") $ do
                H.li $ do
                    H.a "Common Misconceptions of Lojban" B.! A.href "#common-misconceptions"
                    H.ul B.! A.class_ (H.textValue "sublist") $ do
                        H.li $ do H.a "On Lojban's Purpose and Design" B.! A.href "#misconceptions-purpose"
                        H.ul B.! A.class_ (H.textValue "sublist") $ do
                            H.li $ do H.a "Lojban is like Newspeak." B.! A.href "#misconception-newspeak"
                            H.li $ do H.a "Lojban is a programming language, not a real language." B.! A.href "#misconception-program"
                            H.li $ do H.a "Lojban is made to be a 'perfect language'." B.! A.href "#misconception-perfect"
                            H.li $ do H.a "Lojban promotes mass surveillance because it makes human communication parsable by computers." B.! A.href "#misconception-surveillance"
                            H.li $ do H.a "Lojban is emotionless or robotic." B.! A.href "#misconception-emotionless"
                            H.li $ do H.a "Lojban's grammar is so alien-ish, it is impossible to learn." B.! A.href "#misconception-alienish"
                        H.li $ do H.a "On What You Can and Cannot Do in Lojban" B.! A.href "#misconceptions-expressiveness"
                        H.ul B.! A.class_ (H.textValue "sublist") $ do
                            H.li $ do H.a "It is impossible to tell a lie in Lojban." B.! A.href "#misconception-lying"
                            H.li $ do H.a "It is impossible to translate <insert something here> into Lojban." B.! A.href "#misconception-translation"
                            H.li $ do H.a "It is impossible to be creative in Lojban because speaking logically and rigorously kills imagination." B.! A.href "#misconception-creative"
                            H.li $ do H.a "It is impossible to do wordplays or puns in Lojban." B.! A.href "#misconception-wordplay"
                            H.li $ do H.a "You can't be figurative in Lojban." B.! A.href "#misconception-figurative"
                            H.li $ do H.a "You can't joke in lojban." B.! A.href "#misconception-jokes"
                        H.li $ do H.a "On Community" B.! A.href "#misconceptions-community"
                        H.ul B.! A.class_ (H.textValue "sublist") $ do
                            H.li $ do H.a "Nobody speaks Lojban." B.! A.href "#misconception-nobody"
                            H.li $ do H.a "Lojban speakers are pedants who enjoy picking on other people's grammar." B.! A.href "#misconception-xekce"
                        H.li $ do H.a "Other Misconceptions" B.! A.href "#misconceptions-other"
                        H.ul B.! A.class_ (H.textValue "sublist") $ do
                            H.li $ do H.a "Attitudinals are weird" B.! A.href "#misconception-attitudinal"

    H.div B.! A.id "what-is-lojban" B.! A.class_ (H.textValue "link-hook") $ do
        H.h2 $ H.toHtml ("What is Lojban?" :: T.Text)
        H.p $ H.toHtml ("Lojban is a constructed language (conlang) aimed to be completely regular and logical in grammatical structure. It has been developed for over 60 years with dozens of workers and hundreds of supporters, and currently dozens speak the language fluently." :: T.Text)
        H.p $ H.toHtml ("The main features of Lojban are:" :: T.Text)
        H.ul $ do
            H.li $ do
                H.div $ do
                    H.b "Unambiguous syntax."
                    " Consider the following English sentence: \"I saw Jack inside the train.\" Who is inside the train? I, or Jack? We don't know because both interpretations are valid per the syntax of English. Such ambiguity in meaning arising from sentence structure (instead of multiple meanings of the same word) is called \"syntactic ambiguity\". Lojban is designed to be devoid of syntactic ambiguities: In every Lojban sentence, there is no ambiguity in the relation between its constituent elements."
            H.li $ do
                H.div $ do
                    H.b "Semantics based on predicate logic."
                    " Consider the following pair of sentences: \"All engelangs have a goal.\" and \"All auxlangs share a goal.\" The syntactic structures of the two sentences are identical. However, the semantic structures are different: In the first sentence, different engelangs can have different goals, so there can be more than one goal in total; in the second sentence, because of the meaning of the word \"share,\" there can be only one goal in total, and every auxlang has the same goal. Therefore, in English, the syntactic structure and the semantic structure of a sentence don't necessarily match. In Lojban, there is a consistent relationship between a sentence's syntactic structure and its semantic structure (expressed with logical formulae), so the meaning of a sentence is more transparent even if you don't recognize its words."
            H.li $ do
                H.div $ do
                    H.b "Regular, rule-based grammar."
                    " Natural languages often have quirks — For example, inconsistent spelling, irregular inflection, set phrases that don't fit into other grammar rules, and so on. Lojban is designed to be devoid of those. Once you've learned a grammar rule in Lojban, you can apply it wherever applicable — No need to worry about exceptions."
            H.li $ do
                H.div $ do
                    H.b "No mandatory grammatical category."
                    " Natural languages often have mandatory categories, where you "
                    H.i "must"
                    " give specific additional information about what is spoken. For example, tense is a mandatory category in English, and it is impossible to talk about an event while leaving its time unspecified. In Lojban, every grammatical category is optional. You can be as vague as you want — and as precise as you want. This doesn't contradict Lojban's unambiguous grammar — when you leave something vague, the listener knows exactly what you left vague, thanks to Lojban's unambiguous grammar."
            H.li $ do
                H.div $ do
                    H.b "Speakable and usable by humans."
                    " Although Lojban's preciseness in grammar is unseen in natural languages, it is designed to be learnable and speakable. Everything expressible with a human language, from technical to casual or emotional, can be expressed in Lojban too. Also, its flexible morphology and syntax enables you to combine existing words to express new ideas. Over the years, people have produced materials across a wide range of forms and registers, from translations to original works, and from love songs to academic essays."
        H.p $ H.toHtml ("However, Lojban is not:" :: T.Text)
        H.ul $ do
            H.li $ do
                H.div $ do
                    H.b "Mathematical formula and/or programming language."
                    " You'll see Lojban being compared to them because they all have a rigorous structure. The similarity ends here. Lojban is made for human communication, and its domain of use is that of a human language, not of a mathematical formula or a programming language. Lojban is a human language, but with a rigorous structure."
            H.li $ do
                H.div $ do
                    H.b "A perfect language."
                    " There are still many unresolved issues to be settled, and many usages to be discovered. Don't think of Lojban as a perfect, complete, and immutable end product; think of it as a journey — and every part of your participation makes Lojban better."

    H.div B.! A.id "how-can-i-learn" B.! A.class_ (H.textValue "link-hook") $ do
        H.h2 $ H.toHtml ("How can I learn Lojban?" :: T.Text)
        H.p $ H.toHtml ("There are two main parts of learning a language: Grammar and Vocabulary." :: T.Text)
        H.ul $ do
            H.li $ do
                H.div $ do
                    "There are many courses dedicated to guide a complete beginner through the grammar of Lojban. You can go to "
                    H.a "the resources page"
                        B.! A.href "https://lojban.io/resources/#courses"
                    " and pick a course you like. In addition, the reference grammar, "
                    H.i $ do
                        H.a "The Complete Lojban Language"
                            B.! A.href "https://lojban.org/publications/cll/cll_v1.1_xhtml-section-chunks/"
                    ", is written in a friendly manner and ample with explanations and examples, and many Lojbanists use it to learn the grammar as well. Generally, the courses are written in the order of most basic and/or commonly-used grammar to more advanced and/or less-used grammar, so they are useful for learners who want to quickly become conversational; "
                    H.i "The Complete Lojban Language"
                    " arranges the grammar by topics, so it is suitable as a reference."
            H.li $ do
                H.div "As for vocabulary: The grammatical words are explained in the grammar courses pretty well, so just read one of the courses above and they shouldn't be a problem. For the content words, most of them are made out of ~1400 root words, so learning them should be your top priority. In practice, you only need less than half of them to get to conversational fluency (see the next question). The definitions for words are written in a rather unfamiliar way, so it is recommended to read the first few pages of a grammar course before starting to memorize vocabulary."
        H.p $ H.toHtml ("When learning, you should prioritize acquiring vocabulary, because when it comes to making yourself understood, having a working vocabulary is much more useful than having a firm grasp of grammar." :: T.Text)
        H.p $ do
            "Also, join "
            H.a "the Lojban community"
                B.! A.href "https://discord.com/invite/dGP5A6Fpj7"
            " and talk there. Don't be afraid of being picked on for minute grammar details, that's a thing in the past. Joining a community gives many benefits: you can ask questions directly to experts, practice with fellow learners and get feedback, see how proficient speakers use the language, just to name a few. Besides, even if you can learn completely by yourself, you never know how fluent you actually are before you use it with other people."

    H.div B.! A.id "is-lojban-hard" B.! A.class_ (H.textValue "link-hook") $ do
        H.h2 $ H.toHtml ("Is Lojban hard to learn?" :: T.Text)
        H.p $ H.toHtml ("As a constructed language, Lojban is easier to learn than natural languages because of its regular grammar and small vocabulary. A commonly given figure is that it takes a complete beginner a few months to become a proficient user. And while Lojban is not as minimalist as Toki Pona, it is possible to reach proficiency with a vocabulary of 700~800 words." :: T.Text)
        H.p $ do 
            "The grammar may feel unfamiliar at first glance, but its regular, rule-based syntax makes it easier in the long run. Besides, the grammar you'll need to get by in a group chat can be explained in just "
            H.a "a page of A4 paper"
                B.! A.href "/static/images/Primer2.pdf"
            ". Everything else is syntactic sugar. Actually, Lojban grammar is so easy that most Lojbanists find themselves having learned the grammar thoroughly before acquiring a working vocabulary."

    H.div B.! A.id "llm" B.! A.class_ (H.textValue "link-hook") $ do
        H.h2 $ H.toHtml ("Can Generative AIs/LLMs like ChatGPT be my guide/tutor to Lojban?" :: T.Text)
        H.p $ do
            "No. Generative AIs/LLMs find success in natural languages thanks to the massive amount of data available for training. They are effectively statistical models: they predict the most likely words that come after the text you give, kinda like the auto-completion in search bars, but more sophisticated. As a result, they just give you a response that is "
            H.i "probable"
            " based on the data they've been trained on, which is not necessarily "
            H.i "correct"
            "."
        H.p $ do
            "For smaller niches like Lojban, there isn't enough high-quality data to be trained on. Therefore, even basic questions can fall outside of the AI's training field and make it hallucinate. (Not to mention the possible inclusion of misunderstandings about Lojban if they just scraped the web for training data.) Moreover, since an LLM is just a "
            H.i "text predictor"
            ", the "
            H.i "content"
            " of an LLM's response can change drastically depending on how you ask the question. For example, you can get completely "
            H.a "different"
                B.! A.href "/static/images/GPT_log_1.png"
            " "
            H.a "answers"
                B.! A.href "/static/images/GPT_log_2.png"
            " for \"Can I drop the "
            H.code "cu"
            " in "
            H.code "lo xasli cu barda"
            "?\" (The correct answer: you can't) from GPT-4o by asking it in different ways."
        H.p $ do
            "Do not use generative AI or LLM to learn Lojban. Generative AIs and LLMs make frequent mistakes even for the basic questions, and you, as a beginner, cannot discern information from hallucination in their responses. Believing their responses wholesale will hinder further learning because of all the contradictions and hallucinations in them. We suggest you go to "
            H.a "the resource page"
                B.! A.href "https://lojban.io/resources/"
            " and use them as your reference, and join "
            H.a "the Lojban community"
                B.! A.href "https://discord.com/invite/dGP5A6Fpj7"
            ". Any answer you could want from an AI or LLM, you can get from the friendly Lojbanists in #ckule and other such places!"

    H.div B.! A.id "history" B.! A.class_ (H.textValue "link-hook") $ do
        H.h2 $ H.toHtml ("What is the history of Lojban?" :: T.Text)
        H.p $ do
            "Lojban's predecessor, "
            H.b "Loglan"
            ", was started by Dr. James Cooke Brown in 1955. It was created to explore a question rooted in the Sapir-Whorf hypothesis (a hypothesis that language influences worldview or cognition): "
            H.i "If someone speaks a language with a rigorously logical structure, does that influence their patterns of thought?"
            " Loglan gained attention after being introduced in the June 1960 issue of Scientific American, and it later appeared in a handful of science fiction works."
        H.p $ do
            "As the language developed, Brown began asserting copyright over its components and placed restrictions on how the community could modify or extend the language. In response, a group of users formed the Logical Language Group (LLG), and started a new, independent project — one that would become "
            H.b "Lojban"
            "."
        H.p "Lojban aimed to be culturally neutral, and the designers incorporated a wide range of features — some inspired by natural languages, others entirely original — to ensure it was not biased toward any existing linguistic tradition. As a result, Lojban's grammar became highly expressive and broad in scope; it's hard to find a grammatical feature that Lojban doesn't implement in some form."
        H.p $ do
            "In 1997, "
            H.i "The Complete Lojban Language"
            " was published, marking a major milestone: the specification of Lojban's baseline grammar. The language was then placed into a five-year \"freeze\" to give learners time to stabilize their understanding and usage of the language."
        H.p $ do
            "The freeze isn't the end of Lojban. By the end of the freeze, the community had grown more experienced and began exploring ways to improve and adapt the language. Several ideas were proposed, and some — most notably "
            H.b "xorlo"
            " (which adjusted how quantification works) and "
            H.b "dotside"
            " (which adjusted the morphology of names) — were incorporated into the official grammar."
        H.p "Fluent Lojban speakers began to emerge in the 2000s. Since then, the language has sustained a living tradition: many classic works of literature have been translated into Lojban; original songs and poems have been written; and everyday conversations, texts, and chats occur in Lojban. Most importantly, the language continues to thrive thanks to an active, collaborative community that maintains and develops it."

    H.div B.! A.id "cool-conlangs" B.! A.class_ (H.textValue "link-hook") $ do
        H.h2 $ H.toHtml ("What are some other cool constructed languages I can check out?" :: T.Text)
        H.p "Lojban belongs to a broader category of conlangs called \"engineered languages (engelangs)\". Engelangs are designed around a specific, objective goal, and such goal is often to test the limit of human languages. In Lojban, the goal is to have an entirely regular and unambiguous syntax, but as one of the earliest developed engelang, it also acquired much, much more during development. As a result, you get to get a bit of everything in Lojban."
        H.p "With that being said, if you like certain aspects of Lojban that it didn't explore fully, there are other languages that really push them into limits."
        H.ul $ do
            H.li $ do
                H.div $ do
                    "If you like the minute semantic distinction between "
                    H.code "pe"
                    ", "
                    H.code "po"
                    ", "
                    H.code "po'e"
                    ", or "
                    H.code "lo"
                    ", "
                    H.code "loi"
                    ", "
                    H.code "lo'i"
                    ", or "
                    H.code "mu'e"
                    ", "
                    H.code "pu'u"
                    ", "
                    H.code "za'i"
                    ", "
                    H.code "zu'o"
                    ", you can check out "
                    H.a "Ithkuil"
                        B.! A.href "https://www.ithkuil.net/"
                    " ("
                    H.a "community resources"
                        B.! A.href "https://yuorb.github.io/en/"
                    "), which tries to minimize "
                    H.i "semantic"
                    " ambiguity and reflect aspects in human cognition that don't necessarily show up in natural languages."
            H.li $ do
                H.div $ do
                    "On the other hand, if you think all those are too complicated and prefer simplicity instead, check out "
                    H.a "Toki Pona"
                        B.! A.href "https://tokipona.org/"
                    ", which only has 120 words (less than 1/10 of Lojban root words!), but still expressive enough to sustain a living literary tradition of a wide range of registers, from casual, everyday conversations to highly academic, technical topics."
            H.li $ do
                H.div $ do
                    "Lojban belongs to a subcategory of engelangs called \"logical languages (loglangs)\", which are designed to explore the relation between human language and formal logic. Lojban constitutes a basal branch of said category, having only an unambiguous syntax and semantics \"inspired by predicate logic\". Later loglangs explore deeper, taking advances in formal semantics, and impose more rigorous correspondence of language and logic. The most prominent of those is "
                    H.a "Toaq"
                        B.! A.href "https://toaq.net/"
                    ", which has an active community and several fluent speakers. There are other loglangs such as "
                    H.a "Xextan"
                        B.! A.href "https://github.com/Xextan"
                    " and "
                    H.a "Eberban"
                        B.! A.href "https://eberban.github.io/eberban/"
                    ", which can give you an insight of how the structure of human language and the structure of formal logic syncretize."
    H.hr
    H.div B.! A.id "common-misconceptions" B.! A.class_ (H.textValue "link-hook") $ do
        H.h2 $ H.toHtml ("Common Misconceptions of Lojban" :: T.Text)
    H.div B.! A.id "misconceptions-purpose" B.! A.class_ (H.textValue "link-hook") $ do
        H.h3 $ H.toHtml ("On Lojban's Purpose and Design" :: T.Text)
    H.div B.! A.id "misconception-newspeak" B.! A.class_ (H.textValue "link-hook") $ do
        H.h4 $ H.toHtml ("Misconception: Lojban is like Newspeak." :: T.Text)
        H.p $ do
            "This comparison stems from the idea that Lojban reduces ambiguity, much like Newspeak in "
            H.i "1984"
            " was designed to limit thought. But the resemblance is only superficial. Newspeak aims to restrict expression and control thought by eliminating words and concepts: It often intentionally removes nuance and conflates antonyms. \"It's a beautiful thing, the destruction of words.\" is a famous quote from the book where it's unclear if the speaker is being sarcastic because the word \"ugly\" was removed from the language. Such conflation is unacceptable in Lojban, as Lojban is designed to enable "
            H.i "more precise and expressive"
            " communication. Far from suppressing complex or subversive ideas, Lojban allows speakers to express nuances that natural languages often leave vague or ambiguous. It expands possibilities rather than narrowing them."
        H.p $ do
            H.b "In short: "
            "This misconception assumes Lojban limits thought, when it actually allows for greater nuance and clarity."

    H.div B.! A.id "misconception-program" B.! A.class_ (H.textValue "link-hook") $ do
        H.h4 $ H.toHtml ("Misconception: Lojban is a programming language, not a real language." :: T.Text)
        H.p $ do
            "Lojban is designed to be a "
            H.i "human"
            " language. That is, you can express everything you can express in English (or any other natural language) in Lojban. If you have ever used a programming language, you'd know that while programming language and human language both have \"language\" in their names, they are very dissimilar: We use human language to describe the world around us, and to communicate with other people; On the other hand, we use programming language to tell computers what to do (and in a hyper-specific way or they wouldn't understand). We don't talk about weather to computers (Well, at least before ChatGPT), just like we don't give a detailed algorithm to a human just to ask them to fetch a cup of water. Their domains of use are different. In this regard, Lojban is a full-fledged human language — we use Lojban to talk about weather, to tell stories, to express our emotions… just like how we use English."
        H.p $ do
            "While Lojban's grammatical structure is easy for computers to parse, their intent is to help computers understand and use the language, just like how programming languages use English keywords (like "
            H.code "if"
            ", "
            H.code "while"
            ", "
            H.code "float"
            "…) to help programmers understand them. Thinking Lojban is a programming language just because its syntax is parsable by computers is like thinking English is a programming language because the keywords of most programming languages are in English."
        H.p $ do
            H.b "In short: "
            "Lojban is a human language, used by humans to talk with other people; Its logical structure makes it easier for computers to parse, just like programming languages use English keywords to make it easier for humans to understand."

    H.div B.! A.id "misconception-perfect" B.! A.class_ (H.textValue "link-hook") $ do
        H.h4 $ H.toHtml ("Misconception: Lojban is made to be a 'perfect language'." :: T.Text)
        H.p "Some early materials (and enthusiastic fans) may paint Lojban as the ultimate solution to linguistic ambiguity or miscommunication. In practice, Lojban still has quirks, debates over usage, and evolving norms — just like any other language. It's an experiment as much as it is a tool."
        H.p $ do 
            "What does it mean to be \"perfect\"? \"Perfect\" means without flaw, and thus cannot be made any better. This is not the case for Lojban. The most authoritative book on Lojban (for now), "
            H.i $ do
                H.a "The Complete Lojban Language"
                    B.! A.href "https://lojban.org/publications/cll/cll_v1.1_xhtml-section-chunks/"
            ", acknowledges that Lojban is currently in an incomplete state. In various sections, it invites future people to continue discovering more usages, and it leaves many unresolved issues in the grammar for the future people to settle about. (The most prominent examples are Section "
            H.a "13.16"
                B.! A.href "https://lojban.org/publications/cll/cll_v1.1_xhtml-section-chunks/section-attitudinals-conclusion.html"
            " and Section "
            H.a "16.15"
                B.! A.href "https://lojban.org/publications/cll/cll_v1.1_xhtml-section-chunks/section-logic-conclusion.html"
            ".) The notion of Lojban being \"perfect\" — that is, unimprovable in any way and thus has to stay this way forever — not only kills the language's vitality, but actually goes against what the authors of the CLL envision Lojban to be."
        H.p "It's not possible for anything to be \"perfect\", anyway: Different people have different preferences, a brillancy for one can be a design flaw for another — it's just impossible to be something everyone likes. And even a single person's preferences constantly change over time, experience, mood, etc. It's like there cannot be a \"perfect organism\" — each organism has its own way to adapt to its environment and fill its particular niche. (Or maybe you can say each organism is perfect in its own way.)"
        H.p $ do 
            "Lojban isn't perfect, and it can never be — because the notion that a language can be \"perfect\" is a misguided one. However, we the Lojban community practice Lojban, make friends with it, create original contents in it, experiment with it, have fun together in it, and work together to make a better Lojban — and "
            H.i "that"
            ", is perfect."
        H.p $ do
            H.b "In short: "
            "Nothing can be perfect, and Lojban is no exception. The notion of a language 'being perfect' "
            H.i "kills"
            " it. Like every living language, Lojban evolves to adapt to its environment, and we the community strives to make Lojban lively (and better)."

    H.div B.! A.id "misconception-surveillance" B.! A.class_ (H.textValue "link-hook") $ do
        H.h4 $ H.toHtml ("Misconception: Lojban promotes mass surveillance because it makes human communication parsable by computers." :: T.Text)
        H.p $ do
            "This is a misunderstanding of what it means for a language to be "
            H.i "parsable"
            ". Lojban is designed to be syntactically unambiguous, which means computers can reliably determine sentence structure. But "
            H.i "syntactic"
            " clarity doesn't automatically mean "
            H.i "semantic"
            " understanding. Consider the following sentence: "
            H.b "The clendits larthed a scoll, but it tamberly stened, preverizing them."
            " The syntax of the sentence is clear, but without knowing the meaning of the words, we can't really make sense of the sentence. This is how a parser sees it when parsing Lojban texts, since it has no knowledge of the Lojban words' meaning."
        H.p "Moreover, natural languages like English are already extensively parsed by computers every day. Technologies like search engines, speech recognition, and large language models (LLMs) demonstrate that machines can process English — often more effectively than Lojban — because of the massive amounts of available data and decades of research dedicated to it. The idea that Lojban somehow gives machines an edge is misleading when English already dominates the field of machine parsing."
        H.p "Lastly, while ambiguity in grammar does make it harder for a computer to parse — it isn't going to help against surveillance. Most syntactic ambiguities can be resolved by context, so a computer can still easily understand it by just looking around at the other sentences. Especially since we have LLMs now. Besides… If someone really wants to do mass surveillance, do you think they will stop at a syntactically ambiguous sentence or two?"
        H.p $ do
            H.b "In short: "
            "Being syntactically clear doesn't make Lojban more vulnerable to surveillance — English is already widely machine-parsed due to massive data and research, and ambiguity offers no real protection."

    H.div B.! A.id "misconception-emotionless" B.! A.class_ (H.textValue "link-hook") $ do
        H.h4 $ H.toHtml ("Misconception: Lojban is emotionless or robotic." :: T.Text)
        H.p "This misconception stems from two sources: the misconception that Lojban is a programming language, and the confusion over the word \"logical\"."
        H.ul $ do
            H.li $ do
                H.div $ do
                    "Programming languages are emotionless — because being emotional to computers isn't going to help them do the job. But Lojban is a human language, and humans are emotional. An English sentence that elicits emotions from a human will elicit the same emotion when translated into other languages: There's not much of a difference between "
                    H.code ".oi sai le mi birka cu se cortu"
                    " and \"Damn! My arm's hurting.\" in terms of emotion."
            H.li $ do
                H.div "The \"logical\" in Lojban means the rigorous syntactic structure of the sentences, and implies nothing about semantic concepts the language can or cannot express. Assuming that logic and expressiveness are in conflict in the first place is mistaken. Furthermore, the idea that logic and emotion must be mutually exclusive is in itself a misconception: Both are indispensable parts of our minds, and modern neuroscience has shown that they two work together, rather than in conflict, in our minds."
        H.p "Lojban acknowledges that humans are emotional, and rather than suppressing them with logical structure (which this misconception wrongly assumes), it provides speakers a whole section of grammar to express them: the attitudinal system. Because emotions are often spontaneous and occur outside of the rational thoughts, Lojban gives the attitudinal system a flexible grammar that doesn't interfere with the rigorous sentence structure, allowing people to freely express their emotions and/or attitudes on various things in speech. Therefore, you can be as emotional as you want in Lojban, and you can express your emotions directly, explicitly, and precisely."
        H.p $ do
            H.b "In short: "
            "Every human language is by nature emotional, because humans are emotional. Lojban acknowledges the need of expressing emotions and provides an explicit and expressive system for emotions and attitudes — often more precise than natural languages."

    H.div B.! A.id "misconception-alienish" B.! A.class_ (H.textValue "link-hook") $ do
        H.h4 $ H.toHtml ("Misconception: Lojban's grammar is so alien-ish, it is impossible to learn." :: T.Text)
        H.p $ do
            "This was the belief in the early days — when the language was still in development and no one spoke the language yet. Because no natural languages follow a rigorous structure and an unambiguous syntax, it was assumed no one could acquire Lojban, and since it is impossible to acquire… Why bother learning it? It became a self-fulfilling prophecy. The insistence of using Lojban-exclusive grammatical terms to emphasize how Lojban is \"different from all the natural languages\" (like "
            H.code "bridi"
            ", "
            H.code "selbri"
            ", "
            H.code "jufra"
            "…) exacerbated said belief."
        H.p $ do 
            "The emergence of fluent speakers in the 2000s disproved the belief, and it made clear that it was the "
            H.i "attitude towards the language"
            ", instead of the grammatical structure, that hindered people from acquiring the language. In hindsight, having a rigorous syntax doesn't preclude learnability at all: All programming languages follow a rigorous syntax, yet people learn them without any problem. And it is definitely possible to think in programming languages — Programmers do it every time they program. A semantics based on predicate logic likewise isn't a problem: Predicate logic is designed to model natural languages after all."
        H.p "It is very easy to paint something as \"alien-ish\" and/or \"impossible\" even when they are actually quite familiar and intuitive. And such misrepresentation often propagates only because of the propagators' lack of knowledge. Many beginners, when they started learning Lojban, reported that Lojban grammar is much more intuitive and easy to grasp than what they have heard from elsewhere. By the way, Lojbanists now prefer to explain Lojban grammar with their closest equivalent terms in English to illustrate the similarity of grammar between Lojban and natural languages."
        H.p $ do
            H.b "In short: "
            "Having a rigorous syntax isn't alien-ish and doesn't preclude learnability at all. The alien-ish grammatical terms for Lojban are historical artifacts, and there are now much more accessible approaches to understand Lojban grammar."

    H.div B.! A.id "misconceptions-expressiveness" B.! A.class_ (H.textValue "link-hook") $ do
        H.h3 $ H.toHtml ("On What You Can and Cannot Do in Lojban" :: T.Text)
    H.div B.! A.id "misconception-lying" B.! A.class_ (H.textValue "link-hook") $ do
        H.h4 $ H.toHtml ("Misconception: It is impossible to tell a lie in Lojban." :: T.Text)
        H.p $ do
            "This is a myth based on the idea that Lojban's structure forces logical precision. While Lojban does encourage clarity, it absolutely allows for lies, jokes, metaphors, and exaggerations. The language doesn't "
            H.i "prevent"
            " falsehoods — it just makes it easier to distinguish between claims, assumptions, and levels of certainty. You can still lie fluently in Lojban; you'll just be doing it with logical grammar."
        H.p $ do
            "Think mathematical formula: All mathematical formula follow a logical structure, yet there still are many formula that are false: "
            H.code "1 + 1 = 3"
            " is an example of a false formula. If you tell "
            H.code "1 + 1 = 3"
            " to an unsuspecting person, congratulations! You just told a lie with a rigorous logical structure, that of mathematics. What logical structure and unambiguous syntax do is to help resolving ambiguity between truth and falsehood: "
            H.code "6 / 2 (1 + 2) = 1"
            " can be either true or false depending on the operator precedence you observe, and only by imposing an unambiguous way to evaluate it can we tell if the formula is true or not."
        H.p $ do
            H.b "In short: "
            "Logical structure doesn't prevent falsehoods — it just helps distinguish truth from falsehood."

    H.div B.! A.id "misconception-translation" B.! A.class_ (H.textValue "link-hook") $ do
        H.h4 $ H.toHtml ("Misconception: It is impossible to translate <insert something here> into Lojban." :: T.Text)
        H.p "Lojban is designed to be able to express everything expressible with a human language. Sometimes, when a student has learned a grammar and runs across something the grammar doesn't apply, they assume it's impossible to express it in the language. What the student doesn't know is that there is more than one way to express the same thing. Also, if something doesn't work, some other technique will. It's just that the student hasn't learned the needed grammar yet. Lojban is no exception."
        H.p "Think of it like this: Say you are a student of English and you've learned the basic color terms like \"red\", \"green\", \"blue\" etc. and you see a color that doesn't fall neatly into one of them. Is it impossible to express the color in English? No, there are still multiple ways. For example, using less common color terms, using paraphrases like \"<noun>-colored\" or \"a color between <color> and <color>\"... you just haven't learned them yet. If you run across something you cannot express, do not assume it's impossible. Instead, find someone more knowledgeable in the language than you, and ask them how they would express it instead. This is true for any language."
        H.p $ do
            "Sometimes, Lojban lacks idioms or direct equivalents for many culturally-specific concepts. This is not limited to Lojban, actually: Many Chinese "
            H.i "Chengyus"
            " (four-character idioms) don't have their equivalents in English. However, just like the natural languages, what Lojban offers is "
            H.i "expressive paraphrasing"
            ". You might not be able to translate a phrase word-for-word, but you can almost always render its meaning clearly and faithfully. The flexibility of Lojban's syntax and vocabulary actually enables powerful translation strategies — it just sometimes takes a bit of creativity and explanation. And this is true for any language."
        H.p "Lastly, things based on the structure of the language itself are by definition untranslatable: This includes puns, double meanings of a word, and syntactic ambiguities. They are untranslatable, not only to Lojban, but also to other languages in general. For example, while it's obvious that \"I see the girl with a telescope\" cannot be translated into Lojban while retaining its double meaning (since it relies on a syntactic ambiguity), it is not translatable into Chinese either because Chinese use different words and different constructions for the two different meanings of \"with\". When you encounter a sentence like this, it's best to take a step back, look at the context of the sentence, and ask yourself: Is the pun/double meaning/syntactic ambiguity really needed under this context? What effect does it have, and if I remove it, does the whole thing still make sense? Can I get the same effect using the particularities in the target language? You can often find some other ways that achieve the same effect. And this is true for any language."
        H.p "Translation is difficult, as it is often about striking a balance between the different contexts of the source language and the target language. It requires a deep understanding of the vocabulary, grammar and underlying culture to both the source and the target language, and the answer is never \"this is impossible to translate\" nor is it as simple as \"<language> would always do X here\". That's why translation is a profession and translator is an occupation. Of course you don't need to be a professional translator to translate something into Lojban, however, when translating into Lojban, keep an open mind and don't get predisposed to what is \"possible\" and what is \"impossible\". This is true for any language."
        H.p $ do
            H.b "In short: "
            "There are no simple answers like \"It's impossible\" or \"It's always translated as X\" in translation. Think of how it works under the bigger context, and ask someone more knowledgeable for how they would do it."

    H.div B.! A.id "misconception-creative" B.! A.class_ (H.textValue "link-hook") $ do
        H.h4 $ H.toHtml ("Misconception: It is impossible to be creative in Lojban because speaking logically and rigorously kills imagination." :: T.Text)
        H.p $ do
            "This comes from an improper understanding of the terms \"logical\" and \"rigorous\". Lojban is not meant to be a language of strict emotionlessness, and the \"rigor\" Lojban uses is not restricting "
            H.i "what"
            " you can say, or "
            H.i "how"
            " you can say it. It's just preventing ambiguity. Ambiguity is largely unassociated with imagination."
        H.p $ do
            "On the contrary, Lojban can be a playground for creativity. Its logical grammar doesn't restrict style — it provides new tools for poetic structure, invented metaphors, experimental storytelling, and unusual perspectives. Lojban's highly flexible word order and grammatical marking mean that a huge variety of universally-understandable idiolects are possible, as well as faithfully representing the meaning of natural language sentences in translations. Creativity in Lojban may look different from English, but it's very much alive. In fact, the need to think outside of familiar linguistic patterns often "
            H.i "stimulates"
            " rather than stifles imagination. On the other hand, there are also many things you can express in Lojban that cannot be said in any other language, does that mean natural languages stifle creativity?"
        H.p $ do
            "Think poetry: Poems are subject to various meters, rhymes... that restrict the types and forms of sentences a poet can use. Does that kill creativity? Absolutely not. In fact, some of the most imaginative works in literary history are subject to the most restrictive meters, for example, "
            H.i "Divine Comedy"
            " by Dante. These restrictions actually free up thoughts, because when you cannot use your everyday, familiar sentence structures, you are forced to think outside of the box — and thus, these restrictions actually force you to be creative. Speaking with logic and rigor is no different in this regard."
        H.p $ do
            H.b "In short: "
            "Lojban actually opens up new avenues for creativity, especially in poetry and metaphor."

    H.div B.! A.id "misconception-wordplay" B.! A.class_ (H.textValue "link-hook") $ do
        H.h4 $ H.toHtml ("Misconception: It is impossible to do wordplays or puns in Lojban." :: T.Text)
        H.p "While Lojban doesn't support the same kinds of puns based on irregularity or ambiguity common in natural languages, it opens the door to different kinds of wordplay. To illustrate this point, consider the following example: The Chinese language is rich in homophones. Many Chinese speakers believe punning is impossible in English, because English is much poorer in homophones. We know it's not true: Puns are very common in English, they just rely less on homophones and use other wordplay elements instead. The same is true for Lojban: While puns relying on homophones and syntactic ambiguities can't work, there are still many other ways — Lojban is particularly rich in near-homophones because of its morphology, for example. Sound correspondences, like alliteration and rhyming, are another common type of wordplay that can be achieved in Lojban: Lojban's flexible morphology enables you to create easily understandable words with different wordplay effects. Once you have a firm grasp of the grammar and vocabulary in Lojban, you can always find ways to bend them (while still maintaining its grammatical correctness) to create humorous effects, often in ways unthinkable in English or other natural languages. This is true for any language."
        H.p $ do
            H.b "In short: "
            "While Lojban doesn't support the same kinds of puns based on irregularity or ambiguity common in natural languages, it supports a different style of wordplay through morphology, neologisms, and playful structure."

    H.div B.! A.id "misconception-figurative" B.! A.class_ (H.textValue "link-hook") $ do
        H.h4 $ H.toHtml ("Misconception: You can't be figurative in Lojban." :: T.Text)
        H.p "Lojban prefers to describe in a literal way because different languages contain different metaphor systems. Therefore, many figurative senses are culture or language-specific, and if you use them to people who speak a different language and/or with a different cultural background, you risk being misunderstood. For example, \"put on airs\" is a figurative speech in English, which literally doesn't make sense in Chinese. The Chinese equivalent of the English saying translates to \"put on racks\", which uses a different metaphor."
        H.p $ do
            "What if you really need to be figurative? Maybe there isn't a good existing word for description, or maybe being figurative makes for a more lively description. Yes, Lojban allows figurative speech too. Many figurative expressions mark themselves clearly: In the sentence \"I've been busy as a bee\", I don't claim myself to be a bee, because the \"as\" makes clear that it's a metaphor. (However, note that there "
            H.i "might"
            " be a culture where bees are seen as lazy animals, and if you say the sentence to people of such culture, they will interpret it very differently from what you intend.) Such constructions exist in Lojban as well. If the grammar doesn't make it clear, there is a way to explicitly mark figurative speech in Lojban: "
            H.code "pe'a"
            ", it tells the listener you're not using words in their literal meaning."
        H.p "Lojban doesn't ban figurative speech; They are avoided because a word/expression not used in its literal sense can be interpreted differently by people of different cultures. When talking in Lojban, think of the audience's cultural background (Lojban aims to be culturally neutral after all), and adjust your speech so they can understand."
        H.p $ do
            H.b "In short: "
            "You can be figurative in Lojban, and you can use "
            H.code "pe'a"
            " to mark them explicitly. However, be aware that figurative speeches may be interpreted differently by people of other cultures."

    H.div B.! A.id "misconception-jokes" B.! A.class_ (H.textValue "link-hook") $ do
        H.h4 $ H.toHtml ("Misconception: You can't joke in Lojban." :: T.Text)
        H.p $ do
            "This misconception assumes that Lojban's logical precision leaves no room for playful language. Yes, jokes relying on syntactic ambiguities cannot work in Lojban, but there are plenty of other ways to make room for surprise. For example, Lojban's logical precision does not force you to say "
            H.i "everything"
            " in the first go, allowing you to conceal some meaning and using them later as punchline. An example of a potential Lojban joke:"
        H.p $ do
            H.pre $ do
                H.code "- .i mi kaidji tu'a le tcati\n- .i le tcati cu zvati le jupku'a\n- ki'esai .i mi bazi daspo ty."
        H.p "Which translates as:"
        H.p $ do
            H.blockquote $ do
                "\"I want the tea\""
                H.br
                "\"The tea is in the kitchen\""
                H.br
                "\"Thanks. I will destroy it.\""
        H.p $ do
            "This joke uses the fact that "
            H.code "tu'a le tcati"
            " literally means \"an event of the tea doing something\". While in most contexts, it would be interpreted as something like \"getting the tea\" or \"drinking the tea\", in this case an unexpected interpretation is used, namely that something is to be destroyed. Note that this joke cannot be easily translated into natural languages because of the semantic particularities of "
            H.code "tu'a"
            " in Lojban."
        H.p $ do
            "The above is just one of the many possible types of jokes that work in Lojban. You can go to a joke collection and see what makes the jokes funny; As long as the joke doesn't rely on syntactic ambiguity, faulty logic and/or cultural-specific contexts, they still work in Lojban."
        H.p $ do
            H.b "In short: "
            "Most jokes don't rely on syntactic ambiguity or faulty logic, and thus still work in Lojban; In addition, Lojban's unique semantics gives room for jokes unseen in natural languages."

    H.div B.! A.id "misconceptions-community" B.! A.class_ (H.textValue "link-hook") $ do
        H.h3 $ H.toHtml ("On Community" :: T.Text)
    H.div B.! A.id "misconception-nobody" B.! A.class_ (H.textValue "link-hook") $ do
        H.h4 $ H.toHtml ("Misconception: Nobody speaks Lojban." :: T.Text)
        H.p $ do
            "While it's true that Lojban has a small speaker base, there is a dedicated and active community. People write literature, hold conversations, and develop tools in and for Lojban. "
            H.a "The resource page"
                B.! A.href "https://lojban.io/resources/"
            " showcases what the community has done over the years. In addition, there is a regular, weekly voice chat in the community. It's a living language in the sense that it's evolving and used, even if it's not spoken by millions."
        H.p "Lojban often gets associated with science fiction and geek culture, partly because it's a constructed language (conlang), and partly because of its logical structure, which appeals to people interested in formal systems like math, programming, or philosophy. However, the language itself isn't limited to any one subculture: Lojban appeals to different people for different reasons, and people here in the Lojban community come from a wide range of backgrounds. While the demographics of the Lojban community may differ from the general population, it is unfair to reduce the community to a single \"trait\", just like any other niche community."
        H.p $ do
            H.b "In short: "
            "Lojban has a small but active and supportive community, with ongoing conversations, literature, and tools."

    H.div B.! A.id "misconception-xekce" B.! A.class_ (H.textValue "link-hook") $ do
        H.h4 $ H.toHtml ("Misconception: Lojban speakers are pedants who enjoy picking on other people's grammar." :: T.Text)
        H.p "This misconception has an unfortunate basis in reality. The Lojban community was plagued with those kinds of behavior. We call those behaviors \"xekce\", and we eventually identified and got rid of them."
        H.p $ do
            "Some early materials and enthusiast fans paint Lojban as a \"perfect\" language, which in itself is a misconception (See "
            H.a "here"
                B.! A.href "#misconception-perfect"
            "). People who think Lojban is \"perfect\" tend to have a belief that every aspect of Lojban should go in their \"perfect\" way, and every sentence should be phrased \"perfectly\" (however they may define that), because that's how they see the \"perfect\" Lojban to be. Also, some people confuse Lojban's emphasis on precision and unambiguity in syntax with \"Only my interpretation is correct\". Those people are the exception, not the norm. Sadly, those people are generally the most motivated ones, and they are happy to pan every sentence that is not in their own \"perfect\" way. It only takes one or two people like that to kill all discussions."
        H.p $ do
            "When we eventually realized that those kinds of behavior stifles the community, we made another space, "
            H.a "roljbogu'e"
                B.! A.href "https://discord.com/invite/dGP5A6Fpj7"
            ", where clogging discussions just to correct other people's grammar is banned. The vast majority of Lojbanists jumped to roljbogu'e and became only active there, and now roljbogu'e enjoys much more activity than the original space. This shows how an (average) Lojbanist hates those kinds of behavior. Most Lojbanists are learners themselves and are excited to help others understand the language. Like any niche group, there may be debates and disagreements — but the majority of speakers are supportive and enthusiastic, not judgmental. Lojban is still growing, and now the community norms are flexible, not rigid or elitist."
        H.p $ do
            H.b "In short: "
            "While those people do exist, they are the exception, not the norm. The majority are friendly learners interested in exploring a unique language together."

    H.div B.! A.id "misconceptions-other" B.! A.class_ (H.textValue "link-hook") $ do
        H.h3 $ H.toHtml ("Other Misconceptions" :: T.Text)
       
    H.div B.! A.id "misconception-attitudinal" B.! A.class_ (H.textValue "link-hook") $ do
        H.h4 $ H.toHtml ("Misconception: Attitudinals are weird." :: T.Text)
        H.p "One of the goals of Lojban is correspondence in speaking and writing: Everything conveyed through speaking can be perfectly conveyed in writing, and vice versa. This is not the case for natural languages: Natural languages make heavy use of suprasegmental features like pauses or intonations for a variety of uses: giving syntactic clues, expressing attitudes… just to name a few. However, suprasegmental features are not faithfully represented in writing: we use punctuation marks, but they aren't enough of them to encompass the wide range of possible intonation and the meaning behind them. An exclamation mark can mean surprise, excitement, anger, and so on, which are conveyed through different intonations."
        H.p "Lojban tackled this issue by analyzing the meaning conveyed through intonations, and giving each meaning a phonetic form. This way, things originally conveyed through intonation only are put into words, and thus no meaning will be lost when written down."
        H.p "The advent of text messaging shows texts alone are inadequate for conveying thoughts with natural languages. Because most information about attitudes and intent are conveyed through intonation only, they are lost when put into text, making many texts look out of context. As a result, people added additional tags to express their attitude and intent — that is, emoticons, emojis and tone indicators. While they aren't considered a standard, proper part of the language, they play a significant role in online communication."
        H.p "While attitudinals in Lojban and emoticons/emojis/tone indicators in online messaging come from different roots, their purpose is the same: To convey meanings expressed only through tone in natural languages. As emoticons/emojis/tone indicators are now widespread and considered an important part of online communication, attitudinals, whose purposes and usages are the same, aren't any weirder and should be familiar to any internet users."
        H.p $ do
            H.b "In short: "
            "Attitudinals are effectively emojis and tone indicators incorporated into the grammatical part of the language. If you can use emojis and tone indicators, attitudinals are familiar to you."
    H.div B.! A.class_ (H.textValue "buttons") $ do
        H.a (H.toHtml ("Go to Resources" :: T.Text))
            B.! A.href (H.textValue "/resources")







