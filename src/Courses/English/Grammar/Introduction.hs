{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Courses.English.Grammar.Introduction (course) where

import Core
import Courses.Util.Vocabulary
import Courses.Util.Sentences
import Courses.Util.ExerciseGenerators
import Util (combineFunctions, combineFunctionsUniformly, generatorFromSingleton, generatorFromList, containsWord)
import Data.FileEmbed (embedStringFile)
import Control.Applicative ((<$>))
import qualified Data.Text as T
import qualified Text.Pandoc as P

--Interesting words: djica, sidju, jinga
-- introduce djica alongside questions: "I want you to be happy" / "Do you want me to be happy?" / "What do you want?" / "Who wants you to be happy" / "Who do you want to be happy?"

------- Lesson plans
plan1 :: P.Pandoc
Right plan1 = P.runPure $ P.readMarkdown P.def $ $(embedStringFile "courses/english/introduction/planning/1.md")

plan2 :: P.Pandoc
Right plan2 = P.runPure $ P.readMarkdown P.def $ $(embedStringFile "courses/english/introduction/planning/2.md")

plan3 :: P.Pandoc
Right plan3 = P.runPure $ P.readMarkdown P.def $ $(embedStringFile "courses/english/introduction/planning/3.md")

-------- Vocabulary
vocabularyGenerator1 :: VocabularyBuilder
vocabularyGenerator1 = createVocabularyBuilder
    -- Selbri
    [
        ("actions", ["tavla", "dunda"]),
        ("properties", ["prenu", "zdani", "mlatu"]),
        ("relations", ["pendo"])
    ]
    -- Sumti
    [
        ("genericPersons", ["mi", "do"]),
        ("genericPointable", ["ti", "ta"])
    ]

vocabularyGenerator2 :: VocabularyBuilder
vocabularyGenerator2 = createVocabularyBuilder
    -- Selbri
    [
        ("actions", ["tavla", "dunda", "ctuca"]),
        ("properties", ["prenu", "zdani", "mlatu", "gerku", "melbi"]),
        ("relations", ["nelci", "pendo"])
    ]
    -- Sumti
    [
        ("genericPersons", ["mi", "do", "lo prenu ku"]),
        ("semiGenericPersons", ["lo tavla ku", "lo se tavla ku", "lo dunda ku", "lo te dunda ku"]),
        ("animals", ["lo mlatu ku", "lo gerku ku"]),
        ("genericPointable", ["ti", "ta"]),
        ("places", ["lo zdani ku"]),
        ("subjects", ["lo zdani ku", "lo mlatu ku", "lo gerku ku", "lo se dunda ku"])
    ]

-- TODO: should "ctuca" really be here? does it denote any kind of teaching or only teaching in formal settings with an instructor and and audience?
vocabularyGenerator3 :: VocabularyBuilder
vocabularyGenerator3 = createVocabularyBuilder
    -- Selbri
    [
        ("actions", ["tavla", "dunda", "ctuca", "nupre", "cusku"]),
        ("properties", ["prenu", "zdani", "mlatu", "gerku", "melbi"]),
        ("relations", ["nelci", "pendo", "gleki"])
    ]
    -- Sumti
    [
        ("genericPersons", ["mi", "do", "lo prenu ku"]),
        ("semiGenericPersons", ["lo tavla ku", "lo se tavla ku", "lo dunda ku", "lo te dunda ku"]),
        ("animals", ["lo mlatu ku", "lo gerku ku"]),
        ("genericPointable", ["ti", "ta"]),
        ("places", ["lo zdani ku"]),
        ("subjects", ["lo zdani ku", "lo mlatu ku", "lo gerku ku", "lo se dunda ku"])
    ]

-------- Translations
translations1_nice :: ExerciseGenerator
translations1_nice = generateTranslationExercise basicSentenceCanonicalizer $ combineFunctionsUniformly [tavlaReflexive, dundaReordered] where
    tavlaReflexive = generatorFromList
        [ (["mi tavla mi"], ["I am talking to myself.", "I was talking to myself.", "We were talking to ourselves."])
        , (["do tavla do"], ["You are talking to yourself."])
        ]
    dundaReordered = generatorFromList
        [ (["do dunda ti mi"], ["You gave me this.", "You gave us this."])
        , (["mi dunda ta do"], ["I gave you that.", "We gave you that."])
        ]

translations1 :: ExerciseGenerator
translations1 = combineFunctions [(1, translations1_nice), (4, more_translations)] where
    more_translations = generateTranslationExercise basicSentenceCanonicalizer $ combineFunctionsUniformly $ others ++ [talkingWithSecondPerson, pendo, prenu, demonstrative] where
        talkingWithSecondPerson = generatorFromList
            [ (["mi tavla do"], ["I am talking to you.", "I was talking to you.", "We are talking to you.", "We were talking to you."])
            , (["do tavla mi"], ["You are talking to me.", "You are talking to us."])
            ]
        pendo = generatorFromList
            [ (["do pendo mi"], ["You are my friend."])
            , (["mi pendo do"], ["I am your friend."])
            ]
        prenu = generatorFromList
            [ (["mi prenu"], ["I am a person.", "We are persons."])
            , (["do prenu"], ["You are a person.", "You are persons."])
            ]
        demonstrative = generatorFromList
            [ (["ti mlatu"], ["This is a cat.", "These are cats."])
            , (["ta mlatu"], ["That is a cat.", "Those are cats."])
            , (["ta zdani"], ["That is a house.", "Those are houses."])
            ]
        others = generatorFromSingleton <$>
            -- not marked as "nice" because it becomes a special exercise in the next lesson ("translate without zo'e")
            [ (["mi tavla zo'e do"], ["I was talking about you.", "We were talking about you.", "I am talking about you.", "We are talking about you.", "I will talk about you.", "We will talk about you."])
            -- not marked as "nice" because it becomes a special exercise in the next lesson ("translate without zo'e")
            , (["mi dunda zo'e do"], ["I gave you something.", "I will give you something."])
            -- not marked as "nice" because the cannonical answer changes to "mi se zdani" in the next lesson
            , (["zdani mi"], ["I have a house.", "We have a house.", "We have houses."])
            ]

translations2_nice :: ExerciseGenerator
translations2_nice = combineFunctions [(1, restricted_translations), (5, normal_translations)] where
    special = combineFunctions [(2, talkingAbout), (1, gaveSomething)] where
        talkingAbout = generatorFromList
            [ (["mi tavla fi do"], ["I was talking about you.", "We were talking about you.", "I am talking about you.", "We are talking about you.", "I will talk about you.", "We will talk about you."])
            , (["tavla fi do"], ["Somebody was talking about you."])
            , (["mi tavla fi lo mlatu ku"], ["I was talking about the cat.", "I was talking about the cats.", "I am talking about the cat.", "I am talking about the cats."])
            , (["tavla fi lo mlatu ku"], ["Somebody was talking about the cat.", "Somebody was talking about the cats."])
            , (["mi tavla fi lo gerku ku"], ["I was talking about the dog.", "I was talking about the dogs.", "I am talking about the dog.", "I am talking about the dogs."])
            , (["tavla fi lo gerku ku"], ["Somebody was talking about the dog.", "Somebody was talking about the dogs."])
            ]
        gaveSomething = generatorFromList
            [ (["mi dunda fi do"], ["I gave you something.", "I will give you something."])
            ]
    normal_translations = generateTranslationExercise basicSentenceCanonicalizer $ combineFunctionsUniformly [special, beautifulToMe, beautifulGift, hasHouse, likedGift, giftingAnimal, others] where
        beautifulToMe = generatorFromList
            [ (["lo zdani ku melbi mi"], ["The house is beautiful to me.", "The houses are beautiful to me."])
            , (["lo mlatu ku melbi mi"], ["The cat is beautiful to me.", "The cats are beautiful to me."])
            , (["lo gerku ku melbi mi"], ["The dog is beautiful to me.", "The dogs are beautiful to me."])
            ]
        beautifulGift = generatorFromList
            [ (["lo se dunda ku melbi mi"], ["The gift is beautiful to me.", "The gifts are beautiful to me."])
            , (["lo se dunda ku melbi"], ["The gift is beautiful.", "The gifts are beautiful."])
            ]
        hasHouse = generatorFromList
            [ (["lo ctuca ku se zdani"], ["The instructor has a house."])
            , (["lo prenu ku se zdani"], ["The person has a house."])
            , (["lo tavla ku se zdani"], ["The speaker has a house."])
            , (["lo se tavla ku se zdani"], ["The listener has a house."])
            , (["lo ctuca ku se zdani"], ["The instructor has a house."])
            , (["lo dunda ku se zdani"], ["The donor has a house."])
            , (["lo te dunda ku se zdani"], ["The recipient has a house."])
            ]
        likedGift = generatorFromList
            [ (["lo te dunda ku nelci lo se dunda ku"], ["The recipient liked the gift.", "The recipient will like the gift.", "The recipients liked the gifts."])
            , (["lo ctuca ku nelci lo se dunda ku"], ["The instructor liked the gift.", "The instructor will like the gift."])
            ]
        giftingAnimal = generatorFromList
            -- mlatu
            [ (["mi dunda lo mlatu ku lo pendo ku"], ["I gave the cat to a friend.", "I gave the cats to a friend."])
            , (["mi te dunda lo mlatu ku"], ["I was given a cat.", "I was given cats.", "We were given a cat.", "We were given cats."])
            , (["dunda lo mlatu ku lo ctuca ku"], ["Somebody gave a cat to the instructor", "Somebody gave cats to the instructor"])
            , (["lo ctuca ku dunda lo mlatu ku mi"], ["The instructor gave me a cat.", "The instructor gave me cats.", "The instructor gave us a cat.", "The instructor gave us cats."])
            , (["dunda lo mlatu ku mi"], ["Somebody gave me a cat.", "Somebody gave me cats.", "Somebody gave us a cat.", "Somebody gave us cats."])
            -- gerku
            , (["mi dunda lo gerku ku lo pendo ku"], ["I gave the dog to a friend.", "I gave the dogs to a friend."])
            , (["mi te dunda lo gerku ku"], ["I was given a dog.", "I was given dogs.", "We were given a dog.", "We were given dogs."])
            , (["dunda lo gerku ku lo ctuca ku"], ["Somebody gave a dog to the instructor", "Somebody gave dogs to the instructor"])
            , (["lo ctuca ku dunda lo gerku ku mi"], ["The instructor gave me a dog.", "The instructor gave me dogs.", "The instructor gave us a dog.", "The instructor gave us dogs."])
            , (["dunda lo gerku ku mi"], ["Somebody gave me a dog.", "Somebody gave me dogs.", "Somebody gave us a dog.", "Somebody gave us dogs."])
            ]
        others = generatorFromList
            [ (["lo tavla ku pendo mi"], ["The speaker is my friend.", "The speakers are my friends."]) -- is "speaker" a good choice? maybe it implies voice...
            , (["mi nelci lo dunda ku"], ["I like the donor.", "I like the donors."])
            , (["mi nelci lo xe ctuca ku"], ["I like the teaching method."])
            , (["lo se tavla ku prenu"], ["The listener is a person.", "The listeners are persons."]) -- is "listener" a good choice?
            ]
    restricted_translations = generateRestrictedTranslationExercise "Translate without using \"zo'e\"" (not . containsWord (T.pack "zo'e")) basicSentenceCanonicalizer special

translations2 :: ExerciseGenerator
translations2 =  combineFunctions [(1, translations1_nice), (8, translations2_nice), (4, translations2)] where
    t1 =  generateTranslationExercise basicSentenceCanonicalizer <$> [talkingToAnimal, teachingSecondPerson, likingAnimals, beautiful, others] where
        talkingToAnimal = generatorFromList
            [ (["lo prenu ku tavla lo mlatu ku"], ["A person is talking to a cat.", "The person talks to cats."])
            , (["lo prenu ku tavla lo gerku ku"], ["A person is talking to a dog.", "The person talks to dogs."])
            ]
        teachingSecondPerson = generatorFromList
            [ (["mi ctuca do"], ["I will teach you.", "We will teach you.", "I taught you.", "We taught you."])
            , (["do ctuca mi"], ["You will teach me.", "You will teach us.", "You taught me.", "You taught us."])
            ]
        likingAnimals = generatorFromList
            [ (["mi nelci lo mlatu ku"], ["I like the cat.", "I like cats."])
            , (["mi nelci lo gerku ku"], ["I like the dog.", "I like dogs."])
            , (["lo gerku ku nelci lo mlatu ku"], ["The dog likes the cat.", "Dogs like cats."])
            , (["lo mlatu ku nelci lo gerku ku"], ["The cat likes the dog.", "Cats like dogs."])
            ]
        beautiful = generatorFromList
            [ (["lo zdani ku melbi"], ["The house is beautiful.", "The houses are beautiful."])
            , (["lo se dunda ku melbi"], ["The gift is beautiful.", "The gifts are beautiful."])
            , (["lo mlatu ku melbi"], ["The cat is beautiful.", "The cats are beautiful."])
            , (["lo gerku ku melbi"], ["The dog is beautiful.", "The dogs are beautiful."])
            ]
        others = generatorFromList
            [ (["ctuca mi"], ["Somebody taught me.", "Somebody taught us."])
            , (["mi se zdani"], ["I have a house.", "We have a house."])
            , (["do melbi mi"], ["You are beautiful to me."])
            , (["do melbi"], ["You are beautiful."])
            ]

-- Are the sentences involving tavla really sensible?
-- Teach nu and du'u, but not su'u
-- words common enough: gleki, tavla, dunda, nelci, mlatu, gerku, prenu, nupre, zdani
-- pending words: ctuca, melbi, pendo?
-- consider using: morji, ciksi, jijnu (useful for teaching du'u)
-- TODO: teach ko'a?
-- TODO: nonuniform translations -- separate by selbri and then separate even more
-- TODO: programmatic translation generation
-- TODO: indicate optional words using parenthesis
translations3 :: ExerciseGenerator
translations3 = combineFunctionsUniformly $ generateTranslationExercise basicSentenceCanonicalizer <$> generatorFromSingleton <$>
    [ (["mi gleki lo nu do tavla mi fi lo mlatu ku kei ku"], ["I am happy that you talked to me about cats."])
    , (["mi gleki lo nu do pendo mi kei ku"], ["I am happy that you are my friend."])
    , (["mi gleki lo nu do dunda lo mlatu ku mi kei ku"], ["I am happy that you gave me the cat.", "I am happy that you gave me cats."])
    , (["mi gleki lo nu do dunda lo gerku ku mi kei ku"], ["I am happy that you gave me the dog.", "I am happy that you gave me dogs."])
    , (["mi gleki lo nu mi te dunda lo mlatu ku kei ku"], ["I am happy that I was given a cat.", "I am happy that I was given cats."])
    , (["mi gleki lo nu mi te dunda lo gerku ku kei ku"], ["I am happy that I was given a dog.", "I am happy that I was given dogs."])
    , (["mi gleki lo nu do tavla mi kei ku"], ["I am happy that you talked to me."])
    , (["mi gleki lo nu do ctuca mi kei ku"], ["I am happy that you taught me."])
    , (["mi gleki lo nu do gleki kei ku"], ["I am happy that you are happy."])
    , (["mi gleki lo nu mi prenu kei ku"], ["I am happy that I am a person."])
    , (["mi gleki lo nu do prenu kei ku"], ["I am happy that you are a person."])
    , (["mi gleki lo nu mi se zdani kei ku"], ["I am happy that I have a house."])
    , (["mi gleki lo nu do se zdani kei ku"], ["I am happy that you have a house."])
    , (["mi gleki lo nu do nelci mi kei ku"], ["I am happy that you like me.", "I am happy that you like us.", "We are happy that you like us."])
    , (["mi gleki lo nu lo prenu ku nelci mi kei ku"], ["I am happy that people like me.", "I am happy that people like us.", "We are happy that people like us."])
    , (["mi gleki lo nu lo prenu ku nelci do kei ku"], ["I am happy that people like you.", "We are happy that people like you."])
    , (["mi gleki lo nu lo te dunda ku pendo mi kei ku"], ["I am happy that the recipient is my friend."])
    , (["mi tavla fi lo nu do se zdani kei ku"], ["I talked about you having a house."])
    , (["do tavla fi lo nu lo mlatu ku nelci lo gerku ku kei ku"], ["You talked about cats liking dogs."])
    , (["do tavla fi lo nu lo gerku ku nelci lo mlatu ku kei ku"], ["You talked about dogs liking cats."])
    , (["do tavla mi lo nu lo gerku ku nelci lo mlatu ku kei ku"], ["You talked to me about dogs liking cats."])
    , (["do tavla mi lo nu lo mlatu ku nelci lo gerku ku kei ku"], ["You talked to me about cats liking dogs."])
    , (["mi tavla fi lo se nupre ku"], ["I talked about the promise."])
    , (["do tavla fi lo se nupre ku"], ["You talked about the promise."])
    , (["mi tavla do lo se nupre ku"], ["I talked to you about the promise.", "We talked to you about the promise."])
    , (["do tavla mi lo se nupre ku"], ["You talked to me about the promise.", "You talked to us about the promise."])
    , (["lo nupre ku tavla mi"], ["The promisor talked to me.", "The promisor talked to us."])
    , (["lo nupre ku tavla fi lo mlatu ku"], ["The promisor talked about the cat.", "The promisor talked about the cats."])
    , (["lo nupre ku tavla fi lo gerku ku"], ["The promisor talked about the dog.", "The promisor talked about the dogs."])
    , (["lo nupre ku tavla fi lo zdani ku"], ["The promisor talked about the house.", "The promisor talked about the houses."])
    , (["lo nupre ku tavla mi lo mlatu ku"], ["The promisor talked to me about the cat.", "The promisor talked to me about the cats."])
    , (["lo nupre ku tavla mi lo gerku ku"], ["The promisor talked to me about the dog.", "The promisor talked to me about the dogs."])
    , (["lo nupre ku tavla mi lo zdani ku"], ["The promisor talked to me about the house.", "The promisor talked to me about the houses."])
    , (["mi nelci lo nu tavla do kei ku", "mi nelci lo nu mi tavla do kei ku"], ["I like to talk to you."]) -- is nelci really adequate?
    , (["do nelci lo nu nupre kei ku", "do nelci lo nu do nupre kei ku"], ["You like to make promises."]) -- is nelci really adequate?
    , (["lo prenu ku nelci lo nu nupre kei ku"], ["People like to make promises."]) -- is nelci really adequate?
    , (["do nupre lo nu dunda lo mlatu ku kei ku", "do nupre lo nu do dunda lo mlatu ku kei ku"], ["You promised to donate the cat.", "You promised to donate the cats."])
    , (["do nupre lo nu dunda lo gerku ku kei ku", "do nupre lo nu do dunda lo gerku ku"], ["You promised to donate the dog.", "You promised to donate the dogs."])
    , (["do nupre lo nu dunda lo mlatu ku mi kei ku", "do nupre lo nu do dunda lo mlatu ku mi kei ku"], ["You promised to donate the cat to me.", "You promised to donate the cats to me.", "You promised to donate the cats to us."])
    , (["do nupre lo nu dunda lo gerku ku mi kei ku", "do nupre lo nu do dunda lo gerku ku mi kei ku"], ["You promised to donate the dog to me.", "You promised to donate the dogs to me.", "You promised to donate the dogs to us."])
    , (["do nupre lo nu dunda lo mlatu ku kei ku mi", "do nupre lo nu do dunda lo mlatu ku kei ku mi"], ["You promised me to donate the cat.", "You promised me to donate the cats.", "You promised us to donate the cat.", "You promised us to donate the cats."])
    , (["do nupre lo nu dunda lo gerku ku kei ku mi", "do nupre lo nu do dunda lo gerku ku kei ku mi"], ["You promised me to donate the dog.", "You promised me to donate the dogs.", "You promised us to donate the dog.", "You promised us to donate the dogs."])
    , (["do nupre lo nu dunda lo zdani ku kei ku mi", "do nupre lo nu do dunda lo zdani ku kei ku mi"], ["You promised me to donate the house.", "You promised me to donate the houses.", "You promised us to donate the house.", "You promised us to donate the houses."])
    , (["do nupre lo nu dunda lo zdani ku mi kei ku mi", "do nupre lo nu do dunda lo zdani ku mi kei ku mi"], ["You promised to donate the house to me.", "You promised to donate the houses to us."])
    , (["do nupre lo nu pendo kei ku", "do nupre lo nu do pendo kei ku"], ["You promised to be friendly."])
    , (["do nupre lo nu ctuca mi kei ku", "do nupre lo nu do ctuca mi kei ku"], ["You promised to teach me."])
    , (["mi nupre lo nu ctuca do kei ku", "mi nupre lo nu mi ctuca do kei ku"], ["I promised to teach you."])
    , (["lo prenu ku nupre"], ["People make promises."])
    , (["do nupre"], ["You made a promise."])
    , (["do nupre fi mi"], ["You promised me.", "You promised us."])
    , (["mi nupre fi do"], ["I promised you.", "We promised you."])
    , (["mi cusku"], ["I said something.", "I was saying something.", "I will say something."])
    , (["do cusku"], ["You said something.", "You were saying something."])
    , (["lo prenu ku cusku"], ["The person said something.", "The person was saying something."])
    , (["mi cusku lo se du'u do melbi kei ku"], ["I said that you are beautiful."])
    , (["mi cusku lo se du'u lo prenu ku melbi kei ku"], ["I said that the person is beautiful."])
    , (["do cusku lo se du'u mi melbi kei ku"], ["You said that I am beautiful."])
    , (["do cusku lo se du'u lo prenu ku melbi kei ku"], ["You said that the person is beautiful."])
    , (["mi cusku lo se du'u mi nelci do kei ku", "mi cusku lo se du'u nelci do kei ku"], ["I said that I like you."])
    , (["do cusku lo se du'u do nelci mi kei ku", "do cusku lo se du'u nelci mi kei ku"], ["You said that you like me."])
    , (["lo prenu ku cusku lo se du'u mi nelci do kei ku"], ["The person said that I like you."])
    , (["lo prenu ku cusku lo se du'u do nelci mi kei ku"], ["The person said that you like me."])
    , (["lo prenu ku cusku lo se du'u nelci mi kei ku"], ["The person said that she likes me."])
    , (["lo prenu ku cusku lo se du'u nelci do kei ku"], ["The person said that she likes you."])
    , (["mi cusku lo se du'u mi dunda lo mlatu ku kei ku", "mi cusku lo se du'u dunda lo mlatu ku kei ku"], ["I said that I would donate the cat.", "I said that I would donate the cats."])
    , (["do cusku lo se du'u do dunda lo mlatu ku kei ku", "do cusku lo se du'u dunda lo mlatu ku kei ku"], ["You said that you would donate the cat.", "You said that you would donate the cats."])
    , (["lo prenu ku cusku lo se du'u dunda lo mlatu ku kei ku"], ["The person said said that she would donate the cat.", "The person said that she would donate the cats."])
    , (["mi cusku lo se du'u mi dunda lo mlatu ku do kei ku", "mi cusku lo se du'u dunda lo mlatu ku do kei ku"], ["I said that I would give you the cat.", "I said that I would give you the cats."])
    , (["do cusku lo se du'u do dunda lo mlatu ku mi kei ku", "do cusku lo se du'u dunda lo mlatu ku mi kei ku"], ["You said that you would give me the cat.", "You said that you would give me the cats."])
    , (["mi cusku lo se du'u mi dunda lo gerku ku do kei ku", "mi cusku lo se du'u dunda lo gerku ku do kei ku"], ["I said that I would give you the dog.", "I said that I would give you the dogs."])
    , (["do cusku lo se du'u do dunda lo gerku ku mi kei ku", "do cusku lo se du'u dunda lo gerku ku mi kei ku"], ["You said that you would give me the dog.", "You said that you would give me the dogs."])
    , (["lo prenu ku cusku lo se du'u dunda lo mlatu ku mi kei ku"], ["The person said that she would give me the cat.", "The person said that she would give me the cats."])
    , (["lo prenu ku cusku lo se du'u dunda lo mlatu ku do kei ku"], ["The person said that she would give you the cat.", "The person said that she would give you the cats."])
    , (["lo prenu ku cusku lo se du'u dunda lo gerku ku mi kei ku"], ["The person said that she would give me the dog.", "The person said that she would give me the dogs."])
    , (["lo prenu ku cusku lo se du'u dunda lo gerku ku do kei ku"], ["The person said that she would give you the dog.", "The person said that she would give you the dogs."])
    , (["lo prenu ku cusku lo se du'u do dunda lo mlatu ku mi kei ku"], ["The person said that you would give me the cat.", "The person said that you would give me the cats."])
    , (["lo prenu ku cusku lo se du'u do dunda lo gerku ku mi kei ku"], ["The person said that you would give me the dog.", "The person said that you would give me the dogs."])
    , (["mi cusku lo se du'u pendo kei ku", "mi cusku lo se du'u mi pendo kei ku"], ["I said that I would be friendly."])
    , (["do cusku lo se du'u pendo kei ku", "do cusku lo se du'u do pendo kei ku"], ["You said that you would be friendly."])
    , (["mi cusku lo se du'u do pendo kei ku"], ["I said that you would be friendly."])
    -- Wait until terminator ellision has been explained to use the following sentences
    {-, (["mi cusku lo se du'u mi nelci lo nu tavla do kei ku kei ku", "mi cusku lo se du'u mi nelci lo nu mi tavla do kei ku kei ku"], ["I said that I like to talk to you."])-}
    {-, (["mi cusku lo se du'u do nelci lo nu tavla mi kei ku kei ku", "mi cusku lo se du'u do nelci lo nu do tavla mi kei ku kei ku"], ["I said that you like to talk to me."])-}
    ]

translations4 :: [ExerciseGenerator]
translations4 = generateTranslationExercise basicSentenceCanonicalizer <$> generatorFromSingleton <$>
    [ (["lo prenu ku sutra tavla"], ["The person talks quickly.", "The person is talking quickly.", "A person is talking quickly.", "People talk quickly"])
    ]

--TODO: pause immediately after lesson 4


-------- Tanru
-- useful gismu: sutra, pelxu
-- lo melbi prenu, lo sutra mlatu, lo sutra gerku, lo gleki prenu, lo melbi prenu, mi mutce gleki
-------- Questions
-- useful gismu: melbi
-- xu [...] melbi do

-------- Exercises
exercises1 :: Dictionary -> ExerciseGenerator
exercises1 dictionary =
    combineFunctions
        [ (20, generateGrammaticalClassExercise vocabulary)
        , (15, generateBridiJufraExercise vocabulary displayBridi)
        , (20, generateSelbriIdentificationExercise vocabulary displayBridi)
        , (20, generateContextualizedGismuPlacePositionExercise dictionary vocabulary displayBridi)
        , (10, generateContextualizedGismuPlaceMeaningExercise dictionary vocabulary displayBridi)
        , (40, translations1)
        ]
    where
        vocabulary = vocabularyGenerator1 dictionary
        displayBridi = combineFunctions [(7, displayStandardSimpleBridi), (3, displayVariantSimpleBridi)]

exercises2 :: Dictionary -> ExerciseGenerator
exercises2 dictionary =
    combineFunctions
        [ (10, generateGrammaticalClassExercise vocabulary)
        , (10, generateBridiJufraExercise vocabulary displayBridi)
        , (10, generateSelbriIdentificationExercise vocabulary displayBridi)
        , (10, generateContextualizedGismuPlacePositionExercise dictionary vocabulary displayBridi)
        , (20, generateContextualizedGismuPlaceMeaningExercise dictionary vocabulary displayBridi)
        , (30, generateIsolatedGismuPlacesExercise dictionary vocabulary)
        , (50, translations2)
        ]
    where
        vocabulary = vocabularyGenerator2 dictionary
        displayBridi = combineFunctions [(7, displayStandardSimpleBridi), (2, displayVariantSimpleBridi), (1, displayReorderedStandardSimpleBridi)]

exercises3 :: Dictionary -> ExerciseGenerator
exercises3 dictionary =
    combineFunctions
        [ (5, generateGrammaticalClassExercise vocabulary)
        , (5, generateBridiJufraExercise vocabulary displayBridi)
        , (5, generateSelbriIdentificationExercise vocabulary displayBridi)
        , (5, generateContextualizedGismuPlacePositionExercise dictionary vocabulary displayBridi)
        , (20, generateContextualizedGismuPlaceMeaningExercise dictionary vocabulary displayBridi)
        , (30, generateIsolatedGismuPlacesExercise dictionary vocabulary)
        , (70, translations3)
        ]
    where
        vocabulary = vocabularyGenerator3 dictionary
        displayBridi = combineFunctions [(7, displayStandardSimpleBridi), (2, displayVariantSimpleBridi), (1, displayReorderedStandardSimpleBridi)]

-------- Lessons
lesson1 :: LessonBuilder
lesson1 dictionary = Lesson "Basics 1" (exercises1 dictionary) plan1

lesson2 :: LessonBuilder
lesson2 dictionary = Lesson "Basics 2" (exercises2 dictionary) plan2

lesson3 :: LessonBuilder
lesson3 dictionary = Lesson "Abstractions" (exercises3 dictionary) plan3

-------- Course
course :: CourseBuilder
course = createCourseBuilder "Introduction to Lojban for English speakers" [lesson1, lesson2, lesson3]
