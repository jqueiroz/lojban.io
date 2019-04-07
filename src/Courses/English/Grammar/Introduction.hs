{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Courses.English.Grammar.Introduction (course) where

import Core
import Courses.Util.Vocabulary
import Courses.Util.Sentences
import Courses.Util.ExerciseGenerators
import Util (combineFunctions, combineFunctionsUniformly, generatorFromSingleton, generatorFromList, generatorFromWeightedList)
import Data.FileEmbed (embedStringFile)
import Control.Applicative ((<$>))
import qualified Data.Text as T
import qualified Text.Pandoc as P

-- introduce djica alongside questions: "I want you to be happy" / "Do you want me to be happy?" / "What do you want?" / "Who wants you to be happy" / "Who do you want to be happy?"
-- TODO: remove the translations that make the least sense (in progress...)

-- TODO: programmatic translation generation
-- TODO: indicate optional words using parenthesis

-- Considerations
--   * is "speaker" a good choice? maybe it implies voice or authority...
-- TODO: consider adding some translations using observatives
-- TODO: cleanup all tenses before canonicalization (translations with incorrect tenses will be accepted, but this is likely a small price to pay in order to accept correct translations including tenses)
-- TODO: also accept "ma'a" (and similar terms) whenever "mi" is accepted

------- Lesson plans
plan1 :: P.Pandoc
Right plan1 = P.runPure $ P.readMarkdown P.def $ $(embedStringFile "courses/english/grammar/introduction/planning/1.md")

plan2 :: P.Pandoc
Right plan2 = P.runPure $ P.readMarkdown P.def $ $(embedStringFile "courses/english/grammar/introduction/planning/2.md")

plan3 :: P.Pandoc
Right plan3 = P.runPure $ P.readMarkdown P.def $ $(embedStringFile "courses/english/grammar/introduction/planning/3.md")

plan4 :: P.Pandoc
Right plan4 = P.runPure $ P.readMarkdown P.def $ $(embedStringFile "courses/english/grammar/introduction/planning/4.md")

plan5 :: P.Pandoc
Right plan5 = P.runPure $ P.readMarkdown P.def $ $(embedStringFile "courses/english/grammar/introduction/planning/5.md")

plan1to5 :: P.Pandoc
Right plan1to5 = P.runPure $ P.readMarkdown P.def $ ""

plan7 :: P.Pandoc
Right plan7 = P.runPure $ P.readMarkdown P.def $ $(embedStringFile "courses/english/grammar/introduction/planning/7.md")

-------- Vocabulary
vocabularyGenerator1 :: VocabularyBuilder
vocabularyGenerator1 = createVocabularyBuilder
    -- Selbri
    [
        ("actions", (1,) <$> ["tavla", "dunda"]),
        ("relations", (1,) <$> ["pendo"]),
        ("properties", (1,) <$> ["prenu", "zdani", "mlatu"])
    ]
    -- Sumti
    [
        ("genericPersons", (1,) <$> ["mi", "do"]),
        ("genericPointable", (1,) <$> ["ti", "ta"])
    ]

-- New words: ctuca; nelci; gerku, melbi
vocabularyGenerator2 :: VocabularyBuilder
vocabularyGenerator2 = createVocabularyBuilder
    -- Selbri
    [
        ("actions", ((1,) <$> ["tavla", "dunda"]) ++ ((2,) <$> ["ctuca"])),
        ("relations", ((1,) <$> ["pendo"]) ++ ((2,) <$> ["nelci"])),
        ("properties", ((1,) <$> ["prenu", "zdani", "mlatu"]) ++ ((2,) <$> ["gerku", "melbi"]))
    ]
    -- Sumti
    [
        ("genericPersons", (1,) <$> ["mi", "do", "lo prenu ku"]),
        ("semiGenericPersons", (1,) <$> ["lo tavla ku", "lo se tavla ku", "lo dunda ku", "lo te dunda ku"]),
        ("animals", (1,) <$> ["lo mlatu ku", "lo gerku ku"]),
        ("genericPointable", (1,) <$> ["ti", "ta"]),
        ("places", (1,) <$> ["lo zdani ku"]),
        ("subjects", (1,) <$> ["lo zdani ku", "lo mlatu ku", "lo gerku ku", "lo se dunda ku"])
    ]

-- New words: ciska, djuno
vocabularyGenerator3 :: VocabularyBuilder
vocabularyGenerator3 = createVocabularyBuilder
    -- Selbri
    [
        ("actions", ((0,) <$> ["tavla", "dunda"]) ++ ((1,) <$> ["ctuca"]) ++ ((2,) <$> ["ciska", "djuno"])),
        ("relations", (0,) <$> ["pendo", "nelci"]),
        ("properties", (0,) <$> ["prenu", "zdani", "mlatu", "gerku", "melbi"])
    ]
    -- Sumti
    [
        ("genericPersons", (1,) <$> ["mi", "do", "lo prenu ku"]),
        ("semiGenericPersons", (1,) <$> ["lo tavla ku", "lo se tavla ku", "lo dunda ku", "lo te dunda ku", "lo ciska ku"]),
        ("animals", (1,) <$> ["lo mlatu ku", "lo gerku ku"]),
        ("genericPointable", (1,) <$> ["ti", "ta"]),
        ("places", (1,) <$> ["lo zdani ku"]),
        ("subjects", (1,) <$> ["lo zdani ku", "lo mlatu ku", "lo gerku ku", "lo se dunda ku"])
    ]

-- New words: nupre, cusku; gleki
vocabularyGenerator4 :: VocabularyBuilder
vocabularyGenerator4 = createVocabularyBuilder
    -- Selbri
    [
        ("actions", ((0,) <$> ["tavla", "dunda"]) ++ ((1,) <$> ["ctuca", "ciska", "djuno"]) ++ ((2,) <$> ["nupre", "cusku"])),
        ("relations", ((0,) <$> ["pendo", "nelci"]) ++ ((1,) <$> ["gleki"])),
        ("properties", (0,) <$> ["prenu", "zdani", "mlatu", "gerku", "melbi"])
    ]
    -- Sumti
    [
        ("genericPersons", (1,) <$> ["mi", "do", "lo prenu ku"]),
        ("semiGenericPersons", (1,) <$> ["lo tavla ku", "lo se tavla ku", "lo dunda ku", "lo te dunda ku", "lo ciska ku"]),
        ("animals", (1,) <$> ["lo mlatu ku", "lo gerku ku"]),
        ("genericPointable", (1,) <$> ["ti", "ta"]),
        ("places", (1,) <$> ["lo zdani ku"]),
        ("subjects", (1,) <$> ["lo zdani ku", "lo mlatu ku", "lo gerku ku", "lo se dunda ku"])
    ]

-- New words: cu
vocabularyGenerator5 :: VocabularyBuilder
vocabularyGenerator5 = vocabularyGenerator4

-- New words: plise, vecnu, skami, pilno
vocabularyGenerator7 :: VocabularyBuilder
vocabularyGenerator7 = createVocabularyBuilder
    -- Selbri
    [
        ("actions", ((0,) <$> ["tavla", "dunda"]) ++ ((1,) <$> ["ctuca", "ciska", "djuno", "nupre", "cusku"]) ++ ((3,) <$> ["vecnu", "pilno"])),
        ("relations", ((0,) <$> ["pendo", "nelci", "gleki"])),
        ("properties", (0,) <$> ["prenu", "zdani", "mlatu", "gerku", "melbi", "plise", "skami"])
    ]
    -- Sumti
    [
    ]

-- Sentence comparer
sentenceComparer :: SentenceComparer
sentenceComparer x y = (length xs == length ys) && (all wordComparer $ zip xs ys) where
    xs = T.words x
    ys = T.words y
    wordComparer :: (T.Text, T.Text) -> Bool
    wordComparer (x, y) = (wordComparer' x y) || (wordComparer' y x)
    wordComparer' "nu" "su'u" = True
    wordComparer' "du'u" "su'u" = True
    wordComparer' x y = x == y

-------- Translations
-- Lesson 1
translations1_nice :: TranslationGenerator
translations1_nice = combineFunctionsUniformly [tavlaReflexive, dundaReordered] where
    tavlaReflexive = generatorFromList
        [ (["mi tavla mi", "mi tavla vo'a"], ["I am talking to myself.", "I was talking to myself.", "We were talking to ourselves."])
        , (["do tavla do", "do tavla vo'a"], ["You are talking to yourself."])
        ]
    dundaReordered = generatorFromList
        [ (["do dunda ti mi"], ["You gave me this.", "You gave us this."])
        , (["mi dunda ta do"], ["I gave you that.", "We gave you that."])
        ]

translations1_normal :: TranslationGenerator
translations1_normal = combineFunctionsUniformly $ others ++ [talkingWithSecondPerson, pendo, prenu, demonstrative, zdani] where
    talkingWithSecondPerson = generatorFromList
        [ (["mi tavla do"], ["I am talking to you.", "I was talking to you.", "We are talking to you.", "We were talking to you."])
        , (["do tavla mi"], ["You are talking to me.", "You are talking to us."])
        ]
    pendo = generatorFromList
        [ (["do pendo mi"], ["You are my friend."])
        , (["mi pendo do"], ["I am your friend."])
        ]
    prenu = generatorFromList
        [ (["mi prenu"], ["I am a person."])
        , (["do prenu"], ["You are a person."])
        ]
    demonstrative = generatorFromList
        [ (["ti mlatu"], ["This is a cat.", "These are cats."])
        , (["ta mlatu"], ["That is a cat.", "Those are cats."])
        , (["ta zdani"], ["That is a house.", "Those are houses."])
        ]
    zdani = generatorFromList
        -- not marked as "nice" because the cannonical answer changes to "mi se zdani" in the next lesson
        [ (["zdani mi"], ["I have a house.", "We have a house.", "We have houses."])
        -- not marked as "nice" because the cannonical answer changes to "do se zdani" in the next lesson
        , (["zdani do"], ["You have a house.", "You have houses."])
        ]
    others = generatorFromSingleton <$>
        -- not marked as "nice" because it becomes a special exercise in the next lesson ("translate without zo'e")
        [ (["mi tavla zo'e mi", "mi tavla zo'e vo'a"], ["I was talking about myself.", "We were talking about ourselves.", "I will talk about myself."])
        -- not marked as "nice" because it becomes a special exercise in the next lesson ("translate without zo'e")
        , (["mi tavla zo'e do"], ["I was talking about you.", "We were talking about you.", "I am talking about you.", "We are talking about you.", "I will talk about you.", "We will talk about you."])
        -- not marked as "nice" because it becomes a special exercise in the next lesson ("translate without zo'e")
        , (["mi dunda zo'e do"], ["I gave you something.", "I will give you something."])
        ]

translationExercises1_nice :: ExerciseGenerator
translationExercises1_nice = generateTranslationExercise basicSentenceCanonicalizer sentenceComparer translations1_nice

translationExercises1_normal :: ExerciseGenerator
translationExercises1_normal = generateTranslationExercise basicSentenceCanonicalizer sentenceComparer translations1_normal

translationExercises1 :: ExerciseGenerator
translationExercises1 = combineFunctions [(1, translationExercises1_nice), (4, translationExercises1_normal)]

-- Lesson 2
translations2_restricted :: TranslationGenerator
translations2_restricted = combineFunctions [(2, talkingAbout), (1, gaveSomething)] where
    talkingAbout = generatorFromList
        [ (["mi tavla fi mi", "mi tavla fi vo'a"], ["I was talking about myself.", "We were talking about ourselves.", "I will talk about myself."])
        , (["mi tavla fi do"], ["I was talking about you.", "We were talking about you.", "I am talking about you.", "We are talking about you.", "I will talk about you.", "We will talk about you."])
        , (["tavla fi mi"], ["Somebody was talking about me.", "Somebody was talking about us."])
        , (["tavla fi do"], ["Somebody was talking about you."])
        , (["mi tavla fi lo mlatu ku"], ["I was talking about the cat.", "I was talking about the cats.", "I was talking about cats", "I am talking about the cat.", "I am talking about the cats.", "I am talking about cats."])
        , (["tavla fi lo mlatu ku"], ["Somebody was talking about the cat.", "Somebody was talking about the cats.", "Somebody was talking about cats."])
        , (["mi tavla fi lo gerku ku"], ["I was talking about the dog.", "I was talking about the dogs.", "I was talking about dogs.", "I am talking about the dog.", "I am talking about the dogs.", "I am talking about dogs."])
        , (["tavla fi lo gerku ku"], ["Somebody was talking about the dog.", "Somebody was talking about the dogs.", "Somebody was talking about dogs."])
        ]
    gaveSomething = generatorFromList
        [ (["mi dunda fi do"], ["I gave you something.", "I will give you something."])
        , (["do dunda fi mi"], ["You gave me something."])
        ]

translations2_nice :: TranslationGenerator
translations2_nice = combineFunctions $ [(2, translations2_restricted), (2, teaching)] ++ ((1,) <$> [hasHouse, niceGift, giftingAnimal, friends, others]) where
    hasHouse = generatorFromList
        [ (["lo ctuca ku se zdani"], ["The instructor has a house."])
        , (["lo prenu ku se zdani"], ["The person has a house."])
        , (["lo tavla ku se zdani", "lo cusku ku se zdani"], ["The speaker has a house."])
        , (["lo se tavla ku se zdani"], ["The listener has a house."])
        , (["lo dunda ku se zdani"], ["The donor has a house."])
        , (["lo te dunda ku se zdani"], ["The beneficiary has a house."])
        ]
    niceGift = combineFunctionsUniformly [beautifulGift, likedGift] where
        beautifulGift = generatorFromList
            [ (["lo se dunda ku melbi mi"], ["The gift is beautiful to me.", "The gifts are beautiful to me."])
            , (["lo se dunda ku melbi"], ["The gift is beautiful.", "The gifts are beautiful."])
            ]
        likedGift = generatorFromList
            [ (["lo te dunda ku nelci lo se dunda ku"], ["The recipient liked the gift.", "The recipient will like the gift.", "The recipients liked the gifts."])
            , (["lo ctuca ku nelci lo se dunda ku"], ["The instructor liked the gift.", "The instructor will like the gift."])
            ]
    giftingAnimal = generatorFromList
        -- mlatu
        [ (["mi dunda lo mlatu ku lo pendo ku"], ["I gave the cat to a friend.", "I gave the cats to a friend."])
        , (["mi te dunda lo mlatu ku"], ["I was given a cat.", "We were given a cat."])
        , (["dunda lo mlatu ku lo ctuca ku"], ["Somebody gave a cat to the instructor", "Somebody gave the cat to the instructor.", "Somebody gave the cats to the instructor."])
        , (["lo ctuca ku dunda lo mlatu ku mi"], ["The instructor gave me a cat.", "The instructor gave me the cat.", "The instructor gave me the cats.", "The instructor gave us a cat.", "The instructor gave us the cat.", "The instructor gave us the cats."])
        , (["dunda lo mlatu ku mi"], ["Somebody gave me a cat.", "Somebody gave me the cat.", "Somebody gave me the cats.", "Somebody gave us a cat.", "Someboy gave us the cat.", "Somebody gave us the cats."])
        -- gerku
        , (["mi dunda lo gerku ku lo pendo ku"], ["I gave the dog to a friend.", "I gave the dogs to a friend."])
        , (["mi te dunda lo gerku ku"], ["I was given a dog.", "We were given a dog."])
        , (["dunda lo gerku ku lo ctuca ku"], ["Somebody gave a dog to the instructor", "Somebody gave the dog to the instructor.", "Somebody gave the dogs to the instructor."])
        , (["lo ctuca ku dunda lo gerku ku mi"], ["The instructor gave me a dog.", "The instructor gave me the dog.", "The instructor gave me the dogs.", "The instructor gave us a dog.", "The instructor gave us the dog.", "The instructor gave us the dogs."])
        , (["dunda lo gerku ku mi"], ["Somebody gave me a dog.", "Somebody gave me the dog.", "Somebody gave me the dogs.", "Somebody gave us a dog.", "Someboy gave us the dog.", "Somebody gave us the dogs."])
        ]
    teaching = generatorFromList
        [ (["mi ctuca lo mlatu ku"], ["I am teaching the cat.", "I am teaching the cats.", "I taught the cat.", "I taught the cats.", "We are teaching the cat.", "We are teaching the cats."])
        , (["mi ctuca lo gerku ku"], ["I am teaching the dog.", "I am teaching the dogs.", "I taught the dog.", "I taught the dogs.", "We are teaching the dog.", "We are teaching the dogs."])
        , (["mi ctuca do"], ["I will teach you.", "We will teach you.", "I taught you.", "We taught you."])
        , (["do ctuca mi"], ["You will teach me.", "You will teach us.", "You taught me.", "You taught us."])
        , (["mi ctuca mi", "mi ctuca vo'a"], ["I taught myself."])
        , (["ctuca mi"], ["Somebody taught me.", "Somebody taught us."])
        , (["ctuca do"], ["Somebody taught you."])
        ]
    friends = generatorFromList
        [ (["lo tavla ku pendo mi", "lo cusku ku pendo mi"], ["The speaker is my friend.", "The speakers are my friends."])
        , (["lo se tavla ku pendo mi"], ["The listener is my friend.", "The listeners are my friends."])
        , (["lo dunda ku pendo mi"], ["The donor is my friend.", "The donors are my friends."])
        , (["lo te dunda ku pendo mi"], ["The beneficiary is my friend.", "The beneficiaries are my friends."])
        , (["lo ctuca ku pendo mi"], ["The instructor is my friend.", "The instructors are my friends."])
        , (["lo tavla ku pendo", "lo cusku ku pendo"], ["The speaker is friendly.", "The speakers are friendly."])
        , (["lo se tavla ku pendo"], ["The listener is friendly.", "The listeners are friendly."])
        , (["lo dunda ku pendo"], ["The donor is friendly.", "The donors are friendly."])
        , (["lo te dunda ku pendo"], ["The beneficiary is friendly.", "The beneficiaries are friendly."])
        , (["lo ctuca ku pendo"], ["The instructor is friendly.", "The instructors are friendly."])
        ]
    others = generatorFromList
        [ (["mi nelci lo xe ctuca ku"], ["I like the teaching method."])
        , (["do nelci lo xe ctuca ku"], ["You like the teaching method."])
        ]

translations2_normal :: TranslationGenerator
translations2_normal = combineFunctionsUniformly [talkingToAnimal, likingAnimals, animalFriends, beautiful, person, others] where
        talkingToAnimal = generatorFromList
            [ (["lo prenu ku tavla lo mlatu ku"], ["A person is talking to a cat.", "The person talks to cats."])
            , (["lo prenu ku tavla lo gerku ku"], ["A person is talking to a dog.", "The person talks to dogs."])
            ]
        likingAnimals = generatorFromList
            [ (["mi nelci lo mlatu ku"], ["I like the cat.", "I like cats.", "We like cats."])
            , (["mi nelci lo gerku ku"], ["I like the dog.", "I like dogs.", "We like dogs."])
            , (["lo gerku ku nelci lo mlatu ku"], ["The dog likes the cat.", "Dogs like cats."])
            , (["lo mlatu ku nelci lo gerku ku"], ["The cat likes the dog.", "Cats like dogs."])
            ]
        animalFriends = generatorFromList
            [ (["lo mlatu ku pendo mi"], ["The cat is my friend."])
            , (["lo gerku ku pendo mi"], ["The dog is my friend."])
            , (["lo mlatu ku pendo"], ["The cat is friendly.", "Cats are friendly."])
            , (["lo gerku ku pendo"], ["The dog is friendly.", "Dogs are friendly."])
            ]
        beautiful = generatorFromList
            [ (["lo zdani ku melbi"], ["The house is beautiful.", "The houses are beautiful."])
            , (["lo mlatu ku melbi"], ["The cat is beautiful.", "The cats are beautiful.", "Cats are beautiful."])
            , (["lo gerku ku melbi"], ["The dog is beautiful.", "The dogs are beautiful.", "Dogs are beautiful."])
            , (["lo zdani ku melbi mi"], ["The house is beautiful to me.", "The houses are beautiful to me."])
            , (["lo mlatu ku melbi mi"], ["The cat is beautiful to me.", "The cats are beautiful to me.", "Cats are beautiful to me."])
            , (["lo gerku ku melbi mi"], ["The dog is beautiful to me.", "The dogs are beautiful to me.", "Dogs are beautiful to me."])
            , (["lo ctuca ku melbi"], ["The instructor is beautiful."])
            , (["lo ctuca ku melbi mi"], ["The instructor is beautiful to me."])
            ]
        person = generatorFromList
            [ (["lo tavla ku prenu"], ["The speaker is a person."])
            , (["lo se tavla ku prenu"], ["The listener is a person."])
            , (["lo dunda ku prenu"], ["The donor is a person."])
            , (["lo te dunda ku prenu"], ["The beneficiary is a person."])
            , (["lo ctuca ku prenu"], ["The instructor is a person."])
            ]
        others = generatorFromList
            [ (["mi se zdani"], ["I have a house.", "We have a house.", "We have houses."])
            , (["do se zdani"], ["You have a house.", "You have houses."])
            , (["mi nelci lo prenu ku"], ["I like people.", "We like people."])
            , (["lo prenu ku nelci mi"], ["People like me.", "People like us."])
            , (["do melbi mi"], ["You are beautiful to me."])
            , (["do melbi"], ["You are beautiful."])
            , (["mi nelci lo dunda ku"], ["I like the donor.", "I like the donors."])
            , (["mi nelci lo ctuca ku"], ["I like the instructor.", "I like the instructors."])
            , (["lo ctuca ku nelci mi"], ["The instructor likes me", "The instructors like me."])
            ]

translationExercises2_nice :: ExerciseGenerator
translationExercises2_nice = combineFunctions [(1, restricted), (5, nice)] where
    restricted = generateBlacklistedWordTranslationExercise (T.pack "zo'e") basicSentenceCanonicalizer sentenceComparer translations2_restricted
    nice = generateTranslationExercise basicSentenceCanonicalizer sentenceComparer translations2_nice

translationExercises2_normal :: ExerciseGenerator
translationExercises2_normal = generateTranslationExercise basicSentenceCanonicalizer sentenceComparer translations2_normal

translationExercises2 :: ExerciseGenerator
translationExercises2 =  combineFunctions [(1, translationExercises1_nice), (10, translationExercises2_nice), (5, translationExercises2_normal)]

-- Lesson 3
translations3_restricted_xu :: TranslationGenerator
translations3_restricted_xu = combineFunctions [(2, talkingAbout), (1, gaveSomething), (4, writing), (2, know)] where
    talkingAbout = generatorFromList
        [ (["xu do tavla fi do", "xu do tavla fi vo'a"], ["Are you talking about yourself?", "Were you talking about yourself?"])
        , (["xu do tavla fi mi"], ["Are you talking about me?", "Were you talking about me?"])
        , (["xu tavla fi mi"], ["Was somebody talking about me?"])
        , (["xu do tavla fi lo mlatu ku"], ["Were you talking about the cat?", "Were you talking about the cats?", "Were you talking about cats?"])
        , (["xu do tavla fi lo gerku ku"], ["Were you talking about the dog?", "Were you talking about the dogs?", "Were you talking about dogs?"])
        , (["xu tavla do lo mlatu ku"], ["Did somebody talk to you about the cat?", "Did somebody talk to you about the cats?"])
        , (["xu tavla do lo gerku ku"], ["Did somebody talk to you about the dog?", "Did somebody talk to you about the dogs?"])
        ]
    gaveSomething = generatorFromList
        [ (["xu do dunda fi mi"], ["Did you give me something?", "Are you going to give me something?"])
        , (["xu mi dunda fi do"], ["Did I give you something?"])
        ]
    writing = generatorFromList
        [ (["xu do ciska fi ti"], ["Did you write here?"])
        , (["xu do ciska fi ta"], ["Did you write there?"])
        , (["xu ciska fi ti"], ["Did somebody write here?"])
        , (["xu ciska fi ta"], ["Did somebody write there?"])
        , (["xu do ciska fo ti"], ["Do you write using this?", "Did you write something using this?"])
        , (["xu do ciska fo ta"], ["Do you write using that?", "Did you write something using that?"])
        ]
    know = generatorFromList
        [ (["xu do djuno fi lo mlatu ku"], ["Do you know about cats?"])
        , (["xu do djuno fi lo gerku ku"], ["Do you know about dogs?"])
        ]

translations3_normal_xu :: TranslationGenerator
translations3_normal_xu = combineFunctions $ [(3, translations3_restricted_xu), (3, writing), (2, know)] ++ ((1,) <$> [hasHouse, nice, talking, teaching, friends, others]) where
    hasHouse = generatorFromList
        [ (["xu do se zdani"], ["Do you have a house?"])
        , (["xu lo prenu ku se zdani"], ["Does the person have a house?"])
        , (["xu lo ctuca ku se zdani"], ["Does the instructor have a house?"])
        ]
    nice = combineFunctionsUniformly [beautiful, like] where
        beautiful = generatorFromList
            [ (["xu lo se dunda ku melbi do"], ["Is the gift beautiful to you?", "Are the gifts beatiful to you?"])
            , (["xu lo se dunda ku melbi"], ["Is the gift beautiful?", "Are the gifts beautiful?"])
            , (["xu lo ctuca ku melbi do"], ["Is the instructor beautiful to you?"])
            , (["xu lo ctuca ku melbi"], ["Is the instructor beautiful?"])
            , (["xu lo zdani ku melbi do"], ["Is the house beautiful to you?"])
            , (["xu lo zdani ku melbi"], ["Is the house beautiful?"])
            , (["xu lo mlatu ku melbi do"], ["Is the cat beautiful to you?"])
            , (["xu lo mlatu ku melbi"], ["Is the cat beautiful?"])
            , (["xu lo gerku ku melbi do"], ["Is the dog beautiful to you?"])
            , (["xu lo gerku ku melbi"], ["Is the dog beautiful?"])
            ]
        like = generatorFromList
            [ (["xu do nelci lo se dunda ku"], ["Did you like the gift?"])
            , (["xu lo te dunda ku nelci lo se dunda ku"], ["Did the recipient like the gift?"])
            , (["xu lo ctuca ku nelci lo se dunda ku"], ["Did the instructor like the gift?"])
            , (["xu do nelci lo ctuca ku"], ["Did you like the instructor?"])
            ]
    talking = generatorFromList
        [ (["xu do tavla mi"], ["Are you talking to me?"])
        , (["xu do tavla lo mlatu ku"], ["Are you talking to the cat?"])
        , (["xu do tavla lo gerku ku"], ["Are you talking to the dog?"])
        ]
    teaching = generatorFromList
        [ (["xu mi ctuca lo mlatu ku"], ["Are you teaching the cat?", "Did you teach the cat?"])
        , (["xu do ctuca lo gerku ku"], ["Are you teaching the dog?", "Did you teach the dog?"])
        , (["xu do ctuca mi"], ["Are you going to teach me?"])
        , (["xu do ctuca do", "xu do ctuca vo'a"], ["Did you teach yourself?"])
        , (["xu ctuca do"], ["Did somebody teach you?"])
        ]
    friends = generatorFromList
        [ (["xu do pendo mi"], ["Are you my friend?"])
        , (["xu lo ctuca ku pendo do"], ["Is the instructor your friend?"])
        , (["xu lo dunda ku pendo do"], ["Is the donor your friend?"])
        , (["xu lo te dunda ku pendo do"], ["Is the beneficiary your friend?"])
        ]
    writing = generatorFromList
        [ (["xu do ciska"], ["Do you write?"])
        , (["xu lo prenu ku ciska"], ["Do people write?"])
        , (["xu do ciska ti"], ["Did you write this?"])
        , (["xu do ciska ta"], ["Did you write that?"])
        ]
    know = generatorFromList
        [ (["xu do djuno lo se ciska ku"], ["Did you know that what was written is true?"]) -- probably not okay (du'u vs sedu'u)
        , (["xu do djuno lo te ctuca ku"], ["Did you know that what was taught is true?"]) -- probably not okay (du'u vs sedu'u)
        , (["xu do djuno"], ["Did you know?"])
        ]
    others = generatorFromList
        [ (["xu do nelci lo xe ctuca ku"], ["Do you like the teaching method?"])
        ]

translations3_restricted_ma :: TranslationGenerator
translations3_restricted_ma = combineFunctions [(2, talkingAbout), (1, gaveSomething), (4, writing), (2, know)] where
    talkingAbout = generatorFromList
        [ (["ma tavla fi mi"], ["Who is talking about me?", "Who is talking about us?", "Who was talking about me?", "Who was walking about us?"])
        , (["ma tavla fi do"], ["Who is talking about you?", "Who was talking about you?"])
        , (["ma tavla fi lo mlatu ku"], ["Who is talking about the cat?"])
        , (["ma tavla fi lo gerku ku"], ["Who is talking about the dog?"])
        , (["do tavla fi ma"], ["What are you talking about?", "What were you talking about?"])
        , (["lo prenu ku tavla fi ma"], ["What is the person talking about?", "What was the person talking about?"])
        , (["lo dunda ku tavla fi ma"], ["What is the donor talking about?", "What was the donor talking about?"])
        , (["lo te dunda ku tavla fi ma"], ["What is the beneficiary talking about?", "What was the beneficiary talking about?"])
        , (["lo ciska ku tavla fi ma"], ["What is the writer talking about?", "What was the writer talking about?"])
        ]
    gaveSomething = generatorFromList
        [ (["ma dunda fi mi"], ["Who donated to me?"])
        , (["ma dunda fi do"], ["Who donated to you?"])
        , (["do dunda fi ma"], ["To whom did you donate?"])
        , (["lo prenu ku dunda fi ma"], ["To whom did the person donate?"])
        ]
    writing = generatorFromList
        -- instrument
        [ (["do ciska fo ma"], ["Which instrument do you use to write?"])
        , (["do ciska fi ta ma"], ["Which instrument did you use to write there?"])
        -- medium
        , (["do ciska fi ma"], ["On what medium will you write?", "On what medium did you write?"])
        , (["do ciska fi ma ti"], ["On what medium will you write using this?"])
        , (["ciska fi ma ti"], ["On what medium will this instrument be used to write?"])
        -- who
        , (["ma ciska fi ta"], ["Who wrote there?"])
        , (["ma ciska fo ti"], ["Who writes using this?"])
        , (["ma ciska fo ta"], ["Who writes using that?"])
        ]
    know = generatorFromList
        [ (["ma djuno fi lo mlatu ku"], ["Who knows about cats?"])
        , (["ma djuno fi lo gerku ku"], ["Who knows about dogs?"])
        , (["do djuno fi ma"], ["What subjects do you know?"])
        ]

translations3_normal_ma :: TranslationGenerator
translations3_normal_ma = combineFunctions $ [(3, translations3_restricted_ma), (3, writing), (2, know)] ++ ((1,) <$> [hasHouse, nice, talking, giving, teaching]) where
    hasHouse = generatorFromList
        [ (["ma se zdani"], ["Who has a house?"])
        , (["ta zdani ma", "zdani ma"], ["Whose house is that?"])
        , (["do nelci ma"], ["What do you like?"])
        ]
    nice = combineFunctionsUniformly [what, who] where
        what = generatorFromList
            [ (["ma melbi do"], ["What is beautiful to you?"])
            , (["ma melbi"], ["What is beautiful?"])
            ]
        who = generatorFromList
            [ (["ti melbi ma"], ["Who finds this beautiful?"])
            , (["ta melbi ma"], ["Who finds that beautiful?"])
            , (["lo se dunda ku melbi ma"], ["The gift is beautiful to whom?", "The gifts are beautiful to whom?"])
            , (["lo mlatu ku melbi ma"], ["The cat is beautiful to whom?", "The cats are beautiful to whom?"])
            , (["lo gerku ku melbi ma"], ["The dog is beautiful to whom?", "The dogs are beautiful to whom?"])
            , (["ma nelci lo se dunda ku"], ["Who liked the gift?", "Who likes the gift?"])
            , (["ma nelci lo mlatu ku"], ["Who likes cats?", "Who likes the cat?"])
            , (["ma nelci lo gerku ku"], ["Who likes dogs?", "Who likes the dog?"])
            ]
    talking = generatorFromList
        [ (["mi tavla ma"], ["Who am I talking to?"])
        , (["do tavla ma"], ["Who are you talking to?"])
        , (["ma tavla mi"], ["Who is talking to me?"])
        , (["ma tavla do"], ["Who is talking to you?"])
        ]
    giving = combineFunctions [(2, general), (1, mlatu), (1, gerku)] where
        general = generatorFromList
            [ (["do te dunda ma"], ["What were you given?", "What did you receive?"])
            , (["do dunda ma mi"], ["What did you give me?", "What will you give me?"])
            , (["do dunda ma"], ["What did you donate?"])
            , (["lo ctuca ku dunda ma do"], ["What did the instructor give you?"])
            , (["ma dunda ta do"], ["Who gave you that?"])
            , (["ma dunda ti mi"], ["Who gave me this?"])
            , (["ma dunda ti"], ["Who donated this?"])
            , (["ma dunda ta"], ["Who donated that?"])
            , (["ma dunda lo zdani ku"], ["Who donated the house?"])
            ]
        mlatu = generatorFromList
            [ (["ma te dunda lo mlatu ku"], ["Who was given a cat?"])
            , (["do dunda lo mlatu ku ma"], ["To whom did you give the cat?", "To whom did you give the cats?"])
            , (["ma dunda lo mlatu ku lo ctuca ku"], ["Who gave the cat to the instructor?"])
            , (["ma dunda lo mlatu ku mi"], ["Who gave me a cat?"])
            , (["ma dunda lo mlatu ku do"], ["Who gave you a cat?"])
            , (["ma dunda lo mlatu ku"], ["Who donated the cat?"])
            ]
        gerku = generatorFromList
            [ (["ma te dunda lo gerku ku"], ["Who was given a dog?"])
            , (["do dunda lo gerku ku ma"], ["To whom did you give the dog?", "To whom did you give the dogs?"])
            , (["ma dunda lo gerku ku lo ctuca ku"], ["Who gave the dog to the instructor?"])
            , (["ma dunda lo gerku ku mi"], ["Who gave me a dog?"])
            , (["ma dunda lo gerku ku do"], ["Who gave you a dog?"])
            , (["ma dunda lo gerku ku"], ["Who donated the dog?"])
            ]
    teaching = generatorFromList
        [ (["mi ctuca ma"], ["Who are we going to teach?"])
        , (["do ctuca ma"], ["Who are you teaching?", "Who are you going to teach?"])
        , (["ma ctuca do"], ["Who is teaching you?", "Who taught you?"])
        , (["ma ctuca lo mlatu ku"], ["Who taught the cat?"])
        , (["ma ctuca lo gerku ku"], ["Who taught the dog?"])
        ]
    writing = generatorFromList
        -- instrument
        [ (["ciska ti fo ma"], ["Which instrument was used to write this?"])
        , (["do ciska ta fo ma"], ["Which instrument did you use to write that?"])
        -- what
        , (["do ciska ma"], ["What did you write?", "What are you going to write?"])
        , (["do ciska ma ta"], ["What did you write there?", "What are you going to write there?"])
        , (["do ciska ma fo ta"], ["What did you write using that?"])
        , (["ciska ma ta"], ["What is written there?"])
        , (["ciska ma fo ti"], ["What was written using this?"])
        -- who
        , (["ma ciska"], ["Who is writing?"])
        , (["ma ciska ti"], ["Who wrote this?"])
        ]
    know = generatorFromList
        [ (["ma djuno fi lo mlatu ku"], ["Who knows about cats?"])
        , (["ma djuno fi lo gerku ku"], ["Who knows about dogs?"])
        , (["do djuno fi ma"], ["What subjects do you know?"])
        ]

translations3_normal_mo :: TranslationGenerator
translations3_normal_mo = generatorFromList
    [ (["mi mo"], ["What am I doing?"])
    , (["do mo"], ["What are you doing?"])
    , (["lo prenu ku mo"], ["What is the person doing?"])
    , (["lo ctuca ku mo"], ["What is the instructor doing?"])
    , (["lo ciska ku mo"], ["What is the writer doing?"])
    ]

translations3_restricted :: TranslationGenerator
translations3_restricted = combineFunctionsUniformly [translations3_restricted_xu, translations3_restricted_ma]

translations3_normal :: TranslationGenerator
translations3_normal = combineFunctions [(4, translations3_normal_xu), (4, translations3_normal_ma), (1, translations3_normal_mo)]

translationExercises3 :: ExerciseGenerator
translationExercises3 = combineFunctions [(1, restricted), (5, normal)] where
    restricted = generateBlacklistedWordTranslationExercise (T.pack "zo'e") basicSentenceCanonicalizer sentenceComparer translations3_restricted
    normal = generateTranslationExercise basicSentenceCanonicalizer sentenceComparer translations3_normal

questionExercises3 = generateNarrowFillingBlanksExerciseByAlternatives ["mo", "ma"] $ combineFunctionsUniformly [translations3_normal_ma, translations3_normal_mo]
questionExercises3_simplified = generateNarrowFillingBlanksExerciseByAlternatives ["mo", "ma"] $ simplifyTranslationGenerator $ combineFunctionsUniformly [translations3_normal_ma, translations3_normal_mo]

-- Lesson 4
-- CHECK: Are events vs facts being used correctly?
translations4_nu :: TranslationGenerator
translations4_nu = combineFunctions [(2, gleki), (1, tavla), (2, nupre)] where
    gleki = combineFunctionsUniformly [talking, beautiful, givingAnimals, liking, teaching, owningHouse, know, other] where
        talking = generatorFromList
            -- talking to someone
            [ (["mi gleki lo nu do tavla mi kei ku"], ["I am happy that you talked to me."])
            , (["mi gleki lo nu mi tavla do kei ku", "mi gleki lo nu tavla do kei ku"], ["I am happy that I talked to you."])
            , (["mi gleki lo nu do tavla lo ciska ku kei ku"], ["I am glad that you talked to the writer."])
            , (["mi gleki lo nu lo ciska ku tavla do kei ku"], ["I am glad that the writer talked to you."])
            -- talking about animals
            , (["mi gleki lo nu do tavla mi lo mlatu ku kei ku"], ["I am glad that you talked to me about the cat.", "I am happy that you talked to me about cats."])
            , (["mi gleki lo nu do tavla mi lo gerku ku kei ku"], ["I am glad that you talked to me about the dog.", "I am happy that you talked to me about dogs."])
            ]
        beautiful = generatorFromList
            [ (["mi gleki lo nu do se melbi mi kei ku"], ["I am happy that you find me beautiful."])
            , (["mi gleki lo nu do se melbi lo se dunda ku kei ku"], ["I am happy that you found the gift beautiful."])
            ]
        givingAnimals = generatorFromList
            [ (["mi gleki lo nu do dunda lo mlatu ku mi kei ku"], ["I am happy that you gave me the cat.", "I am happy that you gave me the cats."])
            , (["mi gleki lo nu do dunda lo gerku ku mi kei ku"], ["I am happy that you gave me the dog.", "I am happy that you gave me the dogs."])
            , (["mi gleki lo nu mi te dunda lo mlatu ku kei ku"], ["I am happy that I was given a cat.", "I am happy that I was given the cat."])
            , (["mi gleki lo nu mi te dunda lo gerku ku kei ku"], ["I am happy that I was given a dog.", "I am happy that I was given the dog."])
            ]
        liking = generatorFromList
            [ (["mi gleki lo nu do nelci mi kei ku"], ["I am happy that you like me."])
            , (["mi gleki lo nu lo prenu ku nelci mi kei ku"], ["I am happy that people like me."])
            , (["mi gleki lo nu lo prenu ku nelci do kei ku"], ["I am happy that people like you.", "I am glad that people like you."])
            , (["mi gleki lo nu lo ctuca ku nelci mi kei ku"], ["I am happy that the instructor likes me."])
            , (["mi gleki lo nu lo ctuca ku nelci do kei ku"], ["I am happy that the instructor likes you.", "I am glad that the instructor likes you."])
            , (["mi gleki lo nu do nelci lo ctuca ku kei ku"], ["I am glad that you like the instructor."])
            , (["mi gleki lo nu do nelci lo ciska ku kei ku"], ["I am glad that you like the writer."])
            ]
        teaching = generatorFromList
            [ (["mi gleki lo nu do ctuca mi kei ku"], ["I am happy that you taught me."])
            , (["mi gleki lo nu mi ctuca do kei ku"], ["I am happy that I taught you."])
            ]
        owningHouse = generatorFromList
            [ (["mi gleki lo nu mi se zdani kei ku"], ["I am happy that I have a house."])
            , (["mi gleki lo nu do se zdani kei ku"], ["I am glad that you have a house."])
            ]
        know = generatorFromList
            [ (["mi gleki lo nu mi djuno fi lo mlatu ku kei ku"], ["I am happy that I know about cats."])
            , (["mi gleki lo nu mi djuno fi lo gerku ku kei ku"], ["I am happy that I know about dogs."])
            , (["mi gleki lo nu do djuno fi lo mlatu ku kei ku"], ["I am happy that you know about cats."])
            , (["mi gleki lo nu do djuno fi lo gerku ku kei ku"], ["I am happy that you know about dogs."])
            ]
        other = generatorFromList
            [ (["mi gleki lo nu do gleki kei ku"], ["I am happy that you are happy."])
            , (["mi gleki lo nu mi prenu kei ku"], ["I am happy that I am a person."])
            , (["mi gleki lo nu do prenu kei ku"], ["I am happy that you are a person."])
            , (["mi gleki lo nu do pendo mi kei ku"], ["I am happy that you are my friend."])
            , (["mi gleki lo nu lo ctuca ku pendo mi kei ku"], ["I am happy that the instructor is my friend."])
            , (["mi gleki lo nu lo ciska ku pendo mi kei ku"], ["I am happy that the writer is my friend."])
            , (["mi gleki lo nu lo te dunda ku pendo mi kei ku"], ["I am happy that the beneficiary is my friend."])
            , (["do gleki ma"], ["Why are you happy?", "What are you happy about?"])
            ]
    tavla = combineFunctionsUniformly [owningHouse, promisorTalked, promiseeTalked] where
        owningHouse = generatorFromList
            [ (["mi tavla fi lo nu do se zdani kei ku"], ["I talked about you having a house.", "We talked about you having a house."])
            ]
        promisorTalked = generatorFromList
            [ (["lo nupre ku tavla mi"], ["The promisor talked to me.", "The promisor talked to us."])
            , (["lo nupre ku tavla fi lo mlatu ku"], ["The promisor talked about the cat.", "The promisor talked about the cats."])
            , (["lo nupre ku tavla fi lo gerku ku"], ["The promisor talked about the dog.", "The promisor talked about the dogs."])
            , (["lo nupre ku tavla fi lo zdani ku"], ["The promisor talked about the house.", "The promisor talked about the houses."])
            , (["lo nupre ku tavla mi lo mlatu ku"], ["The promisor talked to me about the cat.", "The promisor talked to me about the cats."])
            , (["lo nupre ku tavla mi lo gerku ku"], ["The promisor talked to me about the dog.", "The promisor talked to me about the dogs."])
            , (["lo nupre ku tavla mi lo zdani ku"], ["The promisor talked to me about the house.", "The promisor talked to me about the houses."])
            ]
        promiseeTalked = generatorFromList
            [ (["lo te nupre ku tavla mi"], ["The promisee talked to me.", "The promisee talked to us."])
            , (["lo te nupre ku tavla fi lo mlatu ku"], ["The promisee talked about the cat.", "The promisee talked about the cats."])
            , (["lo te nupre ku tavla fi lo gerku ku"], ["The promisee talked about the dog.", "The promisee talked about the dogs."])
            , (["lo te nupre ku tavla fi lo zdani ku"], ["The promisee talked about the house.", "The promisee talked about the houses."])
            , (["lo te nupre ku tavla mi lo mlatu ku"], ["The promisee talked to me about the cat.", "The promisee talked to me about the cats."])
            , (["lo te nupre ku tavla mi lo gerku ku"], ["The promisee talked to me about the dog.", "The promisee talked to me about the dogs."])
            , (["lo te nupre ku tavla mi lo zdani ku"], ["The promisee talked to me about the house.", "The promisee talked to me about the houses."])
            ]
    nupre = combineFunctionsUniformly [donatingAnimals, donatingHouses, teaching, beingFriendly] where
        donatingAnimals = generatorFromList
            [ (["do nupre lo nu dunda lo mlatu ku kei ku", "do nupre lo nu do dunda lo mlatu ku kei ku"], ["You promised to donate the cat.", "You promised to donate the cats."])
            , (["do nupre lo nu dunda lo gerku ku kei ku", "do nupre lo nu do dunda lo gerku ku kei ku"], ["You promised to donate the dog.", "You promised to donate the dogs."])
            , (["do nupre lo nu dunda lo mlatu ku mi kei ku", "do nupre lo nu do dunda lo mlatu ku mi kei ku"], ["You promised to donate the cat to me.", "You promised to donate the cats to me.", "You promised to donate the cats to us."])
            , (["do nupre lo nu dunda lo gerku ku mi kei ku", "do nupre lo nu do dunda lo gerku ku mi kei ku"], ["You promised to donate the dog to me.", "You promised to donate the dogs to me.", "You promised to donate the dogs to us."])
-- preciso revisar a partir daqui (ver oq faz sentido)
            , (["do nupre lo nu dunda lo mlatu ku kei ku mi", "do nupre lo nu do dunda lo mlatu ku kei ku mi"], ["You promised me to donate the cat.", "You promised me to donate the cats.", "You promised us to donate the cat.", "You promised us to donate the cats."])
            , (["do nupre lo nu dunda lo gerku ku kei ku mi", "do nupre lo nu do dunda lo gerku ku kei ku mi"], ["You promised me to donate the dog.", "You promised me to donate the dogs.", "You promised us to donate the dog.", "You promised us to donate the dogs."])
            ]
        donatingHouses = generatorFromList
            [ (["do nupre lo nu dunda lo zdani ku kei ku mi", "do nupre lo nu do dunda lo zdani ku kei ku mi"], ["You promised me to donate the house.", "You promised me to donate the houses.", "You promised us to donate the house.", "You promised us to donate the houses."])
            , (["do nupre lo nu dunda lo zdani ku mi kei ku", "do nupre lo nu do dunda lo zdani ku mi kei ku"], ["You promised to donate the house to me.", "You promised to donate the houses to us."])
            ]
        teaching = generatorFromList
            [ (["do nupre lo nu ctuca mi kei ku", "do nupre lo nu do ctuca mi kei ku"], ["You promised to teach me.", "You promised to teach us."])
            , (["mi nupre lo nu ctuca do kei ku", "mi nupre lo nu mi ctuca do kei ku"], ["I promised to teach you."])
            ]
        beingFriendly = generatorFromList
            [ (["do nupre lo nu pendo kei ku", "do nupre lo nu do pendo kei ku"], ["You promised to be friendly."])
            , (["lo ciska ku nupre lo nu pendo kei ku"], ["The writer promised to be friendly."])
            ]

translations4_du'u :: TranslationGenerator
translations4_du'u = combineFunctions [(2, djuno)] where
    djuno = combineFunctionsUniformly [teaching, friend, donating, promising, liking, talking, writing] where
        teaching = generatorFromList
            [ (["mi djuno lo du'u do ctuca mi kei ku"], ["I know that you taught me."])
            ]
        friend = generatorFromList
            [ (["mi djuno lo du'u do pendo mi kei ku"], ["I know that you are my friend."])
            , (["mi djuno lo du'u mi pendo kei ku"], ["I know that I am friendly."])
            , (["mi djuno lo du'u do pendo kei ku"], ["I know that you are friendly."])
            , (["mi djuno lo du'u lo ciska ku pendo do kei ku"], ["I know the writer is your friend."])
            , (["mi djuno lo du'u lo ciska ku pendo kei ku"], ["I know that the writer is friendly."])
            ]
        donating = generatorFromList
            [ (["mi djuno lo du'u do dunda kei ku"], ["I know that you made a donation.", "I know that you made donations."])
            , (["mi djuno lo du'u lo prenu ku dunda kei ku"], ["I know that people make donations."])
            , (["mi djuno lo du'u do dunda lo zdani ku kei ku"], ["I know that you donated the house."])
            , (["mi djuno lo du'u do dunda lo mlatu ku kei ku"], ["I know that you donated the cat."])
            , (["mi djuno lo du'u do dunda lo gerku ku kei ku"], ["I know that you donated the dog."])
            , (["mi djuno lo du'u lo ciska ku dunda kei ku"], ["I know that the writer made a donation.", "I know that the writer makes donations."])
            ]
        promising = generatorFromList
            [ (["mi djuno lo du'u do nupre kei ku"], ["I know that you made a promise."])
            , (["mi djuno lo du'u lo prenu ku nupre kei ku"], ["I know that people make promises."])
            , (["mi djuno lo du'u do nupre fi mi kei ku"], ["I know that you promised me."])
            , (["mi djuno lo du'u lo ctuca ku nupre fi do kei ku"], ["I know that the instructor promised you."])
            , (["mi djuno lo du'u lo ciska ku nupre fi do kei ku"], ["I know that the writer promised you."])
            ]
        liking = generatorFromList
            [ (["mi djuno lo du'u do nelci mi kei ku"], ["I know that you like me."])
            , (["mi djuno lo du'u do nelci lo ciska ku kei ku"], ["I know that you like the writer."])
            , (["mi djuno lo du'u do nelci lo mlatu ku kei ku"], ["I know that you like cats."])
            , (["mi djuno lo du'u do nelci lo gerku ku kei ku"], ["I know that you like dogs."])
            , (["mi djuno lo du'u lo prenu ku nelci lo mlatu ku kei ku"], ["I know that people like cats."])
            , (["mi djuno lo du'u lo prenu ku nelci lo gerku ku kei ku"], ["I know that people like dogs."])
            , (["mi djuno lo du'u lo mlatu ku nelci lo gerku ku kei ku"], ["I know that cats like dogs."])
            , (["mi djuno lo du'u lo gerku ku nelci lo mlatu ku kei ku"], ["I know that dogs like cats."])
            , (["mi djuno lo du'u lo mlatu ku nelci lo prenu ku kei ku"], ["I know that cats like people."])
            , (["mi djuno lo du'u lo gerku ku nelci lo prenu ku kei ku"], ["I know that dogs like people."])
            ]
        talking = generatorFromList
            [ (["mi djuno lo du'u do tavla mi kei ku"], ["I know that you were talking to me."])
            , (["mi djuno lo du'u do tavla lo nupre ku kei ku"], ["I know that you talked to the promisor."])
            , (["mi djuno lo du'u do tavla lo te nupre ku kei ku"], ["I know that you talked to the promisee."])
            , (["mi djuno lo du'u do tavla fi lo se nupre ku kei ku"], ["I know that you were talking about the promise."])
            , (["mi djuno lo du'u do tavla fi lo ciska ku kei ku"], ["I know that you were talking about the writer."])
            , (["mi djuno lo du'u do tavla fi lo ctuca ku kei ku"], ["I know that you were talking about the instructor."])
            , (["mi djuno lo du'u do tavla fi lo mlatu ku kei ku"], ["I know that you were talking about the cat."])
            , (["mi djuno lo du'u do tavla fi lo gerku ku kei ku"], ["I know that you were talking about the dog."])
            ]
        writing = generatorFromList
            [ (["mi djuno lo du'u do ciska fo ta kei ku"], ["I know that you write using that."])
            , (["mi djuno lo du'u do ciska fi ta kei ku"], ["I know that you wrote something there."])
            , (["mi djuno lo du'u lo ctuca ku ciska fi ta kei ku"], ["I know that the instructor wrote something there."])
            ]

translations4_sedu'u :: TranslationGenerator
translations4_sedu'u = combineFunctions [(2, cusku)] where
    cusku = combineFunctionsUniformly [beautiful, likingPeople, likingAnimals, donatingAnimals, beingFriendly, others] where
        beautiful = generatorFromList
            [ (["mi cusku lo se du'u do melbi kei ku"], ["I said that you are beautiful."])
            , (["mi cusku lo se du'u lo prenu ku melbi kei ku"], ["I said that the person is beautiful."])
            , (["do cusku lo se du'u mi melbi kei ku"], ["You said that I am beautiful."])
            , (["do cusku lo se du'u lo prenu ku melbi kei ku"], ["You said that the person is beautiful."])
            ]
        likingPeople = generatorFromList
            [ (["mi cusku lo se du'u mi nelci do kei ku", "mi cusku lo se du'u nelci do kei ku"], ["I said that I like you."])
            , (["do cusku lo se du'u do nelci mi kei ku", "do cusku lo se du'u nelci mi kei ku"], ["You said that you like me."])
            , (["mi cusku lo se du'u mi nelci lo ciska ku kei ku", "mi cusku lo se du'u nelci lo ciska ku kei ku"], ["I said that I like the writer."])
            , (["lo prenu ku cusku lo se du'u mi nelci do kei ku"], ["The person said that I like you."])
            , (["lo prenu ku cusku lo se du'u do nelci mi kei ku"], ["The person said that you like me."])
            , (["lo prenu ku cusku lo se du'u nelci mi kei ku", "lo prenu ku cusku lo se du'u ko'a nelci mi kei ku"], ["The person said that she likes me."])
            , (["lo prenu ku cusku lo se du'u nelci do kei ku", "lo prenu ku cusku lo se du'u ko'a nelci do kei ku"], ["The person said that she likes you."])
            ]
        likingAnimals = generatorFromList
            -- mlatu
            [ (["mi cusku lo se du'u mi nelci lo mlatu ku kei ku", "mi cusku lo se du'u nelci lo mlatu ku kei ku"], ["I said that I like the cat.", "I said that I like the cats.", "I said that I like cats."])
            , (["mi cusku lo se du'u do nelci lo mlatu ku kei ku"], ["I said that you like the cat.", "I said that you like the cats.", "I said that you like cats."])
            , (["do cusku lo se du'u do nelci lo mlatu ku kei ku", "do cusku lo se du'u nelci lo mlatu ku kei ku"], ["You said that you like the cat.", "You said that you like the cats.", "You said that you like cats."])
            , (["do cusku lo se du'u mi nelci lo mlatu ku kei ku"], ["You said that I like the cat.", "You said that I like the cats.", "You said that I like cats."])
            , (["lo prenu ku cusku lo se du'u mi nelci lo mlatu ku kei ku"], ["The person said that I like the cat.", "The person said that I like the cats.", "The person said that I like cats."])
            , (["lo prenu ku cusku lo se du'u do nelci lo mlatu ku kei ku"], ["The person said that you like the cat.", "The person said that you like the cats.", "The person said that you like cats."])
            , (["lo prenu ku cusku lo se du'u nelci lo mlatu ku kei ku", "lo prenu ku cusku lo se du'u ko'a nelci lo mlatu ku kei ku"], ["The person said that she liked the cat.", "The person said that she liked the cats.", "The person said that she likes cats."])
            -- gerku
            , (["mi cusku lo se du'u mi nelci lo gerku ku kei ku", "mi cusku lo se du'u nelci lo gerku ku kei ku"], ["I said that I like the dog.", "I said that I like the dogs.", "I said that I like dogs."])
            , (["mi cusku lo se du'u do nelci lo gerku ku kei ku"], ["I said that you like the dog.", "I said that you like the dogs.", "I said that you like dogs."])
            , (["do cusku lo se du'u do nelci lo gerku ku kei ku", "do cusku lo se du'u nelci lo gerku ku kei ku"], ["You said that you like the dog.", "You said that you like the dogs.", "You said that you like dogs."])
            , (["do cusku lo se du'u mi nelci lo gerku ku kei ku"], ["You said that I like the dog.", "You said that I like the dogs.", "You said that I like dogs."])
            , (["lo prenu ku cusku lo se du'u mi nelci lo gerku ku kei ku"], ["The person said that I like the dog.", "The person said that I like the dogs.", "The person said that I like dogs."])
            , (["lo prenu ku cusku lo se du'u do nelci lo gerku ku kei ku"], ["The person said that you like the dog.", "The person said that you like the dogs.", "The person said that you like dogs."])
            , (["lo prenu ku cusku lo se du'u nelci lo gerku ku kei ku", "lo prenu ku cusku lo se du'u ko'a nelci lo gerku ku kei ku"], ["The person said that she liked the dog.", "The person said that she liked the dogs.", "The person said that she likes dogs."])
            ]
        donatingAnimals = generatorFromList
            [ (["mi cusku lo se du'u mi dunda lo mlatu ku kei ku", "mi cusku lo se du'u dunda lo mlatu ku kei ku"], ["I said that I would donate the cat.", "I said that I would donate the cats."])
            , (["do cusku lo se du'u do dunda lo mlatu ku kei ku", "do cusku lo se du'u dunda lo mlatu ku kei ku"], ["You said that you would donate the cat.", "You said that you would donate the cats."])
            , (["lo prenu ku cusku lo se du'u dunda lo mlatu ku kei ku", "lo prenu ku cusku lo se du'u ko'a dunda lo mlatu ku kei ku"], ["The person said said that she would donate the cat.", "The person said that she would donate the cats."])
            , (["mi cusku lo se du'u mi dunda lo mlatu ku do kei ku", "mi cusku lo se du'u dunda lo mlatu ku do kei ku"], ["I said that I would give you the cat.", "I said that I would give you the cats."])
            , (["do cusku lo se du'u do dunda lo mlatu ku mi kei ku", "do cusku lo se du'u dunda lo mlatu ku mi kei ku"], ["You said that you would give me the cat.", "You said that you would give me the cats."])
            , (["mi cusku lo se du'u mi dunda lo gerku ku do kei ku", "mi cusku lo se du'u dunda lo gerku ku do kei ku"], ["I said that I would give you the dog.", "I said that I would give you the dogs."])
            , (["do cusku lo se du'u do dunda lo gerku ku mi kei ku", "do cusku lo se du'u dunda lo gerku ku mi kei ku"], ["You said that you would give me the dog.", "You said that you would give me the dogs."])
            , (["lo prenu ku cusku lo se du'u dunda lo mlatu ku mi kei ku", "lo prenu ku cusku lo se du'u ko'a dunda lo mlatu ku mi kei ku"], ["The person said that she would give me the cat.", "The person said that she would give me the cats."])
            , (["lo prenu ku cusku lo se du'u dunda lo mlatu ku do kei ku", "lo prenu ku cusku lo se du'u ko'a dunda lo mlatu ku do kei ku"], ["The person said that she would give you the cat.", "The person said that she would give you the cats."])
            , (["lo prenu ku cusku lo se du'u dunda lo gerku ku mi kei ku", "lo prenu ku cusku lo se du'u ko'a dunda lo gerku ku mi kei ku"], ["The person said that she would give me the dog.", "The person said that she would give me the dogs."])
            , (["lo prenu ku cusku lo se du'u dunda lo gerku ku do kei ku", "lo prenu ku cusku lo se du'u ko'a dunda lo gerku ku do kei ku"], ["The person said that she would give you the dog.", "The person said that she would give you the dogs."])
            , (["lo prenu ku cusku lo se du'u do dunda lo mlatu ku mi kei ku"], ["The person said that you would give me the cat.", "The person said that you would give me the cats."])
            , (["lo prenu ku cusku lo se du'u do dunda lo gerku ku mi kei ku"], ["The person said that you would give me the dog.", "The person said that you would give me the dogs."])
            ]
        beingFriendly = generatorFromList
            [ (["mi cusku lo se du'u pendo kei ku", "mi cusku lo se du'u mi pendo kei ku"], ["I said that I would be friendly."])
            , (["do cusku lo se du'u pendo kei ku", "do cusku lo se du'u do pendo kei ku"], ["You said that you would be friendly."])
            , (["mi cusku lo se du'u do pendo kei ku"], ["I said that you would be friendly."])
            , (["mi cusku lo se du'u lo ciska ku pendo kei ku"], ["I said that the writer would be friendly."])
            ]
        others = generatorFromList
            [ (["mi nelci lo nu tavla do kei ku", "mi nelci lo nu mi tavla do kei ku"], ["I like to talk to you."]) -- is nelci really adequate?
            , (["do nelci lo nu nupre kei ku", "do nelci lo nu do nupre kei ku"], ["You like to make promises."]) -- is nelci really adequate?
            , (["lo prenu ku nelci lo nu nupre kei ku"], ["People like to make promises."]) -- is nelci really adequate?
            , (["do cusku ma"], ["What did you say?"])
            , (["lo prenu ku cusku ma"], ["What did the person say?"])
            , (["lo prenu ku cusku ma do"], ["What did the person say to you?"])
            -- Wait until terminator ellision has been explained to use the following sentences
            {-, (["mi cusku lo se du'u mi nelci lo nu tavla do kei ku kei ku", "mi cusku lo se du'u mi nelci lo nu mi tavla do kei ku kei ku"], ["I said that I like to talk to you."])-}
            {-, (["mi cusku lo se du'u do nelci lo nu tavla mi kei ku kei ku", "mi cusku lo se du'u do nelci lo nu do tavla mi kei ku kei ku"], ["I said that you like to talk to me."])-}
            ]

translations4_extra = combineFunctionsUniformly [gleki, tavla, nupre, cusku, ciska] where
    gleki = generatorFromList
        [ (["mi gleki"], ["I am happy."])
        , (["lo prenu ku gleki"], ["The person is happy."])
        , (["lo gerku ku gleki"], ["The dog is happy.", "The dogs are happy."])
        , (["lo mlatu ku gleki"], ["The cat is happy.", "The cats are happy."])
        ]
    tavla = generatorFromList
        [ (["mi tavla fi lo se nupre ku"], ["I talked about the promise."])
        , (["do tavla fi lo se nupre ku"], ["You talked about the promise."])
        , (["mi tavla do lo se nupre ku"], ["I talked to you about the promise."])
        , (["do tavla mi lo se nupre ku"], ["You talked to me about the promise."])
        ]
    nupre = generatorFromList
        [ (["lo prenu ku nupre"], ["People make promises."])
        , (["do nupre"], ["You made a promise."])
        , (["do nupre fi mi"], ["You promised me.", "You promised us."])
        , (["mi nupre fi do"], ["I promised you.", "We promised you."])
        ]
    cusku = generatorFromList
        [ (["mi cusku"], ["I said something.", "I was saying something.", "I will say something."])
        , (["do cusku"], ["You said something.", "You were saying something."])
        , (["lo prenu ku cusku"], ["The person said something.", "The person was saying something."])
        ]
    ciska = generatorFromList
        [ (["mi ciska fo ta"], ["I write using that."])
        , (["mi ciska fi ta"], ["I wrote something there."])
        , (["lo ctuca ku ciska fi ta"], ["The instructor wrote something there."])
        ]

translations4 :: TranslationGenerator
translations4 = combineFunctions $ ((4,) <$> [translations4_nu, translations4_du'u, translations4_sedu'u]) ++ ((1,) <$> [translations4_extra])

translationExercises4 :: ExerciseGenerator
translationExercises4 = generateTranslationExercise basicSentenceCanonicalizer sentenceComparer translations4

-- "narrow" is required to avoid alternative translations using "ko'a"
abstractionExercises4 = generateNarrowFillingBlanksExerciseByAlternatives ["lo nu", "lo du'u", "lo se du'u"] $ combineFunctionsUniformly [translations4_nu, translations4_du'u, translations4_sedu'u]
abstractionExercises4_simplified = generateNarrowFillingBlanksExerciseByAlternatives ["lo nu", "lo du'u", "lo se du'u"] $ simplifyTranslationGenerator $ combineFunctionsUniformly [translations4_nu, translations4_du'u, translations4_sedu'u]

-- Lesson 5
translations5_restricted :: TranslationGenerator
translations5_restricted = combineFunctions [(2, hasHouse), (3, nice), (3, giving), (2, talking), (3, gleki), (3, nupre), (3, djuno), (3, cusku)] where
    hasHouse = generatorFromList
        [ (["lo ctuca cu se zdani"], ["The instructor has a house."])
        , (["lo prenu cu se zdani"], ["The person has a house."])
        , (["lo tavla cu se zdani", "lo cusku cu se zdani"], ["The speaker has a house."])
        , (["lo se tavla cu se zdani"], ["The listener has a house."])
        , (["lo dunda cu se zdani"], ["The donor has a house."])
        , (["lo te dunda cu se zdani"], ["The beneficiary has a house."])
        , (["xu lo prenu cu se zdani"], ["Does the person have a house?"])
        , (["xu lo ctuca cu se zdani"], ["Does the instructor have a house?"])
        ]
    nice = combineFunctionsUniformly [beautiful, like] where
        beautiful = generatorFromList
            [ (["lo se dunda cu melbi mi"], ["The gift is beautiful to me.", "The gifts are beautiful to me."])
            , (["lo se dunda cu melbi"], ["The gift is beautiful.", "The gifts are beautiful."])
            , (["xu lo se dunda cu melbi do"], ["Is the gift beautiful to you?", "Are the gifts beatiful to you?"])
            , (["xu lo se dunda cu melbi"], ["Is the gift beautiful?", "Are the gifts beautiful?"])
            , (["xu lo ctuca cu melbi do"], ["Is the instructor beautiful to you?"])
            , (["xu lo ctuca cu melbi"], ["Is the instructor beautiful?"])
            , (["xu lo zdani cu melbi do"], ["Is the house beautiful to you?"])
            , (["xu lo zdani cu melbi"], ["Is the house beautiful?"])
            , (["xu lo mlatu cu melbi do"], ["Is the cat beautiful to you?"])
            , (["xu lo mlatu cu melbi"], ["Is the cat beautiful?"])
            , (["xu lo gerku cu melbi do"], ["Is the dog beautiful to you?"])
            , (["xu lo gerku cu melbi"], ["Is the dog beautiful?"])
            , (["lo se dunda cu melbi ma"], ["The gift is beautiful to whom?", "The gifts are beautiful to whom?"])
            , (["lo mlatu cu melbi ma"], ["The cat is beautiful to whom?", "The cats are beautiful to whom?"])
            , (["lo gerku cu melbi ma"], ["The dog is beautiful to whom?", "The dogs are beautiful to whom?"])
            ]
        like = generatorFromList
            [ (["lo te dunda cu nelci lo se dunda"], ["The recipient liked the gift.", "The recipient will like the gift.", "The recipients liked the gifts."])
            , (["lo ctuca cu nelci lo se dunda"], ["The instructor liked the gift.", "The instructor will like the gift."])
            , (["xu do nelci lo se dunda"], ["Did you like the gift?"])
            , (["xu lo te dunda cu nelci lo se dunda"], ["Did the recipient like the gift?"])
            , (["xu lo ctuca cu nelci lo se dunda"], ["Did the instructor like the gift?"])
            , (["xu do nelci lo ctuca cu"], ["Did you like the instructor?"])
            , (["ma nelci lo se dunda"], ["Who liked the gift?", "Who likes the gift?"])
            , (["ma nelci lo mlatu"], ["Who likes cats?", "Who likes the cat?"])
            , (["ma nelci lo gerku"], ["Who likes dogs?", "Who likes the dog?"])
            ]
    giving = combineFunctions [(1, general), (3, mlatu), (3, gerku)] where
        general = generatorFromList
            [ (["lo ctuca cu dunda ma do"], ["What did the instructor give you?"])
            , (["ma dunda lo zdani"], ["Who donated the house?"])
            ]
        mlatu = generatorFromList
            [ (["mi dunda lo mlatu lo pendo"], ["I gave the cat to a friend.", "I gave the cats to a friend."])
            , (["mi te dunda lo mlatu"], ["I was given a cat.", "We were given a cat."])
            , (["dunda lo mlatu lo ctuca"], ["Somebody gave a cat to the instructor", "Somebody gave the cat to the instructor.", "Somebody gave the cats to the instructor."])
            , (["lo ctuca cu dunda lo mlatu mi"], ["The instructor gave me a cat.", "The instructor gave me the cat.", "The instructor gave me the cats.", "The instructor gave us a cat.", "The instructor gave us the cat.", "The instructor gave us the cats."])
            , (["dunda lo mlatu mi"], ["Somebody gave me a cat.", "Somebody gave me the cat.", "Somebody gave me the cats.", "Somebody gave us a cat.", "Someboy gave us the cat.", "Somebody gave us the cats."])
            , (["ma te dunda lo mlatu"], ["Who was given a cat?"])
            , (["do dunda lo mlatu ma"], ["To whom did you give the cat?", "To whom did you give the cats?"])
            , (["ma dunda lo mlatu lo ctuca"], ["Who gave the cat to the instructor?"])
            , (["ma dunda lo mlatu mi"], ["Who gave me a cat?"])
            , (["ma dunda lo mlatu do"], ["Who gave you a cat?"])
            , (["ma dunda lo mlatu"], ["Who donated the cat?"])
            ]
        gerku = generatorFromList
            [ (["mi dunda lo gerku lo pendo"], ["I gave the dog to a friend.", "I gave the dogs to a friend."])
            , (["mi te dunda lo gerku"], ["I was given a dog.", "We were given a dog."])
            , (["dunda lo gerku lo ctuca"], ["Somebody gave a dog to the instructor", "Somebody gave the dog to the instructor.", "Somebody gave the dogs to the instructor."])
            , (["lo ctuca cu dunda lo gerku mi"], ["The instructor gave me a dog.", "The instructor gave me the dog.", "The instructor gave me the dogs.", "The instructor gave us a dog.", "The instructor gave us the dog.", "The instructor gave us the dogs."])
            , (["dunda lo gerku mi"], ["Somebody gave me a dog.", "Somebody gave me the dog.", "Somebody gave me the dogs.", "Somebody gave us a dog.", "Someboy gave us the dog.", "Somebody gave us the dogs."])
            , (["ma te dunda lo gerku"], ["Who was given a dog?"])
            , (["do dunda lo gerku ma"], ["To whom did you give the dog?", "To whom did you give the dogs?"])
            , (["ma dunda lo gerku lo ctuca"], ["Who gave the dog to the instructor?"])
            , (["ma dunda lo gerku mi"], ["Who gave me a dog?"])
            , (["ma dunda lo gerku do"], ["Who gave you a dog?"])
            , (["ma dunda lo gerku"], ["Who donated the dog?"])
            ]
    talking = generatorFromList
        [ (["lo prenu cu tavla lo mlatu"], ["A person is talking to a cat.", "The person talks to cats."])
        , (["lo prenu cu tavla lo gerku"], ["A person is talking to a dog.", "The person talks to dogs."])
        , (["xu do tavla fi lo mlatu"], ["Were you talking about the cat?", "Were you talking about the cats?", "Were you talking about cats?"])
        , (["ma tavla fi lo mlatu"], ["Who is talking about the cat?"])
        , (["ma tavla fi lo gerku"], ["Who is talking about the dog?"])
        , (["lo prenu cu tavla fi ma"], ["What is the person talking about?", "What was the person talking about?"])
        , (["lo dunda cu tavla fi ma"], ["What is the donor talking about?", "What was the donor talking about?"])
        , (["lo te dunda cu tavla fi ma"], ["What is the beneficiary talking about?", "What was the beneficiary talking about?"])
        , (["lo ciska cu tavla fi ma"], ["What is the writer talking about?", "What was the writer talking about?"])
        , (["xu do tavla fi lo gerku"], ["Were you talking about the dog?", "Were you talking about the dogs?", "Were you talking about dogs?"])
        ]
    gleki = combineFunctionsUniformly [givingAnimals, liking, know, other] where
        givingAnimals = generatorFromList
            [ (["mi gleki lo nu do dunda lo mlatu mi"], ["I am happy that you gave me the cat.", "I am happy that you gave me the cats."])
            , (["mi gleki lo nu do dunda lo gerku mi"], ["I am happy that you gave me the dog.", "I am happy that you gave me the dogs."])
            , (["mi gleki lo nu mi te dunda lo mlatu"], ["I am happy that I was given a cat.", "I am happy that I was given the cat."])
            , (["mi gleki lo nu mi te dunda lo gerku"], ["I am happy that I was given a dog.", "I am happy that I was given the dog."])
            ]
        liking = generatorFromList
            [ (["mi gleki lo nu lo prenu cu nelci mi"], ["I am happy that people like me."])
            , (["mi gleki lo nu lo prenu cu nelci do"], ["I am happy that people like you.", "I am glad that people like you."])
            , (["mi gleki lo nu lo ctuca cu nelci mi"], ["I am happy that the instructor likes me."])
            , (["mi gleki lo nu lo ctuca cu nelci do"], ["I am happy that the instructor likes you.", "I am glad that the instructor likes you."])
            , (["mi gleki lo nu do nelci lo ctuca"], ["I am glad that you like the instructor."])
            , (["mi gleki lo nu do nelci lo ciska"], ["I am glad that you like the writer."])
            ]
        know = generatorFromList
            [ (["mi gleki lo nu mi djuno fi lo mlatu"], ["I am happy that I know about cats."])
            , (["mi gleki lo nu mi djuno fi lo gerku"], ["I am happy that I know about dogs."])
            , (["mi gleki lo nu do djuno fi lo mlatu"], ["I am happy that you know about cats."])
            , (["mi gleki lo nu do djuno fi lo gerku"], ["I am happy that you know about dogs."])
            ]
        other = generatorFromList
            [ (["mi gleki lo nu lo ctuca cu pendo mi"], ["I am happy that the instructor is my friend."])
            , (["mi gleki lo nu lo ciska cu pendo mi"], ["I am happy that the writer is my friend."])
            , (["mi gleki lo nu lo te dunda cu pendo mi"], ["I am happy that the beneficiary is my friend."])
            ]
    nupre = combineFunctionsUniformly [donatingAnimals, donatingHouses] where
        donatingAnimals = generatorFromList
            [ (["do nupre lo nu dunda lo mlatu", "do nupre lo nu do dunda lo mlatu"], ["You promised to donate the cat.", "You promised to donate the cats."])
            , (["do nupre lo nu dunda lo gerku", "do nupre lo nu do dunda lo gerku"], ["You promised to donate the dog.", "You promised to donate the dogs."])
            , (["do nupre lo nu dunda lo mlatu mi", "do nupre lo nu do dunda lo mlatu mi"], ["You promised to donate the cat to me.", "You promised to donate the cats to me.", "You promised to donate the cats to us."])
            , (["do nupre lo nu dunda lo gerku mi", "do nupre lo nu do dunda lo gerku mi"], ["You promised to donate the dog to me.", "You promised to donate the dogs to me.", "You promised to donate the dogs to us."])
            , (["do nupre lo nu dunda lo mlatu kei mi", "do nupre lo nu do dunda lo mlatu kei mi"], ["You promised me to donate the cat.", "You promised me to donate the cats.", "You promised us to donate the cat.", "You promised us to donate the cats."]) -- excellent!
            , (["do nupre lo nu dunda lo gerku kei mi", "do nupre lo nu do dunda lo gerku kei mi"], ["You promised me to donate the dog.", "You promised me to donate the dogs.", "You promised us to donate the dog.", "You promised us to donate the dogs."]) -- excellent!
            ]
        donatingHouses = generatorFromList
            [ (["do nupre lo nu dunda lo zdani kei mi", "do nupre lo nu do dunda lo zdani kei mi"], ["You promised me to donate the house.", "You promised me to donate the houses.", "You promised us to donate the house.", "You promised us to donate the houses."])
            , (["do nupre lo nu dunda lo zdani mi", "do nupre lo nu do dunda lo zdani mi"], ["You promised to donate the house to me.", "You promised to donate the houses to us."]) -- excellent!
            ]
    djuno = combineFunctionsUniformly [liking, talking] where
        liking = generatorFromList
            [ (["mi djuno lo du'u do nelci lo ciska"], ["I know that you like the writer."])
            , (["mi djuno lo du'u do nelci lo mlatu"], ["I know that you like cats."])
            , (["mi djuno lo du'u do nelci lo gerku"], ["I know that you like dogs."])
            , (["mi djuno lo du'u lo prenu cu nelci lo mlatu"], ["I know that people like cats."])
            , (["mi djuno lo du'u lo prenu cu nelci lo gerku"], ["I know that people like dogs."])
            , (["mi djuno lo du'u lo mlatu cu nelci lo gerku"], ["I know that cats like dogs."])
            , (["mi djuno lo du'u lo gerku cu nelci lo mlatu"], ["I know that dogs like cats."])
            , (["mi djuno lo du'u lo mlatu cu nelci lo prenu"], ["I know that cats like people."])
            , (["mi djuno lo du'u lo gerku cu nelci lo prenu"], ["I know that dogs like people."])
            ]
        talking = generatorFromList
            [ (["mi djuno lo du'u do tavla lo nupre"], ["I know that you talked to the promisor."])
            , (["mi djuno lo du'u do tavla lo te nupre"], ["I know that you talked to the promisee."])
            , (["mi djuno lo du'u do tavla fi lo se nupre"], ["I know that you were talking about the promise."])
            , (["mi djuno lo du'u do tavla fi lo ciska"], ["I know that you were talking about the writer."])
            , (["mi djuno lo du'u do tavla fi lo ctuca"], ["I know that you were talking about the instructor."])
            , (["mi djuno lo du'u do tavla fi lo mlatu"], ["I know that you were talking about the cat."])
            , (["mi djuno lo du'u do tavla fi lo gerku"], ["I know that you were talking about the dog."])
            ]
    cusku = combineFunctionsUniformly [likingPeople, likingAnimals, donatingAnimals, others] where
        likingPeople = generatorFromList
            [ (["mi cusku lo se du'u mi nelci lo ciska", "mi cusku lo se du'u nelci lo ciska"], ["I said that I like the writer."])
            , (["lo prenu cu cusku lo se du'u mi nelci do"], ["The person said that I like you."])
            , (["lo prenu cu cusku lo se du'u do nelci mi"], ["The person said that you like me."])
            , (["lo prenu cu cusku lo se du'u nelci mi"], ["The person said that she likes me."])
            , (["lo prenu cu cusku lo se du'u nelci do"], ["The person said that she likes you."])
            ]
        likingAnimals = generatorFromList
            -- mlatu
            [ (["mi cusku lo se du'u mi nelci lo mlatu", "mi cusku lo se du'u nelci lo mlatu"], ["I said that I like the cat.", "I said that I like the cats.", "I said that I like cats."])
            , (["mi cusku lo se du'u do nelci lo mlatu"], ["I said that you like the cat.", "I said that you like the cats.", "I said that you like cats."])
            , (["do cusku lo se du'u do nelci lo mlatu", "do cusku lo se du'u nelci lo mlatu"], ["You said that you like the cat.", "You said that you like the cats.", "You said that you like cats."])
            , (["do cusku lo se du'u mi nelci lo mlatu"], ["You said that I like the cat.", "You said that I like the cats.", "You said that I like cats."])
            , (["lo prenu cu cusku lo se du'u mi nelci lo mlatu"], ["The person said that I like the cat.", "The person said that I like the cats.", "The person said that I like cats."])
            , (["lo prenu cu cusku lo se du'u do nelci lo mlatu"], ["The person said that you like the cat.", "The person said that you like the cats.", "The person said that you like cats."])
            -- gerku
            , (["mi cusku lo se du'u mi nelci lo gerku", "mi cusku lo se du'u nelci lo gerku"], ["I said that I like the dog.", "I said that I like the dogs.", "I said that I like dogs."])
            , (["mi cusku lo se du'u do nelci lo gerku"], ["I said that you like the dog.", "I said that you like the dogs.", "I said that you like dogs."])
            , (["do cusku lo se du'u do nelci lo gerku", "do cusku lo se du'u nelci lo gerku"], ["You said that you like the dog.", "You said that you like the dogs.", "You said that you like dogs."])
            , (["do cusku lo se du'u mi nelci lo gerku"], ["You said that I like the dog.", "You said that I like the dogs.", "You said that I like dogs."])
            , (["lo prenu cu cusku lo se du'u mi nelci lo gerku"], ["The person said that I like the dog.", "The person said that I like the dogs.", "The person said that I like dogs."])
            , (["lo prenu cu cusku lo se du'u do nelci lo gerku"], ["The person said that you like the dog.", "The person said that you like the dogs.", "The person said that you like dogs."])
            ]
        donatingAnimals = generatorFromList
            [ (["lo prenu cu cusku lo se du'u dunda lo mlatu mi"], ["The person said that she would give me the cat.", "The person said that she would give me the cats."])
            , (["lo prenu cu cusku lo se du'u dunda lo mlatu do"], ["The person said that she would give you the cat.", "The person said that she would give you the cats."])
            , (["lo prenu cu cusku lo se du'u dunda lo gerku mi"], ["The person said that she would give me the dog.", "The person said that she would give me the dogs."])
            , (["lo prenu cu cusku lo se du'u dunda lo gerku do"], ["The person said that she would give you the dog.", "The person said that she would give you the dogs."])
            , (["lo prenu cu cusku lo se du'u do dunda lo mlatu mi"], ["The person said that you would give me the cat.", "The person said that you would give me the cats."])
            , (["lo prenu cu cusku lo se du'u do dunda lo gerku mi"], ["The person said that you would give me the dog.", "The person said that you would give me the dogs."])
            ]
        others = generatorFromList
            [ (["mi cusku lo se du'u lo ciska ku pendo kei ku"], ["I said that the writer would be friendly."])
            , (["mi cusku lo se du'u mi nelci lo nu tavla do", "mi cusku lo se du'u mi nelci lo nu mi tavla do kei ku kei ku"], ["I said that I like to talk to you."]) -- is nelci really adequate?
            , (["mi cusku lo se du'u do nelci lo nu tavla mi", "mi cusku lo se du'u do nelci lo nu do tavla mi kei ku kei ku"], ["I said that you like to talk to me."]) -- is nelci really adequate?
            , (["xu do cusku lo se du'u mi melbi do kei ku", "xu do cusku lo se du'u mi melbi kei ku"], ["Did you say that you find me beautiful?"])
            , (["xu do cusku lo se du'u mi melbi kei ku", "xu do cusku lo se du'u mi melbi do kei ku"], ["Did you say that I am beautiful?"])
            ]

translationExercises5_restricted :: ExerciseGenerator
translationExercises5_restricted = generateBlacklistedWordTranslationExercise (T.pack "ku") basicSentenceCanonicalizer sentenceComparer translations5_restricted

-- Checkpoint: Lessons 1--5
translationExercises1to5_simplified :: ExerciseGenerator
translationExercises1to5_simplified = simplifyCanonicalAnswer . combineFunctions [(4, translationExercises2_nice), (1, translationExercises2_normal), (5, translationExercises3), (6, translationExercises4), (5, translationExercises5_restricted)]

-- Lesson 6
-- questionExercises5 :: "What did you promise", "What did you say, ..."
-- Interesting: xu do djuno lo se cusku

translations7_noi :: TranslationGenerator
translations7_noi = combineFunctionsUniformly [computer, uses, knower, instructor, friend, house, animals] where
    usesComputers =
        [ (["lo ctuca noi {ke'a} pilno lo skami cu pendo"], ["The instructor, who uses computers, is friendly."])
        , (["lo ctuca noi {ke'a} pilno lo skami cu tavla mi"], ["The instructor, who uses computers, talked to me."])
        , (["lo ctuca noi {ke'a} pilno lo skami cu tavla fi do"], ["The instructor, who uses computers, is talking about you.", "The instructor, who uses computers, talked about you."])
        , (["xu lo ctuca noi {ke'a} pilno lo skami cu tavla mi"], ["Is the instructor, who uses computers, talking to me?"])
        , (["xu lo ctuca noi {ke'a} pilno lo skami cu tavla fi mi"], ["Is the instructor, who uses computers, talking about me?"])
        ]
    computer = generatorFromList $ usesComputers ++
        [ (["lo ctuca noi {ke'a} dunda lo skami cu se zdani"], ["The instructor, who donated the computer, has a house."])
        , (["lo tavla noi {ke'a} dunda lo skami cu se zdani"], ["The speaker, who donated the computer, has a house."])
        ]
    uses = generatorFromList $ usesComputers ++
        [ (["lo ctuca noi {ke'a} pendo cu cusku lo se du'u lo skami cu se pilno"], ["The instructor, who is friendly, said that computers are useful."])
        ]
    knower = generatorFromList
        [ (["lo djuno noi {ke'a} nelci lo nu {ke'a} tavla (ke'a|vo'a) ku'o pendo"], ["The knower, who enjoys talking to himself, is friendly."])
        , (["lo djuno noi {ke'a} nelci lo nu {ke'a} tavla (ke'a|vo'a) ku'o ciska ta"], ["The knower, who enjoys talking to himself, wrote that."])
        , (["lo djuno noi {ke'a} nelci lo nu {ke'a} tavla (ke'a|vo'a) ku'o ctuca mi"], ["The knower, who enjoys talking to himself, taught us."])
        , (["lo djuno noi {ke'a} nelci lo nu {ke'a} tavla (ke'a|vo'a) ku'o pilno lo skami"], ["The knower, who enjoys talking to himself, uses computers."])
        ]
    instructor = generatorFromList
        [ (["lo ctuca noi {ke'a} djuno fi lo gerku ku'o dunda lo plise"], ["The instructor, who knows about dogs, donated the apple."])
        , (["lo ctuca noi {ke'a} tavla do cu nelci lo plise"], ["The instructor, who talked to you, likes apples."])
        , (["lo ctuca noi {ke'a} tavla fi do cu nelci lo plise"], ["The instructor, who talked about you, likes apples.", "The instructor, who talked about you, liked the apple."])
        , (["lo ctuca noi {ke'a} pendo mi cu se zdani"], ["The instructor, who is my friend, has a house."])
        , (["lo ctuca noi {ke'a} pendo mi cu cusku lo se du'u {ra|ko'a} nelci lo mlatu"], ["The instructor, who is my friend, said that he likes cats."])
        ]
    friend = generatorFromList
        -- [ (["mi nelci lo nu lo dunda noi {ke'a} simsa lo ctuca cu pendo"], ["I am happy that the donor, who looked like a teacher, was friendly."])
        [ (["lo dunda noi {ke'a} pendo mi cu gleki"], ["The donor, who is my friend, is happy."])
        , (["lo te dunda noi {ke'a} pendo mi cu gleki"], ["The beneficiary, who is my friend, is happy."])
        , (["lo vecnu noi {ke'a} pendo mi cu gleki"], ["The seller, who is my friend, is happy."])
        , (["lo te vecnu noi {ke'a} pendo mi cu gleki"], ["The buyer, who is my friend, is happy."])

        , (["lo dunda noi {ke'a} pendo mi cu ciska ta"], ["The donor, who is my friend, wrote that."])
        , (["lo te dunda noi {ke'a} pendo mi ciska ta"], ["The beneficiary, who is my friend, wrote that."])
        , (["lo vecnu noi {ke'a} pendo mi cu ciska ta"], ["The seller, who is my friend, wrote that."])
        , (["lo te vecnu noi {ke'a} pendo mi cu ciska ta"], ["The buyer, who is my friend, wrote that."])
        ]
    house = generatorFromList
        [ (["lo zdani noi {ke'a} melbi do cu se dunda fi mi"], ["The house, which you found beautiful, was donated to me."])
        , (["lo zdani noi {ke'a} melbi do cu se dunda mi"], ["The house, which you found beautiful, was donated by me."])
        , (["lo zdani noi {ke'a} melbi do cu se vecnu"], ["The house, which you found beautiful, was sold."])
        , (["lo zdani noi {ke'a} melbi do cu se vecnu lo ctuca"], ["The house, which you found beautiful, was sold by the instructor."])
        , (["lo zdani noi {ke'a} melbi do cu se vecnu fi lo ctuca"], ["The house, which you found beautiful, was sold to the instructor."])
        ]
    animals = combineFunctionsUniformly [mlatu, gerku] where
        mlatu = generatorFromList
            [ (["lo mlatu noi {ke'a} melbi do cu nelci lo gerku"], ["The cat, which you find beautiful, likes dogs."])
            , (["mi nelci lo mlatu noi {ke'a} melbi"], ["I like the cat, which is beautiful."])
            , (["mi nelci lo se dunda noi {ke'a} mlatu"], ["I liked the gift, which was a cat."])
            , (["lo mlatu noi {ke'a} pendo do cu gleki"], ["The cat, who is your friend, is happy."])
            , (["lo pendo noi {ke'a} vecnu lo skami cu nelci lo mlatu"], ["The friend, who sells computers, likes cats."])
            ]
        gerku = generatorFromList
            [ (["lo gerku noi {ke'a} melbi do cu nelci lo mlatu"], ["The dog, which you find beautiful, likes cats."])
            , (["mi nelci lo gerku noi {ke'a} melbi"], ["I like the dog, which is beautiful."])
            , (["mi nelci lo se dunda noi {ke'a} gerku"], ["I liked the gift, which was a dog."])
            , (["lo gerku noi {ke'a} pendo do cu gleki"], ["The dog, who is your friend, is happy."])
            , (["lo pendo noi {ke'a} vecnu lo skami cu nelci lo gerku"], ["The friend, who sells computers, likes dogs."])
            ]

translations7_poi :: TranslationGenerator
translations7_poi = combineFunctionsUniformly [computer, uses, house, animals, general] where
    usesComputers =
        [ (["lo skami poi mi pilno (ke'a|) ku'o melbi"], ["The computer that I use is beautiful."])
        , (["lo skami poi do pilno (ke'a|) ku'o melbi"], ["The computer that you use is beautiful."])
        , (["mi nelci lo skami poi do pilno (ke'a|)"], ["I like the computer that you use."])
        , (["xu do nelci lo skami poi do pilno (ke'a|)"], ["Do you like the computer that you use?"])
        , (["xu do se melbi lo skami poi mi pilno (ke'a|)"], ["Do you find the computer that I use beautiful?"])
        , (["mi nupre lo nu {mi} pilno lo skami poi do dunda ze'a mi"], ["I promised to use the computers that you gave me."])
        , (["mi nupre lo nu {mi} pilno lo skami poi do vecnu ze'a mi"], ["I promised to use the computers that you sold me."])
        , (["mi pilno lo skami poi do dunda ke'a mi lo nu {mi} te vecnu lo mlatu"], ["I used the computer that you gave me to buy a cat."])
        , (["mi pilno lo skami poi do dunda ke'a mi lo nu {mi} te vecnu lo gerku"], ["I used the computer that you gave me to buy a dog."])
        ]
    computer = generatorFromList $ usesComputers ++
        [ (["lo ctuca poi {ke'a} dunda lo skami cu se zdani"], ["The instructor who donated the computer has a house."])
        , (["lo skami poi do tavla fi ke'a cu melbi"], ["The computer that you talked about is beautiful."])
        , (["lo skami poi do dunda (ke'a|) cu melbi"], ["The computer that you donated is beautiful."])
        , (["lo skami poi do nupre lo nu {do} dunda {ke'a} ku'o melbi"], ["The computer that you promised to donate is beautiful."])
        , (["mi nelci lo skami poi do tavla fi ke'a"], ["I like the computer that you talked about."])
        , (["mi nelci lo skami poi do dunda ke'a"], ["I like the computer that you donated."])
        , (["mi nelci lo skami poi do nupre lo nu {do} dunda {ke'a}"], ["I like the computer that you promised to donate."])
        , (["xu do nelci lo skami poi mi tavla fi ke'a"], ["Did you like the computer that I talked about?"])
        , (["mi nelci lo skami poi do vecnu (ke'a|)"], ["I like the computers that you sell.", "I liked the computer that you sold."])
        , (["xu do nelci lo skami poi mi vecnu (ke'a|)"], ["Did you like the computer that I sold?"])
        ]
    uses = generatorFromList $ usesComputers ++
        [ (["lo ctuca noi {ke'a} pendo cu cusku lo se du'u lo skami cu se pilno"], ["The instructor, who is friendly, said that computers are useful."])
        , (["mi gleki lo nu do dunda lo se pilno mi"], ["I am happy that you gave me the tool."])
        , (["mi gleki lo nu do vecnu lo se pilno mi"], ["I am happy that you sold me the tool."])
        , (["xu do gleki lo nu do te vecnu lo se pilno"], ["Are you happy that you bought the tool?"])
        , (["xu do gleki lo nu mi dunda lo se pilno do"], ["Are you happy that I gave you the tool?"])
        , (["xu do gleki lo nu mi vecnu lo se pilno do"], ["Are you happy that I sold you the tool?"])
        ]
    house = generatorFromList
        [ (["lo zdani poi {ke'a} melbi do cu se dunda fi mi"], ["The house that you found beautiful was donated to me."])
        , (["lo zdani poi {ke'a} melbi do cu se dunda mi"], ["The house that you found beautiful was donated by me."])
        , (["lo zdani poi {ke'a} melbi do cu se vecnu"], ["The house that you found beautiful was sold."])
        , (["lo zdani poi {ke'a} melbi do cu se vecnu lo ctuca"], ["The house that you found beautiful was sold by the instructor."])
        , (["lo zdani poi {ke'a} melbi do cu se vecnu fi lo ctuca"], ["The house that you found beautiful was sold to the instructor."])
        , (["xu do vecnu lo zdani poi melbi mi"], ["Did you sell the house that I found beautiful?", "Are you selling the house that I found beautiful?"])
        , (["xu lo ctuca vecnu lo zdani poi {ke'a} melbi do"], ["Did the instructor sell the house that you found beautiful?", "Will the instructor sell the house that you found beautiful?"])
        ]
    animals = combineFunctionsUniformly [mlatu, gerku] where
        mlatu = generatorFromList
            -- [ (["mi nelci lo mlatu poi {ke'a} simsa lo gerku"], ["I like cats that look like dogs."])
            [ (["mi nelci lo mlatu poi {ke'a} pendo lo gerku"], ["I like cats that are friendly to dogs."])
            , (["mi nelci lo mlatu poi {ke'a} nelci lo plise"], ["I like cats that like apples."])
            , (["mi nelci lo plise poi {ke'a} melbi lo mlatu"], ["I like apples that are beautiful to cats."])
            , (["mi nelci lo mlatu poi {ke'a} melbi", "mi nelci lo melbi mlatu"], ["I like the beautiful cat."])
            , (["mi tavla lo prenu poi {ke'a} dunda lo mlatu"], ["I talked to the person who donated the cat."])
            , (["mi tavla lo prenu poi {ke'a} dunda lo mlatu ku mi"], ["I talked to the person who gave me the cat.", "I talked to the person who gave me the cats."])
            , (["mi djuno lo du'u lo mlatu poi do dunda ke'a mi ku'o pendo"], ["I know that the cat you gave me is friendly."])
            , (["mi dunda lo mlatu poi do tavla fi ke'a"], ["I donated the cat that you were talking about."])
            , (["mi nelci lo mlatu poi do tavla fi ke'a"], ["I like the cat that you were talking about."])
            , (["mi nupre lo nu {mi} tavla lo prenu poi {ke'a} dunda lo mlatu"], ["I promised to talk to the person who donated the cat."])
            , (["mi tavla fi lo mlatu poi do nupre lo nu {do} dunda"], ["I talked about the cat that you promised to donate."])
            , (["mi djuno lo du'u do nupre fi lo pendo poi {ke'a} dunda lo mlatu"], ["I know that you made a promise to the friend who donated the cat."])
            ]
        gerku = generatorFromList
            -- [ (["mi nelci lo gerku poi {ke'a} simsa lo mlatu"], ["I like dogs that look like cats."])
            [ (["mi nelci lo gerku poi {ke'a} pendo lo mlatu"], ["I like dogs that are friendly to cats."])
            , (["mi nelci lo gerku poi {ke'a} nelci lo plise"], ["I like dogs that like apples."])
            , (["mi nelci lo plise poi {ke'a} melbi lo gerku"], ["I like apples that are beautiful to dogs."])
            , (["mi nelci lo gerku poi {ke'a} melbi", "mi nelci lo melbi gerku"], ["I like the beautiful dog."])
            , (["mi tavla lo prenu poi {ke'a} dunda lo gerku"], ["I talked to the person who donated the dog."])
            , (["mi tavla lo prenu poi {ke'a} dunda lo gerku ku mi"], ["I talked to the person who gave me the dog.", "I talked to the person who gave me the dogs."])
            , (["mi djuno lo du'u lo gerku poi do dunda ke'a mi ku'o pendo"], ["I know that the dog you gave me is friendly."])
            , (["mi dunda lo gerku poi do tavla fi ke'a"], ["I donated the dog that you were talking about."])
            , (["mi nelci lo gerku poi do tavla fi ke'a"], ["I like the dog that you were talking about."])
            , (["mi nupre lo nu {mi} tavla lo prenu poi {ke'a} dunda lo gerku"], ["I promised to talk to the person who donated the dog."])
            , (["mi tavla fi lo gerku poi do nupre lo nu {do} dunda"], ["I talked about the dog that you promised to donate."])
            , (["mi djuno lo du'u do nupre fi lo pendo poi {ke'a} dunda lo gerku"], ["I know that you made a promise to the friend who donated the dog."])
            ]
    general = generatorFromList
        [ (["mi tavla lo prenu poi {ke'a} nupre fi do"], ["I talked to the person who promised you."])
        , (["xu do tavla lo prenu poi {ke'a} nupre fi mi"], ["Did you talk to the person who promised me?"])
        , (["mi tavla fi lo zdani poi do nupre lo nu {do} dunda"], ["I talked about the house that you promised to donate."])
        , (["xu do tavla fi lo zdani poi do nupre lo nu {do} dunda"], ["Did you talk about the house that you promised to donate?"])
        , (["mi tavla fi lo plise poi do dunda ke'a lo mlatu"], ["I am talking about the apple that you gave to the cat."])
        , (["mi tavla fi lo plise poi do vecnu"], ["I am talking about the apple that you sold."])
        , (["xu lo gerku nelci lo plise poi do dunda {ke'a}"], ["Did the dog like the apple that you gave?"])
        , (["mi tavla lo prenu poi {ke'a} dunda lo plise"], ["I talked to the person who donated the apple."])
        , (["mi tavla fi lo prenu poi {ke'a} dunda lo plise"], ["I talked about person who donated the apple."])
        , (["xu do tavla lo prenu poi {ke'a} dunda lo plise"], ["Did you talk to the person who donated the apple?"])
        , (["mi dunda lo plise poi do tavla fi ke'a"], ["I donated the apple that you were talking about."])
        , (["mi nupre lo nu {mi} tavla lo prenu poi {ke'a} dunda lo plise"], ["I promised to talk to the person who donated the apple."])
        , (["mi tavla fi lo plise poi do nupre lo nu {do} dunda {ke'a}"], ["I talked about the apple that you promised to donate."])
        , (["mi djuno lo du'u do nupre fi lo pendo poi {ke'a} dunda lo plise"], ["I know that you made a promise to the friend who donated the apple."])
        , (["xu do nupre fi lo pendo poi {ke'a} dunda lo plise"], ["Did you make a promise to the friend who donated the apple?"])
        , (["mi cusku lo se du'u mi nelci lo plise poi do vecnu {ke'a}"], ["I said that I liked the apple that you sold."])
        , (["xu do nelci lo plise poi mi vecnu {ke'a}"], ["Did you like the apple that I sold?"])
        , (["xu do nelci lo plise poi mi te vecnu ke'a"], ["Did you like the apple that I bought?"])
        ]

translations7 :: TranslationGenerator
translations7 = expandTranslationGenerator $ combineFunctionsUniformly [translations7_noi, translations7_poi]

translationExercises7 :: ExerciseGenerator
translationExercises7 = generateTranslationExercise basicSentenceCanonicalizer sentenceComparer translations7

translations9 :: [ExerciseGenerator]
translations9 = generateTranslationExercise basicSentenceCanonicalizer sentenceComparer <$> generatorFromSingleton <$>
    [ (["lo prenu ku sutra tavla"], ["The person talks quickly.", "The person is talking quickly.", "A person is talking quickly.", "People talk quickly"])
    ]

--TODO: pause immediately after lesson 5


-------- Tanru
-- useful gismu: sutra, pelxu
-- lo melbi prenu, lo sutra mlatu, lo sutra gerku, lo gleki prenu, lo melbi prenu, mi mutce gleki
-------- Questions
-- Are you talking about the donation? (lo ka dunda)
-- Who wants to talk to me?
-- Who do you think you are talking to? (?)

-------- Exercises
exercises1 :: Dictionary -> ExerciseGenerator
exercises1 dictionary =
    combineFunctions
        [ (20, generateGrammaticalClassExercise vocabulary)
        , (15, generateBridiJufraExercise vocabulary displayBridi)
        , (20, generateSelbriIdentificationExercise vocabulary displayBridi)
        , (10, generateContextualizedGismuPlacePositionExercise dictionary vocabulary displayBridi)
        , (20, generateContextualizedGismuPlaceMeaningExercise dictionary vocabulary displayBridi)
        , (40, translationExercises1)
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
        , (20, generateContextualizedGismuPlacePositionExercise dictionary vocabulary displayBridi)
        , (20, generateContextualizedGismuPlaceMeaningExercise dictionary vocabulary displayBridi)
        , (30, generateIsolatedBrivlaPlacesExercise dictionary $ generatorFromWeightedList $ getVocabularySelbri vocabulary "actions")
        , (60, translationExercises2)
        ]
    where
        vocabulary = vocabularyGenerator2 dictionary
        displayBridi = combineFunctions [(7, displayStandardSimpleBridi), (2, displayVariantSimpleBridi), (1, displayReorderedStandardSimpleBridi)]

exercises3 :: Dictionary -> ExerciseGenerator
exercises3 dictionary =
    combineFunctions
        [ (20, generateIsolatedBrivlaPlacesExercise dictionary $ generatorFromWeightedList $ getVocabularySelbri vocabulary "actions")
        , (20, generateContextualizedGismuPlaceMeaningExercise dictionary vocabulary displayBridi)
        , (20, questionExercises3)
        , (80, translationExercises3)
        ]
    where
        vocabulary = vocabularyGenerator3 dictionary
        displayBridi = combineFunctions [(7, displayStandardSimpleBridi), (2, displayVariantSimpleBridi), (1, displayReorderedStandardSimpleBridi)]

exercises4 :: Dictionary -> ExerciseGenerator
exercises4 dictionary =
    combineFunctions
        [ (30, generateIsolatedBrivlaPlacesExercise dictionary $ generatorFromWeightedList $ getVocabularySelbri vocabulary "actions")
        , (10, generateContextualizedGismuPlaceMeaningExercise dictionary vocabulary displayBridi)
        , (40, abstractionExercises4)
        , (70, translationExercises4)
        ]
    where
        vocabulary = vocabularyGenerator4 dictionary
        displayBridi = combineFunctions [(7, displayStandardSimpleBridi), (2, displayVariantSimpleBridi), (1, displayReorderedStandardSimpleBridi)]

exercises5 :: Dictionary -> ExerciseGenerator
exercises5 dictionary =
    combineFunctions
        [ (20, generateIsolatedBrivlaPlacesExercise dictionary $ generatorFromWeightedList $ getVocabularySelbri vocabulary "actions")
        , (10, generateContextualizedGismuPlaceMeaningExercise dictionary vocabulary displayBridi)
        , (70, translationExercises5_restricted)
        ]
    where
        vocabulary = vocabularyGenerator5 dictionary
        displayBridi = simplifyBridiDisplayer $ (combineFunctions [(7, displayStandardSimpleBridi), (2, displayVariantSimpleBridi), (1, displayReorderedStandardSimpleBridi)])

exercises1to5 :: Dictionary -> ExerciseGenerator
exercises1to5 dictionary =
    combineFunctions
        [ (5, generateGrammaticalClassExercise vocabulary)
        , (5, generateBridiJufraExercise vocabulary displayBridi)
        , (5, generateSelbriIdentificationExercise vocabulary displayBridi)
        , (5, generateContextualizedGismuPlacePositionExercise dictionary vocabulary displayBridi)
        , (15, generateContextualizedGismuPlaceMeaningExercise dictionary vocabulary displayBridi)
        , (15, generateIsolatedBrivlaPlacesExercise dictionary $ generatorFromWeightedList $ getVocabularySelbri vocabulary "actions")
        , (60, translationExercises1to5_simplified)
        , (12, questionExercises3_simplified)
        , (12, abstractionExercises4_simplified)
        ]
    where
        vocabulary = vocabularyGenerator5 dictionary
        displayBridi = simplifyBridiDisplayer $ (combineFunctions [(7, displayStandardSimpleBridi), (2, displayVariantSimpleBridi), (1, displayReorderedStandardSimpleBridi)])

exercises7 :: Dictionary -> ExerciseGenerator
exercises7 dictionary =
    combineFunctions
        [ (20, generateIsolatedBrivlaPlacesExercise dictionary $ generatorFromWeightedList $ getVocabularySelbri vocabulary "actions")
        , (70, translationExercises7)
        ]
    where
        vocabulary = vocabularyGenerator7 dictionary

-- Reminder: from now on, mix propositions and questions

-------- Lessons
lesson1 :: LessonBuilder
lesson1 dictionary = Lesson "Basics 1" (exercises1 dictionary) plan1

lesson2 :: LessonBuilder
lesson2 dictionary = Lesson "Basics 2" (exercises2 dictionary) plan2

lesson3 :: LessonBuilder
lesson3 dictionary = Lesson "Questions 1" (exercises3 dictionary) plan3

lesson4 :: LessonBuilder
lesson4 dictionary = Lesson "Abstractions 1" (exercises4 dictionary) plan4

lesson5 :: LessonBuilder
lesson5 dictionary = Lesson "Terminator elision" (exercises5 dictionary) plan5

checkpoint1to5 :: LessonBuilder
checkpoint1to5 dictionary = Lesson "Checkpoint: Lessons 1–5" (exercises1to5 dictionary) plan1to5

lesson7 :: LessonBuilder
lesson7 dictionary = Lesson "Relative clauses" (exercises7 dictionary) plan7

-------- Course
style :: CourseStyle
style = CourseStyle color1 iconUrl where
    -- Color1
    color1 = Just
        "#4c4c4c"
    -- Icon url
    iconUrl = Just
        -- Source: https://www.flaticon.com/free-icon/jigsaw_993723#term=jigsaw&page=1&position=3
        "https://image.flaticon.com/icons/svg/993/993723.svg"

        -- Source: https://www.flaticon.com/free-icon/jigsaw_993686
        --"https://image.flaticon.com/icons/svg/993/993686.svg"

        -- Source: https://www.flaticon.com/free-icon/puzzle_755205
        --"https://image.flaticon.com/icons/svg/755/755205.svg"

course :: CourseBuilder
course = createCourseBuilder title style lessons where
    title = "Introduction to Grammar"
    lessons = [lesson1, lesson2, lesson3, lesson4, lesson5, checkpoint1to5, lesson7]
