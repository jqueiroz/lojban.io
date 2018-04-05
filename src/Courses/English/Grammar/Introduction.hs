{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

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

-- introduce djica alongside questions: "I want you to be happy" / "Do you want me to be happy?" / "What do you want?" / "Who wants you to be happy" / "Who do you want to be happy?"
-- TODO: remove the translations that make the least sense (in progress...)

-- TODO: programmatic translation generation
-- TODO: indicate optional words using parenthesis

-- Considerations
--   * is "speaker" a good choice? maybe it implies voice or authority...

------- Lesson plans
plan1 :: P.Pandoc
Right plan1 = P.runPure $ P.readMarkdown P.def $ $(embedStringFile "courses/english/introduction/planning/1.md")

plan2 :: P.Pandoc
Right plan2 = P.runPure $ P.readMarkdown P.def $ $(embedStringFile "courses/english/introduction/planning/2.md")

plan3 :: P.Pandoc
Right plan3 = P.runPure $ P.readMarkdown P.def $ $(embedStringFile "courses/english/introduction/planning/3.md")

plan4 :: P.Pandoc
Right plan4 = P.runPure $ P.readMarkdown P.def $ $(embedStringFile "courses/english/introduction/planning/4.md")

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

vocabularyGenerator3 :: VocabularyBuilder
vocabularyGenerator3 = createVocabularyBuilder
    -- Selbri
    [
        ("actions", ((0,) <$> ["tavla", "dunda"]) ++ ((1,) <$> ["ctuca"]) ++ ((2,) <$> ["ciska"])),
        ("relations", (0,) <$> ["nelci", "pendo"]),
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

--TODO: add all new from generator3 (and consider creating exercises using them)
vocabularyGenerator4 :: VocabularyBuilder
vocabularyGenerator4 = createVocabularyBuilder
    -- Selbri
    [
        ("actions", ((0,) <$> ["tavla", "dunda"]) ++ ((1,) <$> ["ctuca", "ciska"]) ++ ((2,) <$> ["nupre", "cusku"])),
        ("relations", ((0,) <$> ["nelci", "pendo"]) ++ ((1,) <$> ["gleki"])),
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

-------- Translations
-- Lesson 1
translations1_nice :: TranslationGenerator
translations1_nice = combineFunctionsUniformly [tavlaReflexive, dundaReordered] where
    tavlaReflexive = generatorFromList
        [ (["mi tavla mi"], ["I am talking to myself.", "I was talking to myself.", "We were talking to ourselves."])
        , (["do tavla do"], ["You are talking to yourself."])
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
        [ (["mi tavla zo'e mi"], ["I was talking about myself.", "We were talking about ourselves.", "I will talk about myself."])
        -- not marked as "nice" because it becomes a special exercise in the next lesson ("translate without zo'e")
        , (["mi tavla zo'e do"], ["I was talking about you.", "We were talking about you.", "I am talking about you.", "We are talking about you.", "I will talk about you.", "We will talk about you."])
        -- not marked as "nice" because it becomes a special exercise in the next lesson ("translate without zo'e")
        , (["mi dunda zo'e do"], ["I gave you something.", "I will give you something."])
        ]

translationExercises1_nice :: ExerciseGenerator
translationExercises1_nice = generateTranslationExercise basicSentenceCanonicalizer translations1_nice

translationExercises1_normal :: ExerciseGenerator
translationExercises1_normal = generateTranslationExercise basicSentenceCanonicalizer translations1_normal

translationExercises1 :: ExerciseGenerator
translationExercises1 = combineFunctions [(1, translationExercises1_nice), (4, translationExercises1_normal)]

-- Lesson 2
translations2_restricted :: TranslationGenerator
translations2_restricted = combineFunctions [(2, talkingAbout), (1, gaveSomething)] where
    talkingAbout = generatorFromList
        [ (["mi tavla fi mi"], ["I was talking about myself.", "We were talking about ourselves.", "I will talk about myself."])
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
        , (["lo tavla ku se zdani"], ["The speaker has a house."])
        , (["lo se tavla ku se zdani"], ["The listener has a house."])
        , (["lo dunda ku se zdani"], ["The donor has a house."])
        , (["lo te dunda ku se zdani"], ["The recipient has a house."])
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
        , (["mi ctuca mi"], ["I taught myself."])
        , (["ctuca mi"], ["Somebody taught me.", "Somebody taught us."])
        , (["ctuca do"], ["Somebody taught you."])
        ]
    friends = generatorFromList
        [ (["lo tavla ku pendo mi"], ["The speaker is my friend.", "The speakers are my friends."])
        , (["lo se tavla ku pendo mi"], ["The listener is my friend.", "The listeners are my friends."])
        , (["lo dunda ku pendo mi"], ["The donor is my friend.", "The donors are my friends."])
        , (["lo te dunda ku pendo mi"], ["The recipient is my friend.", "The recipients are my friends."])
        , (["lo ctuca ku pendo mi"], ["The instructor is my friend.", "The instructors are my friends."])
        , (["lo tavla ku pendo"], ["The speaker is friendly.", "The speakers are friendly."])
        , (["lo se tavla ku pendo"], ["The listener is friendly.", "The listeners are friendly."])
        , (["lo dunda ku pendo"], ["The donor is friendly.", "The donors are friendly."])
        , (["lo te dunda ku pendo"], ["The recipient is friendly.", "The recipients are friendly."])
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
            , (["lo te dunda ku prenu"], ["The recipient is a person."])
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
    restricted = generateRestrictedTranslationExercise "Translate without using \"zo'e\"" (not . containsWord (T.pack "zo'e")) basicSentenceCanonicalizer translations2_restricted
    nice = generateTranslationExercise basicSentenceCanonicalizer translations2_nice

translationExercises2_normal :: ExerciseGenerator
translationExercises2_normal = generateTranslationExercise basicSentenceCanonicalizer translations2_normal

translationExercises2 :: ExerciseGenerator
translationExercises2 =  combineFunctions [(1, translationExercises1_nice), (10, translationExercises2_nice), (5, translationExercises2_normal)]

-- Lesson 3
translations3_restricted_xu :: TranslationGenerator
translations3_restricted_xu = combineFunctions [(2, talkingAbout), (1, gaveSomething), (4, writing)] where
    talkingAbout = generatorFromList
        [ (["xu do tavla fi do"], ["Are you talking about yourself?", "Were you talking about yourself?"])
        , (["xu do tavla fi mi"], ["Are you talking about me?", "Were you talking about me?"])
        , (["xu tavla fi mi"], ["Was somebody talking about me?"])
        , (["xu do tavla fi lo mlatu ku"], ["Were you talking about the cat?", "Were you talking about the cats?", "Were you talking about cats?"])
        , (["xu do tavla fi lo gerku ku"], ["Were you talking about the dog?", "Were you talking about the dogs?", "Were you talking about dogs?"])
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

translations3_normal_xu :: TranslationGenerator
translations3_normal_xu = combineFunctions $ [(3, translations3_restricted_xu), (3, writing)] ++ ((1,) <$> [hasHouse, nice, talking, teaching, friends, others]) where
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
        , (["xu do ctuca do"], ["Did you teach yourself?"])
        , (["xu ctuca do"], ["Did somebody teach you?"])
        ]
    friends = generatorFromList
        [ (["xu do pendo mi"], ["Are you my friend?"])
        , (["xu lo ctuca ku pendo do"], ["Is the instructor your friend?"])
        , (["xu lo dunda ku pendo do"], ["Is the donor your friend?"])
        , (["xu lo te dunda ku pendo do"], ["Is the recipient your friend?"])
        ]
    writing = generatorFromList
        [ (["xu do ciska"], ["Do you write?"])
        , (["xu lo prenu ku ciska"], ["Do people write?"])
        , (["xu do ciska ti"], ["Did you write this?"])
        , (["xu do ciska ta"], ["Did you write that?"])
        ]
    others = generatorFromList
        [ (["xu do nelci lo xe ctuca ku"], ["Do you like the teaching method?"])
        ]

translations3_restricted_ma :: TranslationGenerator
translations3_restricted_ma = combineFunctions [(2, talkingAbout), (1, gaveSomething), (4, writing)] where
    talkingAbout = generatorFromList
        [ (["ma tavla fi mi"], ["Who is talking about me?", "Who is talking about us?", "Who was talking about me?", "Who was walking about us?"])
        , (["ma tavla fi do"], ["Who is talking about you?", "Who was talking about you?"])
        , (["ma tavla fi lo mlatu ku"], ["Who is talking about the cat?"])
        , (["ma tavla fi lo gerku ku"], ["Who is talking about the dog?"])
        , (["do tavla fi ma"], ["What are you talking about?", "What were you talking about?"])
        , (["lo prenu ku tavla fi ma"], ["What is the person talking about?", "What was the person talking about?"])
        , (["lo dunda ku tavla fi ma"], ["What is the donor talking about?", "What was the donor talking about?"])
        , (["lo te dunda ku tavla fi ma"], ["What is the recipient talking about?", "What was the recipient talking about?"])
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

translations3_normal_ma :: TranslationGenerator
translations3_normal_ma = combineFunctions $ [(3, translations3_restricted_ma), (3, writing)] ++ ((1,) <$> [hasHouse, nice, talking, giving, teaching]) where
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
            , (["ma dunda do ta"], ["Who gave you that?"])
            , (["ma dunda mi ti"], ["Who gave me this?"])
            , (["ma dunda ti"], ["Who donated this?"])
            , (["ma dunda ta"], ["Who donated that?"])
            , (["ma dunda fi lo zdani ku"], ["Who donated the house?"])
            ]
        mlatu = generatorFromList
            [ (["ma te dunda lo mlatu ku"], ["Who was given a cat?"])
            , (["do dunda lo mlatu ku ma"], ["To whom did you give the cat?", "To whom did you give the cats?"])
            , (["ma dunda lo mlatu ku lo ctuca ku"], ["Who gave the cat to the instructor?"])
            , (["ma dunda lo mlatu ku mi"], ["Who gave me a cat?"])
            , (["ma dunda lo mlatu ku do"], ["Who gave you a cat?"])
            , (["ma dunda fi lo mlatu ku"], ["Who donated the cat?"])
            ]
        gerku = generatorFromList
            [ (["ma te dunda lo gerku ku"], ["Who was given a dog?"])
            , (["do dunda lo gerku ku ma"], ["To whom did you give the dog?", "To whom did you give the dogs?"])
            , (["ma dunda lo gerku ku lo ctuca ku"], ["Who gave the dog to the instructor?"])
            , (["ma dunda lo gerku ku mi"], ["Who gave me a dog?"])
            , (["ma dunda lo gerku ku do"], ["Who gave you a dog?"])
            , (["ma dunda fi lo gerku ku"], ["Who donated the dog?"])
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
        [ (["ciska ti zo'e ma"], ["Which instrument was used to write this?"])
        , (["do ciska ta zo'e ma"], ["Which instrument did you use to write that?"])
        -- what
        , (["do ciska ma"], ["What did you write?", "What are you going to write?"])
        , (["do ciska ma ta"], ["What did you write there?", "What are you going to write there?"])
        , (["do ciska ma zo'e ta"], ["What did you write using that?"])
        , (["ciska ma ta"], ["What is written there?"])
        , (["ciska ma zo'e ti"], ["What was written using this?"])
        -- who
        , (["ma ciska"], ["Who is writing?"])
        , (["ma ciska ti"], ["Who wrote this?"])
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
    restricted = generateRestrictedTranslationExercise "Translate without using \"zo'e\"" (not . containsWord (T.pack "zo'e")) basicSentenceCanonicalizer translations3_restricted
    normal = generateTranslationExercise basicSentenceCanonicalizer translations3_normal

questionExercises3 = generateFillingBlanksExercise ["mo", "ma"] $ combineFunctionsUniformly [translations3_normal_ma, translations3_normal_mo]

-- Lesson 4
-- Are the sentences involving tavla really sensible?
-- words common enough: gleki, tavla, dunda, nelci, mlatu, gerku, prenu, nupre, zdani
-- pending words: ctuca, melbi, pendo?
-- TODO: teach ko'a?
-- TODO: more examples using du'u
-- consider using: morji, ciksi, jijnu (useful for teaching du'u)
-- TODO: add sentences using promisee
translations4 :: TranslationGenerator
translations4 = combineFunctions [(2, gleki), (1, tavla), (2, nupre), (2, cusku)] where
    gleki = combineFunctionsUniformly [nothingSpecific, talking, beautiful, givingAnimals, liking, teaching, owningHouse, other] where
        nothingSpecific = generatorFromList
            [ (["mi gleki"], ["I am happy."])
            , (["lo prenu ku gleki"], ["The person is happy."])
            , (["lo gerku ku gleki"], ["The dog is happy.", "The dogs are happy."])
            , (["lo mlatu ku gleki"], ["The cat is happy.", "The cats are happy."])
            ]
        talking = generatorFromList
            -- talking to someone
            [ (["mi gleki lo nu do tavla mi kei ku"], ["I am happy that you talked to me."])
            , (["mi gleki lo nu mi tavla do kei ku", "mi gleki lo nu tavla do kei ku"], ["I am happy that I talked to you."])
            -- talking about animals
            , (["mi gleki lo nu do tavla mi lo mlatu ku kei ku"], ["I am happy that you talked to me about the cat.", "I am happy that you talked to me about cats."])
            , (["mi gleki lo nu do tavla mi lo gerku ku kei ku"], ["I am happy that you talked to me about the dog.", "I am happy that you talked to me about dogs."])
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
            , (["mi gleki lo nu lo prenu ku nelci do kei ku"], ["I am happy that people like you."])
            , (["mi gleki lo nu lo ctuca ku nelci mi kei ku"], ["I am happy that the instructor likes me."])
            , (["mi gleki lo nu lo ctuca ku nelci do kei ku"], ["I am happy that the instructor likes you."])
            , (["mi gleki lo nu do nelci lo ctuca ku kei ku"], ["I am happy that you like the instructor."])
            ]
        teaching = generatorFromList
            [ (["mi gleki lo nu do ctuca mi kei ku"], ["I am happy that you taught me."])
            , (["mi gleki lo nu mi ctuca do kei ku"], ["I am happy that I taught you."])
            ]
        owningHouse = generatorFromList
            [ (["mi gleki lo nu mi se zdani kei ku"], ["I am happy that I have a house."])
            , (["mi gleki lo nu do se zdani kei ku"], ["I am happy that you have a house."])
            ]
        other = generatorFromList
            [ (["mi gleki lo nu do gleki kei ku"], ["I am happy that you are happy."])
            , (["mi gleki lo nu mi prenu kei ku"], ["I am happy that I am a person."])
            , (["mi gleki lo nu do prenu kei ku"], ["I am happy that you are a person."])
            , (["mi gleki lo nu do pendo mi kei ku"], ["I am happy that you are my friend."])
            , (["mi gleki lo nu lo ctuca ku pendo mi kei ku"], ["I am happy that the instructor is my friend."])
            , (["mi gleki lo nu lo te dunda ku pendo mi kei ku"], ["I am happy that the recipient is my friend."])
            ]
    tavla = combineFunctionsUniformly [owningHouse, animalsLikingEachOther, promise, promisorTalked] where
        owningHouse = generatorFromList
            [ (["mi tavla fi lo nu do se zdani kei ku"], ["I talked about you having a house.", "We talked about you having a house."])
            ]
        animalsLikingEachOther = generatorFromList
            [ (["do tavla fi lo nu lo mlatu ku nelci lo gerku ku kei ku"], ["You talked about cats liking dogs."])
            , (["do tavla fi lo nu lo gerku ku nelci lo mlatu ku kei ku"], ["You talked about dogs liking cats."])
            , (["do tavla mi lo nu lo gerku ku nelci lo mlatu ku kei ku"], ["You talked to me about dogs liking cats."])
            , (["do tavla mi lo nu lo mlatu ku nelci lo gerku ku kei ku"], ["You talked to me about cats liking dogs."])
            ]
        promise = generatorFromList
            [ (["mi tavla fi lo se nupre ku"], ["I talked about the promise."])
            , (["do tavla fi lo se nupre ku"], ["You talked about the promise."])
            , (["mi tavla do lo se nupre ku"], ["I talked to you about the promise."])
            , (["do tavla mi lo se nupre ku"], ["You talked to me about the promise."])
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
    nupre = combineFunctionsUniformly [nothingSpecific, donatingAnimals, donatingHouses, teaching, beingFriendly] where
        nothingSpecific = generatorFromList
            [ (["lo prenu ku nupre"], ["People make promises."])
            , (["do nupre"], ["You made a promise."])
            , (["do nupre fi mi"], ["You promised me.", "You promised us."])
            , (["mi nupre fi do"], ["I promised you.", "We promised you."])
            ]
        donatingAnimals = generatorFromList
            [ (["do nupre lo nu dunda lo mlatu ku kei ku", "do nupre lo nu do dunda lo mlatu ku kei ku"], ["You promised to donate the cat.", "You promised to donate the cats."])
            , (["do nupre lo nu dunda lo gerku ku kei ku", "do nupre lo nu do dunda lo gerku ku"], ["You promised to donate the dog.", "You promised to donate the dogs."])
            , (["do nupre lo nu dunda lo mlatu ku mi kei ku", "do nupre lo nu do dunda lo mlatu ku mi kei ku"], ["You promised to donate the cat to me.", "You promised to donate the cats to me.", "You promised to donate the cats to us."])
            , (["do nupre lo nu dunda lo gerku ku mi kei ku", "do nupre lo nu do dunda lo gerku ku mi kei ku"], ["You promised to donate the dog to me.", "You promised to donate the dogs to me.", "You promised to donate the dogs to us."])
-- preciso revisar a partir daqui (ver oq faz sentido)
            , (["do nupre lo nu dunda lo mlatu ku kei ku mi", "do nupre lo nu do dunda lo mlatu ku kei ku mi"], ["You promised me to donate the cat.", "You promised me to donate the cats.", "You promised us to donate the cat.", "You promised us to donate the cats."])
            , (["do nupre lo nu dunda lo gerku ku kei ku mi", "do nupre lo nu do dunda lo gerku ku kei ku mi"], ["You promised me to donate the dog.", "You promised me to donate the dogs.", "You promised us to donate the dog.", "You promised us to donate the dogs."])
            ]
        donatingHouses = generatorFromList
            [ (["do nupre lo nu dunda lo zdani ku kei ku mi", "do nupre lo nu do dunda lo zdani ku kei ku mi"], ["You promised me to donate the house.", "You promised me to donate the houses.", "You promised us to donate the house.", "You promised us to donate the houses."])
            , (["do nupre lo nu dunda lo zdani ku mi kei ku mi", "do nupre lo nu do dunda lo zdani ku mi kei ku mi"], ["You promised to donate the house to me.", "You promised to donate the houses to us."])
            ]
        teaching = generatorFromList
            [ (["do nupre lo nu ctuca mi kei ku", "do nupre lo nu do ctuca mi kei ku"], ["You promised to teach me.", "You promised to teach us."])
            , (["mi nupre lo nu ctuca do kei ku", "mi nupre lo nu mi ctuca do kei ku"], ["I promised to teach you."])
            ]
        beingFriendly = generatorFromList
            [ (["do nupre lo nu pendo kei ku", "do nupre lo nu do pendo kei ku"], ["You promised to be friendly."])
            ]
    cusku = combineFunctionsUniformly [nothingSpecific, beautiful, likingPeople, likingAnimals, donatingAnimals, beingFriendly, others] where
        nothingSpecific = generatorFromList
            [ (["mi cusku"], ["I said something.", "I was saying something.", "I will say something."])
            , (["do cusku"], ["You said something.", "You were saying something."])
            , (["lo prenu ku cusku"], ["The person said something.", "The person was saying something."])
            ]
        beautiful = generatorFromList
            [ (["mi cusku lo se du'u do melbi kei ku"], ["I said that you are beautiful."])
            , (["mi cusku lo se du'u lo prenu ku melbi kei ku"], ["I said that the person is beautiful."])
            , (["do cusku lo se du'u mi melbi kei ku"], ["You said that I am beautiful."])
            , (["do cusku lo se du'u lo prenu ku melbi kei ku"], ["You said that the person is beautiful."])
            ]
        likingPeople = generatorFromList
            [ (["mi cusku lo se du'u mi nelci do kei ku", "mi cusku lo se du'u nelci do kei ku"], ["I said that I like you."])
            , (["do cusku lo se du'u do nelci mi kei ku", "do cusku lo se du'u nelci mi kei ku"], ["You said that you like me."])
            , (["lo prenu ku cusku lo se du'u mi nelci do kei ku"], ["The person said that I like you."])
            , (["lo prenu ku cusku lo se du'u do nelci mi kei ku"], ["The person said that you like me."])
            , (["lo prenu ku cusku lo se du'u nelci mi kei ku"], ["The person said that she likes me."])
            , (["lo prenu ku cusku lo se du'u nelci do kei ku"], ["The person said that she likes you."])
            ]
        likingAnimals = generatorFromList
            -- mlatu
            [ (["mi cusku lo se du'u mi nelci lo mlatu ku kei ku", "mi cusku lo se du'u nelci lo mlatu ku kei ku"], ["I said that I like the cat.", "I said that I like the cats.", "I said that I like cats."])
            , (["mi cusku lo se du'u do nelci lo mlatu ku kei ku"], ["I said that you like the cat.", "I said that you like the cats.", "I said that you like cats."])
            , (["do cusku lo se du'u do nelci lo mlatu ku kei ku", "do cusku lo se du'u nelci lo mlatu ku kei ku"], ["You said that you like the cat.", "You said that you like the cats.", "You said that you like cats."])
            , (["do cusku lo se du'u mi nelci lo mlatu ku kei ku"], ["You said that I like the cat.", "You said that I like the cats.", "You said that I like cats."])
            , (["lo prenu ku cusku lo se du'u mi nelci lo mlatu ku kei ku"], ["The person said that I like the cat.", "The person said that I like the cats.", "The person said that I like cats."])
            , (["lo prenu ku cusku lo se du'u do nelci lo mlatu ku kei ku"], ["The person said that you like the cat.", "The person said that you like the cats.", "The person said that you like cats."])
            -- gerku
            , (["mi cusku lo se du'u mi nelci lo gerku ku kei ku", "mi cusku lo se du'u nelci lo gerku ku kei ku"], ["I said that I like the dog.", "I said that I like the dogs.", "I said that I like dogs."])
            , (["mi cusku lo se du'u do nelci lo gerku ku kei ku"], ["I said that you like the dog.", "I said that you like the dogs.", "I said that you like dogs."])
            , (["do cusku lo se du'u do nelci lo gerku ku kei ku", "do cusku lo se du'u nelci lo gerku ku kei ku"], ["You said that you like the dog.", "You said that you like the dogs.", "You said that you like dogs."])
            , (["do cusku lo se du'u mi nelci lo gerku ku kei ku"], ["You said that I like the dog.", "You said that I like the dogs.", "You said that I like dogs."])
            , (["lo prenu ku cusku lo se du'u mi nelci lo gerku ku kei ku"], ["The person said that I like the dog.", "The person said that I like the dogs.", "The person said that I like dogs."])
            , (["lo prenu ku cusku lo se du'u do nelci lo gerku ku kei ku"], ["The person said that you like the dog.", "The person said that you like the dogs.", "The person said that you like dogs."])
            ]
        donatingAnimals = generatorFromList
            [ (["mi cusku lo se du'u mi dunda lo mlatu ku kei ku", "mi cusku lo se du'u dunda lo mlatu ku kei ku"], ["I said that I would donate the cat.", "I said that I would donate the cats."])
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
            ]
        beingFriendly = generatorFromList
            [ (["mi cusku lo se du'u pendo kei ku", "mi cusku lo se du'u mi pendo kei ku"], ["I said that I would be friendly."])
            , (["do cusku lo se du'u pendo kei ku", "do cusku lo se du'u do pendo kei ku"], ["You said that you would be friendly."])
            , (["mi cusku lo se du'u do pendo kei ku"], ["I said that you would be friendly."])
            ]
        others = generatorFromList
            [ (["mi nelci lo nu tavla do kei ku", "mi nelci lo nu mi tavla do kei ku"], ["I like to talk to you."]) -- is nelci really adequate?
            , (["do nelci lo nu nupre kei ku", "do nelci lo nu do nupre kei ku"], ["You like to make promises."]) -- is nelci really adequate?
            , (["lo prenu ku nelci lo nu nupre kei ku"], ["People like to make promises."]) -- is nelci really adequate?
            -- Wait until terminator ellision has been explained to use the following sentences
            {-, (["mi cusku lo se du'u mi nelci lo nu tavla do kei ku kei ku", "mi cusku lo se du'u mi nelci lo nu mi tavla do kei ku kei ku"], ["I said that I like to talk to you."])-}
            {-, (["mi cusku lo se du'u do nelci lo nu tavla mi kei ku kei ku", "mi cusku lo se du'u do nelci lo nu do tavla mi kei ku kei ku"], ["I said that you like to talk to me."])-}
            ]


translationExercises4 :: ExerciseGenerator
translationExercises4 = generateTranslationExercise basicSentenceCanonicalizer translations4

translations5 :: [ExerciseGenerator]
translations5 = generateTranslationExercise basicSentenceCanonicalizer <$> generatorFromSingleton <$>
    [
    ]

translations9 :: [ExerciseGenerator]
translations9 = generateTranslationExercise basicSentenceCanonicalizer <$> generatorFromSingleton <$>
    [ (["lo prenu ku sutra tavla"], ["The person talks quickly.", "The person is talking quickly.", "A person is talking quickly.", "People talk quickly"])
    ]

--TODO: pause immediately after lesson 4


-------- Tanru
-- useful gismu: sutra, pelxu
-- lo melbi prenu, lo sutra mlatu, lo sutra gerku, lo gleki prenu, lo melbi prenu, mi mutce gleki
-------- Questions
-- useful gismu: melbi
-- xu [...] melbi do
-- Why are you happy?
-- What did you give me?
-- What did you receive?
-- What did you say?
-- Did you say that you find me beautiful?
-- Who is talking to you?
-- Who is talking to you about me?
-- What did you do to me?
-- What did you do to the gift?
--   Are you going to teach me?
--   Were you talking about me?
--   Is the house/cat/dog beautiful to you?
--   Did somebody talk to you about the cat?
--   Do you like the teaching method?
-- Are you talking about the donation? (lo ka dunda)
-- Who wants to talk to me?
-- Who do you think you are talking to? (?)
-- I know that you are my friend
-- Do you know that the cat is my friend?
-- generateFillBlanksExercise "mi (ma) do" (think more about context)
-- lo se nupre === what has been promised

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
        , (10, generateContextualizedGismuPlaceMeaningExercise dictionary vocabulary displayBridi)
        , (30, generateIsolatedGismuPlacesExercise dictionary vocabulary)
        , (50, translationExercises2)
        ]
    where
        vocabulary = vocabularyGenerator2 dictionary
        displayBridi = combineFunctions [(7, displayStandardSimpleBridi), (2, displayVariantSimpleBridi), (1, displayReorderedStandardSimpleBridi)]

exercises3 :: Dictionary -> ExerciseGenerator
exercises3 dictionary =
    combineFunctions
        [ (20, generateIsolatedGismuPlacesExercise dictionary vocabulary)
        , (20, generateContextualizedGismuPlaceMeaningExercise dictionary vocabulary displayBridi)
        , (20, questionExercises3)
        , (70, translationExercises3)
        ]
    where
        vocabulary = vocabularyGenerator3 dictionary
        displayBridi = combineFunctions [(7, displayStandardSimpleBridi), (2, displayVariantSimpleBridi), (1, displayReorderedStandardSimpleBridi)]

-- TODO: exercise: fill with "lo nu" vs "lo du'u" vs "lo se du'u"?
exercises4 :: Dictionary -> ExerciseGenerator
exercises4 dictionary =
    combineFunctions
        [ (5, generateGrammaticalClassExercise vocabulary)
        , (5, generateBridiJufraExercise vocabulary displayBridi)
        , (5, generateSelbriIdentificationExercise vocabulary displayBridi)
        , (5, generateContextualizedGismuPlacePositionExercise dictionary vocabulary displayBridi)
        , (20, generateContextualizedGismuPlaceMeaningExercise dictionary vocabulary displayBridi)
        , (30, generateIsolatedGismuPlacesExercise dictionary vocabulary)
        , (70, translationExercises4)
        ]
    where
        vocabulary = vocabularyGenerator4 dictionary
        displayBridi = combineFunctions [(7, displayStandardSimpleBridi), (2, displayVariantSimpleBridi), (1, displayReorderedStandardSimpleBridi)]

-------- Lessons
lesson1 :: LessonBuilder
lesson1 dictionary = Lesson "Basics 1" (exercises1 dictionary) plan1

lesson2 :: LessonBuilder
lesson2 dictionary = Lesson "Basics 2" (exercises2 dictionary) plan2

lesson3 :: LessonBuilder
lesson3 dictionary = Lesson "Questions 1" (exercises3 dictionary) plan3

lesson4 :: LessonBuilder
lesson4 dictionary = Lesson "Abstractions 1" (exercises4 dictionary) plan3

-------- Course
course :: CourseBuilder
course = createCourseBuilder "Introduction to Lojban for English speakers" [lesson1, lesson2, lesson3, lesson4]
