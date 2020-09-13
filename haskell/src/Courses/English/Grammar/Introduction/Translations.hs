{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | This module provides translations (and exercises relying on translations) for each of the course lessons.
module Courses.English.Grammar.Introduction.Translations where

import Core
import Courses.Framework.TranslationUtils
import Util (combineGenerators, combineGeneratorsUniformly, generatorFromSingleton, generatorFromList)

-- * Lesson 1: Basics 1

-- | Translations for the first lesson.
translations1 :: TranslationGenerator
translations1 = combineGeneratorsUniformly [dunda, demonstrative, zdani] where
    dunda = generatorFromList
        [ (["do dunda ti mi"], ["You give me this.", "You give us this."])
        , (["do dunda ta mi"], ["You give me that.", "You give us that."])
        , (["mi dunda ti do"], ["I give you this.", "We give you this."])
        , (["mi dunda ta do"], ["I give you that.", "We give you that."])
        ]
    demonstrative = generatorFromList
        [ (["ti pelxu"], ["This is yellow.", "These are yellow."])
        , (["ta pelxu"], ["That is yellow.", "Those are yellow."])
        , (["ta zdani"], ["That is a house.", "Those are houses."])
        ]
    zdani = generatorFromList
        [ (["zdani mi"], ["I have a house.", "We have a house.", "We have houses."])
        , (["zdani do"], ["You have a house.", "You have houses."])
        ]

-- * Lesson 2: Basics 2

-- | Overall translations for the second lesson.
translations2 :: TranslationGenerator
translations2 = combineGenerators [(1, translations2_nice), (4, translations2_normal)]

-- | Interesting translations for the second lesson.
--
-- Defined separately so that they may be reused in the third lesson.
translations2_nice :: TranslationGenerator
translations2_nice = combineGeneratorsUniformly [tavlaReflexive, dundaReordered] where
    tavlaReflexive = generatorFromList
        [ (["mi tavla mi", "mi tavla vo'a"], ["I am talking to myself.", "I was talking to myself.", "We were talking to ourselves."])
        , (["do tavla do", "do tavla vo'a"], ["You are talking to yourself."])
        ]
    dundaReordered = generatorFromList
        [ (["do dunda ti mi"], ["You gave me this.", "You gave us this."])
        , (["mi dunda ta do"], ["I gave you that.", "We gave you that."])
        ]

-- | Regular translations for the second lesson.
translations2_normal :: TranslationGenerator
translations2_normal = combineGeneratorsUniformly $ others ++ [talkingWithSecondPerson, pendo, prenu, dunda, demonstrative, zdani] where
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
    dunda = generatorFromList
        [ (["mi dunda zo'e do"], ["I gave something to you."])
        , (["do dunda zo'e mi"], ["You gave something to me."])
        ]
    demonstrative = generatorFromList
        [ (["ti mlatu"], ["This is a cat.", "These are cats."])
        , (["ta mlatu"], ["That is a cat.", "Those are cats."])
        , (["ti pelxu"], ["This is yellow.", "These are yellow."])
        , (["ta pelxu"], ["That is yellow.", "Those are yellow."])
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

-- * Lesson 3: Basics 3

-- | Overall translations for the third lesson.
translations3 :: TranslationGenerator
translations3 =  combineGenerators [(1, translations3_nice), (10, translations3_nice), (5, translations3_normal)]

-- | Translations for the third lesson, with the restriction that some intermediate place is missing.
--
-- Defined separately so that they may be used in 'Translate without using "zo'e"' exercises.
translations3_restricted :: TranslationGenerator
translations3_restricted = combineGenerators [(2, talkingAbout), (1, gaveSomething)] where
    talkingAbout = generatorFromList
        [ (["mi tavla fi mi", "mi tavla fi vo'a"], ["I was talking about myself.", "We were talking about ourselves.", "I will talk about myself."])
        , (["mi tavla fi do"], ["I was talking about you.", "We were talking about you.", "I am talking about you.", "We are talking about you.", "I will talk about you.", "We will talk about you."])
        , (["tavla fi mi"], ["Somebody was talking about me.", "Somebody was talking about us."])
        , (["tavla fi do"], ["Somebody was talking about you."])
        , (["mi tavla fi lo mlatu ku"], ["I was talking about the cat.", "I was talking about the cats.", "I was talking about cats", "I am talking about the cat.", "I am talking about the cats.", "I am talking about cats."])
        , (["tavla fi lo mlatu ku"], ["Somebody was talking about the cat.", "Somebody was talking about the cats.", "Somebody was talking about cats."])
        , (["mi tavla fi lo gerku ku"], ["I was talking about the dog.", "I was talking about the dogs.", "I was talking about dogs.", "I am talking about the dog.", "I am talking about the dogs.", "I am talking about dogs."])
        , (["tavla fi lo gerku ku"], ["Somebody was talking about the dog.", "Somebody was talking about the dogs.", "Somebody was talking about dogs."])
        , (["mi tavla fi lo se dunda ku"], ["I was talking about the gift.", "We were talking about the gift.", "I am talking about the gift.", "We are talking about the gift."])
        ]
    gaveSomething = generatorFromList
        [ (["mi dunda fi do"], ["I gave you something.", "I will give you something."])
        , (["do dunda fi mi"], ["You gave me something."])
        ]

-- | Interesting translations for the third lesson.
--
-- Defined separately so that they may be reused in the checkpoint lesson (Lesson 7).
translations3_nice :: TranslationGenerator
translations3_nice = combineGenerators $ [(2, translations3_restricted), (2, teaching)] ++ ((1,) <$> [hasHouse, niceGift, giftingAnimal, friends, pelxu, others]) where
    hasHouse = generatorFromList
        [ (["lo ctuca ku se zdani"], ["The instructor has a house."])
        , (["lo prenu ku se zdani"], ["The person has a house."])
        , (["lo tavla ku se zdani", "lo cusku ku se zdani"], ["The speaker has a house."])
        , (["lo se tavla ku se zdani"], ["The listener has a house."])
        , (["lo dunda ku se zdani"], ["The donor has a house."])
        , (["lo te dunda ku se zdani"], ["The beneficiary has a house."])
        ]
    niceGift = combineGeneratorsUniformly [beautifulGift, likedGift] where
        beautifulGift = generatorFromList
            [ (["lo se dunda ku melbi mi"], ["The gift is beautiful to me.", "The gifts are beautiful to me."])
            , (["lo se dunda ku melbi"], ["The gift is beautiful.", "The gifts are beautiful."])
            ]
        likedGift = generatorFromList
            [ (["lo te dunda ku nelci lo se dunda ku"], ["The recipient liked the gift.", "The recipient will like the gift.", "The recipients liked the gifts."])
            , (["lo ctuca ku nelci lo se dunda ku"], ["The instructor liked the gift.", "The instructor will like the gift."])
            , (["mi nelci lo se dunda ku"], ["I liked the gift.", "I liked the gifts."])
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
    pelxu = generatorFromList
        [ (["lo zdani ku pelxu"], ["The house is yellow.", "The houses are yellow."])
        , (["lo mlatu ku pelxu"], ["The cat is yellow.", "The cats are yellow."])
        , (["lo gerku ku pelxu"], ["The dog is yellow.", "The dogs are yellow."])
        , (["lo se dunda ku pelxu"], ["The gift is yellow."])
        ]
    others = generatorFromList
        [ (["mi nelci lo xe ctuca ku"], ["I like the teaching method."])
        , (["do nelci lo xe ctuca ku"], ["You like the teaching method."])
        ]

-- | Regular translations for the third lesson.
--
translations3_normal :: TranslationGenerator
translations3_normal = combineGeneratorsUniformly [talkingToAnimal, likingAnimals, animalFriends, beautiful, person, others] where
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

-- * Lesson 4: Tanru
-- TODO: tanru for "adverbs", in addition to the existing tanru for "adjectives" (changes in the canonicalizer will likely be necessary)
-- TODO: sentences/expressions using "sutra" and "lojbo"
-- TODO: if this lesson is placed before "Questions", remove translations using "xu"
-- TODO: add missing "ku"
translations4_expressions :: TranslationGenerator
translations4_expressions = expandTranslationGenerator $ combineGeneratorsUniformly [sutra, lojbo, pendo, gleki, melbi, others] where
    sutra = generatorFromList
        [ (["lo sutra prenu ku"], ["The fast person."])
        , (["lo sutra vecnu ku"], ["The fast seller."])
        , (["lo sutra te vecnu ku"], ["The fast buyer."])
        , (["lo sutra tavla ku"], ["The fast speaker."])
        , (["lo sutra ctuca ku"], ["The fast teacher."])
        , (["lo sutra gerku ku"], ["The fast dog."])
        , (["lo sutra mlatu ku"], ["The fast cat."])
        ]
    lojbo = generatorFromList
        [ (["lo lojbo prenu ku"], ["The Lojbanic person."])
        , (["lo lojbo dunda ku"], ["The Lojbanic donor."])
        , (["lo lojbo vecnu ku"], ["The Lojbanic seller."])
        , (["lo lojbo te vecnu ku"], ["The Lojbanic buyer."])
        , (["lo lojbo tavla ku"], ["The Lojbanic speaker."])
        , (["lo lojbo ctuca ku"], ["The Lojbanic teacher."])
        , (["lo lojbo gerku ku"], ["The Lojbanic dog."])
        , (["lo lojbo mlatu ku"], ["The Lojbanic cat."])
        ]
    pendo = generatorFromList
        [ (["lo pendo prenu ku"], ["The friendly person."])
        , (["lo pendo dunda ku"], ["The friendly donor."])
        , (["lo pendo te dunda  ku"], ["The friendly recipient."])
        , (["lo pendo vecnu ku"], ["The friendly seller."])
        , (["lo pendo te vecnu ku"], ["The friendly buyer."])
        , (["lo pendo tavla ku"], ["The friendly speaker."])
        , (["lo pendo ctuca ku"], ["The friendly teacher."])
        , (["lo pendo gerku ku"], ["The friendly dog."])
        , (["lo pendo mlatu ku"], ["The friendly cat."])
        ]
    gleki = generatorFromList
        [ (["lo gleki prenu ku"], ["The happy person."])
        , (["lo gleki dunda ku"], ["The happy donor."])
        , (["lo gleki te dunda  ku"], ["The happy recipient."])
        , (["lo gleki vecnu ku"], ["The happy seller."])
        , (["lo gleki te vecnu ku"], ["The happy buyer."])
        , (["lo gleki tavla ku"], ["The happy speaker."])
        , (["lo gleki ctuca ku"], ["The happy teacher."])
        , (["lo gleki gerku ku"], ["The happy dog."])
        , (["lo gleki mlatu ku"], ["The happy cat."])
        ]
    melbi = generatorFromList
        [ (["lo melbi prenu ku"], ["The beautiful person."])
        , (["lo melbi dunda ku"], ["The beautiful donor."])
        , (["lo melbi te dunda  ku"], ["The beautiful recipient."])
        , (["lo melbi vecnu ku"], ["The beautiful seller."])
        , (["lo melbi te vecnu ku"], ["The beautiful buyer."])
        , (["lo melbi tavla ku"], ["The beautiful speaker."])
        , (["lo melbi ctuca ku"], ["The beautiful teacher."])
        , (["lo melbi gerku ku"], ["The beautiful dog."])
        , (["lo melbi mlatu ku"], ["The beautiful cat."])
        , (["lo melbi skami ku"], ["The beautiful computer."])
        , (["lo melbi plise ku"], ["The beautiful apple."])
        ]
    others = generatorFromList
        [ (["lo nupre se dunda ku"], ["The promised gift."])
        , (["lo gerku zdani ku"], ["The dog house."])
        , (["lo tavla skami ku"], ["The talking computer."])
        , (["lo tavla gerku ku"], ["The talking dog."])
        , (["lo tavla mlatu ku"], ["The talking cat."])
        ]

translations4_sentences :: TranslationGenerator
translations4_sentences = expandTranslationGenerator $ combineGeneratorsUniformly [adverb, adjective] where
    adverb = combineGeneratorsUniformly [sutra] where
        sutra = generatorFromList
            -- sutra tavla
            [ (["mi sutra tavla"], ["I talk quickly."])
            , (["do sutra tavla"], ["You talk quickly."])
            , (["lo ctuca ku sutra tavla"], ["The teacher talks quickly."])
            , (["lo dunda ku sutra tavla"], ["The donor talks quickly."])
            , (["lo vecnu ku sutra tavla"], ["The seller talks quickly."])
            , (["lo te vecnu ku sutra tavla"], ["The buyer talks quickly."])
            , (["mi sutra tavla lo prenu ku"], ["I talked to the person quickly."])
            , (["mi sutra tavla lo ctuca ku"], ["I talked to the teacher quickly."])
            , (["mi sutra tavla lo dunda ku"], ["I talked to the donor quickly."])
            , (["mi sutra tavla lo vecnu ku"], ["I talked to the seller quickly."])
            , (["mi sutra tavla lo te vecnu ku"], ["I talked to the buyer quickly."])
            -- sutra ctuca (mi)
            , (["mi sutra ctuca"], ["I teach quickly."])
            , (["mi sutra ctuca do"], ["I taught you quickly."])
            , (["mi sutra ctuca lo prenu ku"], ["I taught the person quickly."])
            , (["mi sutra ctuca lo gerku ku"], ["I taught the dog quickly."])
            -- sutra ctuca (do)
            , (["do sutra ctuca"], ["You teach quickly."])
            , (["do sutra ctuca mi"], ["You taught me quickly."])
            , (["do sutra ctuca lo prenu ku"], ["You taught the person quickly."])
            , (["do sutra ctuca lo gerku ku"], ["You taught the dog quickly."])
            ]
        gleki = generatorFromList
            -- tavla (mi)
            [ (["mi gleki tavla"], ["I talk happily", "I was talking happily."])
            , (["mi gleki tavla do"], ["I was talking happily to you."])
            , (["mi gleki tavla lo gerku ku"], ["I was talking happily to the dog."])
            -- tavla (do)
            , (["do gleki tavla"], ["You talk happily", "You were talking happily."])
            , (["do gleki tavla mi"], ["You were talking happily to me."])
            , (["do gleki tavla lo gerku ku"], ["You were talking happily to the dog."])
            -- dunda (mi)
            , (["mi gleki dunda"], ["I donate happily."])
            , (["mi gleki dunda lo mlatu ku"], ["I happily donated the cat."])
            , (["mi gleki dunda lo gerku ku"], ["I happily donated the dog."])
            -- ctuca (mi)
            , (["mi gleki ctuca"], ["I teach happily."])
            , (["mi gleki ctuca do"], ["I happily taught you."])
            , (["mi gleki ctuca lo gerku ku"], ["I happily taught the dog."])
            ]
    adjective = combineGeneratorsUniformly [pendo, gleki] where
        lojbo = generatorFromList
            -- tavla (mi)
            [ (["mi tavla lo lojbo prenu ku", "mi tavla lo prenu ku poi lojbo"], ["I talked to the Lojbanic person."])
            , (["mi tavla lo lojbo dunda ku", "mi tavla lo dunda ku poi lojbo"], ["I talked to the Lojbanic donor."])
            , (["mi tavla lo lojbo vecnu ku", "mi tavla lo vecnu ku poi lojbo"], ["I talked to the Lojbanic seller."])
            , (["mi tavla lo lojbo te vecnu ku", "mi tavla lo te vecnu ku poi lojbo"], ["I talked to the Lojbanic buyer."])
            , (["mi tavla lo lojbo ctuca ku", "mi tavla lo ctuca ku poi lojbo"], ["I talked to the Lojbanic teacher."])
            ]
        pendo = generatorFromList
            -- tavla (mi)
            [ (["mi tavla lo pendo prenu ku", "mi tavla lo prenu ku poi pendo"], ["I talked to the friendly person."])
            , (["mi tavla lo pendo dunda ku", "mi tavla lo dunda ku poi pendo"], ["I talked to the friendly donor."])
            , (["mi tavla lo pendo te dunda ku", "mi tavla lo te dunda ku poi pendo"], ["I talked to the friendly recipient."])
            , (["mi tavla lo pendo vecnu ku", "mi tavla lo vecnu ku poi pendo"], ["I talked to the friendly seller."])
            , (["mi tavla lo pendo te vecnu ku", "mi tavla lo te vecnu ku poi pendo"], ["I talked to the friendly buyer."])
            , (["mi tavla lo pendo ctuca ku", "mi tavla lo ctuca ku poi pendo"], ["I talked to the friendly teacher."])
            -- tavla (xu do)
            --, (["xu do tavla lo pendo prenu", "xu do tavla lo prenu poi pendo"], ["Did you talk to the friendly person?"])
            --, (["xu do tavla lo pendo dunda", "xu do tavla lo dunda poi pendo"], ["Did you talk to the friendly donor?"])
            --, (["xu do tavla lo pendo te dunda ", "xu do tavla lo te dunda poi pendo"], ["Did you talk to the friendly recipient?"])
            --, (["xu do tavla lo pendo vecnu", "xu do tavla lo vecnu poi pendo"], ["Did you talk to the friendly seller?"])
            --, (["xu do tavla lo pendo te vecnu", "xu do tavla lo te vecnu poi pendo"], ["Did you talk to the friendly buyer?"])
            --, (["xu do tavla lo pendo ctuca", "xu do tavla lo ctuca poi pendo"], ["Did you talk to the friendly teacher?"])
            -- nelci (mi)
            , (["mi nelci lo pendo prenu ku", "mi nelci lo prenu ku poi pendo"], ["I like friendly people."])
            , (["mi nelci lo pendo dunda ku", "mi nelci lo dunda ku poi pendo"], ["I like friendly donors."])
            , (["mi nelci lo pendo te dunda ku", "mi nelci lo te dunda ku poi pendo"], ["I like friendly recipients."])
            , (["mi nelci lo pendo vecnu ku", "mi nelci lo vecnu ku poi pendo"], ["I like friendly sellers."])
            , (["mi nelci lo pendo te vecnu ku", "mi nelci lo te vecnu ku poi pendo"], ["I like friendly buyers."])
            , (["mi nelci lo pendo ctuca ku", "mi nelci lo ctuca ku poi pendo"], ["I like friendly teachers."])
            , (["mi nelci lo pendo gerku ku", "mi nelci lo gerku ku poi pendo"], ["I like friendly dogs."])
            , (["mi nelci lo pendo mlatu ku", "mi nelci lo mlatu ku poi pendo"], ["I like friendly cats."])
            -- nelci (xu do)
            --, (["xu do nelci lo pendo prenu", "xu do nelci lo prenu poi pendo"], ["Do you like friendly people?"])
            --, (["xu do nelci lo pendo dunda", "xu do nelci lo dunda poi pendo"], ["Do you like friendly donors?"])
            --, (["xu do nelci lo pendo te dunda", "xu do nelci lo te dunda poi pendo"], ["Do you like friendly recipients?"])
            --, (["xu do nelci lo pendo vecnu", "xu do nelci lo vecnu poi pendo"], ["Do you like friendly sellers?"])
            --, (["xu do nelci lo pendo te vecnu", "xu do nelci lo te vecnu poi pendo"], ["Do you like friendly buyers?"])
            --, (["xu do nelci lo pendo ctuca", "xu do nelci lo ctuca poi pendo"], ["Do you like friendly teachers?"])
            --, (["xu do nelci lo pendo gerku", "xu do nelci lo gerku poi pendo"], ["Do you like friendly dogs?"])
            --, (["xu do nelci lo pendo mlatu", "xu do nelci lo mlatu poi pendo"], ["Do you like friendly cats?"])
            ]
        gleki = generatorFromList
            -- tavla (mi)
            [ (["mi tavla lo gleki prenu ku", "mi tavla lo prenu ku poi gleki"], ["I talked to the happy person."])
            , (["mi tavla lo gleki dunda ku", "mi tavla lo dunda ku poi gleki"], ["I talked to the happy donor."])
            , (["mi tavla lo gleki te dunda ku", "mi tavla lo te dunda ku poi gleki"], ["I talked to the happy recipient."])
            , (["mi tavla lo gleki vecnu ku", "mi tavla lo vecnu ku poi gleki"], ["I talked to the happy seller."])
            , (["mi tavla lo gleki te vecnu ku", "mi tavla lo te vecnu ku poi gleki"], ["I talked to the happy buyer."])
            , (["mi tavla lo gleki ctuca ku", "mi tavla lo ctuca ku poi gleki"], ["I talked to the happy teacher."])
            -- tavla (xu do)
            --, (["xu do tavla lo gleki prenu", "xu do tavla lo prenu poi gleki"], ["Did you talk to the happy person?"])
            --, (["xu do tavla lo gleki dunda", "xu do tavla lo dunda poi gleki"], ["Did you talk to the happy donor?"])
            --, (["xu do tavla lo gleki te dunda ", "xu do tavla lo te dunda poi gleki"], ["Did you talk to the happy recipient?"])
            --, (["xu do tavla lo gleki vecnu", "xu do tavla lo vecnu poi gleki"], ["Did you talk to the happy seller?"])
            --, (["xu do tavla lo gleki te vecnu", "xu do tavla lo te vecnu poi gleki"], ["Did you talk to the happy buyer?"])
            --, (["xu do tavla lo gleki ctuca", "xu do tavla lo ctuca poi gleki"], ["Did you talk to the happy teacher?"])
            -- nelci (mi)
            , (["mi nelci lo gleki prenu ku", "mi nelci lo prenu ku poi gleki"], ["I like happy people."])
            , (["mi nelci lo gleki ctuca ku", "mi nelci lo ctuca ku poi gleki"], ["I like happy teachers."])
            , (["mi nelci lo gleki gerku ku", "mi nelci lo gerku ku poi gleki"], ["I like happy dogs."])
            , (["mi nelci lo gleki mlatu ku", "mi nelci lo mlatu ku poi gleki"], ["I like happy cats."])
            -- nelci (xu do)
            --, (["xu do nelci lo gleki prenu", "xu do nelci lo prenu poi gleki"], ["Do you like happy people?"])
            --, (["xu do nelci lo gleki ctuca", "xu do nelci lo ctuca poi gleki"], ["Do you like happy teachers?"])
            --, (["xu do nelci lo gleki gerku", "xu do nelci lo gerku poi gleki"], ["Do you like happy dogs?"])
            --, (["xu do nelci lo gleki mlatu", "xu do nelci lo mlatu poi gleki"], ["Do you like happy cats?"])
            ]

-- TODO: use the following sentences
--translations99 :: [ExerciseGenerator]
--translations99 = generateTranslationExercise sentenceCanonicalizer sentenceComparer <$> generatorFromSingleton <$>
    --[ (["lo prenu ku sutra tavla"], ["The person talks quickly.", "The person is talking quickly.", "A person is talking quickly.", "People talk quickly"])
    --]

-- * Lesson 5: Questions 1
--
-- | Overall translations for the fourth lesson (always involving "xu", "ma" or "mo").
translations5 :: TranslationGenerator
translations5 = combineGenerators [(4, translations5_xu), (4, translations5_ma), (1, translations5_mo)]

-- | Translations for the fourth lesson involving "xu", with the restriction that some intermediate place is missing.
--
-- Defined separately so that they may be used in 'Translate without using "zo'e"' exercises.
translations5_restricted_xu :: TranslationGenerator
translations5_restricted_xu = combineGenerators [(2, talkingAbout), (1, gaveSomething), (4, writing), (2, know)] where
    talkingAbout = generatorFromList
        [ (["xu do tavla fi do", "xu do tavla fi vo'a"], ["Are you talking about yourself?", "Were you talking about yourself?"])
        , (["xu do tavla fi mi"], ["Are you talking about me?", "Were you talking about me?"])
        , (["xu tavla fi mi"], ["Was somebody talking about me?"])
        , (["xu do tavla fi lo se dunda ku"], ["Are you talking about the gift?", "Did you talk about the gift?"])
        , (["xu do tavla fi lo mlatu ku"], ["Were you talking about the cat?", "Were you talking about the cats?", "Were you talking about cats?"])
        , (["xu do tavla fi lo gerku ku"], ["Were you talking about the dog?", "Were you talking about the dogs?", "Were you talking about dogs?"])
        , (["xu tavla fi lo se dunda ku"], ["Did somebody talk about the gift?"])
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

-- | Overall translations for the fourth lesson involving "xu".
translations5_xu :: TranslationGenerator
translations5_xu = combineGenerators $ [(3, translations5_restricted_xu), (3, writing), (2, know)] ++ ((1,) <$> [hasHouse, nice, talking, teaching, friends, others]) where
    hasHouse = generatorFromList
        [ (["xu do se zdani"], ["Do you have a house?"])
        , (["xu lo prenu ku se zdani"], ["Does the person have a house?"])
        , (["xu lo ctuca ku se zdani"], ["Does the instructor have a house?"])
        ]
    nice = combineGeneratorsUniformly [beautiful, like] where
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

-- | Translations for the fourth lesson involving "ma", with the restriction that some intermediate place is missing.
--
-- Defined separately so that they may be used in 'Translate without using "zo'e"' exercises.
translations5_restricted_ma :: TranslationGenerator
translations5_restricted_ma = combineGenerators [(2, talkingAbout), (1, gaveSomething), (4, writing), (2, know)] where
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

-- | Overall translations for the fourth lesson involving "ma".
translations5_ma :: TranslationGenerator
translations5_ma = combineGenerators $ [(3, translations5_restricted_ma), (3, writing), (2, know)] ++ ((1,) <$> [hasHouse, nice, talking, giving, teaching]) where
    hasHouse = generatorFromList
        [ (["ma se zdani"], ["Who has a house?"])
        , (["ta zdani ma", "zdani ma"], ["Whose house is that?"])
        , (["do nelci ma"], ["What do you like?"])
        ]
    nice = combineGeneratorsUniformly [what, who] where
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
    giving = combineGenerators [(2, general), (1, mlatu), (1, gerku)] where
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

-- | Translations for the fourth lesson involving "mo".
translations5_mo :: TranslationGenerator
translations5_mo = generatorFromList
    [ (["mi mo"], ["What am I doing?"])
    , (["do mo"], ["What are you doing?"])
    , (["lo prenu ku mo"], ["What is the person doing?"])
    , (["lo ctuca ku mo"], ["What is the instructor doing?"])
    , (["lo ciska ku mo"], ["What is the writer doing?"])
    ]

-- | Translations for the fourth lesson involving "xu" or "ma", with the restriction that some intermediate place is missing.
--
-- Defined separately so that they may be used in 'Translate without using "zo'e"' exercises.
translations5_restricted :: TranslationGenerator
translations5_restricted = combineGeneratorsUniformly [translations5_restricted_xu, translations5_restricted_ma]

-- * Lesson 6: Abstractions 1
-- CHECK: Are events vs facts being used correctly?
translations6_nu :: TranslationGenerator
translations6_nu = combineGenerators [(2, gleki), (1, tavla), (2, nupre)] where
    gleki = combineGeneratorsUniformly [talking, beautiful, givingAnimals, liking, teaching, owningHouse, know, other] where
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
            --, (["do gleki ma"], ["Why are you happy?", "What are you happy about?"])
            ]
    tavla = combineGeneratorsUniformly [owningHouse] where
        owningHouse = generatorFromList
            [ (["mi tavla fi lo nu do se zdani kei ku"], ["I talked about you having a house.", "We talked about you having a house."])
            ]
            -- TODO: find out where the following sentences belong
        --promisorTalked = generatorFromList
            --[ (["lo nupre ku tavla mi"], ["The promisor talked to me.", "The promisor talked to us."])
            --, (["lo nupre ku tavla fi lo mlatu ku"], ["The promisor talked about the cat.", "The promisor talked about the cats."])
            --, (["lo nupre ku tavla fi lo gerku ku"], ["The promisor talked about the dog.", "The promisor talked about the dogs."])
            --, (["lo nupre ku tavla fi lo zdani ku"], ["The promisor talked about the house.", "The promisor talked about the houses."])
            --, (["lo nupre ku tavla mi lo mlatu ku"], ["The promisor talked to me about the cat.", "The promisor talked to me about the cats."])
            --, (["lo nupre ku tavla mi lo gerku ku"], ["The promisor talked to me about the dog.", "The promisor talked to me about the dogs."])
            --, (["lo nupre ku tavla mi lo zdani ku"], ["The promisor talked to me about the house.", "The promisor talked to me about the houses."])
            --]
        --promiseeTalked = generatorFromList
            --[ (["lo te nupre ku tavla mi"], ["The promisee talked to me.", "The promisee talked to us."])
            --, (["lo te nupre ku tavla fi lo mlatu ku"], ["The promisee talked about the cat.", "The promisee talked about the cats."])
            --, (["lo te nupre ku tavla fi lo gerku ku"], ["The promisee talked about the dog.", "The promisee talked about the dogs."])
            --, (["lo te nupre ku tavla fi lo zdani ku"], ["The promisee talked about the house.", "The promisee talked about the houses."])
            --, (["lo te nupre ku tavla mi lo mlatu ku"], ["The promisee talked to me about the cat.", "The promisee talked to me about the cats."])
            --, (["lo te nupre ku tavla mi lo gerku ku"], ["The promisee talked to me about the dog.", "The promisee talked to me about the dogs."])
            --, (["lo te nupre ku tavla mi lo zdani ku"], ["The promisee talked to me about the house.", "The promisee talked to me about the houses."])
            --]
    nupre = combineGeneratorsUniformly [donatingAnimals, donatingHouses, teaching, beingFriendly] where
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

translations6_du'u :: TranslationGenerator
translations6_du'u = combineGenerators [(2, djuno)] where
    djuno = combineGeneratorsUniformly [teaching, friend, donating, promising, liking, talking, writing] where
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

translations6_sedu'u :: TranslationGenerator
translations6_sedu'u = combineGenerators [(2, cusku)] where
    cusku = combineGeneratorsUniformly [beautiful, likingPeople, likingAnimals, donatingAnimals, beingFriendly] where
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
            , (["lo prenu ku cusku lo se du'u nelci mi kei ku", "lo prenu ku cusku lo se du'u ri nelci mi kei ku"], ["The person said that she likes me."])
            , (["lo prenu ku cusku lo se du'u nelci do kei ku", "lo prenu ku cusku lo se du'u ri nelci do kei ku"], ["The person said that she likes you."])
            ]
        likingAnimals = generatorFromList
            -- mlatu
            [ (["mi cusku lo se du'u mi nelci lo mlatu ku kei ku", "mi cusku lo se du'u nelci lo mlatu ku kei ku"], ["I said that I like the cat.", "I said that I like the cats.", "I said that I like cats."])
            , (["mi cusku lo se du'u do nelci lo mlatu ku kei ku"], ["I said that you like the cat.", "I said that you like the cats.", "I said that you like cats."])
            , (["do cusku lo se du'u do nelci lo mlatu ku kei ku", "do cusku lo se du'u nelci lo mlatu ku kei ku"], ["You said that you like the cat.", "You said that you like the cats.", "You said that you like cats."])
            , (["do cusku lo se du'u mi nelci lo mlatu ku kei ku"], ["You said that I like the cat.", "You said that I like the cats.", "You said that I like cats."])
            , (["lo prenu ku cusku lo se du'u mi nelci lo mlatu ku kei ku"], ["The person said that I like the cat.", "The person said that I like the cats.", "The person said that I like cats."])
            , (["lo prenu ku cusku lo se du'u do nelci lo mlatu ku kei ku"], ["The person said that you like the cat.", "The person said that you like the cats.", "The person said that you like cats."])
            , (["lo prenu ku cusku lo se du'u nelci lo mlatu ku kei ku", "lo prenu ku cusku lo se du'u ri nelci lo mlatu ku kei ku"], ["The person said that she liked the cat.", "The person said that she liked the cats.", "The person said that she likes cats."])
            -- gerku
            , (["mi cusku lo se du'u mi nelci lo gerku ku kei ku", "mi cusku lo se du'u nelci lo gerku ku kei ku"], ["I said that I like the dog.", "I said that I like the dogs.", "I said that I like dogs."])
            , (["mi cusku lo se du'u do nelci lo gerku ku kei ku"], ["I said that you like the dog.", "I said that you like the dogs.", "I said that you like dogs."])
            , (["do cusku lo se du'u do nelci lo gerku ku kei ku", "do cusku lo se du'u nelci lo gerku ku kei ku"], ["You said that you like the dog.", "You said that you like the dogs.", "You said that you like dogs."])
            , (["do cusku lo se du'u mi nelci lo gerku ku kei ku"], ["You said that I like the dog.", "You said that I like the dogs.", "You said that I like dogs."])
            , (["lo prenu ku cusku lo se du'u mi nelci lo gerku ku kei ku"], ["The person said that I like the dog.", "The person said that I like the dogs.", "The person said that I like dogs."])
            , (["lo prenu ku cusku lo se du'u do nelci lo gerku ku kei ku"], ["The person said that you like the dog.", "The person said that you like the dogs.", "The person said that you like dogs."])
            , (["lo prenu ku cusku lo se du'u nelci lo gerku ku kei ku", "lo prenu ku cusku lo se du'u ri nelci lo gerku ku kei ku"], ["The person said that she liked the dog.", "The person said that she liked the dogs.", "The person said that she likes dogs."])
            ]
        donatingAnimals = generatorFromList
            [ (["mi cusku lo se du'u mi dunda lo mlatu ku kei ku", "mi cusku lo se du'u dunda lo mlatu ku kei ku"], ["I said that I would donate the cat.", "I said that I would donate the cats."])
            , (["do cusku lo se du'u do dunda lo mlatu ku kei ku", "do cusku lo se du'u dunda lo mlatu ku kei ku"], ["You said that you would donate the cat.", "You said that you would donate the cats."])
            , (["lo prenu ku cusku lo se du'u dunda lo mlatu ku kei ku", "lo prenu ku cusku lo se du'u ri dunda lo mlatu ku kei ku"], ["The person said said that she would donate the cat.", "The person said that she would donate the cats."])
            , (["mi cusku lo se du'u mi dunda lo mlatu ku do kei ku", "mi cusku lo se du'u dunda lo mlatu ku do kei ku"], ["I said that I would give you the cat.", "I said that I would give you the cats."])
            , (["do cusku lo se du'u do dunda lo mlatu ku mi kei ku", "do cusku lo se du'u dunda lo mlatu ku mi kei ku"], ["You said that you would give me the cat.", "You said that you would give me the cats."])
            , (["mi cusku lo se du'u mi dunda lo gerku ku do kei ku", "mi cusku lo se du'u dunda lo gerku ku do kei ku"], ["I said that I would give you the dog.", "I said that I would give you the dogs."])
            , (["do cusku lo se du'u do dunda lo gerku ku mi kei ku", "do cusku lo se du'u dunda lo gerku ku mi kei ku"], ["You said that you would give me the dog.", "You said that you would give me the dogs."])
            , (["lo prenu ku cusku lo se du'u dunda lo mlatu ku mi kei ku", "lo prenu ku cusku lo se du'u ri dunda lo mlatu ku mi kei ku"], ["The person said that she would give me the cat.", "The person said that she would give me the cats."])
            , (["lo prenu ku cusku lo se du'u dunda lo mlatu ku do kei ku", "lo prenu ku cusku lo se du'u ri dunda lo mlatu ku do kei ku"], ["The person said that she would give you the cat.", "The person said that she would give you the cats."])
            , (["lo prenu ku cusku lo se du'u dunda lo gerku ku mi kei ku", "lo prenu ku cusku lo se du'u ri dunda lo gerku ku mi kei ku"], ["The person said that she would give me the dog.", "The person said that she would give me the dogs."])
            , (["lo prenu ku cusku lo se du'u dunda lo gerku ku do kei ku", "lo prenu ku cusku lo se du'u ri dunda lo gerku ku do kei ku"], ["The person said that she would give you the dog.", "The person said that she would give you the dogs."])
            , (["lo prenu ku cusku lo se du'u do dunda lo mlatu ku mi kei ku"], ["The person said that you would give me the cat.", "The person said that you would give me the cats."])
            , (["lo prenu ku cusku lo se du'u do dunda lo gerku ku mi kei ku"], ["The person said that you would give me the dog.", "The person said that you would give me the dogs."])
            ]
        beingFriendly = generatorFromList
            [ (["mi cusku lo se du'u pendo kei ku", "mi cusku lo se du'u mi pendo kei ku"], ["I said that I would be friendly."])
            , (["do cusku lo se du'u pendo kei ku", "do cusku lo se du'u do pendo kei ku"], ["You said that you would be friendly."])
            , (["mi cusku lo se du'u do pendo kei ku"], ["I said that you would be friendly."])
            , (["mi cusku lo se du'u lo ciska ku pendo kei ku"], ["I said that the writer would be friendly."])
            ]
        -- TODO: find out where the following sentences belong
            --[ (["mi nelci lo nu tavla do kei ku", "mi nelci lo nu mi tavla do kei ku"], ["I like to talk to you."]) -- is nelci really adequate?
            --, (["do nelci lo nu nupre kei ku", "do nelci lo nu do nupre kei ku"], ["You like to make promises."]) -- is nelci really adequate?
            --, (["lo prenu ku nelci lo nu nupre kei ku"], ["People like to make promises."]) -- is nelci really adequate?
            --, (["do cusku ma"], ["What did you say?"])
            --, (["lo prenu ku cusku ma"], ["What did the person say?"])
            --, (["lo prenu ku cusku ma do"], ["What did the person say to you?"])
            -- Wait until terminator ellision has been explained to use the following sentences
            {-, (["mi cusku lo se du'u mi nelci lo nu tavla do kei ku kei ku", "mi cusku lo se du'u mi nelci lo nu mi tavla do kei ku kei ku"], ["I said that I like to talk to you."])-}
            {-, (["mi cusku lo se du'u do nelci lo nu tavla mi kei ku kei ku", "mi cusku lo se du'u do nelci lo nu do tavla mi kei ku kei ku"], ["I said that you like to talk to me."])-}
            --]

translations6_extra :: TranslationGenerator
translations6_extra = combineGeneratorsUniformly [gleki, tavla, nupre, cusku, ciska] where
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

translations6 :: TranslationGenerator
translations6 = combineGenerators $ ((4,) <$> [translations6_nu, translations6_du'u, translations6_sedu'u]) ++ ((1,) <$> [translations6_extra])

-- * Lesson 7: Terminator elision
translations7 :: TranslationGenerator
translations7 = translations7_restricted

translations7_restricted :: TranslationGenerator
translations7_restricted = expandTranslationGenerator $ combineGenerators [(2, hasHouse), (3, nice), (3, giving), (2, talking), (3, gleki), (3, nupre), (3, djuno), (3, cusku)] where
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
    nice = combineGeneratorsUniformly [beautiful, like] where
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
            , (["xu do nelci lo ctuca ku"], ["Did you like the instructor?"])
            , (["ma nelci lo se dunda"], ["Who liked the gift?", "Who likes the gift?"])
            , (["ma nelci lo mlatu"], ["Who likes cats?", "Who likes the cat?"])
            , (["ma nelci lo gerku"], ["Who likes dogs?", "Who likes the dog?"])
            ]
    giving = combineGenerators [(1, general), (3, mlatu), (3, gerku)] where
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
    gleki = combineGeneratorsUniformly [givingAnimals, liking, know, other] where
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
    nupre = combineGeneratorsUniformly [donatingAnimals, donatingHouses] where
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
    djuno = combineGeneratorsUniformly [liking, talking] where
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
    cusku = combineGeneratorsUniformly [likingPeople, likingAnimals, donatingAnimals, others] where
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
            [ (["mi cusku lo se du'u lo ciska cu pendo"], ["I said that the writer would be friendly."])
            , (["mi cusku lo se du'u mi nelci lo nu {mi} tavla do", "mi cusku lo se du'u mi nelci lo nu mi tavla do"], ["I said that I like to talk to you."]) -- is nelci really adequate?
            , (["mi cusku lo se du'u do nelci lo nu {do} tavla mi"], ["I said that you like to talk to me."]) -- is nelci really adequate?
            , (["xu do cusku lo se du'u mi melbi do", "xu do cusku lo se du'u mi melbi"], ["Did you say that you find me beautiful?"])
            , (["xu do cusku lo se du'u mi melbi", "xu do cusku lo se du'u mi melbi do"], ["Did you say that I am beautiful?"])
            ]

-- * Lesson 8: Checkpoint -- Lessons 1-7
translations1to7 :: TranslationGenerator
translations1to7 = simplifyTerminatorsInTranslationGenerator $ combineGeneratorsUniformly [translations3, translations4_sentences, translations5, translations6, translations7]

-- * Lesson 9: Relative clauses
-- questionExercises5 :: "What did you promise", "What did you say, ..."
-- Interesting: xu do djuno lo se cusku
-- TODO: add some translations involving "this computer" ("ti poi skami", but also accept "lo vi skami")

-- TODO: questions with "ma"
translations9_noi :: TranslationGenerator
translations9_noi = expandTranslationGenerator $ combineGeneratorsUniformly [computer, uses, knower, instructor, friend, house, animals] where
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
        [ (["lo djuno noi {ke'a} nelci lo nu {ke'a} tavla vo'a ku'o pendo", "lo djuno noi {ke'a} nelci lo nu ke'a tavla ke'a ku'o pendo"], ["The knower, who enjoys talking to himself, is friendly."])
        , (["lo djuno noi {ke'a} nelci lo nu {ke'a} tavla vo'a ku'o ciska ta", "lo djuno noi {ke'a} nelci lo nu ke'a tavla ke'a ku'o ciska ta"], ["The knower, who enjoys talking to himself, wrote that."])
        , (["lo djuno noi {ke'a} nelci lo nu {ke'a} tavla vo'a ku'o ctuca mi", "lo djuno noi {ke'a} nelci lo nu ke'a tavla ke'a ku'o ctuca mi"], ["The knower, who enjoys talking to himself, taught us."])
        , (["lo djuno noi {ke'a} nelci lo nu {ke'a} tavla vo'a ku'o pilno lo skami", "lo djuno noi {ke'a} nelci lo nu ke'a tavla ke'a ku'o pilno lo skami"], ["The knower, who enjoys talking to himself, uses computers."])
        ]
    instructor = generatorFromList
        [ (["lo ctuca noi {ke'a} djuno fi lo gerku ku'o dunda lo plise"], ["The instructor, who knows about dogs, donated the apple."])
        , (["lo ctuca noi {ke'a} tavla do cu nelci lo plise"], ["The instructor, who talked to you, likes apples."])
        , (["lo ctuca noi {ke'a} tavla fi do cu nelci lo plise"], ["The instructor, who talked about you, likes apples.", "The instructor, who talked about you, liked the apple."])
        , (["lo ctuca noi {ke'a} pendo mi cu se zdani"], ["The instructor, who is my friend, has a house."])
        , (["lo ctuca noi {ke'a} pendo mi cu cusku lo se du'u {ri} nelci lo mlatu"], ["The instructor, who is my friend, said that he likes cats."])
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
    animals = combineGeneratorsUniformly [mlatu, gerku] where
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

translations9_poi :: TranslationGenerator
translations9_poi = expandTranslationGenerator $ combineGeneratorsUniformly [computer, uses, house, animals, general] where
    usesComputers =
        [ (["lo skami poi mi pilno (ke'a|) ku'o melbi"], ["The computer that I use is beautiful."])
        , (["lo skami poi do pilno (ke'a|) ku'o melbi"], ["The computer that you use is beautiful."])
        , (["mi nelci lo skami poi do pilno (ke'a|)"], ["I like the computer that you use."])
        , (["xu do nelci lo skami poi do pilno (ke'a|)"], ["Do you like the computer that you use?"])
        , (["xu do se melbi lo skami poi mi pilno (ke'a|)"], ["Do you find the computer that I use beautiful?"])
        , (["mi nupre lo nu {mi} pilno lo skami poi do dunda (ke'a|fi) mi"], ["I promised to use the computers that you gave me."])
        , (["mi nupre lo nu {mi} pilno lo skami poi do vecnu ke'a mi"], ["I promised to use the computers that you sold me."])
        , (["mi pilno lo skami poi do dunda (ke'a|fi) mi lo nu (mi|) te vecnu lo mlatu"], ["I used the computer that you gave me to buy a cat."])
        , (["mi pilno lo skami poi do dunda (ke'a|fi) mi lo nu (mi|) te vecnu lo gerku"], ["I used the computer that you gave me to buy a dog."])
        ]
    computer = generatorFromList $ usesComputers ++
        [ (["lo ctuca poi {ke'a} dunda lo skami cu se zdani"], ["The instructor who donated the computer has a house."])
        , (["lo skami poi do tavla fi ke'a cu melbi"], ["The computer that you talked about is beautiful."])
        , (["lo skami poi do dunda (ke'a|) cu melbi"], ["The computer that you donated is beautiful."])
        , (["lo skami poi do nupre lo nu {do} dunda {ke'a} ku'o melbi"], ["The computer that you promised to donate is beautiful."])
        , (["mi nelci lo skami poi do tavla fi ke'a"], ["I like the computer that you talked about."])
        , (["mi nelci lo skami poi do dunda (ke'a|)"], ["I like the computer that you donated."])
        , (["mi nelci lo skami poi do nupre lo nu {do} dunda {ke'a}"], ["I like the computer that you promised to donate."])
        , (["xu do nelci lo skami poi mi tavla fi ke'a"], ["Did you like the computer that I talked about?"])
        , (["mi nelci lo skami poi do vecnu (ke'a|)"], ["I like the computers that you sell.", "I liked the computer that you sold."])
        , (["xu do nelci lo skami poi mi vecnu (ke'a|)"], ["Did you like the computer that I sold?"])
        ]
    uses = generatorFromList $ usesComputers ++
        [ (["lo ctuca noi {ke'a} pendo cu cusku lo se du'u lo skami cu se pilno"], ["The instructor, who is friendly, said that computers are useful."])
        , (["lo ctuca noi {ke'a} pendo cu gleki lo nu do dunda lo se pilno mi"], ["The instructor, who is friendly, happy that you gave me the tool."])
        , (["lo ctuca noi {ke'a} pendo cu gleki lo nu do vecnu lo se pilno mi"], ["The instructor, who is friendly, happy that you sold me the tool."])
        , (["xu lo ctuca noi {ke'a} pendo cu gleki lo nu do te vecnu lo se pilno"], ["Is the instructor, who is friendly, happy that you bought the tool?"])
        , (["xu lo ctuca noi {ke'a} pendo cu gleki lo nu mi dunda lo se pilno do"], ["Is the instructor, who is friendly, happy that I gave you the tool?"])
        , (["xu lo ctuca noi {ke'a} pendo cu gleki lo nu mi vecnu lo se pilno do"], ["Is the instructor, who is friendly, happy that I sold you the tool?"])
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
    animals = combineGeneratorsUniformly [mlatu, gerku] where
        mlatu = generatorFromList
            -- [ (["mi nelci lo mlatu poi {ke'a} simsa lo gerku"], ["I like cats that look like dogs."])
            [ (["mi nelci lo mlatu poi {ke'a} pendo lo gerku"], ["I like cats that are friendly to dogs."])
            , (["mi nelci lo mlatu poi {ke'a} nelci lo plise"], ["I like cats that like apples."])
            , (["mi nelci lo plise poi {ke'a} melbi lo mlatu"], ["I like apples that are beautiful to cats."])
            , (["mi nelci lo mlatu poi {ke'a} melbi", "mi nelci lo melbi mlatu"], ["I like the beautiful cat."])
            , (["mi tavla lo prenu poi {ke'a} dunda lo mlatu"], ["I talked to the person who donated the cat."])
            , (["mi tavla lo prenu poi {ke'a} dunda lo mlatu ku mi"], ["I talked to the person who gave me the cat.", "I talked to the person who gave me the cats."])
            , (["mi djuno lo du'u lo mlatu poi do dunda (ke'a|fi) mi ku'o pendo"], ["I know that the cat you gave me is friendly."])
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
            , (["mi djuno lo du'u lo gerku poi do dunda (ke'a|fi) mi ku'o pendo"], ["I know that the dog you gave me is friendly."])
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
        , (["xu lo gerku cu nelci lo plise poi do dunda {ke'a}"], ["Did the dog like the apple that you gave?"])
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

translations9 :: TranslationGenerator
translations9 = combineGeneratorsUniformly [translations9_noi, translations9_poi]

-- * Lesson 10: Linked sumti
translations10 :: TranslationGenerator
translations10 = expandTranslationGenerator $ combineGeneratorsUniformly [required_terminator, cmene_complex, general, vecnu_zdani, ctuca, tavla, bangu, zdani, cmene] where
    required_terminator = generatorFromList
        [ (["mi nelci lo cmene (be do be'o|pe do ge'u) noi melbi"], ["I like your name, which is beautiful."])
        , (["mi nelci lo zdani (be do be'o|pe do ge'u) noi melbi"], ["I like your house, which is beautiful."])
        , (["mi nelci lo bangu (be do be'o|pe do ge'u) noi mi nupre lo nu {mi} ctuca fo (ke'a|zo'e)"], ["I like your language, which I promised to teach."])
        ]
    cmene_complex = generatorFromList
        [ (["ma se cmene lo dunda be lo mlatu bei do"], ["What is the name of the one who gave you the cat?"])
        , (["ma se cmene lo dunda be lo mlatu"], ["What is the name of the one who donated the cat?"])
        , (["ma se cmene lo vecnu be lo mlatu"], ["What is the name of the one who sold the cat?"])
        , (["ma se cmene lo vecnu be lo mlatu bei do"], ["What is the name of the one who sold you the cat?"])

        , (["ma se cmene lo dunda be lo gerku bei do"], ["What is the name of the one who gave you the dog?"])
        , (["ma se cmene lo dunda be lo gerku"], ["What is the name of the one who donated the dog?"])
        , (["ma se cmene lo vecnu be lo gerku"], ["What is the name of the one who sold the dog?"])
        , (["ma se cmene lo vecnu be lo gerku bei do"], ["What is the name of the one who sold you the dog?"])

        , (["ma se cmene lo tavla be do"], ["What is the name of the one who talked to you?"])
        , (["ma se cmene lo tavla be do bei lo zdani"], ["What is the name of the one who talked to you about the house?"])
        , (["ma se cmene lo tavla be zo'e bei lo zdani"], ["What is the name of the one who talked about the house?"])
        ]
    general = generatorFromList
        [ (["mi nelci lo se nupre be do"], ["I like the promise that you made."])
        , (["mi nelci lo se dunda be do"], ["I liked the gift that you gave."])
        , (["mi nelci lo se dunda be do bei mi"], ["I liked the gift that you gave me."])
        , (["mi nelci lo se dunda be zo'e bei do"], ["I liked the gift that you received."])
        , (["mi nelci lo se dunda be zo'e bei mi"], ["I liked the gift that I received."])
        ]
    vecnu_zdani = generatorFromList
        [ (["mi tavla lo vecnu be lo zdani"], ["I talked to the one who sold the house."])
        , (["mi tavla lo vecnu be lo zdani (be|pe) do"], ["I talked to the one who sold your house."])
        , (["mi tavla lo vecnu be lo zdani bei do"], ["I talked to the one who sold the house to you."])
        , (["mi tavla lo vecnu be lo zdani (be|pe) do bei mi"], ["I talked to the one who sold me your house."])
        , (["xu do tavla lo vecnu be lo zdani"], ["Did you talk to the one who sold the house?"])
        , (["xu do tavla lo vecnu be lo zdani be do"], ["Did you talk to the one who sold your house?"])
        , (["xu do tavla lo vecnu be lo zdani bei do"], ["Did you talk to the one who sold you the house?"])
        , (["do vecnu lo zdani (be|pe) ma"], ["Whose house did you sell?"])
        , (["do vecnu lo zdani (be|pe) mi ma"], ["To whom did you sell my house?"])
        ]
    ctuca = generatorFromList
        [ (["do dunda lo zdani lo ctuca be ma"], ["Whose instructor did you donate the house to?"])
        , (["xu do dunda lo zdani lo ctuca be mi"], ["Did you donate the house to the instructor who taught me?"])
        , (["xu do dunda lo zdani lo ctuca be do"], ["Did you donate the house to the instructor who taught you?"])
        , (["xu do nelci lo ctuca (be|pe) do"], ["Do you like your instructor?", "Did you like your instructor?"])
        , (["xu do nelci lo ctuca (be|pe) mi"], ["Do you like my instructor?"])
        , (["mi pendo lo ctuca (be|pe) mi"], ["I am friends with my instructor."])
        , (["mi pendo lo ctuca (be|pe) do"], ["I am friends with your instructor."])
        , (["xu do pendo lo ctuca (be|pe) mi"], ["Are you friends with my instructor?"])
        , (["xu do pendo lo ctuca (be|pe) do"], ["Are you friends with your instructor?"])
        ]
    tavla = generatorFromList
        [ (["mi dunda lo mlatu lo tavla be do"], ["I donated the cat to the one who was talking to you."])
        , (["mi dunda lo mlatu lo tavla be zo'e bei do"], ["I donated the cat to the one who was talking about you."])
        , (["mi dunda lo gerku lo tavla be do"], ["I donated the dog to the one who was talking to you."])
        , (["mi dunda lo gerku lo tavla be zo'e bei do"], ["I donated the dog to the one who was talking about you."])

        , (["lo tavla be do cu melbi"], ["The one who was talking to you is beautiful."])
        , (["lo tavla be zo'e bei do cu melbi"], ["The one who was talking about you is beautiful."])
        , (["lo tavla be zo'e bei mi cu melbi"], ["The one who was talking about me is beautiful."])

        , (["lo tavla be do cu melbi mi"], ["The one who was talking to you is beautiful to me."])
        , (["mi se melbi lo tavla be zo'e bei do"], ["The one who was talking about you is beautiful to me."])
        , (["mi se melbi lo tavla be zo'e bei mi"], ["The one who was talking about me is beautiful to me."])
        ]
    bangu = generatorFromList
        [ (["mi nelci lo bangu (be|pe) mi"], ["I like my language."])
        , (["mi nelci lo bangu (be|pe) do"], ["I like your language."])
        , (["xu do nelci lo bangu (be|pe) mi"], ["Do you like my language?"])
        , (["xu do nelci lo bangu (be|pe) do"], ["Do you like your language?"])
        ]
    zdani = generatorFromList
        [ (["mi nelci lo zdani (be|pe) mi"], ["I like my house."])
        , (["mi nelci lo zdani (be|pe) do"], ["I like your house."])
        , (["xu do nelci lo zdani (be|pe) mi"], ["Do you like my house?"])
        , (["xu do nelci lo zdani (be|pe) do"], ["Do you like your house?"])
        , (["lo zdani (be|pe) mi cu melbi"], ["My house is beautiful."])
        , (["lo zdani (be|pe) do cu melbi"], ["Your house is beautiful."])
        ]
    cmene = generatorFromList
        [ (["mi nelci lo cmene (be|pe) mi"], ["I like my name."])
        , (["mi nelci lo cmene (be|pe) do"], ["I like your name."])
        , (["xu do nelci lo cmene (be|pe) mi"], ["Do you like my name?"])
        , (["xu do nelci lo cmene (be|pe) do"], ["Do you like your name?"])
        ]

-- * Lesson 11: Sumtcita
-- TODO: include sentences like "lo jinga be gau do"
-- TODO: include sentences like "lo dunda be fi do" (will require significant improvements in text canonicalizer)
translations11 :: TranslationGenerator
translations11 = expandTranslationGenerator $ combineGeneratorsUniformly [pi'o, mu'i, gau] where
    pi'o = generatorFromList
        -- fanva
        [ (["mi fanva sepi'o lo skami"], ["I translated using the computer."])
        , (["mi fanva sepi'o lo te ctuca be do"], ["I translated using what you taught."])
        , (["mi fanva sepi'o lo te ctuca be do bei mi"], ["I translated using what you taught me."])
        -- tavla (mi)
        , (["mi tavla sepi'o lo skami"], ["I talk using a computer."])
        , (["mi tavla lo pendo sepi'o lo skami"], ["I talk to friends using a computer."])
        , (["mi tavla lo pendo (be|pe) mi sepi'o lo skami"], ["I talk to my friend using a computer.", "I talked to my friend using a computer.", "I talked to my friends using a computer."])
        -- tavla (xu do)
        , (["xu do tavla sepi'o lo skami"], ["Do you talk using a computer?"])
        , (["xu do tavla lo pendo sepi'o lo skami"], ["Do you talk to friends using a computer?"])
        , (["xu do tavla lo pendo (be|pe) do sepi'o lo skami"], ["Do you talk to your friends using a computer?"])
        -- ciska
        , (["mi ciska sepi'o lo skami", "mi ciska fo lo skami"], ["I write using a computer."])
        , (["mi ciska sepi'o lo se dunda be do", "mi ciska fo lo se dunda be do"], ["I write using the gift that you gave."])
        , (["mi ciska sepi'o lo se dunda be do bei mi", "mi ciska fo lo se dunda be do bei mi"], ["I write using the gift that you gave me."])
        -- ctuca
        , (["mi ctuca sepi'o lo skami"], ["I teach using a computer."])
        , (["mi ctuca lo prenu sepi'o lo skami"], ["I teach people using a computer."])
        , (["mi ctuca sepi'o lo se dunda be do"], ["I teach using the gift that you gave."])
        , (["mi ctuca sepi'o lo se dunda be do bei mi"], ["I teach using the gift that you gave me."])
        , (["mi ctuca sepi'o lo se vecnu be do"], ["I teach using what you sold."])
        , (["mi ctuca sepi'o lo se vecnu be do bei mi"], ["I teach using what you sold me."])
        , (["mi ctuca do sepi'o lo skami"], ["I taught you using a computers."])
        -- vecnu
        , (["mi vecnu lo gerku (be|pe) mi sepi'o lo skami"], ["I sold my dog using a computer."])
        , (["mi vecnu lo mlatu (be|pe) mi sepi'o lo skami"], ["I sold my cat using a computer."])
        ]
    mu'i = generatorFromList
        -- tavla
        [ (["mu'i ma do tavla mi"], ["Why do you talk to me?", "Why are you talking to me?"])
        , (["mu'i ma do tavla fi mi"], ["Why were you talking about me?"])
        , (["mu'i ma tavla fi lo mlatu"], ["Why talk about cats?", "Why talk about the cat?"])
        , (["mu'i ma tavla fi lo gerku"], ["Why talk about dogs?", "Why talk about the dog?"])
        -- nelci
        , (["mu'i ma do nelci lo zdani (be|pe) mi"], ["Why did you like my house?"])
        , (["mu'i ma do nelci lo se dunda"], ["Why did you like the gift?"])
        , (["mu'i ma do nelci lo prenu"], ["Why do you like people?"])
        -- pilno
        , (["mu'i ma lo ctuca cu pilno lo skami"], ["Why does the instructor use computers?"])
        -- dunda
        , (["mu'i ma do dunda lo mlatu"], ["Why did you donate the cat?"])
        , (["mu'i ma do dunda lo gerku"], ["Why did you donate the dog?"])
        , (["mu'i ma dunda"], ["Why donate?"])
        -- ctuca
        , (["mu'i ma do ctuca"], ["Why do you teach?"])
        , (["mu'i ma do se melbi lo zdani"], ["Why do you find the house beautiful?"])
        -- ciska
        , (["mu'i ma lo prenu cu ciska"], ["Why do people write?"])
        , (["mu'i ma do ciska ti"], ["Why did you write this?"])
        , (["mu'i ma do ciska ta"], ["Why did you write that?"])
        -- others
        , (["mu'i ma lo prenu cu se zdani"], ["Why do people have houses?"])
        , (["mu'i ma do pendo"], ["Why are you friendly?"])
        ]
    gau = generatorFromList
        -- tavla
        [ (["gau do mi tavla do"], ["You made me talk to you."])
        , (["gau do mi tavla fi lo gerku"], ["You made me talk about the dog."])
        , (["gau do mi tavla fi lo mlatu"], ["You made me talk about the cat."])
        , (["gau ma do tavla fi lo gerku"], ["Who made you talk about the dog?"])
        , (["gau ma do tavla fi lo mlatu"], ["Who made you talk about the cat?"])
        -- dunda
        , (["gau do mi dunda lo gerku"], ["You made me donate the dog."])
        , (["gau do mi dunda lo mlatu"], ["You made me donate the cat."])
        , (["gau do mi dunda lo zdani"], ["You made me donate the house."])
        , (["gau ma do dunda lo zdani"], ["Who made you donate the house?"])
        , (["gau ma do dunda lo gerku"], ["Who made you donate the dog?"])
        , (["gau ma do dunda lo mlatu"], ["Who made you donate the cat?"])
        -- vecnu
        , (["gau do mi vecnu lo gerku"], ["You made me sell the dog."])
        , (["gau do mi vecnu lo mlatu"], ["You made me sell the cat."])
        , (["gau do mi vecnu lo zdani"], ["You made me sell the house."])
        , (["gau ma do vecnu lo zdani"], ["Who made you sell the house?"])
        , (["gau ma do vecnu lo gerku"], ["Who made you sell the dog?"])
        , (["gau ma do vecnu lo mlatu"], ["Who made you sell the cat?"])
        -- ciska
        , (["gau do mi ciska ta"], ["You made me write that."])
        , (["gau ma do ciska ta"], ["Who made you write that?"])
        -- gleki
        , (["gau do mi gleki"], ["You make me happy."])
        , (["xu gau mi do gleki"], ["Do I make you happy?"])
        -- melbi
        , (["gau mi lo zdani cu melbi"], ["I made the house beautiful."])
        , (["gau do lo zdani cu melbi"], ["You made the house beautiful."])
        , (["xu gau do lo zdani cu melbi"], ["Did you make the house beautiful?"])
        , (["gau ma lo zdani cu melbi"], ["Who made the house beautiful?"])
        -- cmene
        , (["gau mi cmene lo gerku"], ["I named the dog."])
        , (["gau mi cmene lo mlatu"], ["I named the cat."])
        ]

-- * Lesson 12: Tenses 1
translations12_pu :: TranslationGenerator
translations12_pu = expandTranslationGenerator $ combineGeneratorsUniformly [fanva, dunda, vecnu, gleki, ciska] where
    fanva = generatorFromList
        [ (["mi pu fanva fi lo lojbo"], ["I translated to Lojban."])
        , (["mi pu fanva fo lo lojbo"], ["I translated from Lojban."])
        , (["mi pu fanva ti"], ["I translated this."])
        , (["mi pu fanva ti lo lojbo"], ["I translated this to Lojban."])
        , (["xu do pu fanva fi ma"], ["To what language did you translate?"])
        , (["xu do pu fanva fo ma"], ["From what language did you translate?"])
        ]
    dunda = generatorFromList
        [ (["mi pu dunda lo mlatu"], ["I donated the cat."])
        , (["mi pu dunda lo gerku"], ["I donated the dog."])
        , (["mi pu dunda lo zdani"], ["I donated the house."])
        , (["xu do pu dunda lo mlatu"], ["Did you donate the cat?"])
        , (["xu do pu dunda lo gerku"], ["Did you donate the dog?"])
        , (["xu do pu dunda lo zdani"], ["Did you donate the house?"])
        ]
    vecnu = generatorFromList
        [ (["mi pu vecnu lo mlatu"], ["I sold the cat."])
        , (["mi pu vecnu lo gerku"], ["I sold the dog."])
        , (["mi pu vecnu lo zdani"], ["I sold the house."])
        , (["xu do pu vecnu lo mlatu"], ["Did you sell the cat?"])
        , (["xu do pu vecnu lo gerku"], ["Did you sell the dog?"])
        , (["xu do pu vecnu lo zdani"], ["Did you sell the house?"])
        ]
    gleki = generatorFromList
        [ (["mi pu gleki"], ["I was happy."])
        , (["do pu gleki"], ["You were happy."])
        , (["xu do pu gleki"], ["Were you happy?"])
        , (["lo prenu pu gleki"], ["The person was happy."])
        , (["lo vecnu pu gleki"], ["The seller was happy."])
        , (["lo te vecnu pu gleki"], ["The buyer was happy."])
        ]
    ciska = generatorFromList
        [ (["do pu ciska ta"], ["You wrote that."])
        , (["mi pu ciska ta"], ["I wrote that."])
        , (["xu mi pu ciska ta"], ["Did I write that?"])
        , (["do pu ciska ma"], ["What did you write?"])
        , (["xu do pu ciska fo ti"], ["Did you write something using this?"])
        , (["xu do pu ciska fo ta"], ["Did you write something using that?"])
        ]

-- TODO: translations with "ca ma" (when) and "pu/ca/ba SUMTI"
translations12_ca :: TranslationGenerator
translations12_ca = expandTranslationGenerator $ combineGeneratorsUniformly [fanva, dunda, vecnu, gleki, ciska] where
    fanva = generatorFromList
        [ (["mi ca fanva fi lo lojbo"], ["I am translating to Lojban."])
        , (["mi ca fanva fo lo lojbo"], ["I am translating from Lojban."])
        , (["mi ca fanva ti"], ["I am translating this."])
        , (["mi ca fanva ti lo lojbo"], ["I am translating this to Lojban."])
        , (["xu do ca fanva fi ma"], ["To what language are you translating?"])
        , (["xu do ca fanva fo ma"], ["From what language are you translating?"])
        ]
    dunda = generatorFromList
        [ (["mi ca dunda lo mlatu"], ["I am donating the cat."])
        , (["mi ca dunda lo gerku"], ["I am donating the dog."])
        , (["mi ca dunda lo zdani"], ["I am donating the house."])
        , (["mi ca dunda"], ["I am donating."])
        , (["xu do ca dunda lo mlatu"], ["Are you donating the cat?"])
        , (["xu do ca dunda lo gerku"], ["Are you donating the dog?"])
        , (["xu do ca dunda lo zdani"], ["Are you donating the house?"])
        , (["xu do ca dunda"], ["Are you donating?"])
        ]
    vecnu = generatorFromList
        [ (["mi ca vecnu lo mlatu"], ["I am selling the cat."])
        , (["mi ca vecnu lo gerku"], ["I am selling the dog."])
        , (["mi ca vecnu lo zdani"], ["I am selling the house."])
        , (["mi ca vecnu"], ["I am selling."])
        , (["xu do ca vecnu lo mlatu"], ["Are you selling the cat?"])
        , (["xu do ca vecnu lo gerku"], ["Are you selling the dog?"])
        , (["xu do ca vecnu lo zdani"], ["Are you selling the house?"])
        , (["xu do ca vecnu"], ["Are you selling?"])
        ]
    gleki = generatorFromList
        [ (["mi ca gleki"], ["I am happy."])
        , (["xu do ca gleki"], ["Are you happy?"])
        , (["lo prenu ca gleki"], ["The person is happy."])
        , (["lo vecnu ca gleki"], ["The seller is happy."])
        , (["lo te vecnu ca gleki"], ["The buyer is happy."])
        ]
    ciska = generatorFromList
        [ (["xu do ca ciska ta"], ["Are you writing that?"])
        , (["mi ca ciska ta"], ["I am writing that."])
        , (["do ca ciska ma"], ["What are you writing?"])
        , (["xu do ca ciska fo ti"], ["Are you writing something using this?"])
        , (["xu do ca ciska fo ta"], ["Are you writing something using that?"])
        ]

translations12_ba :: TranslationGenerator
translations12_ba = expandTranslationGenerator $ combineGeneratorsUniformly [fanva, dunda, vecnu, gleki, ciska] where
    fanva = generatorFromList
        [ (["mi ba fanva fi lo lojbo"], ["I will translate to Lojban."])
        , (["mi ba fanva fo lo lojbo"], ["I will translate from Lojban."])
        , (["mi ba fanva ti"], ["I will translate this."])
        , (["mi ba fanva ti lo lojbo"], ["I will translate this to Lojban."])
        , (["xu do ba fanva fi ma"], ["To what language will you translate?"])
        , (["xu do ba fanva fo ma"], ["From what language will you translate?"])
        ]
    dunda = generatorFromList
        [ (["mi ba dunda lo mlatu"], ["I will donate the cat."])
        , (["mi ba dunda lo gerku"], ["I will donate the dog."])
        , (["mi ba dunda lo zdani"], ["I will donate the house."])
        , (["xu do ba dunda lo mlatu"], ["Will you donate the cat?"])
        , (["xu do ba dunda lo gerku"], ["Will you donate the dog?"])
        , (["xu do ba dunda lo zdani"], ["Will you donate the house?"])
        ]
    vecnu = generatorFromList
        [ (["mi ba vecnu lo mlatu"], ["I will sell the cat."])
        , (["mi ba vecnu lo gerku"], ["I will sell the dog."])
        , (["mi ba vecnu lo zdani"], ["I will sell the house."])
        , (["xu do ba vecnu lo mlatu"], ["Will you sell the cat?"])
        , (["xu do ba vecnu lo gerku"], ["Will you sell the dog?"])
        , (["xu do ba vecnu lo zdani"], ["Will you sell the house?"])
        ]
    gleki = generatorFromList
        [ (["mi ba gleki"], ["I will be happy."])
        , (["do ba gleki"], ["You will be happy."])
        , (["lo prenu ba gleki"], ["The person will be happy."])
        , (["lo vecnu ba gleki"], ["The seller will be happy."])
        , (["lo te vecnu ba gleki"], ["The buyer will be happy."])
        ]
    ciska = generatorFromList
        [ (["xu do ba ciska ta"], ["Will you write that?"])
        , (["mi ba ciska ta"], ["I will write that."])
        , (["do ba ciska ma"], ["What will you write?"])
        , (["xu do ba ciska fo ti"], ["Will you write something using this?"])
        , (["xu do ba ciska fo ta"], ["Will you write something using that?"])
        ]

translations12_unrestricted :: TranslationGenerator
translations12_unrestricted = expandTranslationGenerator $ combineGeneratorsUniformly [cmene, vecnu] where
    cmene = generatorFromList
        [ (["mi nelci lo pu cmene (be|pe) do"], ["I like your former name."])
        , (["xu do nelci lo pu cmene (be|pe) do"], ["Did you like your former name?"])
        ]
    vecnu = generatorFromList
        [ (["lo pu vecnu cu gleki"], ["The former seller is happy."])
        , (["lo ba te vecnu cu gleki"], ["The future buyer is happy."])
        ]

translations12_restricted :: TranslationGenerator
translations12_restricted = combineGeneratorsUniformly [translations12_pu, translations12_ca, translations12_ba]

-- * Lesson 14: Quotations 1
translations14_zo :: TranslationGenerator
translations14_zo = expandTranslationGenerator $ combineGeneratorsUniformly [cusku] where
    cusku = generatorFromList
        -- mi cusku
        [ (["mi cusku zo do"], ["I said \"do\".", "I said \"you\"."])
        , (["mi cusku zo prenu"], ["I said \"prenu\".", "I said \"person\"."])
        , (["mi cusku zo pendo"], ["I said \"pendo\".", "I said \"friend\"."])
        , (["mi cusku zo zdani"], ["I said \"zdani\".", "I said \"house\"."])
        , (["mi cusku zo skami"], ["I said \"skami\".", "I said \"computer\"."])
        , (["mi cusku zo gleki"], ["I said \"gleki\".", "I said \"happy\"."])
        , (["mi cusku zo tavla"], ["I said \"tavla\".", "I said \"talk\"."])
        , (["mi cusku zo djuno"], ["I said \"djuno\".", "I said \"know\"."])
        , (["mi cusku zo pilno"], ["I said \"pilno\".", "I said \"use\"."])
        , (["mi cusku zo bangu"], ["I said \"bangu\".", "I said \"language\"."])
        -- xu do cusku
        , (["xu do cusku zo do"], ["Did you say \"do\"?", "Did you say \"you\"?"])
        , (["xu do cusku zo prenu"], ["Did you say \"prenu\"?", "Did you say \"person\"?"])
        , (["xu do cusku zo pendo"], ["Did you say \"pendo\"?", "Did you say \"friend\"?"])
        , (["xu do cusku zo zdani"], ["Did you say \"zdani\"?", "Did you say \"house\"?"])
        , (["xu do cusku zo skami"], ["Did you say \"skami\"?", "Did you say \"computer\"?"])
        , (["xu do cusku zo gleki"], ["Did you say \"gleki\"?", "Did you say \"happy\"?"])
        , (["xu do cusku zo tavla"], ["Did you say \"tavla\"?", "Did you say \"talk\"?"])
        , (["xu do cusku zo djuno"], ["Did you say \"djuno\"?", "Did you say \"know\"?"])
        , (["xu do cusku zo pilno"], ["Did you say \"pilno\"?", "Did you say \"use\"?"])
        , (["xu do cusku zo bangu"], ["Did you say \"bangu\"?", "Did you say \"language\"?"])
        -- ma cusku
        , (["ma cusku zo do"], ["Who said \"do\"?", "Who said \"you\"?"])
        , (["ma cusku zo prenu"], ["Who said \"prenu\"?", "Who said \"person\"?"])
        , (["ma cusku zo pendo"], ["Who said \"pendo\"?", "Who said \"friend\"?"])
        , (["ma cusku zo zdani"], ["Who said \"zdani\"?", "Who said \"house\"?"])
        , (["ma cusku zo skami"], ["Who said \"skami\"?", "Who said \"computer\"?"])
        , (["ma cusku zo gleki"], ["Who said \"gleki\"?", "Who said \"happy\"?"])
        , (["ma cusku zo tavla"], ["Who said \"tavla\"?", "Who said \"talk\"?"])
        , (["ma cusku zo djuno"], ["Who said \"djuno\"?", "Who said \"know\"?"])
        , (["ma cusku zo pilno"], ["Who said \"pilno\"?", "Who said \"use\"?"])
        , (["ma cusku zo bangu"], ["Who said \"bangu\"?", "Who said \"language\"?"])
        ]

translations14_lu :: TranslationGenerator
translations14_lu = expandTranslationGenerator $ combineGeneratorsUniformly [cusku] where
    cusku = generatorFromList
        -- Propositions
        [ (["mi cusku lu mi nelci do li'u"], ["I said \"mi nelci do\".", "I said \"I like you\"."])
        , (["mi cusku lu mi dunda lo mlatu li'u"], ["I said \"mi dunda lo mlatu\".", "I said \"I donated the cat\"."])
        , (["mi cusku lu do dunda lo mlatu li'u"], ["I said \"do dunda lo mlatu\".", "I said \"You donated the cat\"."])
        , (["mi cusku lu mi se zdani li'u"], ["I said \"mi se zdani\".", "I said \"I have a house\"."])
        , (["mi cusku lu do se zdani li'u"], ["I said \"do se zdani\".", "I said \"You have a house\"."])

        , (["mi cusku lu ma cusku zo do li'u"], ["I said \"ma cusku zo do\"", "I said \"Who said 'you'?\"."])
        , (["mi cusku lu ma cusku zo zdani li'u"], ["I said \"ma cusku zo zdani\"", "I said \"Who said 'house'?\"."])
        , (["mi cusku lu ma cusku zo mlatu li'u"], ["I said \"ma cusku zo mlatu\"", "I said \"Who said 'cat'?\"."])
        -- Questions
        , (["xu do cusku lu mi nelci do li'u"], ["Did you say \"mi nelci do\"?", "Did you say \"I like you\"?"])
        , (["xu do cusku lu mi dunda lo mlatu li'u"], ["Did you say \"mi dunda lo mlatu\"?", "Did you say \"I donated the cat\"?"])
        , (["xu do cusku lu do dunda lo mlatu li'u"], ["Did you say \"do dunda lo mlatu\"?", "Did you say \"You donated the cat\"?"])
        , (["xu do cusku lu mi se zdani li'u"], ["Did you say \"mi se zdani\"?", "Did you say \"I have a house\"?"])
        , (["xu do cusku lu do se zdani li'u"], ["Did you say \"do se zdani\"?", "Did you say \"You have a house\"?"])

        , (["ma cusku lu mi se zdani li'u"], ["Who said \"mi se zdani\"?", "Who said \"I have a house\"?"])
        ]

-- * Lesson 15: Relative phrases
translations15_expressions :: TranslationGenerator
translations15_expressions = expandTranslationGenerator $ combineGeneratorsUniformly [gerku, mlatu, pendo] where
    gerku = generatorFromList
        [ (["lo mi gerku"], ["My dog."])
        , (["lo gerku pe lo prenu"], ["The person's dog."])
        , (["lo gerku pe lo pendo"], ["The friend's dog."])
        , (["lo gerku pe lo nupre"], ["The promisor's dog."])
        , (["lo gerku pe lo dunda"], ["The donor's dog."])
        , (["lo gerku pe lo te dunda"], ["The recipient's dog."])
        , (["lo gerku pe lo vecnu"], ["The seller's dog."])
        , (["lo gerku pe lo te vecnu"], ["The buyer's dog."])
        ]
    mlatu = generatorFromList
        [ (["lo mi mlatu"], ["My cat."])
        , (["lo mlatu pe lo prenu"], ["The person's cat."])
        , (["lo mlatu pe lo pendo"], ["The friend's cat."])
        , (["lo mlatu pe lo nupre"], ["The promisor's cat."])
        , (["lo mlatu pe lo dunda"], ["The donor's cat."])
        , (["lo mlatu pe lo te dunda"], ["The recipient's cat."])
        , (["lo mlatu pe lo vecnu"], ["The seller's cat."])
        , (["lo mlatu pe lo te vecnu"], ["The buyer's cat."])
        ]
    pendo = generatorFromList
        [ (["lo mi pendo"], ["My friend."])
        , (["lo do pendo"], ["Your friend."])
        , (["lo gerku pe lo mi pendo"], ["My friend's dog."])
        , (["lo mlatu pe lo mi pendo"], ["My friend's cat."])
        , (["lo pendo pe lo mi pendo"], ["My friend's friend."])
        , (["lo gerku pe lo do pendo"], ["Your friend's dog."])
        , (["lo mlatu pe lo do pendo"], ["Your friend's cat."])
        , (["lo pendo pe lo do pendo"], ["Your friend's friend."])
        ]

translations15_sentences :: TranslationGenerator
translations15_sentences = expandTranslationGenerator $ combineGeneratorsUniformly [gerku, mlatu, pendo] where
    gerku = generatorFromList
        [ (["mi dunda lo mi gerku"], ["I donated my dog."])
        , (["lo mi gerku cu pendo"], ["My dog is friendly."])
        , (["xu do dunda lo do gerku"], ["Did you donate your dog?"])
        , (["xu lo do gerku cu pendo"], ["Is your dog is friendly?"])
        ]
    mlatu = generatorFromList
        [ (["mi dunda lo mi mlatu"], ["I donated my cat."])
        , (["lo mi mlatu cu pendo"], ["My cat is friendly."])
        , (["xu do dunda lo do mlatu"], ["Did you donate your cat?"])
        , (["xu lo do mlatu cu pendo"], ["Is your cat is friendly?"])
        ]
    pendo = generatorFromList
        -- mi
        [ (["mi tavla lo mi pendo"], ["I talked to my friend."])
        , (["mi nelci lo mi pendo"], ["I like my friend."])
        , (["mi tavla lo do pendo"], ["I talked to your friend."])
        , (["mi nelci lo do pendo"], ["I like your friend.", "I liked your friend."])
        , (["mi dunda lo mi gerku lo do pendo"], ["I donated my dog to your friend."])
        , (["mi dunda lo mi mlatu lo do pendo"], ["I donated my cat to your friend."])
        , (["mi djuno lo du'u do tavla lo do pendo"], ["I know that you talked to your friend."])
        , (["mi djuno lo du'u do tavla lo mi pendo"], ["I know that you talked to my friend."])
        -- xu do
        , (["xu do tavla lo mi pendo"], ["Did you talk to my friend?"])
        , (["xu do nelci lo mi pendo"], ["Do you like my friend?", "Did you like my friend?"])
        , (["xu do tavla lo do pendo"], ["Did you talk to your friend?"])
        , (["xu do nelci lo do pendo"], ["Do you like your friend?"])
        , (["xu do dunda lo do gerku lo mi pendo"], ["Did you donate your dog to my friend?"])
        , (["xu do dunda lo do mlatu lo mi pendo"], ["Did you donate your cat to my friend?"])
        , (["xu do djuno lo du'u mi tavla lo do pendo"], ["Did you know that I talked to your friend?"])
        , (["xu do djuno lo du'u mi tavla lo mi pendo"], ["Did you know that I talked to my friend?"])
        ]

-- * Lesson 16: Logical connectives 1
translations16_a :: TranslationGenerator
translations16_a = expandTranslationGenerator $ combineGeneratorsUniformly [zdani, tavla, gleki] where
    zdani = generatorFromList
        [ (["mi .a do se zdani"], ["I have a house, or you have a house (or both)."])
        , (["mi .a lo mi pendo cu se zdani"], ["I have a house, or my friend has a house (or both)."])
        , (["do .a lo do pendo cu se zdani"], ["You have a house, or your friend has a house (or both)."])
        ]
    tavla = generatorFromList
        [ (["mi .a do tavla"], ["I will talk, or you will talk (or both)."])
        , (["mi .a do tavla fi lo gerku"], ["I will talk about the dog, or you will talk about the dog (or both)."])
        , (["mi .a do tavla fi lo mlatu"], ["I will talk about the cat, or you will talk about the cat (or both)."])
        , (["mi .a do tavla lo mi pendo"], ["I will talk to my friend, or you will talk to my friend (or both)."])
        , (["mi .a lo mi pendo cu tavla"], ["I will talk, or my friend will talk (or both)."])
        ]
    gleki = generatorFromList
        [ (["mi .a do gleki"], ["I am happy, or you are happy."])
        , (["mi .a lo mi pendo cu gleki"], ["I am happy, or my friend is happy."])
        , (["mi .a lo do pendo cu gleki"], ["I am happy, or your friend is happy."])
        , (["mi .a lo do pendo cu gleki"], ["I am happy, or your friend is happy."])
        ]

translations16_e :: TranslationGenerator
translations16_e = expandTranslationGenerator $ combineGeneratorsUniformly [zdani, tavla, gleki] where
    zdani = generatorFromList
        [ (["mi .e do se zdani"], ["I have a house, and you have a house."])
        , (["mi .e lo mi pendo cu se zdani"], ["I have a house, and my friend has a house."])
        , (["mi .e lo do pendo cu se zdani"], ["I have a house, and your friend has a house."])
        ]
    tavla = generatorFromList
        [ (["mi .e do tavla"], ["I will talk, and you will talk."])
        , (["mi .e do tavla fi lo gerku"], ["I will talk about the dog, and you will talk about the dog."])
        , (["mi .e do tavla fi lo mlatu"], ["I will talk about the cat, and you will talk about the cat."])
        , (["mi .e do tavla lo mi pendo"], ["I will talk to my friend, and you will talk to my friend."])
        , (["mi .e lo mi pendo cu tavla"], ["I will talk, and my friend will talk."])
        ]
    gleki = generatorFromList
        [ (["mi .e do gleki"], ["I am happy, and you are happy."])
        , (["mi .e lo mi pendo cu gleki"], ["I am happy, and my friend is happy."])
        , (["mi .e lo do pendo cu gleki"], ["I am happy, and your friend is happy."])
        , (["do .e lo do pendo cu gleki"], ["You are happy, and your friend is happy."])
        ]

translations16_o :: TranslationGenerator
translations16_o = expandTranslationGenerator $ combineGeneratorsUniformly [zdani, tavla, gleki] where
    zdani = generatorFromList
        [ (["mi .o do se zdani"], ["I have a house, if and only if you have a house."])
        , (["mi .o lo mi pendo cu se zdani"], ["I have a house, if and only if my friend has a house."])
        , (["do .o lo do pendo cu se zdani"], ["You have a house, if and only if your friend has a house."])
        ]
    tavla = generatorFromList
        [ (["mi .o do tavla"], ["I will talk, if and only if you talk."])
        , (["mi .o do tavla fi lo gerku"], ["I will talk about the dog, if and only if you talk about the dog."])
        , (["mi .o do tavla fi lo mlatu"], ["I will talk about the cat, if and only if you talk about the cat."])
        , (["mi .o do tavla lo mi pendo"], ["I will talk to my friend, if and only if you talk to my friend."])
        , (["mi .o lo mi pendo cu tavla"], ["I will talk, if and only if my friend talks."])
        ]
    gleki = generatorFromList
        [ (["mi .o do gleki"], ["I am happy, if and only if you are happy."])
        , (["mi .o lo mi pendo cu gleki"], ["I am happy, if and only if my friend is happy."])
        , (["mi .o lo do pendo cu gleki"], ["I am happy, if and only if your friend is happy."])
        , (["do .o lo do pendo cu gleki"], ["You are happy, if and only if your friend is happy."])
        ]

translations16_u :: TranslationGenerator
translations16_u = expandTranslationGenerator $ combineGeneratorsUniformly [zdani] where
    zdani = generatorFromList
        [ (["mi .u do se zdani"], ["I have a house, regardless of whether you have a house."])
        , (["mi .u lo mi pendo cu se zdani"], ["I have a house, regardless of whether my friend has a house."])
        , (["do .u lo do pendo cu se zdani"], ["You have a house, regardless of whether your friend has a house."])
        ]
    tavla = generatorFromList
        [ (["mi .u do tavla"], ["I will talk, regardless of whether you talk."])
        , (["mi .u do tavla fi lo gerku"], ["I will talk about the dog, regardless of whether you talk about the dog."])
        , (["mi .u do tavla fi lo mlatu"], ["I will talk about the cat, regardless of whether you talk about the cat."])
        , (["mi .u do tavla lo mi pendo"], ["I will talk to my friend, regardless of whether you talk to my friend."])
        , (["mi .u lo mi pendo cu tavla"], ["I will talk, regardless of whether my friend talks."])
        ]
    gleki = generatorFromList
        [ (["mi .u do gleki"], ["I am happy, regardless of whether you are happy."])
        , (["mi .u lo mi pendo cu gleki"], ["I am happy, regardless of whether my friend is happy."])
        , (["mi .u lo do pendo cu gleki"], ["I am happy, regardless of whether your friend is happy."])
        , (["do .u lo do pendo cu gleki"], ["You are happy, regardless of whether your friend is happy."])
        ]

-- * Lesson 17: Negation 1
translations17_na :: TranslationGenerator
translations17_na = expandTranslationGenerator $ combineGeneratorsUniformly [nelci, dunda, vecnu, tavla, pendo, melbi] where
    nelci = generatorFromList
        [ (["mi na nelci do"], ["Not true: I like you."])
        , (["do na nelci mi"], ["Not true: you like me."])
        , (["mi na nelci lo mlatu"], ["Not true: I like the cat."])
        , (["mi na nelci lo gerku"], ["Not true: I like the dog."])
        , (["mi na nelci lo mi mlatu"], ["Not true: I like my cat."])
        , (["mi na nelci lo mi gerku"], ["Not true: I like my dog."])
        , (["mi na nelci lo mi zdani"], ["Not true: I like my house."])
        , (["mi na nelci lo se dunda be do"], ["Not true: I like the gift that you gave."])
        ]
    dunda = generatorFromList
        [ (["mi na dunda lo mlatu"], ["Not true: I will donate the cat."])
        , (["mi na dunda lo gerku"], ["Not true: I will donate the dog."])
        , (["mi na dunda lo zdani"], ["Not true: I will donate the house."])
        , (["na ku gau do mi dunda lo gerku"], ["Not true: you made me donate the dog."])
        , (["na ku gau do mi dunda lo mlatu"], ["Not true: you made me donate the cat."])
        , (["na ku gau do mi dunda lo zdani"], ["Not true: you made me donate the house."])
        ]
    vecnu = generatorFromList
        [ (["lo pu vecnu na gleki"], ["Not true: the former seller is happy."])
        , (["lo ba te vecnu na gleki"], ["Not true: the future buyer is happy."])
        ]
    tavla = generatorFromList
        [ (["na ku gau do mi tavla do"], ["Not true: you made me talk to you."])
        , (["na ku gau do mi tavla fi lo gerku"], ["Not true: you made me talk about the dog."])
        , (["na ku gau do mi tavla fi lo mlatu"], ["Not true: you made me talk about the cat."])
        ]
    pendo = generatorFromList
        [ (["do na pendo"], ["Not true: you are friendly."])
        , (["do na pendo mi"], ["Not true: you are my friend."])
        ]
    melbi = generatorFromList
        [ (["do na melbi"], ["Not true: you are beautiful."])
        , (["mi na melbi"], ["Not true: I am beautiful."])
        , (["lo mlatu na melbi"], ["Not true: cats are beautiful.", "Not true: the cat is beautiful"])
        , (["lo bangu na melbi"], ["Not true: languages are beautiful.", "Not true: the language "])
        ]

translations17_na'e :: TranslationGenerator
translations17_na'e = expandTranslationGenerator $ combineGeneratorsUniformly [nelci, dunda, melbi] where
    nelci = generatorFromList
        [ (["mi na'e nelci do"], ["I other-than-like you."])
        , (["do na'e nelci mi"], ["You other-than-like me."])
        , (["mi na'e nelci lo mlatu"], ["I other-than-like the cat.", "I other-than-like cats."])
        , (["mi na'e nelci lo gerku"], ["I other-than-like the dog.", "I other-than-like dogs."])
        , (["mi na'e nelci lo mi mlatu"], ["I other-than-like my cat."])
        , (["mi na'e nelci lo mi gerku"], ["I other-than-like my dog."])
        , (["mi na'e nelci lo mi zdani"], ["I other-than-like my house."])
        , (["mi na'e nelci lo se dunda be do"], ["I other-than-like the gift that you gave."])
        ]
    dunda = generatorFromList
        [ (["mi na'e dunda lo mlatu"], ["I other-than-donated the computer."])
        , (["xu do na'e dunda lo mlatu"], ["Did you other-than-donate the computer?"])
        , (["lo na'e dunda mlatu cu melbi"], ["The other-than-donated cat is beautiful."])
        ]
    melbi = generatorFromList
        [ (["do na'e melbi"], ["You are other-than-beautiful."])
        , (["mi na'e melbi"], ["I am other-than-beautiful."])
        , (["lo mlatu na'e melbi"], ["Cats are other-than-beautiful.", "The cat is other-than-beautiful."])
        , (["lo bangu na'e melbi"], ["Languages are other-than-beautiful.", "The language is other-than-beautiful."])
        ]

translations17_no'e :: TranslationGenerator
translations17_no'e = expandTranslationGenerator $ combineGeneratorsUniformly [nelci] where
    nelci = generatorFromList
        [ (["mi no'e nelci do"], ["I don't really like you."])
        , (["do no'e nelci mi"], ["You don't really like me."])
        , (["mi no'e nelci lo mlatu"], ["I don't really like the cat.", "I don't really like cats."])
        , (["mi no'e nelci lo gerku"], ["I don't really like the dog.", "I don't really like dogs."])
        , (["mi no'e nelci lo mi mlatu"], ["I don't really like my cat."])
        , (["mi no'e nelci lo mi gerku"], ["I don't really like my dog."])
        , (["mi no'e nelci lo mi zdani"], ["I don't really like my house."])
        , (["mi no'e nelci lo se dunda be do"], ["I don't really like the gift that you gave."])
        ]
    melbi = generatorFromList
        [ (["do no'e melbi"], ["You are not really beautiful."])
        , (["mi no'e melbi"], ["I am not really beautiful."])
        , (["lo mlatu no'e melbi"], ["Cats are not really beautiful.", "The cat is not really beautiful."])
        , (["lo bangu no'e melbi"], ["Languages are not really beautiful.", "The language is not really beautiful."])
        ]

translations17_to'e :: TranslationGenerator
translations17_to'e = expandTranslationGenerator $ combineGeneratorsUniformly [nelci] where
    nelci = generatorFromList
        [ (["mi to'e nelci do"], ["I dislike you."])
        , (["do to'e nelci mi"], ["You dislike me."])
        , (["mi to'e nelci lo mlatu"], ["I dislike the cat.", "I dislike cats."])
        , (["mi to'e nelci lo gerku"], ["I dislike the dog.", "I dislike dogs."])
        , (["mi to'e nelci lo mi mlatu"], ["I dislike my cat."])
        , (["mi to'e nelci lo mi gerku"], ["I dislike my dog."])
        , (["mi to'e nelci lo mi zdani"], ["I dislike my house."])
        , (["mi to'e nelci lo se dunda be do"], ["I dislike the gift that you gave."])
        ]
    melbi = generatorFromList
        [ (["do to'e melbi"], ["You are ugly."])
        , (["mi to'e melbi"], ["I am ugly."])
        , (["lo mlatu to'e melbi"], ["Cats are ugly.", "The cat is ugly."])
        , (["lo bangu to'e melbi"], ["Languages are ugly.", "The language is ugly."])
        ]

translations17 :: TranslationGenerator
translations17 = expandTranslationGenerator $ combineGeneratorsUniformly [translations17_na, translations17_na'e, translations17_no'e, translations17_to'e]

-- * Lesson 18: Misc 1
translations18 :: TranslationGenerator
translations18 = expandTranslationGenerator $ combineGeneratorsUniformly [djica, cusku, lerci, mukti] where
    djica = generatorFromList
        [ (["mi jai se djica", "tu'a mi se djica"], ["I am desired."])
        , (["do jai se djica", "tu'a do se djica"], ["You are desired."])
        , (["xu do jai se djica", "xu tu'a do se djica"], ["Are you desired?"])
        , (["mi djica tu'a do", "do jai se djica mi"], ["I desire (something about) you.", "(Something about) you is desired by me."])
        , (["xu do djica tu'a mi", "xu mi jai se djica do"], ["Do you desire (something about) me?", "Is (something about) me desired by you?"])
        , (["do djica tu'a ma (poi|noi) prenu", "ma (poi|noi) prenu ku'o jai se djica do"], ["What person do you desire (something about)?"])
        , (["mi djica tu'a lo plise", "lo plise cu jai se djica mi"], ["I want (something about) an apple.", "(Something about) an apple is desired by me."])
        , (["xu do djica tu'a lo plise", "xu lo plise cu jai se djica"], ["Do you want (something about) an apple?", "Is (something about) an apple wanted by you?"])
        ]
    lerci = generatorFromList
        [ (["mi jai lerci", "tu'a mi lerci"], ["I am late."])
        , (["do jai lerci", "tu'a do lerci"], ["You are late."])
        , (["xu do jai lerci", "xu tu'a do lerci"], ["Are you late?"])
        , (["mi tavla do noi jai lerci"], ["I am talking to you, who are late."])
        , (["ma (poi|noi) prenu cu jai lerci"], ["Which person is late?"])
        ]
    cusku = generatorFromList
        [ (["do cusku tu'a lo plise"], ["You said (something about) apples."])
        , (["mi cusku tu'a lo mlatu"], ["I said (something about) cats."])
        , (["ma cusku tu'a lo skami"], ["Who said (something about) computers?"])
        ]
    mukti = generatorFromList
        [ (["tu'a do mukti lo nu ctuca kei mi", "do jai mukti lo nu ctuca kei mi", "tu'a do mukti lo nu mi ctuca", "do jai mukti lo nu mi ctuca"], ["(Something about) you motivates me to teach."])
        , (["tu'a do mukti fi mi", "do jai mukti fi mi"], ["(Something about) you motivates me."])
        , (["tu'a mi mukti fi do", "mi jai mukti fi do"], ["(Something about) me motivates you."])
        ]

-- * Lesson 23: Tenses 2
translations23 :: TranslationGenerator
translations23 = expandTranslationGenerator $ combineGeneratorsUniformly [pu'o, ca'o, ba'o, co'a, co'u] where
    pu'o = generatorFromList
        [ (["do pu'o citka lo plise"], ["You are about to eat an apple."])
        , (["do pu'o pendo mi"], ["You are about to be my friend."])
        , (["mi pu'o tavla lo vecnu"], ["I am about to talk to the seller."])
        , (["mi pu'o tavla lo te vecnu"], ["I am about to talk to the buyer."])
        , (["lo zdani pu'o pelxu"], ["The house is about to be yellow."])
        , (["mi pu'o fanva fi lo (lojbo|jbobau)"], ["I am about to translate to Lojban."])
        , (["mi pu'o fanva fo lo (lojbo|jbobau)"], ["I am about to translate from Lojban."])
        , (["mi pu'o dunda lo mlatu"], ["I am about to donate the cat."])
        , (["mi pu'o dunda lo gerku"], ["I am about to donate the dog."])
        , (["mi pu'o vecnu lo zdani"], ["I am about to sell the house."])
        , (["do pu'o gleki"], ["You are about to be happy."])
        , (["lo prenu pu'o gleki"], ["The person is about to be happy."])
        , (["lo vecnu pu'o gleki"], ["The seller is about to be happy."])
        , (["lo te vecnu pu'o gleki"], ["The buyer is about to be happy."])
        , (["xu do pu'o citka"], ["Are you about to write?"])
        , (["xu do pu'o tavla"], ["Are you about to talk?"])
        , (["xu do pu'o fanva"], ["Are you about to translate?"])
        ]
    ca'o = generatorFromList
        [ (["do ca'o citka lo plise"], ["You are currently eating an apple."])
        , (["do ca'o pendo mi"], ["You are currently my friend."])
        , (["mi ca'o tavla lo vecnu"], ["I am currently talking to the seller."])
        , (["mi ca'o tavla lo te vecnu"], ["I am currently talking to the buyer."])
        , (["lo zdani ca'o pelxu"], ["The house is currently yellow."])
        , (["mi ca'o fanva fi lo (lojbo|jbobau)"], ["I am currently translating to Lojban."])
        , (["mi ca'o fanva fo lo (lojbo|jbobau)"], ["I am currently translating from Lojban."])
        , (["mi ca'o dunda lo mlatu"], ["I am currently donating the cat."])
        , (["mi ca'o dunda lo gerku"], ["I am currently donating the dog."])
        , (["mi ca'o vecnu lo zdani"], ["I am currently selling the house."])
        , (["do ca'o gleki"], ["You are currently happy."])
        , (["lo prenu ca'o gleki"], ["The person is currently happy."])
        , (["lo vecnu ca'o gleki"], ["The seller is currently happy."])
        , (["lo te vecnu ca'o gleki"], ["The buyer is currently happy."])
        , (["xu do ca'o citka"], ["Are you currently writing?"])
        , (["xu do ca'o tavla"], ["Are you currently talking?"])
        , (["xu do ca'o fanva"], ["Are you currently translating?"])
        ]
    ba'o = generatorFromList
        [ (["do ba'o citka lo plise"], ["You are have finished eating the apple."])
        , (["do ba'o pendo mi"], ["You have ceased to be my friend."])
        , (["mi ba'o tavla lo vecnu"], ["I have finished talking to the seller."])
        , (["mi ba'o tavla lo te vecnu"], ["I have finished talking to the buyer."])
        , (["lo zdani ba'o pelxu"], ["The house has ceased to be yellow."])
        , (["mi ba'o fanva fi lo (lojbo|jbobau)"], ["I have finished translating to Lojban."])
        , (["mi ba'o fanva fo lo (lojbo|jbobau)"], ["I have finished translating from Lojban."])
        , (["mi ba'o dunda lo mlatu"], ["I have finished donating the cat."])
        , (["mi ba'o dunda lo gerku"], ["I have finished donating the dog."])
        , (["mi ba'o vecnu lo zdani"], ["I have finished selling the house."])
        , (["do ba'o gleki"], ["You have ceased to be happy."])
        , (["lo prenu ba'o gleki"], ["The person has ceased to be happy."])
        , (["lo vecnu ba'o gleki"], ["The seller has ceased to be happy."])
        , (["lo te vecnu ba'o gleki"], ["The buyer has ceased to be happy."])
        , (["xu do ba'o citka"], ["Have you finished writing?"])
        , (["xu do ba'o tavla"], ["Have you finished talking?"])
        , (["xu do ba'o fanva"], ["Have you finished translating?"])
        ]
    co'a = generatorFromList
        [ (["do co'a citka lo plise"], ["You just started eating an apple."])
        , (["do co'a pendo mi"], ["You just became my friend."])
        , (["mi co'a tavla lo vecnu"], ["I just started talking to the seller."])
        , (["mi co'a tavla lo te vecnu"], ["I just started talking to the buyer."])
        , (["lo zdani co'a pelxu"], ["The house just became yellow."])
        , (["mi co'a fanva fi lo (lojbo|jbobau)"], ["I just started translating to Lojban."])
        , (["mi co'a fanva fo lo (lojbo|jbobau)"], ["I just started translating from Lojban."])
        , (["mi co'a dunda lo mlatu"], ["I just started donating the cat."])
        , (["mi co'a dunda lo gerku"], ["I just started donating the dog."])
        , (["mi co'a vecnu lo zdani"], ["I just started selling the house."])
        , (["do co'a gleki"], ["You just became happy."])
        , (["lo prenu co'a gleki"], ["The person just became happy."])
        , (["lo vecnu co'a gleki"], ["The seller just became happy."])
        , (["lo te vecnu co'a gleki"], ["The buyer just became happy."])
        , (["xu do co'a citka"], ["Did you just start writing?"])
        , (["xu do co'a tavla"], ["Did you just start talking?"])
        , (["xu do co'a fanva"], ["Did you just start translating?"])
        ]
    co'u = generatorFromList
        [ (["do co'u citka lo plise"], ["You just finished eating an apple."])
        , (["do co'u pendo mi"], ["You just ceased being my friend."])
        , (["mi co'u tavla lo vecnu"], ["I just finished talking to the seller."])
        , (["mi co'u tavla lo te vecnu"], ["I just finished talking to the buyer."])
        , (["lo zdani co'u pelxu"], ["The house just ceased to be yellow."])
        , (["mi co'u fanva fi lo (lojbo|jbobau)"], ["I just finished translating to Lojban."])
        , (["mi co'u fanva fo lo (lojbo|jbobau)"], ["I just finished translating from Lojban."])
        , (["mi co'u dunda lo mlatu"], ["I just finished donating the cat."])
        , (["mi co'u dunda lo gerku"], ["I just finished donating the dog."])
        , (["mi co'u vecnu lo zdani"], ["I just finished selling the house."])
        , (["do co'u gleki"], ["You just ceased to be happy."])
        , (["lo prenu co'u gleki"], ["The person just ceased to be happy."])
        , (["lo vecnu co'u gleki"], ["The seller just ceased to be happy."])
        , (["lo te vecnu co'u gleki"], ["The buyer just ceased to be happy."])
        , (["xu do co'u citka"], ["Did you just finish writing?"])
        , (["xu do co'u tavla"], ["Did you just finish talking?"])
        , (["xu do co'u fanva"], ["Did you just finish translating?"])
        ]
