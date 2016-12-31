{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Courses.English.Grammar.Introduction (course) where

import Core
import Courses.Util.Vocabulary
import Courses.Util.Sentences
import Courses.Util.ExerciseGenerators
import Util (combineFunctions, combineFunctionsUniformly, containsWord)
import Data.FileEmbed (embedStringFile)
import Control.Applicative ((<$>))
import qualified Data.Text as T
import qualified Text.Pandoc as P

--TODO: differenciate "le" and "lo"? I think that a lot of translations here are misleading

------- Lesson plans
plan1 :: P.Pandoc
Right plan1 = P.readMarkdown P.def $ $(embedStringFile "courses/english/introduction/planning/1.md")

plan2 :: P.Pandoc
Right plan2 = P.readMarkdown P.def $ $(embedStringFile "courses/english/introduction/planning/2.md")

plan3 :: P.Pandoc
Right plan3 = P.readMarkdown P.def $ $(embedStringFile "courses/english/introduction/planning/3.md")

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
        ("actions", ["tavla", "dunda", "ctuca", "nupre"]),
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
translations1_nice :: [ExerciseGenerator]
translations1_nice = generateTranslationExercise basicSentenceCanonicalizer <$>
    [ (["mi tavla mi"], ["I am talking to myself.", "I was talking to myself.", "We were talking to ourselves."])
    , (["do dunda ti mi"], ["You gave me this.", "You gave us this."])
    , (["mi dunda ta do"], ["I gave you that.", "We gave you that."])
    ]

translations1 :: [ExerciseGenerator]
translations1 = (++) translations1_nice $ generateTranslationExercise basicSentenceCanonicalizer <$>
    [ (["mi tavla zo'e do"], ["I was talking about you.", "We were talking about you.", "I am talking about you.", "We are talking about you.", "I will talk about you.", "We will talk about you."])
    , (["mi tavla do"], ["I am talking to you.", "I was talking to you.", "We are talking to you.", "We were talking to you."])
    , (["do tavla mi"], ["You are talking to me.", "You are talking to us."])
    , (["do tavla do"], ["You are talking to yourself."])
    , (["mi dunda zo'e do"], ["I gave you something.", "I will give you something."])
    , (["do pendo mi"], ["You are my friend."])
    , (["mi pendo do"], ["I am your friend."])
    , (["mi prenu"], ["I am a person.", "We are persons."])
    , (["do prenu"], ["You are a person.", "You are persons."])
    , (["ti mlatu"], ["This is a cat.", "These are cats."])
    , (["ta mlatu"], ["That is a cat.", "Those are cats."])
    , (["ta zdani"], ["That is a house.", "Those are houses."])
    , (["zdani mi"], ["I have a house.", "We have a house.", "We have houses."])
    ]

translations2_nice :: [ExerciseGenerator]
translations2_nice = t1 ++ t2 where
    special =
        [ (["mi tavla fi do"], ["I was talking about you.", "We were talking about you.", "I am talking about you.", "We are talking about you.", "I will talk about you.", "We will talk about you."])
        , (["mi tavla fi lo mlatu ku"], ["I was talking about the cat.", "I was talking about the cats.", "I am talking about the cat.", "I am talking about the cats."])
        , (["tavla fi lo mlatu ku"], ["Somebody was talking about the cat.", "Somebody was talking about the cats."])
        , (["mi dunda fi do"], ["I gave you something.", "I will give you something."])
        ]
    t1 = generateTranslationExercise basicSentenceCanonicalizer <$> special ++
        [ (["mi dunda lo mlatu ku lo pendo ku"], ["I gave the cat to somebody's friend.", "I gave the cats to somebody's friend."])
        , (["mi te dunda lo mlatu ku"], ["I was given a cat.", "I was given cats.", "We were given a cat.", "We were given cats."])
        , (["lo te dunda ku nelci lo se dunda ku"], ["The recipient liked the gift.", "The recipient will like the gift.", "The recipients liked the gifts."])
        , (["dunda lo mlatu ku lo ctuca ku"], ["Somebody gave a cat to the instructor", "Somebody gave cats to the instructor"])
        , (["lo ctuca ku dunda lo mlatu ku mi"], ["The instructor gave me a cat.", "The instructor gave me cats.", "The instructor gave us a cat.", "The instructor gave us cats."])
        , (["dunda lo mlatu ku mi"], ["Somebody gave me a cat.", "Somebody gave me cats.", "Somebody gave us a cat.", "Somebody gave us cats."])
        , (["lo tavla ku pendo mi"], ["The speaker is my friend.", "The speakers are my friends."]) -- is "speaker" a good choice? maybe it implies voice...
        , (["mi nelci lo dunda ku"], ["I like the donor.", "I like the donors."])
        , (["mi nelci lo xe ctuca ku"], ["I like the teaching method."])
        , (["lo se dunda ku melbi"], ["The gift is beautiful.", "The gifts are beautiful."])
        , (["lo ctuca ku se zdani"], ["The instructor has a house."])
        , (["lo se tavla ku prenu"], ["The listener is a person.", "The listeners are persons."]) -- is "listener" a good choice?
        , (["lo prenu ku se zdani"], ["The person has a house."])
        , (["lo tavla ku se zdani"], ["The speaker has a house."])
        , (["lo se tavla ku se zdani"], ["The listener has a house."])
        , (["lo ctuca ku se zdani"], ["The instructor has a house."])
        , (["lo dunda ku se zdani"], ["The donor has a house."])
        , (["lo te dunda ku se zdani"], ["The recipient has a house."])
        ]
    t2 = generateRestrictedTranslationExercise "Translate without using \"zo'e\"" (not . containsWord (T.pack "zo'e")) basicSentenceCanonicalizer <$> special

translations2 :: [ExerciseGenerator]
translations2 =  translations1_nice ++ translations2_nice ++ t1 ++ t2 where
    special =
        [ (["mi tavla fi lo gerku ku"], ["I was talking about the dog.", "I was talking about the dogs.", "I am talking about the dog.", "I am talking about the dogs."])
        , (["tavla fi lo gerku ku"], ["Somebody was talking about the dog.", "Somebody was talking about the dogs."])
        ]
    t1 =  generateTranslationExercise basicSentenceCanonicalizer <$> special ++
        [ (["lo prenu ku tavla lo gerku ku"], ["A person is talking to a dog.", "The person talks to dogs."])
        , (["lo prenu ku tavla lo mlatu ku"], ["A person is talking to a cat.", "The person talks to cats."])
        , (["mi te dunda lo gerku ku"], ["I was given a dog.", "I was given dogs.", "We were given a dog.", "We were given dogs."])
        , (["mi dunda lo gerku ku lo pendo ku"], ["I gave the dog to somebody's friend.", "I gave the dogs to somebody's friend."])
        , (["dunda lo gerku ku mi"], ["Somebody gave me a dog.", "Somebody gave me dogs.", "Somebody gave us a dog.", "Somebody gave us dogs."])
        , (["lo ctuca ku dunda lo gerku ku mi"], ["The instructor gave me a dog.", "The instructor gave me dogs."])
        , (["dunda lo gerku ku lo ctuca ku"], ["Somebody gave a dog to the instructor", "Somebody gave dogs to the instructor"])
        , (["mi ctuca do"], ["I will teach you.", "We will teach you.", "I taught you.", "We taught you."])
        , (["do ctuca mi"], ["You will teach me.", "You will teach us.", "You taught me.", "You taught us."])
        , (["lo gerku ku nelci lo mlatu ku"], ["The dog likes the cat.", "Dogs like cats."])
        , (["lo mlatu ku nelci lo gerku ku"], ["The cat likes the dog.", "Cats like dogs."])
        , (["lo zdani ku melbi"], ["The house is beautiful!", "The houses are beautiful!"])
        , (["ctuca mi"], ["Somebody taught me.", "Somebody taught us."])
        , (["mi se zdani"], ["I have a house.", "We have a house."])
        ]
    t2 = generateRestrictedTranslationExercise "Translate without using \"zo'e\"" (not . containsWord (T.pack "zo'e")) basicSentenceCanonicalizer <$> special

translations3 :: [ExerciseGenerator]
translations3 = generateTranslationExercise basicSentenceCanonicalizer <$>
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
    , (["mi nelci lo nu tavla do kei ku", "mi nelci lo nu mi tavla do kei ku"], ["I like to talk to you."]) -- is nelci really adequate?
    , (["do nelci lo nu nupre kei ku", "do nelci lo nu do nupre kei ku"], ["You like to make promises."]) -- is nelci really adequate?
    , (["lo prenu ku nelci lo nu nupre kei ku"], ["People like to make promises."]) -- is nelci really adequate?
    , (["do nupre lo nu dunda lo mlatu ku kei ku", "do nupre lo nu do dunda lo mlatu ku kei ku"], ["You promised to donate the cat.", "You promised to donate the cats."])
    , (["do nupre lo nu dunda lo gerku ku kei ku", "do nupre lo nu do dunda lo gerku ku"], ["You promised to donate the dog.", "You promised to donate the dogs."])
    , (["do nupre lo nu dunda lo mlatu ku mi kei ku", "do nupre lo nu do dunda lo mlatu ku mi kei ku"], ["You promised to donate the cat to me.", "You promised to donate the cats to me."])
    , (["do nupre lo nu dunda lo gerku ku mi kei ku", "do nupre lo nu do dunda lo gerku ku mi kei ku"], ["You promised to donate the dog to me.", "You promised to donate the dogs to me."])
    , (["do nupre lo nu dunda lo mlatu ku kei ku mi", "do nupre lo nu do dunda lo mlatu ku kei ku mi"], ["You promised me to donate the cat.", "You promised me to donate the cats."])
    , (["do nupre lo nu dunda lo gerku ku kei ku mi", "do nupre lo nu do dunda lo gerku ku kei ku mi"], ["You promised me to donate the dog.", "You promised me to donate the dogs."])
    , (["do nupre lo nu do pendo kei ku", "do nupre lo nu pendo kei ku"], ["You promised to be friendly."])
    , (["lo prenu ku nupre"], ["People make promises."])
    , (["do nupre"], ["You made a promise."])
    , (["do nupre fi mi"], ["You promised me.", "You promised us."])
    , (["mi nupre fi do"], ["I promised you.", "We promised you."])
    ]

translations4 :: [ExerciseGenerator]
translations4 = generateTranslationExercise basicSentenceCanonicalizer <$>
    [ (["lo prenu ku sutra tavla"], ["The person talks quickly.", "The person is talking quickly.", "A person is talking quickly.", "People talk quickly"])
    ]


-------- Tanru
-- useful gismu: sutra, pelxu
-- lo melbi prenu, lo sutra mlatu, lo sutra gerku, lo gleki prenu

-------- Exercises
exercises1 :: Dictionary -> ExerciseGenerator
exercises1 dictionary =
    combineFunctions
        [ (20, generateGrammaticalClassExercise vocabulary)
        , (15, generateBridiJufraExercise vocabulary displayBridi)
        , (20, generateSelbriIdentificationExercise vocabulary displayBridi)
        , (20, generateContextualizedGismuPlacePositionExercise dictionary vocabulary displayBridi)
        , (10, generateContextualizedGismuPlaceMeaningExercise dictionary vocabulary displayBridi)
        , (40, combineFunctionsUniformly translations1)
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
        , (50, combineFunctionsUniformly translations2)
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
        , (70, combineFunctionsUniformly translations3)
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
