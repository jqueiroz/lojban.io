{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Courses.English.Grammar.Introduction (course) where

import Core
import Courses.Util.Vocabulary
import Courses.Util.Sentences
import Courses.Util.ExerciseGenerators
import Util (combineFunctions, combineFunctionsUniformly)
import Data.FileEmbed (embedStringFile)
import qualified Text.Pandoc as P

------- Lesson plans
plan1 :: P.Pandoc
Right plan1 = P.readMarkdown P.def $ $(embedStringFile "courses/english/introduction/planning/1.md")

plan2 :: P.Pandoc
Right plan2 = P.readMarkdown P.def $ $(embedStringFile "courses/english/introduction/planning/2.md")

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

vocabulary3 :: VocabularyBuilder
vocabulary3 = createVocabularyBuilder
    -- Selbri
    [
        ("actions", ["tavla", "dunda", "ctuca"]),
        ("properties", ["prenu", "zdani", "mlatu", "gerku", "melbi", "sutra", "pelxu"]),
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

-------- Translations
translations1_nice :: [Translation]
translations1_nice =
    [ ("do dunda ti mi", ["You gave me this.", "You gave us this."])
    , ("mi dunda ta do", ["I gave you that.", "We gave you that."])
    , ("mi tavla mi", ["I am talking to myself.", "I was talking to myself.", "We were talking to ourselves."])
    ]

translations1 :: [Translation]
translations1 = translations1_nice ++
    [ ("zdani mi", ["I have a house."])
    , ("do pendo mi", ["You are my friend."])
    , ("mi pendo do", ["I am your friend."])
    , ("mi tavla do", ["I am talking to you.", "I was talking to you.", "We are talking to you.", "We were talking to you."])
    , ("do tavla mi", ["You are talking to me.", "You are talking to us."])
    , ("do tavla do", ["You are talking to yourself."])
    , ("mi prenu", ["I am a person.", "We are persons."])
    , ("do prenu", ["You are a person.", "You are persons."])
    , ("ta zdani", ["That is a house.", "Those are houses."])
    , ("ti mlatu", ["This is a cat.", "These are cats."])
    , ("ta mlatu", ["That is a cat.", "Those are cats."])
    ]

translations2_nice :: [Translation]
translations2_nice =
    [ ("mi nelci lo dunda ku", ["I like the donor.", "I like the donors."])
    , ("lo se dunda ku melbi", ["The gift is beautiful.", "The gifts are beautiful."])
    , ("mi te dunda lo gerku ku", ["I was given a dog.", "We were given a dog.", "We were given dogs.", "Somebody gave me a dog.", "Somebody gave us a dog.", "Somebody gave us dogs."])
    , ("lo ctuca ku se zdani", ["The instructor has a house."])
    , ("mi nelci lo xe ctuca ku", ["I like the teaching method."])
    , ("lo se tavla ku prenu", ["The listener is a person.", "The listeners are persons."]) -- is "listener" a good choice?
    , ("lo tavla ku pendo mi", ["The speaker is my friend.", "The speakers are my friends."]) -- is "speaker" a good choice? maybe it implies voice...
    ]

translations2 :: [Translation]
translations2 = translations1_nice ++ translations2_nice ++
    [ ("mi se zdani", ["I have a house."])
    , ("lo prenu ku tavla lo gerku ku", ["A person is talking to a dog.", "The person talks to dogs."])
    , ("lo gerku ku nelci lo mlatu ku", ["The dog likes the cat."])
    , ("lo zdani ku melbi", ["The house is beautiful!", "The houses are beautiful!"])
    , ("mi te dunda lo mlatu ku", ["I was given a cat.", "We were given a cat.", "We were given cats.", "Somebody gave me a cat.", "Somebody gave us a cat.", "Somebody gave us cats."])
    , ("mi ctuca do", ["I will teach you."])
    , ("mi se ctuca", ["Somebody teached me."])
    ]

translations3 :: [Translation]
translations3 =
    [ ("lo prenu ku sutra tavla", ["The person talks quickly.", "The person is talking quickly.", "A person is talking quickly."])
    ]

-------- Exercises
exercises1 :: Dictionary -> ExerciseGenerator
exercises1 dictionary =
    combineFunctions
        [ (20, generateGrammaticalClassExercise vocabulary)
        , (15, generateBridiJufraExercise vocabulary displayBridi)
        , (20, generateSelbriIdentificationExercise vocabulary displayBridi)
        , (30, generateContextualizedGismuPlacesExercise dictionary vocabulary displayBridi)
        , (40, generateTranslationExercise basicSentenceCannonicalizer translations1)
        ]
    where
        vocabulary = vocabularyGenerator1 dictionary
        displayBridi = displayVariantSimpleBridi

exercises2 :: Dictionary -> ExerciseGenerator
exercises2 dictionary =
    combineFunctions
        [ (15, generateGrammaticalClassExercise vocabulary)
        , (15, generateBridiJufraExercise vocabulary displayBridi)
        , (15, generateSelbriIdentificationExercise vocabulary displayBridi)
        , (30, generateContextualizedGismuPlacesExercise dictionary vocabulary displayBridi)
        , (30, generateIsolatedGismuPlacesExercise dictionary vocabulary)
        , (50, generateTranslationExercise basicSentenceCannonicalizer translations2)
        ]
    where
        vocabulary = vocabularyGenerator2 dictionary
        displayBridi = combineFunctionsUniformly [displayVariantSimpleBridi, displayReorderedStandardSimpleBridi]

-------- Lessons
lesson1 :: LessonBuilder
lesson1 dictionary = Lesson "Basics 1" (exercises1 dictionary) plan1

lesson2 :: LessonBuilder
lesson2 dictionary = Lesson "Basics 2" (exercises2 dictionary) plan2

-------- Course
course :: CourseBuilder
course = createCourseBuilder "Introduction to Lojban for English speakers" [lesson1, lesson2]
