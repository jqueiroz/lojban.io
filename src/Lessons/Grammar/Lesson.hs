{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Lessons.Grammar.Lesson (course) where

import Core
import Lessons.Grammar.Vocabulary
import Lessons.Grammar.Sentences
import Lessons.Grammar.ExerciseGenerators
import Util (combineFunctions, combineFunctionsUniformly)
import qualified Text.Pandoc as P
import Data.FileEmbed (embedStringFile)

------- Lesson plans
plan1 :: P.Pandoc
Right plan1 = P.readMarkdown P.def $ $(embedStringFile "courses/english/introduction/planning/1.md")

plan2 :: P.Pandoc
Right plan2 = P.readMarkdown P.def $ $(embedStringFile "courses/english/introduction/planning/2.md")

-------- Vocabulary
vocabularyGenerator1 :: VocabularyBuilder
vocabularyGenerator1 = createVocabularyBuilder
    -- Gismu
    ["tavla", "dunda", "prenu", "zdani", "pendo", "mlatu"]
    -- Cmavo
    ["zo'e", "mi", "do", "ti", "ta"]
    -- Cmevla
    []
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
    -- Gismu
    ["tavla", "dunda", "prenu", "zdani", "pendo", "mlatu", "gerku", "melbi", "nelci", "ctuca"]
    -- Cmavo
    ["zo'e", "lo", "ku", "mi", "do", "ti", "ta"]
    -- Cmevla
    []
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
    -- Gismu
    ["tavla", "dunda", "klama", "prenu", "melbi", "sutra", "zdani", "mlatu", "gerku", "pelxu", "nelci", "citka", "catlu", "djica", "djuno", "drata", "kumfa", "mutce", "cusku", "troci", "viska", "xamgu", "gleki"]
    -- Cmavo
    ["zo'e", "lo", "ku", "mi", "do", "ti", "ta"]
    -- Cmevla
    []
    -- Selbri
    [
        ("actions", ["tavla", "dunda", "klama"]),
        ("properties", ["prenu", "melbi", "sutra", "zdani", "mlatu", "gerku", "pelxu"]),
        ("relations", ["nelci"])
    ]
    -- Sumti
    [
        ("genericPersons", ["mi", "do"]),
        ("genericPintable", ["ti", "ta", "tu"])
    ]

-------- Translations
translations1_nice :: [Translation]
translations1_nice =
    [ ("You gave me this.", "do dunda ti mi")
    , ("I gave you that.", "mi dunda ta do")
    , ("I am talking to myself", "mi tavla mi")
    ]

translations1 :: [Translation]
translations1 = translations1_nice ++
    [ ("I have a house.", "zdani mi")
    , ("You are my friend.", "do pendo mi")
    , ("I am your friend.", "mi pendo do")
    , ("I am talking to you.", "mi tavla do")
    , ("You are talking to me.", "do tavla mi")
    , ("You are talking to yourself.", "do tavla do")
    , ("I am a person.", "mi prenu")
    , ("You are a person.", "do prenu")
    , ("That is a house", "ta zdani")
    , ("That is a cat.", "ta mlatu")
    ]

translations2_nice :: [Translation]
translations2_nice =
    [ ("I like the donor.", "mi nelci lo dunda ku")
    , ("The gift is beautiful.", "lo se dunda ku melbi")
    , ("I was given a dog.", "mi te dunda lo gerku ku")
    , ("The instructor has a house.", "lo ctuca ku se zdani")
    , ("I like the teaching method.", "mi nelci lo xe ctuca ku")
    , ("The listener is a person.", "lo se tavla ku prenu") -- is "listener" a good choice?
    , ("The speaker is my friend.", "lo tavla ku pendo mi") -- is "speaker" a good choice? maybe it implies voice...
    ]

translations2 :: [Translation]
translations2 = translations1_nice ++ translations2_nice ++
    [ ("I have a house.", "mi se zdani")
    , ("A person is talking to a dog.", "lo prenu ku tavla lo gerku ku")
    , ("The dog likes the cat.", "lo gerku ku nelci lo mlatu ku")
    , ("The house is beautiful!", "lo zdani ku melbi")
    , ("I was given a cat.", "mi te dunda lo mlatu ku")
    , ("I will teach you.", "mi ctuca do")
    , ("Somebody teached me.", "mi se ctuca")
    ]

-------- Exercises
exercises1 :: Dictionary -> ExerciseGenerator
exercises1 dictionary =
    combineFunctions
        [ (20, generateGrammaticalClassExercise vocabulary)
        , (15, generateBridiJufraExercise vocabulary displayVariantBridi)
        , (20, generateSelbriIdentificationExercise vocabulary displayVariantBridi)
        , (30, generateContextualizedGismuPlacesExercise dictionary vocabulary displayVariantBridi)
        , (40, generateTranslationExercise basicSentenceCannonicalizer translations1)
        ]
    where
        vocabulary = vocabularyGenerator1 dictionary

exercises2 :: Dictionary -> ExerciseGenerator
exercises2 dictionary =
    combineFunctions
        [ (15, generateGrammaticalClassExercise vocabulary)
        , (15, generateBridiJufraExercise vocabulary displayBridi)
        , (15, generateSelbriIdentificationExercise vocabulary displayBridi)
        , (30, generateContextualizedGismuPlacesExercise dictionary vocabulary displayBridi)
        , (50, generateTranslationExercise basicSentenceCannonicalizer translations2)
        , (30, generateIsolatedGismuPlacesExercise dictionary vocabulary)
        ]
    where
        vocabulary = vocabularyGenerator2 dictionary
        displayBridi = combineFunctionsUniformly [displayVariantBridi, displayReorderedVariantBridi]

-------- Lessons
lesson1 :: LessonBuilder
lesson1 dictionary = Lesson "Basics 1" (exercises1 dictionary) plan1

lesson2 :: LessonBuilder
lesson2 dictionary = Lesson "Basics 2" (exercises2 dictionary) plan2

-------- Course
course :: CourseBuilder
course = createCourseBuilder "Introduction to Lojban for English speakers" [lesson1, lesson2]
