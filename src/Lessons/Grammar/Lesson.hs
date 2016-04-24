{-# LANGUAGE OverloadedStrings #-}
module Lessons.Grammar.Lesson where

import Core
import Lessons.Grammar.Vocabulary
import Lessons.Grammar.Sentences (displaySimpleBridi, displayVariantBridi)
import Lessons.Grammar.Exercises
import Util (combineFunctions, combineFunctionsUniformly)
import System.Random (StdGen)

import Control.Applicative ((<$>), (<*>))
import Dictionary (loadDictionary)

-------- Vocabulary
trivialVocabulary :: Dictionary -> Vocabulary
trivialVocabulary dictionary = buildVocabulary dictionary
    -- Gismu
    ["tavla", "dunda", "prenu", "pendo", "citka", "plise", "melbi", "zdani", "mlatu", "gerku", "nelci"]
    -- Cmavo
    ["zo'e", "mi", "do", "ti"]
    -- Cmevla
    []
    -- Selbri
    [
        ("actions", ["tavla", "dunda", "citka"]),
        ("properties", ["prenu", "melbi", "plise", "zdani", "mlatu", "gerku"]),
        ("relations", ["nelci", "pendo"])
    ]
    -- Sumti
    [
        ("persons", ["mi", "do"]),
        ("pointable", ["ti"]),
        ("subjects", ["ti"]),
        ("aliments", ["ti"])
    ]

basicVocabulary :: Dictionary -> Vocabulary
basicVocabulary dictionary = buildVocabulary dictionary
    -- Gismu
    ["tavla", "dunda", "klama", "prenu", "pendo", "citka", "plise", "melbi", "zdani", "sutra", "mlatu", "gerku", "nelci"]
    -- Cmavo
    ["zo'e", "lo", "mi", "do", "ti"]
    -- Cmevla
    []
    -- Selbri
    [
        ("actions", ["tavla", "dunda", "citka", "klama"]),
        ("properties", ["prenu", "melbi", "plise", "zdani", "mlatu", "gerku", "pelxu", "sutra"]),
        ("relations", ["nelci", "pendo"])
    ]
    -- Sumti
    [
        ("persons", ["mi", "do", "lo prenu ku"]),
        ("animals", ["lo mlatu ku", "lo gerku ku"]),
        ("pointable", ["ti"]),
        ("places", ["lo zdani ku"]),
        ("subjects", ["lo zdani ku", "lo mlatu ku", "lo gerku ku"]),
        ("aliments", ["lo plise ku"])
    ]

basicVocabulary' :: Dictionary -> Vocabulary
basicVocabulary' dictionary = buildVocabulary dictionary
    -- Gismu
    ["tavla", "dunda", "klama", "prenu", "melbi", "sutra", "zdani", "mlatu", "gerku", "pelxu", "nelci", "citka", "catlu", "djica", "djuno", "drata", "kumfa", "mutce", "cusku", "troci", "viska", "xamgu", "gleki"]
    -- Cmavo
    ["zo'e", "mi", "do", "ti"]
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
        ("persons", ["mi", "do"]),
        ("pointable", ["ti", "ta", "tu"])
    ]

-------- Lessons
lesson1 :: Dictionary -> StdGen -> Exercise
lesson1 dictionary =
    combineFunctions
        [ (20, generateGrammaticalClassExercise vocabulary)
        , (15, generateBridiJufraExercise vocabulary displaySimpleBridi)
        , (20, generateSelbriIdentificationExercise vocabulary displaySimpleBridi)
        , (30, generateEasyGismuPlacesExercise dictionary vocabulary displaySimpleBridi)
        ]
    where
        vocabulary = trivialVocabulary dictionary

-- TODO: se, te, ...
lesson2 :: Dictionary -> StdGen -> Exercise
lesson2 dictionary =
    combineFunctions
        [ (20, generateGrammaticalClassExercise vocabulary)
        , (15, generateBridiJufraExercise vocabulary displayVariantBridi)
        , (20, generateSelbriIdentificationExercise vocabulary displayVariantBridi)
        , (30, generateEasyGismuPlacesExercise dictionary vocabulary displayVariantBridi)
        ]
    where
        vocabulary = basicVocabulary dictionary
