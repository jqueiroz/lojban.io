{-# LANGUAGE OverloadedStrings #-}
module Lessons.Grammar.Lesson where

import Core
import Lessons.Grammar.Vocabulary
import Lessons.Grammar.Sentences
import Lessons.Grammar.Exercises
import Util (replace, stripRight, chooseItem, chooseItemUniformly, chooseItemsUniformly, combineFunctions, combineFunctionsUniformly)
import System.Random (StdGen)
import qualified Data.Text as T
import qualified Data.Map as M

import Control.Applicative ((<$>), (<*>))
import Dictionary (loadDictionary)

-- Sentence displayers
displayTrivialBridi :: SimpleBridi -> StdGen  -> (T.Text, StdGen)
displayTrivialBridi (SimpleBridi selbri sumti) r0 = (T.unwords . replace "" "zo'e" . stripRight "" $ sumti' ++ [selbri] ++ sumti'', r0) where
    (sumti', sumti'') = splitAt 1 sumti

displaySimpleBridi :: SimpleBridi -> StdGen -> (T.Text, StdGen)
displaySimpleBridi (SimpleBridi selbri sumti) r0 = (T.unwords $ sumtiBefore ++ [selbri] ++ sumtiAfter, r1) where
    sumti' = replace "" "zo'e" . stripRight "" $ sumti
    (beforeCount, r1) = chooseItemUniformly r0 [1..length sumti']
    (sumtiBefore, sumtiAfter) = splitAt beforeCount sumti'

-- Vocabulary
trivialVocabulary :: Dictionary -> Vocabulary
trivialVocabulary dictionary = buildVocabulary dictionary
    -- Gismu
    ["tavla", "dunda", "prenu", "melbi", "zdani", "mlatu", "gerku", "nelci"]
    -- Cmavo
    ["zo'e", "mi", "do", "ti"]
    -- Cmevla
    []
    -- Selbri
    [
        ("actions", ["tavla", "dunda"]),
        ("properties", ["prenu", "melbi", "zdani", "mlatu", "gerku"]),
        ("relations", ["nelci"])
    ]
    -- Sumti
    [
        ("persons", ["mi", "do"]),
        ("pointable", ["ti"]),
        ("subjects", ["ti"])
    ]

basicVocabulary :: Dictionary -> Vocabulary
basicVocabulary dictionary = buildVocabulary dictionary
    -- Gismu
    ["tavla", "dunda", "klama", "prenu", "melbi", "sutra", "zdani", "mlatu", "gerku", "nelci"]
    -- Cmavo
    ["zo'e", "lo", "mi", "do", "ti"]
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
        ("persons", ["mi", "do", "lo prenu ku"]),
        ("animals", ["lo mlatu ku", "lo gerku ku"]),
        ("pointable", ["ti"]),
        ("places", ["lo zdani ku"]),
        ("subjects", ["lo zdani ku", "lo mlatu ku", "lo gerku ku"])
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

-- Lessons
-- TODO: variant bridi structure
lesson1 :: Dictionary -> StdGen -> Exercise
lesson1 dictionary =
    combineFunctions
        [ (15, generateBridiJufraExercise vocabulary displayTrivialBridi)
        , (20, generateSelbriIdentificationExercise vocabulary displayTrivialBridi)
        , (30, generateEasyGismuPlacesExercise dictionary vocabulary displayTrivialBridi)
        , (20, generateGrammaticalClassExercise vocabulary)
        ]
    where
        vocabulary = trivialVocabulary dictionary

lesson2 :: Dictionary -> StdGen -> Exercise
lesson2 dictionary =
    combineFunctions
        [ (15, generateBridiJufraExercise vocabulary displaySimpleBridi)
        , (20, generateSelbriIdentificationExercise vocabulary displaySimpleBridi)
        , (30, generateEasyGismuPlacesExercise dictionary vocabulary displaySimpleBridi)
        , (20, generateGrammaticalClassExercise vocabulary)
        ]
    where
        vocabulary = basicVocabulary dictionary
