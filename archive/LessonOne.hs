{-# LANGUAGE OverloadedStrings #-}
module Lessons.LessonOne where

import Core
import Lessons.Exercises
import Number (lojbanToNumber, numberToSimpleLojban)
import Util (chooseItem, chooseItemUniformly, chooseItemsUniformly, combineFunctions, combineFunctionsUniformly, stripRight, replace)
import Text.Read (readMaybe)
import System.Random (StdGen, random)
import Data.List.Utils (uniq)
import qualified Data.Text as T
import qualified Data.Map as M

-- Frequently use translations such as "The gift is beautiful" (they are harder versions of contextualized gismu place exercises, and I can control which places to ask about)
--   Maybe always create a new variable for exercises of this type, and do automatic generation for them (it should be simple)
--   Difficulties: easy if it involves x1 ("lo mlatu"), hard otherwise ("lo se dunda")
--   But I'm not very interested in gismu in this course -- this will be more useful for vocabulary courses
--   What are you thinking?
--   What did you say?
--   I agree/disagree

-- Lesson 1.1
vocabulary11 :: Dictionary -> Vocabulary
vocabulary11 dictionary = buildVocabulary dictionary
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
        ("pointable", ["ti"])
    ]

lesson11 :: Dictionary -> StdGen -> Exercise
lesson11 dictionary =
    combineFunctions
        [ (4, generateBridiJufraExercise vocabulary)
        , (4, generateGismuMeaningExercise vocabulary)
        , (2, generateBasicGismuPlacesExercise vocabulary)
        ]
    where
        vocabulary = vocabulary11 dictionary

-- Lesson 1.2
