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

-- Lesson 1.1
--TODO: there's no zo'e in Lesson 1 exercises
vocabulary11 :: Dictionary -> Vocabulary
vocabulary11 dictionary = buildVocabulary dictionary
    -- Gismu
    ["tavla", "dunda", "klama", "prenu", "melbi", "sutra", "zdani", "mlatu", "gerku", "pelxu", "nelci"]
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
