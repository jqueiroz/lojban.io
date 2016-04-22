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

-------- Sentence displayers
buildSentenceDisplayer :: (SimpleBridi -> StdGen -> ([T.Text], StdGen)) -> (SimpleBridi -> StdGen -> (T.Text, StdGen))
buildSentenceDisplayer sentenceDisplayer simpleBridi r0 = (T.unwords $ replace "" "zo'e" sentence, r1) where
    (sentence, r1) = sentenceDisplayer simpleBridi r0

-- Ellisis occurs in the first place and in the last places
-- All other missing places are filled with "zo'e"
displaySimpleBridi :: SimpleBridi -> StdGen -> (T.Text, StdGen)
displaySimpleBridi = buildSentenceDisplayer $ \(SimpleBridi selbri sumti) r0 ->
    let
        (sumtiHead, sumtiTail) = splitAt 1 sumti
        sentence = (if sumtiHead == [""] then [] else sumtiHead) ++ [selbri] ++ sumtiTail
    in
        (sentence, r0)

-- A random number of places is displayed before the selbri
-- (Except if the first place is missing, in which case this function behaves as displaySimpleBridi)
displayVariantBridi :: SimpleBridi -> StdGen -> (T.Text, StdGen)
displayVariantBridi = buildSentenceDisplayer $ \(SimpleBridi selbri sumti) r0 ->
    let
        (sumtiHead, sumtiTail) = splitAt 1 sumti
    in
        if sumtiHead == [""] then
            (selbri : sumtiTail, r0)
        else
            let
                (beforeCount, r1) = chooseItemUniformly r0 [1..length sumti]
                (sumtiBefore, sumtiAfter) = splitAt beforeCount sumti
            in
                (sumtiBefore ++ [selbri] ++ sumtiAfter, r1)

-------- Vocabulary
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
