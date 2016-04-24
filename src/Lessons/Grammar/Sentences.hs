{-# LANGUAGE OverloadedStrings #-}
module Lessons.Grammar.Sentences where

import Core
import Lessons.Grammar.Vocabulary
import Util (replace, chooseItem, chooseItemUniformly, chooseItemsUniformly, combineFunctions, combineFunctionsUniformly)
import System.Random (StdGen)
import qualified Data.Text as T
import qualified Data.Map as M

data SimpleBridi = SimpleBridi
    { simpleBridiSelbri :: T.Text
    , simpleBridiSumti :: [T.Text]
    } deriving (Show)

------------------------- ------------------------ Sentence displayers
-- TODO: other display modes (place some sumti before the selbri, introduce fa/fe/fi/fo/fu, introduce se/te/ve/..., etc.)
-- TODO: create functions that use the variant bridi structure (eg. x1 x2 selbri x3 ...), different ways of skipping (fa/fe/... vs se/te/...) according to some randomness, perhaps even unnecessarily sometimes (there should be parameters to control the preferences between fa/fe/... and se/te/... and the level of unnecessary use of fa/fe/..., variant bridi structure, etc.)
type SimpleBridiDisplayer = SimpleBridi -> StdGen -> (T.Text, StdGen)

buildSentenceDisplayer :: (SimpleBridi -> StdGen -> ([T.Text], StdGen)) -> (SimpleBridi -> StdGen -> (T.Text, StdGen))
buildSentenceDisplayer sentenceDisplayer simpleBridi r0 = (T.unwords $ replace "" "zo'e" sentence, r1) where
    (sentence, r1) = sentenceDisplayer simpleBridi r0

-- Ellisis occurs in the first place and in the last places
-- All other missing places are filled with "zo'e"
displaySimpleBridi :: SimpleBridi -> StdGen -> (T.Text, StdGen)
displaySimpleBridi = buildSentenceDisplayer $ \(SimpleBridi selbri sumti) r0 ->
    let
        (sumtiHead, sumtiTail) = splitAt 1 sumti
        sentence = (if sumtiHead == [""] then [] else sumtiHead) ++ [selbri] ++ (stripRight "" sumtiTail)
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

------------------------- ----------------------- Sentence generators
generateNonbridi :: Vocabulary -> StdGen -> (T.Text, StdGen)
generateNonbridi vocabulary r0 = chooseItemUniformly r0 . concat . map (getVocabularySumti vocabulary) $ ["persons", "pointable", "places", "subjects"]

generatePropertyBridi :: Vocabulary -> StdGen -> (SimpleBridi, StdGen)
generatePropertyBridi vocabulary r0 = (SimpleBridi property [object], r2) where
    (property, r1) = chooseItemUniformly r0 properties
    (object, r2) = chooseItemUniformly r1 $ propertyObjects M.! property
    -- Vocabulary
    properties = getVocabularySelbri vocabulary "properties"
    persons = getVocabularySumti vocabulary "persons"
    pointable = getVocabularySumti vocabulary "pointable"
    places = getVocabularySumti vocabulary "places"
    subjects = getVocabularySumti vocabulary "subjects"
    -- Properties
    propertyObjects = M.fromList
        [ ("prenu", persons)
        , ("melbi", persons ++ pointable ++ places)
        , ("sutra", persons ++ pointable)
        , ("zdani", pointable)
        , ("mlatu", pointable)
        , ("gerku", pointable)
        , ("pelxu", pointable)
        , ("cinri", subjects)
        ]

generateRelationBridi :: Vocabulary -> StdGen -> (SimpleBridi, StdGen)
generateRelationBridi vocabulary r0 = (SimpleBridi relation objects, r2) where
    (relation, r1) = chooseItemUniformly r0 relations
    (objects, r2) = (relationObjectsGenerators M.! relation) r1
    -- Vocabulary
    relations = getVocabularySelbri vocabulary "relations"
    persons = getVocabularySumti vocabulary "persons"
    animals = getVocabularySumti vocabulary "animals"
    pointable = getVocabularySumti vocabulary "pointable"
    places = getVocabularySumti vocabulary "places"
    subjects = getVocabularySumti vocabulary "subjects"
    -- Generators
    relationObjectsGenerators :: M.Map T.Text (StdGen -> ([T.Text], StdGen))
    relationObjectsGenerators = M.fromList
        [ ("nelci", \r0 ->
            let
                (x1, r1) = chooseItemUniformly r0 persons
                (x2, r2) = chooseItemUniformly r1 $ filter (/= x1) (persons++animals)
            in ([x1, x2], r2))
        , ("pendo", \ro ->
            let
                (x1, r1) = chooseItemUniformly r0 (persons++animals)
                (x2, y2) = chooseItemUniformly r1 $ filter (/= x1) persons
            in ([x1, x2], r2))
        ]

generateActionBridi :: Vocabulary -> StdGen -> (SimpleBridi, StdGen)
generateActionBridi vocabulary r0 = (SimpleBridi action objects, r2) where
    (action, r1) = chooseItemUniformly r0 actions
    (objects, r2) = (actionObjectsGenerators M.! action) r1
    -- Vocabulary
    actions = getVocabularySelbri vocabulary "actions"
    persons = getVocabularySumti vocabulary "persons"
    pointable = getVocabularySumti vocabulary "pointable"
    places = getVocabularySumti vocabulary "places"
    animals = getVocabularySumti vocabulary "animals"
    subjects = getVocabularySumti vocabulary "subjects"
    aliments = getVocabularySumti vocabulary "aliments"
    -- Generators
    actionObjectsGenerators :: M.Map T.Text (StdGen -> ([T.Text], StdGen))
    actionObjectsGenerators = M.fromList
        [ ("tavla", \r0 ->
            let
                (speaker, r1) = chooseItemUniformly r0 persons
                (listener, r2) = chooseItemUniformly r1 $ filter (/= speaker) persons
                (subject, r3) = chooseItemUniformly r2 subjects
            in ([speaker, listener, subject], r3))
        , ("dunda", \r0 ->
            let
                (donor, r1) = chooseItemUniformly r0 persons
                (gift, r2) = chooseItemUniformly r1 (pointable++animals)
                (receiver, r3) = chooseItemUniformly r2 (filter (/= donor) persons)
            in
                ([donor, gift, receiver], r3))
        , ("citka", \r0 ->
            let
                (subject, r1) = chooseItemUniformly r0 (persons++animals)
                (aliment, r2) = chooseItemUniformly r1 (aliments)
            in ([subject, aliment], r2))
        , ("klama", \r0 ->
            let
                (actor, r1) = chooseItemUniformly r0 persons
                (destination, r2) = chooseItemUniformly r1 places
            in
                ([actor, destination], r2))
        ]

generateSimpleBridi :: Vocabulary -> StdGen -> (SimpleBridi, StdGen)
generateSimpleBridi vocabulary = combineFunctionsUniformly [generatePropertyBridi vocabulary, generateRelationBridi vocabulary, generateActionBridi vocabulary]
