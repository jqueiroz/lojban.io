{-# LANGUAGE OverloadedStrings #-}

module Courses.Util.Sentences
( generateNonbridi
, generateSimpleBridi
, generatePropertyBridi
, generateRelationBridi
, generateActionBridi
, simplifyCanonicalAnswer
) where

import Core
import Courses.Util.Vocabulary
import Language.Lojban.Core
import Language.Lojban.Refinement (simplifyTerminatorsInSentence)
import Util (filterSnd, filterOutWord, filterOutWords, chooseItem, combineFunctions)
import System.Random (StdGen)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Map as M

-- * Terminator ellisis
-- | Decorates an exercise so that 'simplifyTerminatorsInSentence' is applied to its canonical answer.
simplifyCanonicalAnswer :: Exercise -> Exercise
simplifyCanonicalAnswer (TypingExercise title sentences validate canonicalAnswer) = TypingExercise title sentences validate (simplifyTerminatorsInSentence canonicalAnswer)
simplifyCanonicalAnswer x = x

-- * Sentence generators
generateNonbridi :: Vocabulary -> StdGen -> (T.Text, StdGen)
generateNonbridi vocabulary r0 = chooseItem r0 . concatMap (getVocabularySumti vocabulary) $
    ["genericPersons", "semiGenericPersons", "animals", "genericPointable", "places", "subjects"]

generateSimpleBridi :: Vocabulary -> StdGen -> (SimpleBridi, StdGen)
generateSimpleBridi vocabulary = combineFunctions
    [ (weight properties, generatePropertyBridi vocabulary)
    , (weight relations, generateRelationBridi vocabulary)
    , (weight actions, generateActionBridi vocabulary)
    ]
    where
        weight = sum . map fst
        properties = getVocabularySelbri vocabulary "properties"
        relations = getVocabularySelbri vocabulary "relations"
        actions = getVocabularySelbri vocabulary "actions"

generatePropertyBridi :: Vocabulary -> StdGen -> (SimpleBridi, StdGen)
generatePropertyBridi vocabulary r0 = (SimpleBridi False property [object] [], r2) where
    (property, r1) = chooseItem r0 properties
    (object, r2) = chooseItem r1 . filterOutWord property . retrievePropertyObjects $ property
    -- Vocabulary
    properties = getVocabularySelbri vocabulary "properties"
    genericPersons = getVocabularySumti vocabulary "genericPersons"
    semiGenericPersons = getVocabularySumti vocabulary "semiGenericPersons"
    genericPointable = getVocabularySumti vocabulary "genericPointable"
    places = getVocabularySumti vocabulary "places"
    subjects = getVocabularySumti vocabulary "subjects"
    -- Properties
    propertyObjects = M.fromList
        [ ("prenu", genericPersons ++ semiGenericPersons)
        , ("melbi", genericPersons ++ semiGenericPersons ++ genericPointable ++ places)
        , ("sutra", genericPersons ++ semiGenericPersons ++ genericPointable)
        , ("zdani", genericPointable)
        , ("mlatu", genericPointable)
        , ("gerku", genericPointable)
        , ("pelxu", genericPointable)
        , ("plise", genericPointable)
        ]
    retrievePropertyObjects property =
        let objects = M.findWithDefault [] property propertyObjects
        in if null objects
            then error $ "No property objects are available for '" ++ (T.unpack property) ++ "'"
            else objects

generateRelationBridi :: Vocabulary -> StdGen -> (SimpleBridi, StdGen)
generateRelationBridi vocabulary r0 = (SimpleBridi False relation objects [], r2) where
    (relation, r1) = chooseItem r0 relations
    (objects, r2) = (retrieveRelationObjectsGenerator relation) r1
    -- Vocabulary
    relations = filterOutWord "gleki" $ getVocabularySelbri vocabulary "relations"
    genericPersons = getVocabularySumti vocabulary "genericPersons"
    semiGenericPersons = getVocabularySumti vocabulary "semiGenericPersons"
    animals = getVocabularySumti vocabulary "animals"
    genericPointable = getVocabularySumti vocabulary "genericPointable"
    places = getVocabularySumti vocabulary "places"
    subjects = getVocabularySumti vocabulary "subjects"
    -- Generators
    relationObjectsGenerators :: M.Map T.Text (StdGen -> ([T.Text], StdGen))
    relationObjectsGenerators = M.fromList
        [ ("nelci", \r0 ->
            let
                persons = filterOutWord "nelci" $ genericPersons ++ semiGenericPersons
                (x1, r1) = chooseItem r0 persons
                (x2, r2) = chooseItem r1 $ filterSnd (/= x1) (persons++animals)
            in ([x1, x2], r2))
        , ("pendo", \r0 ->
            let
                persons = filterOutWord "pendo" $ genericPersons ++ semiGenericPersons
                (x1, r1) = chooseItem r0 (persons++animals)
                (x2, r2) = chooseItem r1 $ filterSnd (/= x1) persons
            in ([x1, x2], r2))
        ]
    retrieveRelationObjectsGenerator relation = fromMaybe
        (error $ "No relation objects generator are available for '" ++ (T.unpack relation) ++ "'")
        (M.lookup relation relationObjectsGenerators)

generateActionBridi :: Vocabulary -> StdGen -> (SimpleBridi, StdGen)
generateActionBridi vocabulary r0 = (SimpleBridi False action objects [], r2) where
    (action, r1) = chooseItem r0 actions
    (objects, r2) = (actionObjectsGenerators M.! action) r1
    -- Vocabulary
    actions = filterOutWords ["nupre", "cusku", "djuno"] $ getVocabularySelbri vocabulary "actions"
    genericPersons = getVocabularySumti vocabulary "genericPersons"
    semiGenericPersons = getVocabularySumti vocabulary "semiGenericPersons"
    genericPointable = getVocabularySumti vocabulary "genericPointable"
    places = getVocabularySumti vocabulary "places"
    animals = getVocabularySumti vocabulary "animals"
    subjects = getVocabularySumti vocabulary "subjects"
    aliments = getVocabularySumti vocabulary "aliments"
    -- Generators
    actionObjectsGenerators :: M.Map T.Text (StdGen -> ([T.Text], StdGen))
    actionObjectsGenerators = M.fromList
        [ ("tavla", \r0 ->
            let
                persons = filterOutWord "tavla" $ genericPersons ++ semiGenericPersons
                (speaker, r1) = chooseItem r0 persons
                (listener, r2) = chooseItem r1 $ filterSnd (/= speaker) persons
                availableSubjects = filterSnd (/= speaker) . filterSnd (/= listener) $ subjects
            in
                if null availableSubjects then
                    ([speaker, listener], r2)
                else
                    let  (subject, r3) = chooseItem r2 availableSubjects
                    in ([speaker, listener, subject], r3))
        , ("dunda", \r0 ->
            let
                persons = filterOutWord "dunda" $ genericPersons ++ semiGenericPersons
                (donor, r1) = chooseItem r0 persons
                (gift, r2) = chooseItem r1 (genericPointable++animals)
                (receiver, r3) = chooseItem r2 (filterSnd (/= donor) persons)
            in
                ([donor, gift, receiver], r3))
        , ("ctuca", \r0 ->
            --TODO: complete this bridi with more sumti when they are available
            let
                persons = filterOutWord "ctuca" $ genericPersons ++ semiGenericPersons
                (instructor, r1) = chooseItem r0 persons
                (audience, r2) = chooseItem r1 (filterSnd (/= instructor) persons)
            in
                ([instructor, audience], r1))
        , ("citka", \r0 ->
            let
                persons = filterOutWord "citka" $ genericPersons ++ semiGenericPersons
                (subject, r1) = chooseItem r0 (persons++animals)
                (aliment, r2) = chooseItem r1 (aliments)
            in
                ([subject, aliment], r2))
        , ("ciska", \r0 ->
            --TODO: implement more sumti places
            let
                persons = filterOutWord "ciska" $ genericPersons ++ semiGenericPersons
                (writer, r1) = chooseItem r0 persons
                (what, r2) = chooseItem r1 genericPointable
            in
                ([writer, what], r2))
        , ("klama", \r0 ->
            let
                persons = filterOutWord "klama" $ genericPersons ++ semiGenericPersons
                (actor, r1) = chooseItem r0 persons
                (destination, r2) = chooseItem r1 places
            in
                ([actor, destination], r2))
        ]
    retrieveActionObjectsGenerator action = fromMaybe
        (error $ "No action objects generator are available for '" ++ (T.unpack action) ++ "'")
        (M.lookup action actionObjectsGenerators)

-- TODO: write tests
-- Hard tests: "lo se se prenu ku", "mi tavla fa fi do"
