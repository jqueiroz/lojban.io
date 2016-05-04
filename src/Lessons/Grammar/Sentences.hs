{-# LANGUAGE OverloadedStrings #-}

module Lessons.Grammar.Sentences
( SimpleBridi
, SimpleBridiDisplayer
, SentenceCannonicalizer
, simpleBridiSelbri
, simpleBridiSumti
, displaySimpleBridi
, displayVariantBridi
, displayReorderedVariantBridi
, basicSentenceCannonicalizer
, generateNonbridi
, generateSimpleBridi
, generatePropertyBridi
, generateRelationBridi
, generateActionBridi
) where

import Core
import Lessons.Grammar.Vocabulary
import Util (replace, stripRight, chooseItem, chooseItemUniformly, chooseItemsUniformly, combineFunctions, combineFunctionsUniformly)
import Control.Exception (assert)
import System.Random (StdGen, mkStdGen)
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Language.Lojban.Parser.ZasniGerna as ZG

data SimpleBridi = SimpleBridi
    { simpleBridiSelbri :: T.Text
    , simpleBridiSumti :: [T.Text]
    } deriving (Show)

swapSimpleBridiArguments "se" (SimpleBridi selbri (a:b:cs)) = SimpleBridi selbri (b:a:cs)
swapSimpleBridiArguments "se" (SimpleBridi selbri (a:[])) = SimpleBridi selbri ("":[a])
swapSimpleBridiArguments "se" (SimpleBridi selbri []) = SimpleBridi selbri []
swapSimpleBridiArguments "te" (SimpleBridi selbri (a:b:c:ds)) = SimpleBridi selbri (c:b:a:ds)
swapSimpleBridiArguments "te" (SimpleBridi selbri (a:b:[])) = SimpleBridi selbri ("":b:[a])
swapSimpleBridiArguments "te" (SimpleBridi selbri (a:[])) = SimpleBridi selbri ("":"":[a])
swapSimpleBridiArguments "te" (SimpleBridi selbri []) = SimpleBridi selbri []
swapSimpleBridiArguments "ve" (SimpleBridi selbri (a:b:c:d:es)) = SimpleBridi selbri (d:b:c:a:es)
swapSimpleBridiArguments "ve" (SimpleBridi selbri (a:b:c:[])) = SimpleBridi selbri ("":b:c:[a])
swapSimpleBridiArguments "ve" (SimpleBridi selbri (a:b:[])) = SimpleBridi selbri ("":b:"":[a])
swapSimpleBridiArguments "ve" (SimpleBridi selbri (a:[])) = SimpleBridi selbri ("":"":"":[a])
swapSimpleBridiArguments "ve" (SimpleBridi selbri []) = SimpleBridi selbri []
swapSimpleBridiArguments "xe" (SimpleBridi selbri (a:b:c:d:e:fs)) = SimpleBridi selbri (e:b:c:d:a:fs)
swapSimpleBridiArguments "xe" (SimpleBridi selbri (a:b:c:d:[])) = SimpleBridi selbri ("":b:c:d:[a])
swapSimpleBridiArguments "xe" (SimpleBridi selbri (a:b:c:[])) = SimpleBridi selbri ("":b:c:"":[a])
swapSimpleBridiArguments "xe" (SimpleBridi selbri (a:b:[])) = SimpleBridi selbri ("":b:"":"":[a])
swapSimpleBridiArguments "xe" (SimpleBridi selbri (a:[])) = SimpleBridi selbri ("":"":"":"":[a])
swapSimpleBridiArguments "xe" (SimpleBridi selbri []) = SimpleBridi selbri []

swapArguments :: T.Text -> [T.Text] -> [T.Text]
swapArguments "se" (a:b:cs) = (b:a:cs)
swapArguments "te" (a:b:c:ds) = (c:b:a:ds)
swapArguments "ve" (a:b:c:d:es) = (d:b:c:a:es)
swapArguments "xe" (a:b:c:d:e:fs) = (e:b:c:d:a:fs)

------------------------- ------------------------ Sentence displayers
-- TODO: other display modes (place some sumti before the selbri, introduce fa/fe/fi/fo/fu, introduce se/te/ve/..., etc.)
-- TODO: create functions that use the variant bridi structure (eg. x1 x2 selbri x3 ...), different ways of skipping (fa/fe/... vs se/te/...) according to some randomness, perhaps even unnecessarily sometimes (there should be parameters to control the preferences between fa/fe/... and se/te/... and the level of unnecessary use of fa/fe/..., variant bridi structure, etc.)
type SimpleBridiDisplayer = StdGen -> SimpleBridi -> (T.Text, StdGen)

buildSentenceDisplayer :: (StdGen -> SimpleBridi -> ([T.Text], StdGen)) -> SimpleBridiDisplayer
buildSentenceDisplayer sentenceDisplayer r0 simpleBridi = (T.unwords $ replace "" "zo'e" sentence, r1) where
    (sentence, r1) = sentenceDisplayer r0 simpleBridi

-- Ellisis occurs in the first place and in the last places
-- All other missing places are filled with "zo'e"
displaySimpleBridi :: StdGen -> SimpleBridi -> (T.Text, StdGen)
displaySimpleBridi = buildSentenceDisplayer $ \r0 (SimpleBridi selbri sumti) ->
    let
        (sumtiHead, sumtiTail) = splitAt 1 sumti
        sentence = (if sumtiHead == [""] then [] else sumtiHead) ++ [selbri] ++ (stripRight "" sumtiTail)
    in
        (sentence, r0)

-- A random number of places is displayed before the selbri
-- (Except if the first place is empty, in which case this function behaves as displaySimpleBridi)
displayVariantBridi :: StdGen -> SimpleBridi -> (T.Text, StdGen)
displayVariantBridi = buildSentenceDisplayer $ \r0 (SimpleBridi selbri sumti) ->
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

-- A single swap is made using se/te/ve/xe
-- (Except if the first place is empty or there are fewer than two places, in which case this function behaevs as displaySimpleBridi)
displayReorderedVariantBridi :: StdGen -> SimpleBridi -> (T.Text, StdGen)
displayReorderedVariantBridi r0 (bridi@(SimpleBridi selbri sumti))
    | length sumti <= 1 = displayVariantBridi r0 bridi
    | head sumti == ""  = displayVariantBridi r0 bridi
    | otherwise         = displayReorderedVariantBridi' r0 bridi

displayReorderedVariantBridi' :: StdGen -> SimpleBridi -> (T.Text, StdGen)
displayReorderedVariantBridi' = buildSentenceDisplayer $ \r0 (SimpleBridi selbri sumti) ->
    let
        particles = take (length sumti - 1) ["se", "te", "ve", "xe"]
        (particle, r1) = chooseItemUniformly r0 particles
        sumti' = swapArguments particle sumti
        sentence = head sumti' : particle : selbri : tail sumti'
    in
        assert (length sumti >= 2 && head sumti /= "") $ (sentence, r1)

------------------------- ----------------------- Sentence cannonicalizers
--TODO: check whether se/te/ve/xe are left-associative or right-associative

cannonicalizeArgument :: ZG.Text -> Either String T.Text
cannonicalizeArgument (ZG.LE (ZG.Init i) _ _ (ZG.BRIVLA b) _) = Right $ T.pack (i ++ " " ++ b ++ " ku")
cannonicalizeArgument (ZG.KOhA "zo'e") = Right $ ""
cannonicalizeArgument (ZG.KOhA k) = Right $ T.pack k
cannonicalizeArgument _ = Left "unrecognized pattern in function cannonicalizeArgument"

cannonicalizeArguments :: [ZG.Text] -> Either String [T.Text]
cannonicalizeArguments [] = Right []
cannonicalizeArguments (x:xs) = do
    x' <- cannonicalizeArgument x
    xs' <- cannonicalizeArguments xs
    return $ x':xs'

cannonicalizeText :: ZG.Text -> Either String SimpleBridi
cannonicalizeText (ZG.BridiTail (ZG.BRIVLA selbri) (ZG.Terms sumti _)) =
    SimpleBridi (T.pack selbri) <$> (("":) <$> (cannonicalizeArguments sumti))
cannonicalizeText (ZG.BridiTail (ZG.Prefix (ZG.SE se) brivla) (ZG.Terms sumti x)) =
    swapSimpleBridiArguments se <$> cannonicalizeText (ZG.BridiTail brivla (ZG.Terms sumti x))
cannonicalizeText (ZG.Bridi (ZG.Terms sumti1 _) (ZG.BridiTail (ZG.BRIVLA selbri) (ZG.Terms sumti2 _))) =
    SimpleBridi (T.pack selbri) <$> (cannonicalizeArguments $ sumti1 ++ sumti2)
cannonicalizeText (ZG.Bridi (ZG.Terms sumti1 _) (ZG.BRIVLA selbri)) =
    SimpleBridi (T.pack selbri) <$> (cannonicalizeArguments $ sumti1)
cannonicalizeText (ZG.Bridi (ZG.Terms sumti1 x) (ZG.Prefix (ZG.SE se) bridiTail)) =
    swapSimpleBridiArguments se <$> cannonicalizeText (ZG.Bridi (ZG.Terms sumti1 x) bridiTail)
cannonicalizeText (ZG.Bridi (ZG.Terms sumti1 x) (ZG.BridiTail (ZG.Prefix (ZG.SE se) bridiTail) (ZG.Terms sumti2 y))) =
    swapSimpleBridiArguments se <$> cannonicalizeText (ZG.Bridi (ZG.Terms sumti1 x) (ZG.BridiTail bridiTail (ZG.Terms sumti2 y)))
cannonicalizeText (ZG.BRIVLA selbri) =
    Right $ SimpleBridi (T.pack selbri) []
cannonicalizeText (ZG.Prefix (ZG.SE se) x) =
    swapSimpleBridiArguments se <$> cannonicalizeText x
cannonicalizeText _ = Left "unrecognized pattern in function cannonicalizeText"

type SentenceCannonicalizer = T.Text -> Either String T.Text
basicSentenceCannonicalizer :: T.Text -> Either String T.Text
basicSentenceCannonicalizer sentence = do
    (_, x, _) <- ZG.parse (T.unpack sentence)
    y <- cannonicalizeText x
    return . fst $ displaySimpleBridi (mkStdGen 42) y

------------------------- ----------------------- Sentence generators
generateNonbridi :: Vocabulary -> StdGen -> (T.Text, StdGen)
generateNonbridi vocabulary r0 = chooseItemUniformly r0 . concat . map (getVocabularySumti vocabulary) $ ["genericPersons", "genericPointable", "places", "subjects"]

generateSimpleBridi :: Vocabulary -> StdGen -> (SimpleBridi, StdGen)
generateSimpleBridi vocabulary = combineFunctionsUniformly [generatePropertyBridi vocabulary, generateRelationBridi vocabulary, generateActionBridi vocabulary]

generatePropertyBridi :: Vocabulary -> StdGen -> (SimpleBridi, StdGen)
generatePropertyBridi vocabulary r0 = (SimpleBridi property [object], r2) where
    (property, r1) = chooseItemUniformly r0 properties
    (object, r2) = chooseItemUniformly r1 $ retrievePropertyObjects property
    -- Vocabulary
    properties = getVocabularySelbri vocabulary "properties"
    genericPersons = getVocabularySumti vocabulary "genericPersons"
    genericPointable = getVocabularySumti vocabulary "genericPointable"
    places = getVocabularySumti vocabulary "places"
    subjects = getVocabularySumti vocabulary "subjects"
    -- Properties
    propertyObjects = M.fromList
        [ ("prenu", genericPersons)
        , ("melbi", genericPersons ++ genericPointable ++ places)
        , ("sutra", genericPersons ++ genericPointable)
        , ("zdani", genericPointable)
        , ("mlatu", genericPointable)
        , ("gerku", genericPointable)
        , ("pelxu", genericPointable)
        , ("plise", genericPointable)
        ]
    retrievePropertyObjects property =
        let objects = M.findWithDefault [] property propertyObjects
        in if objects == []
            then error $ "No property objects are available for '" ++ (T.unpack property) ++ "'"
            else objects

generateRelationBridi :: Vocabulary -> StdGen -> (SimpleBridi, StdGen)
generateRelationBridi vocabulary r0 = (SimpleBridi relation objects, r2) where
    (relation, r1) = chooseItemUniformly r0 relations
    (objects, r2) = (retrieveRelationObjectsGenerator relation) r1
    -- Vocabulary
    relations = getVocabularySelbri vocabulary "relations"
    genericPersons = getVocabularySumti vocabulary "genericPersons"
    animals = getVocabularySumti vocabulary "animals"
    genericPointable = getVocabularySumti vocabulary "genericPointable"
    places = getVocabularySumti vocabulary "places"
    subjects = getVocabularySumti vocabulary "subjects"
    -- Generators
    relationObjectsGenerators :: M.Map T.Text (StdGen -> ([T.Text], StdGen))
    relationObjectsGenerators = M.fromList
        [ ("nelci", \r0 ->
            let
                (x1, r1) = chooseItemUniformly r0 genericPersons
                (x2, r2) = chooseItemUniformly r1 $ filter (/= x1) (genericPersons++animals)
            in ([x1, x2], r2))
        , ("pendo", \ro ->
            let
                (x1, r1) = chooseItemUniformly r0 (genericPersons++animals)
                (x2, r2) = chooseItemUniformly r1 $ filter (/= x1) genericPersons
            in ([x1, x2], r2))
        ]
    retrieveRelationObjectsGenerator relation =
        case M.lookup relation relationObjectsGenerators of
            Just x -> x
            Nothing -> error $ "No relation objects generator are available for '" ++ (T.unpack relation) ++ "'"

generateActionBridi :: Vocabulary -> StdGen -> (SimpleBridi, StdGen)
generateActionBridi vocabulary r0 = (SimpleBridi action objects, r2) where
    (action, r1) = chooseItemUniformly r0 actions
    (objects, r2) = (actionObjectsGenerators M.! action) r1
    -- Vocabulary
    actions = getVocabularySelbri vocabulary "actions"
    genericPersons = getVocabularySumti vocabulary "genericPersons"
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
                (speaker, r1) = chooseItemUniformly r0 genericPersons
                (listener, r2) = chooseItemUniformly r1 $ filter (/= speaker) genericPersons
                (subject, r3) = if null subjects then ("", r2) else chooseItemUniformly r2 subjects
            in
                ([speaker, listener, subject], r3))
        , ("dunda", \r0 ->
            let
                (donor, r1) = chooseItemUniformly r0 genericPersons
                (gift, r2) = chooseItemUniformly r1 (genericPointable++animals)
                (receiver, r3) = chooseItemUniformly r2 (filter (/= donor) genericPersons)
            in
                ([donor, gift, receiver], r3))
        , ("citka", \r0 ->
            let
                (subject, r1) = chooseItemUniformly r0 (genericPersons++animals)
                (aliment, r2) = chooseItemUniformly r1 (aliments)
            in
                ([subject, aliment], r2))
        , ("klama", \r0 ->
            let
                (actor, r1) = chooseItemUniformly r0 genericPersons
                (destination, r2) = chooseItemUniformly r1 places
            in
                ([actor, destination], r2))
        ]
    retrieveActionObjectsGenerator action =
        case M.lookup action actionObjectsGenerators of
            Just x -> x
            Nothing -> error $ "No action objects generator are available for '" ++ (T.unpack action) ++ "'"
