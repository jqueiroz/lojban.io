{-# LANGUAGE OverloadedStrings #-}

module Courses.Util.Sentences
( SimpleBridi
, SimpleBridiDisplayer
, SentenceCannonicalizer
, simpleBridiSelbri
, simpleBridiSumti
, displayStandardSimpleBridi
, displayVariantSimpleBridi
, displayReorderedStandardSimpleBridi
, basicSentenceCannonicalizer
, generateNonbridi
, generateSimpleBridi
, generatePropertyBridi
, generateRelationBridi
, generateActionBridi
, removeElidableTerminators
) where

import Core
import Courses.Util.Vocabulary
import Util (replace, stripRight, filterOutWord, chooseItem, chooseItemUniformly, chooseItemsUniformly, combineFunctions, combineFunctionsUniformly)
import Control.Exception (assert)
import System.Random (StdGen, mkStdGen)
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Language.Lojban.Parser.ZasniGerna as ZG

data SimpleBridi = SimpleBridi
    { simpleBridiSelbri :: T.Text
    , simpleBridiSumti :: [T.Text]
    } deriving (Show)

-- The following function keeps trailing empty places, if present
swapSimpleBridiArguments :: String -> SimpleBridi -> SimpleBridi
swapSimpleBridiArguments particle (SimpleBridi selbri sumti) = SimpleBridi selbri sumti''' where
    sumti' = sumti ++ replicate 5 (T.pack "///")
    sumti'' = swapArguments particle sumti'
    sumti''' = replace (T.pack "///") (T.pack "") . stripRight "///" $ sumti''

swapArguments :: String -> [T.Text] -> [T.Text]
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

-- The bridi is displayed in standard order ([x1] selbri x2 x3 x4 x5)
--   * Ellisis occurs in the first place and in the last places
--   * All other missing places are filled with "zo'e"
displayStandardSimpleBridi :: StdGen -> SimpleBridi -> (T.Text, StdGen)
displayStandardSimpleBridi = buildSentenceDisplayer $ \r0 (SimpleBridi selbri sumti) ->
    let
        (sumtiHead, sumtiTail) = splitAt 1 sumti
        sentence = (if sumtiHead == [""] then [] else sumtiHead) ++ [selbri] ++ (stripRight "" sumtiTail)
    in
        (sentence, r0)

-- The bridi is displayed with a random number of places before the selbri
--   * Exception: if the first place is empty, then this function behaves as displayStandardSimpleBridi
--   * Ellisis occurs in the last places
--   * All other missing places are filled with "zo'e"
displayVariantSimpleBridi :: StdGen -> SimpleBridi -> (T.Text, StdGen)
displayVariantSimpleBridi = buildSentenceDisplayer $ \r0 (SimpleBridi selbri sumti) ->
    let
        (sumtiHead, sumtiTail) = splitAt 1 sumti
    in
        if sumtiHead == [""] then
            (selbri : (stripRight "" sumtiTail), r0)
        else
            let
                (beforeCount, r1) = chooseItemUniformly r0 [1..length sumti]
                (sumtiBefore, sumtiAfter) = splitAt beforeCount sumti
            in
                (sumtiBefore ++ [selbri] ++ sumtiAfter, r1)

-- The bridi is displayed as in "displayStandardSimpleBridi", but if the x1 is missing then its position is used to hold the last place
--   * Exception: if there are more than five sumti places, then "displayStandardSimpleBridi" is used after all
displayPossiblyReorderedStandardSimpleBridi :: StdGen -> SimpleBridi -> (T.Text, StdGen)
displayPossiblyReorderedStandardSimpleBridi r0 bridi
    | length sumti <= 1 = displayStandardSimpleBridi r0 bridi
    | length sumti >= 6 = displayStandardSimpleBridi r0 bridi
    | head sumti == ""  = displayPossiblyReorderedStandardSimpleBridi' r0 bridi
    | otherwise         = displayStandardSimpleBridi r0 bridi
    where sumti = stripRight "" $ simpleBridiSumti bridi

displayPossiblyReorderedStandardSimpleBridi' :: StdGen -> SimpleBridi -> (T.Text, StdGen)
displayPossiblyReorderedStandardSimpleBridi' = buildSentenceDisplayer $ \r0 (SimpleBridi selbri sumti) ->
    let
        particle = ["se", "te", "ve", "xe"] !! (length sumti - 2)
        sumti' = stripRight "" $ swapArguments particle sumti
        sentence = head sumti' : (T.pack particle) : selbri : (tail sumti')
    in
        assert (length sumti >= 2 && length sumti <= 5 && head sumti == "" && last sumti /= "") $ (sentence, r0)

-- The bridi is displayed with a single place swap
--   * Exception: if the first place is empty or there are fewer than two places, then this function behaves as displayStandardSimpleBridi
displayReorderedStandardSimpleBridi :: StdGen -> SimpleBridi -> (T.Text, StdGen)
displayReorderedStandardSimpleBridi r0 bridi
    | length sumti <= 1 = displayStandardSimpleBridi r0 bridi
    | head sumti == ""  = displayPossiblyReorderedStandardSimpleBridi r0 bridi
    | otherwise         = displayReorderedStandardSimpleBridi' r0 bridi
    where sumti = stripRight "" $ simpleBridiSumti bridi

displayReorderedStandardSimpleBridi' :: StdGen -> SimpleBridi -> (T.Text, StdGen)
displayReorderedStandardSimpleBridi' = buildSentenceDisplayer $ \r0 (SimpleBridi selbri sumti) ->
    let
        particles = take (length sumti - 1) ["se", "te", "ve", "xe"]
        (particle, r1) = chooseItemUniformly r0 particles
        sumti' = swapArguments particle sumti
        sentence = head sumti' : (T.pack particle) : selbri : tail sumti'
    in
        assert (length sumti >= 2 && head sumti /= "" && last sumti /= "") $ (sentence, r1)

------------------------- ----------------------- Terminator ellisis
-- removeElidableTerminators :: ZG.Text -> Either String T.Text
-- removeElidableTerminators (ZG.LE (ZG.Init i) _ _ x _) =
-- removeElidableTerminators (ZG.Terms terms _) =
-- removeElidableTerminators (ZG.BridiTail (ZG.BRIVLA selbri) terms) = selbri `T.append` removeTerminators terms
-- TODO: make this function way more efficient and use the following brute-force version only in unit tests

removeElidableTerminators :: T.Text -> T.Text
removeElidableTerminators t = f [] (T.words t) where
    originalCannonicalization = basicSentenceCannonicalizer t
    f :: [T.Text] -> [T.Text] -> T.Text
    f x [] = T.unwords x
    f x (y:ys) = if basicSentenceCannonicalizer (T.unwords $ x++ys) == originalCannonicalization then f x ys else f (x++[y]) ys

------------------------- ----------------------- Sentence cannonicalizers
--TODO: check whether se/te/ve/xe are left-associative or right-associative
--TODO: create LOTS of unit tests

cannonicalizeArgumentInternally :: ZG.Text -> Either String T.Text
cannonicalizeArgumentInternally (ZG.BRIVLA b) = Right $ T.pack b
cannonicalizeArgumentInternally (ZG.Prefix (ZG.SE se) b) = T.append (T.pack $ se ++ " ") <$> cannonicalizeArgumentInternally b
cannonicalizeArgumentInternally _ = Left "unrecognized pattern in function cannonicalizeArgumentInternally"

cannonicalizeArgument :: ZG.Text -> Either String T.Text
cannonicalizeArgument (ZG.LE (ZG.Init i) _ _ x _) = insertPrefix . insertSuffix <$> cannonicalizeArgumentInternally x where
    insertPrefix = ((T.pack $ i ++ " ") `T.append`)
    insertSuffix = (`T.append` " ku")
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
    return . fst $ displayStandardSimpleBridi (mkStdGen 42) y

------------------------- ----------------------- Sentence generators
generateNonbridi :: Vocabulary -> StdGen -> (T.Text, StdGen)
generateNonbridi vocabulary r0 = chooseItemUniformly r0 . concat . map (getVocabularySumti vocabulary) $
    ["genericPersons", "semiGenericPersons", "animals", "genericPointable", "places", "subjects"]

generateSimpleBridi :: Vocabulary -> StdGen -> (SimpleBridi, StdGen)
generateSimpleBridi vocabulary = combineFunctionsUniformly
    [generatePropertyBridi vocabulary, generateRelationBridi vocabulary, generateActionBridi vocabulary]

generatePropertyBridi :: Vocabulary -> StdGen -> (SimpleBridi, StdGen)
generatePropertyBridi vocabulary r0 = (SimpleBridi property [object], r2) where
    (property, r1) = chooseItemUniformly r0 properties
    (object, r2) = chooseItemUniformly r1 . filterOutWord property . retrievePropertyObjects $ property
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
                (x1, r1) = chooseItemUniformly r0 persons
                (x2, r2) = chooseItemUniformly r1 $ filter (/= x1) (persons++animals)
            in ([x1, x2], r2))
        , ("pendo", \ro ->
            let
                persons = filterOutWord "pendo" $ genericPersons ++ semiGenericPersons
                (x1, r1) = chooseItemUniformly r0 (persons++animals)
                (x2, r2) = chooseItemUniformly r1 $ filter (/= x1) persons
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
                (speaker, r1) = chooseItemUniformly r0 persons
                (listener, r2) = chooseItemUniformly r1 $ filter (/= speaker) persons
                (subject, r3) = if null subjects then ("", r2) else chooseItemUniformly r2 subjects
            in
                ([speaker, listener, subject], r3))
        , ("dunda", \r0 ->
            let
                persons = filterOutWord "dunda" $ genericPersons ++ semiGenericPersons
                (donor, r1) = chooseItemUniformly r0 persons
                (gift, r2) = chooseItemUniformly r1 (genericPointable++animals)
                (receiver, r3) = chooseItemUniformly r2 (filter (/= donor) persons)
            in
                ([donor, gift, receiver], r3))
        , ("ctuca", \r0 ->
            --TODO: complete this bridi with more sumti when they are available
            let
                persons = filterOutWord "ctuca" $ genericPersons ++ semiGenericPersons
                (instructor, r1) = chooseItemUniformly r0 persons
                (audience, r2) = chooseItemUniformly r1 (filter (/= instructor) persons)
            in
                ([instructor, audience], r1))
        , ("citka", \r0 ->
            let
                persons = filterOutWord "citka" $ genericPersons ++ semiGenericPersons
                (subject, r1) = chooseItemUniformly r0 (persons++animals)
                (aliment, r2) = chooseItemUniformly r1 (aliments)
            in
                ([subject, aliment], r2))
        , ("klama", \r0 ->
            let
                persons = filterOutWord "klama" $ genericPersons ++ semiGenericPersons
                (actor, r1) = chooseItemUniformly r0 persons
                (destination, r2) = chooseItemUniformly r1 places
            in
                ([actor, destination], r2))
        ]
    retrieveActionObjectsGenerator action =
        case M.lookup action actionObjectsGenerators of
            Just x -> x
            Nothing -> error $ "No action objects generator are available for '" ++ (T.unpack action) ++ "'"
