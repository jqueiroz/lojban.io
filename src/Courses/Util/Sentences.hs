{-# LANGUAGE OverloadedStrings #-}

module Courses.Util.Sentences
( SimpleBridi
, SimpleBridiDisplayer
, SentenceCanonicalizer
, simpleBridiSelbri
, simpleBridiSumti
, displayStandardSimpleBridi
, displayVariantSimpleBridi
, displayReorderedStandardSimpleBridi
, basicSentenceCanonicalizer
, generateNonbridi
, generateSimpleBridi
, generatePropertyBridi
, generateRelationBridi
, generateActionBridi
, removeElidableTerminators
, parse
) where

import Core
import Courses.Util.Vocabulary
import Util (replace, stripRight, filterOutWord, filterOutWords, headOrDefault, isContiguousSequence, chooseItem, chooseItemUniformly, chooseItemsUniformly, combineFunctions, combineFunctionsUniformly)
import Control.Exception (assert)
import Control.Applicative (liftA2)
import Control.Arrow ((***))
import System.Random (StdGen, mkStdGen)
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Language.Lojban.Parser.ZasniGerna as ZG

data SimpleBridi = SimpleBridi
    { simpleBridiXu :: Bool
    , simpleBridiSelbri :: T.Text
    , simpleBridiSumti :: [T.Text]
    } deriving (Show)

-- The following function keeps trailing empty places, if present
swapSimpleBridiArguments :: String -> SimpleBridi -> SimpleBridi
swapSimpleBridiArguments particle (SimpleBridi xu selbri sumti) = SimpleBridi xu selbri sumti''' where
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
-- TODO: use fa/fe/fi/fo/fu if convenient
type SimpleBridiDisplayer = StdGen -> SimpleBridi -> (T.Text, StdGen)

prependXu :: Bool -> ([T.Text], StdGen) -> ([T.Text], StdGen)
prependXu True = ("xu":) *** id
prependXu False = id

buildSentenceDisplayer :: (StdGen -> SimpleBridi -> ([T.Text], StdGen)) -> SimpleBridiDisplayer
buildSentenceDisplayer sentenceDisplayer r0 simpleBridi = (T.unwords $ replace "" "zo'e" sentence, r1) where
    (sentence, r1) = sentenceDisplayer r0 simpleBridi

-- The bridi is displayed in standard order ([x1] selbri x2 x3 x4 x5)
--   * Ellisis occurs in the first place and in the last places
--   * All other missing places are filled with "zo'e"
displayStandardSimpleBridi :: StdGen -> SimpleBridi -> (T.Text, StdGen)
displayStandardSimpleBridi = buildSentenceDisplayer $ \r0 (SimpleBridi xu selbri sumti) ->
    let
        (sumtiHead, sumtiTail) = splitAt 1 sumti
        sentence = (if sumtiHead == [""] then [] else sumtiHead) ++ [selbri] ++ (stripRight "" sumtiTail)
    in
        prependXu xu $
        (sentence, r0)

-- The bridi is displayed with a random number of places before the selbri
--   * Exception: if the first place is empty, then this function behaves as displayStandardSimpleBridi
--   * Ellisis occurs in the last places
--   * All other missing places are filled with "zo'e"
displayVariantSimpleBridi :: StdGen -> SimpleBridi -> (T.Text, StdGen)
displayVariantSimpleBridi = buildSentenceDisplayer $ \r0 (SimpleBridi xu selbri sumti) ->
    let
        (sumtiHead, sumtiTail) = splitAt 1 sumti
    in
        prependXu xu $
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
displayPossiblyReorderedStandardSimpleBridi' = buildSentenceDisplayer $ \r0 (SimpleBridi xu selbri sumti) ->
    let
        particle = ["se", "te", "ve", "xe"] !! (length sumti - 2)
        sumti' = stripRight "" $ swapArguments particle sumti
        sentence = head sumti' : (T.pack particle) : selbri : (tail sumti')
    in
        assert (length sumti >= 2 && length sumti <= 5 && head sumti == "" && last sumti /= "") $
        prependXu xu $
        (sentence, r0)

-- The bridi is displayed with a single place swap
--   * Exception: if the first place is empty or there are fewer than two places, then this function behaves as displayStandardSimpleBridi
displayReorderedStandardSimpleBridi :: StdGen -> SimpleBridi -> (T.Text, StdGen)
displayReorderedStandardSimpleBridi r0 bridi
    | length sumti <= 1 = displayStandardSimpleBridi r0 bridi
    | head sumti == ""  = displayPossiblyReorderedStandardSimpleBridi r0 bridi
    | otherwise         = displayReorderedStandardSimpleBridi' r0 bridi
    where sumti = stripRight "" $ simpleBridiSumti bridi

displayReorderedStandardSimpleBridi' :: StdGen -> SimpleBridi -> (T.Text, StdGen)
displayReorderedStandardSimpleBridi' = buildSentenceDisplayer $ \r0 (SimpleBridi xu selbri sumti) ->
    let
        particles = take (length sumti - 1) ["se", "te", "ve", "xe"]
        (particle, r1) = chooseItemUniformly r0 particles
        sumti' = swapArguments particle sumti
        sentence = head sumti' : (T.pack particle) : selbri : tail sumti'
    in
        assert (length sumti >= 2 && head sumti /= "" && last sumti /= "") $
        prependXu xu $
        (sentence, r1)

------------------------- ----------------------- Terminator ellisis
-- removeElidableTerminators :: ZG.Text -> Either String T.Text
-- removeElidableTerminators (ZG.LE (ZG.Init i) _ _ x _) =
-- removeElidableTerminators (ZG.Terms terms _) =
-- removeElidableTerminators (ZG.BridiTail (ZG.BRIVLA selbri) terms) = selbri `T.append` removeTerminators terms
-- removeElidableTerminators (ZG.BridiTail (ZG.GOhA selbri) terms) = selbri `T.append` removeTerminators terms
-- TODO: make this function way more efficient and use the following brute-force version only in unit tests

removeElidableTerminators :: T.Text -> T.Text
removeElidableTerminators t = f [] (T.words t) where
    originalCanonicalization = basicSentenceCanonicalizer t
    f :: [T.Text] -> [T.Text] -> T.Text
    f x [] = T.unwords x
    f x (y:ys) = if basicSentenceCanonicalizer (T.unwords $ x++ys) == originalCanonicalization then f x ys else f (x++[y]) ys

------------------------- ----------------------- Sentence canonicalizers
--TODO: check whether se/te/ve/xe are left-associative or right-associative
--TODO: create LOTS of unit tests
--ZasniGerna documentation: https://hackage.haskell.org/package/zasni-gerna-0.0.7/docs/Language-Lojban-Parser-ZasniGerna.html
--TODO: exercises involving su'u -- first replace with (the more general cmavo here) and then canonicalize? this way the specific word will be accepted whenever the more general is -- problem: what about the use of an incorrect, more restrictive word instead?
--TODO: conversion mode that replaces all "NU" words with "nu"?
--TODO: write tests using equivalence classes + canonical output for the class
--TODO: support tanru

---------- Parsing
parse :: T.Text -> Either String (ZG.Free, ZG.Text, ZG.Terminator)
parse = ZG.parse . T.unpack

---------- Types
type StructuredSelbri = ZG.Text
type StructuredTerm = ZG.Text
type StructuredBridi = (StructuredSelbri, [(Int, StructuredTerm)])

---------- Handle place tags (fa/fe/fi/fo/fu)
handlePlaceTags :: StructuredBridi -> Either String StructuredBridi
handlePlaceTags (selbri, []) = Right $ (selbri, [])
handlePlaceTags (selbri, terms) = assert (isContiguousSequence $ map fst terms) $ Right (selbri, f firstPosition terms) where
    firstPosition = fst $ head terms
    f :: Int -> [(Int, StructuredTerm)] -> [(Int, StructuredTerm)]
    f _ [] = []
    f defaultPosition (h:t) = let (tag, term) = retrieveTag (snd h)
                                  position = case tag of Just x -> retrievePosition x; Nothing -> defaultPosition
                              in (position, term) : f (position+1) t
    retrievePosition :: String -> Int
    retrievePosition "fa" = 1
    retrievePosition "fe" = 2
    retrievePosition "fi" = 3
    retrievePosition "fo" = 4
    retrievePosition "fu" = 5
    retrieveTag :: ZG.Text -> (Maybe String, ZG.Text)
    retrieveTag (ZG.Tag (ZG.FA x) y) = (Just x, y)
    retrieveTag x = (Nothing, x)

---------- Handle place permutations (se/te/ve/xe)
swapTerms :: Int -> Int -> [(Int, StructuredTerm)] -> [(Int, StructuredTerm)]
swapTerms x y terms = assert (x /= y) $ map f terms where
    f (k, t) = (if k == x then y else if k == y then x else k, t)
swapTerms2 :: String -> [(Int, StructuredTerm)] -> [(Int, StructuredTerm)]
swapTerms2 "se" = swapTerms 1 2
swapTerms2 "te" = swapTerms 1 3
swapTerms2 "ve" = swapTerms 1 4
swapTerms2 "xe" = swapTerms 1 5

handlePlacePermutations :: StructuredBridi -> Either String StructuredBridi
handlePlacePermutations (ZG.BRIVLA brivla, terms) = Right $ (ZG.BRIVLA brivla, terms)
handlePlacePermutations (ZG.GOhA brivla, terms) = Right $ (ZG.GOhA brivla, terms)
handlePlacePermutations (ZG.Prefix (ZG.SE x) y, terms) = do
    (selbri, terms2) <- handlePlacePermutations (y, terms)
    return $ (selbri, swapTerms2 x terms2)
handlePlacePermutations _ = Left "unrecognized pattern in function handlePlacePermutations"

---------- Retrieve structured bridi
retrieveStructuredBridi :: ZG.Text -> Either String StructuredBridi
------- with x1
-- prami
retrieveStructuredBridi (ZG.BRIVLA brivla) = Right $ (ZG.BRIVLA brivla, [])
retrieveStructuredBridi (ZG.GOhA brivla) = Right $ (ZG.GOhA brivla, [])
-- se prami
retrieveStructuredBridi (ZG.Prefix x y) = Right $ (ZG.Prefix x y, [])
-- prami do / se prami do
retrieveStructuredBridi (ZG.BridiTail selbri (ZG.Terms terms _)) = Right $ (selbri, zip [2..] terms)
------- without x1
-- mi prami
retrieveStructuredBridi (ZG.Bridi (ZG.Terms terms _) (ZG.BRIVLA brivla)) = Right $ (ZG.BRIVLA brivla, zip [1..] terms)
retrieveStructuredBridi (ZG.Bridi (ZG.Terms terms _) (ZG.GOhA brivla)) = Right $ (ZG.GOhA brivla, zip [1..] terms)
-- mi se prami
retrieveStructuredBridi (ZG.Bridi (ZG.Terms terms _) (ZG.Prefix x y)) = Right $ (ZG.Prefix x y, zip [1..] terms)
-- mi prami do / mi se prami do
retrieveStructuredBridi (ZG.Bridi (ZG.Terms terms1 _) (ZG.BridiTail selbri (ZG.Terms terms2 _))) = Right $ (selbri, zip [1..] $ terms1 ++ terms2)
------- invalid
retrieveStructuredBridi x = Left $ "unrecognized pattern in function retrieveStructuredBridi: " ++ show x

---------- Convert structured bridi to simple bridi
-- The structured bridi must already have correct place structure (no place tags, no place reordering)
convertStructuredBridi :: Bool -> StructuredBridi -> Either String SimpleBridi
convertStructuredBridi xu (selbri, terms) = do
    selbri2 <- convertStructuredSelbri selbri
    terms2 <- convertStructuredTerms terms
    return $ SimpleBridi xu selbri2 terms2

convertStructuredSelbri :: StructuredSelbri -> Either String T.Text
convertStructuredSelbri (ZG.BRIVLA brivla) = Right $ T.pack brivla
convertStructuredSelbri (ZG.GOhA brivla) = Right $ T.pack brivla

convertStructuredTerms :: [(Int, StructuredTerm)] -> Either String [T.Text]
convertStructuredTerms terms = do
    let terms2 = map (fmap convertStructuredTerm) terms :: [(Int, Either String T.Text)]
    let terms3 = map (\(i, v) -> case v of Right x -> Right (i, x); Left x -> Left x) terms2 :: [Either String (Int, T.Text)]
    terms4 <- foldr (liftA2 (:)) (Right []) terms3 :: Either String [(Int, T.Text)]
    let terms5 = filter ((/= "zo'e") . snd) terms4 :: [(Int, T.Text)]
    let lastTermNumber = if null terms5 then 0 else maximum (map fst terms5)
    let retrieveTerm i = headOrDefault (T.pack "") $ map snd $ filter ((== i) . fst) terms5
    return $ map retrieveTerm [1..lastTermNumber]

convertStructuredTerm :: StructuredTerm -> Either String T.Text
convertStructuredTerm (ZG.KOhA x) = Right $ T.pack x
convertStructuredTerm (ZG.BRIVLA x) = Right $ T.pack x
convertStructuredTerm (ZG.GOhA x) = Right $ T.pack x
convertStructuredTerm (ZG.Prefix (ZG.SE x) y) = insertPrefix <$> convertStructuredTerm y where
    insertPrefix = ((T.pack $ x ++ " ") `T.append`)
convertStructuredTerm (ZG.NU (ZG.Init x) y w) = convertStructuredTerm (ZG.NU (ZG.InitF x ZG.NF) y w)
convertStructuredTerm (ZG.NU (ZG.InitF x y) w z) = insertPrefix . insertSuffix <$> displayCanonicalBridi <$> canonicalizeText (y, w, z) where
    insertPrefix = ((T.pack $ x ++ " ") `T.append`)
    insertSuffix = (`T.append` " kei")
convertStructuredTerm (ZG.LE (ZG.Init x) ZG.NR ZG.NQ y _) = insertPrefix . insertSuffix <$> convertStructuredTerm y where
    insertPrefix = ((T.pack $ x ++ " ") `T.append`)
    insertSuffix = (`T.append` " ku")

---------- Canonicalization
type SentenceCanonicalizer = T.Text -> Either String T.Text
basicSentenceCanonicalizer :: T.Text -> Either String T.Text
basicSentenceCanonicalizer sentence = displayCanonicalBridi <$> (parse sentence >>= canonicalizeText)

canonicalizeText :: (ZG.Free, ZG.Text, ZG.Terminator) -> Either String SimpleBridi
canonicalizeText (free, text, terminator) = retrieveStructuredBridi text >>= handlePlaceTags >>= handlePlacePermutations >>= convertStructuredBridi xu where
    xu = hasXu free

hasXu :: ZG.Free -> Bool
hasXu (ZG.UI x) = x == "xu"
hasXu (ZG.UIF x y) = (x == "xu") || hasXu y
hasXu (ZG.BUIF x y z) = hasXu z
hasXu (ZG.DOIF x y) = hasXu y
hasXu (ZG.BDOIF x y z) = hasXu z
hasXu (ZG.COIF x y) = hasXu y
hasXu (ZG.BCOIF x y z) = hasXu z
hasXu (ZG.COIs xs y) = any hasXu xs
hasXu (ZG.Vocative xs y z) = any hasXu xs
hasXu _ = False

displayCanonicalBridi :: SimpleBridi -> T.Text
displayCanonicalBridi = fst . displayStandardSimpleBridi (mkStdGen 42)

------------------------- ----------------------- Sentence generators
--This might be incorrect; observatives are bridi, for example
generateNonbridi :: Vocabulary -> StdGen -> (T.Text, StdGen)
generateNonbridi vocabulary r0 = chooseItemUniformly r0 . concat . map (getVocabularySumti vocabulary) $
    ["genericPersons", "semiGenericPersons", "animals", "genericPointable", "places", "subjects"]

generateSimpleBridi :: Vocabulary -> StdGen -> (SimpleBridi, StdGen)
generateSimpleBridi vocabulary = combineFunctionsUniformly
    [generatePropertyBridi vocabulary, generateRelationBridi vocabulary, generateActionBridi vocabulary]

generatePropertyBridi :: Vocabulary -> StdGen -> (SimpleBridi, StdGen)
generatePropertyBridi vocabulary r0 = (SimpleBridi False property [object], r2) where
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
generateRelationBridi vocabulary r0 = (SimpleBridi False relation objects, r2) where
    (relation, r1) = chooseItemUniformly r0 relations
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
                (x1, r1) = chooseItemUniformly r0 persons
                (x2, r2) = chooseItemUniformly r1 $ filter (/= x1) (persons++animals)
            in ([x1, x2], r2))
        , ("pendo", \r0 ->
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
generateActionBridi vocabulary r0 = (SimpleBridi False action objects, r2) where
    (action, r1) = chooseItemUniformly r0 actions
    (objects, r2) = (actionObjectsGenerators M.! action) r1
    -- Vocabulary
    actions = filterOutWords ["nupre", "cusku"] $ getVocabularySelbri vocabulary "actions"
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
                availableSubjects = filter (/= speaker) . filter (/= listener) $ subjects
            in
                if null availableSubjects then
                    ([speaker, listener], r2)
                else
                    let  (subject, r3) = chooseItemUniformly r2 availableSubjects
                    in ([speaker, listener, subject], r3))
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

-- TODO: write tests
-- Hard tests: "lo se se prenu ku", "mi tavla fa fi do"
