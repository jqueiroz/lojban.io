{-# LANGUAGE OverloadedStrings #-}
module Lessons.Exercises where

import Core
import Sentences (SimpleBridi(..), displaySimpleBridi)
import Number (lojbanToNumber, numberToSimpleLojban)
import Util (chooseItem, chooseItemUniformly, chooseItemsUniformly, combineFunctions, combineFunctionsUniformly)
import Text.Read (readMaybe)
import System.Random (StdGen, random)
import Control.Arrow (first)
import Control.Exception (assert)
import qualified Data.Text as T
import qualified Data.Map as M

-- Sentence generators
generateNonbridi :: Vocabulary -> StdGen -> (T.Text, StdGen)
generateNonbridi vocabulary r0 = chooseItemUniformly r0 . concat . map (getVocabularySumti vocabulary) $ ["persons", "pointable", "places", "subjects"]

generateSimpleBridi :: Vocabulary -> StdGen -> (SimpleBridi, StdGen)
generateSimpleBridi vocabulary =
    combineFunctionsUniformly $
        (map generatePropertyBridi properties) ++
        (map generateRelationBridi relations) ++
        (map generateActionBridi actions)
    where
        -- Vocabulary
        properties = getVocabularySelbri vocabulary "properties"
        relations = getVocabularySelbri vocabulary "relations"
        actions = getVocabularySelbri vocabulary "actions"
        persons = getVocabularySumti vocabulary "persons"
        pointable = getVocabularySumti vocabulary "pointable"
        places = getVocabularySumti vocabulary "places"
        subjects = getVocabularySumti vocabulary "subjects"
        -- Properties
        propertyObjects = M.fromList
            [ ("prenu", persons)
            , ("melbi", persons ++ pointable)
            , ("sutra", persons ++ pointable)
            , ("zdani", pointable)
            , ("mlatu", pointable)
            , ("gerku", pointable)
            , ("pelxu", pointable)
            ]
        generatePropertyBridi :: T.Text -> StdGen -> (SimpleBridi, StdGen)
        generatePropertyBridi property r0 = (SimpleBridi property [object], r1) where
            Just objects = M.lookup property propertyObjects
            (object, r1) = chooseItemUniformly r0 objects
        -- Relations
        relationObjectsGenerators :: M.Map T.Text (StdGen -> ([T.Text], StdGen))
        relationObjectsGenerators = M.fromList
            [ ("nelci", \r0 ->
                let
                    (x1, r1) = chooseItemUniformly r0 persons
                    (x2, r2) = chooseItemUniformly r1 (filter (/= x1) persons)
                in ([x1, x2], r2))
            ]
        generateRelationBridi :: T.Text -> StdGen -> (SimpleBridi, StdGen)
        generateRelationBridi relation r0 = (SimpleBridi relation objects, r1) where
            Just objectsGenerators = M.lookup relation relationObjectsGenerators
            (objects, r1) = objectsGenerators r0
        -- Actions
        actionObjectsGenerators :: M.Map T.Text (StdGen -> ([T.Text], StdGen))
        actionObjectsGenerators = M.fromList
            [ ("tavla", \r0 ->
                let
                    (speaker, r1) = chooseItemUniformly r0 persons
                    (listener, r2) = chooseItemUniformly r1 (filter (/= speaker) persons)
                    (subject, r3) = chooseItemUniformly r2 ("" : subjects ++ pointable)
                in ([speaker, listener, subject], r3))
            , ("dunda", \r0 ->
                let
                    (donor, r1) = chooseItemUniformly r0 persons
                    (gift, r2) = chooseItemUniformly r1 pointable
                    (receiver, r3) = chooseItemUniformly r2 (filter (/= donor) persons)
                in
                    ([donor, gift, receiver], r3))
            , ("klama", \r0 ->
                let
                    (actor, r1) = chooseItemUniformly r0 persons
                    (destination, r2) = chooseItemUniformly r1 pointable
                in
                    ([actor, destination], r2))
            ]
        generateActionBridi :: T.Text -> StdGen -> (SimpleBridi, StdGen)
        generateActionBridi action r0 = (SimpleBridi action objects, r1) where
            Just objectsGenerator = M.lookup action actionObjectsGenerators
            (objects, r1) = objectsGenerator r0

-- Exercise: jufra vs bridi
generateBridiJufraExercise :: Vocabulary -> StdGen -> Exercise
generateBridiJufraExercise vocabulary = combineFunctionsUniformly [generateEnglishBridiJufraExercise, generateLojbanBridiJufraExercise vocabulary]

generateLojbanBridiJufraExercise :: Vocabulary -> StdGen -> Exercise
generateLojbanBridiJufraExercise vocabulary r0 = SingleChoiceExercise text correctAlternative incorrectAlternatives True where
    chooseLojbanSentence :: T.Text -> StdGen -> (T.Text, StdGen)
    chooseLojbanSentence "only jufra" r0 = generateNonbridi vocabulary r0
    chooseLojbanSentence "bridi and jufra" r0 = (displaySimpleBridi simpleBridi, r1) where
        (simpleBridi, r1) = generateSimpleBridi vocabulary r0
    allAlternatives = ["only jufra", "bridi and jufra"]
    (correctAlternative, r1) = chooseItemUniformly r0 allAlternatives
    incorrectAlternatives = filter (/= correctAlternative) allAlternatives
    (sentence, _) = chooseLojbanSentence correctAlternative r1
    text = "Is the following sentece a bridi or merely a jufra?\n**" `T.append` sentence `T.append` "**"

generateEnglishBridiJufraExercise :: StdGen -> Exercise
generateEnglishBridiJufraExercise r0 = SingleChoiceExercise text correctAlternative incorrectAlternatives True where
        allAlternatives = ["only jufra", "bridi and jufra"]
        (correctAlternative, r1) = chooseItemUniformly r0 allAlternatives
        incorrectAlternatives = filter (/= correctAlternative) allAlternatives
        (sentence, _) = chooseItemUniformly r1 $ englishSentences correctAlternative
        text = "Is the following sentence a bridi or merely a jufra?\n**" `T.append` sentence `T.append` "**"

englishSentences :: T.Text -> [T.Text]
englishSentences "only jufra" = 
    [ "Yes."
    , "No."
    , "Ouch!"
    , "Maybe next week."
    , "Again?!"
    , "Door."
    , "Easy come, easy go."
    , "Teapot."
    , "Forty-two."
    , "Almost, but not quite, entirely unlike tea."
    ]
englishSentences "bridi and jufra" = 
    [ "Studies." --cfm
    , "Walks." --cfm
    , "I would like to see you."
    , "Most people don't like him."
    , "They don't care about us."
    , "Be happy!"
    , "That's pretty cool."
    , "They would never do that."
    , "I would never have guessed it."
    , "Could you repeat that, please?"
    , "The above proposition is occasionally useful."
    , "Don't panic."
    , "Reality is frequently inaccurate."
    , "Flying is learning how to throw yourself at the ground and miss."
    , "There is another theory which states that this has already happened."
    , "I refuse to answer that question on the grounds that I don't know the answer."
    -- , "I may not have gone where I intended to go, but I think I have ended up where I needed to be."
    -- , "The ships hung in the sky in much the same way that bricks don't."
    -- , "It is a mistake to think you can solve any major problems just with potatoes."
    -- , "If you try and take a cat apart to see how it works, the first thing you have on your hands is a nonworking cat."
    ]

-- Exercise: recall gismu meaning
generateGismuMeaningExercise :: Vocabulary -> StdGen -> Exercise
generateGismuMeaningExercise vocabulary = combineFunctions [(4, f1), (1, f2), (7, f3), (3, f4)] where
        -- Exercise: match gismu with keyword
        f1 :: StdGen -> Exercise
        f1 r0 = MatchingExercise text items where
            (chosenGismu, _) = chooseItemsUniformly r0 3 $ gismuList $ vocabularyWords vocabulary
            item gismu = (gismuText gismu, gismuEnglishKeywords gismu !! 0)
            text = "Match gismu with keyword"
            items = map item chosenGismu
        -- Exercise: match gismu with full definition
        f2 :: StdGen -> Exercise
        f2 r0 = MatchingExercise text items where
            (chosenGismu, _) = chooseItemsUniformly r0 3 $ gismuList $ vocabularyWords vocabulary
            item gismu = (gismuText gismu, gismuEnglishDefinition gismu)
            text = "Match gismu with definition"
            items = map item chosenGismu
        -- Exercise: choose the correct keyword for a gismu
        f3 :: StdGen -> Exercise
        f3 r0 = SingleChoiceExercise text correctAlternative incorrectAlternatives False where
            (chosenGismu, _) = chooseItemsUniformly r0 4 $ gismuList $ vocabularyWords vocabulary
            text = "Select keyword for **" `T.append` (gismuText $ head chosenGismu) `T.append` "**"
            alternative gismu = gismuEnglishKeywords gismu !! 0
            correctAlternative = alternative $ head chosenGismu
            incorrectAlternatives = map alternative $ tail chosenGismu
        -- Exercise: choose the correct definition for a gismu
        f4 :: StdGen -> Exercise
        f4 r0 = SingleChoiceExercise text correctAlternative incorrectAlternatives False where
            (chosenGismu, _) = chooseItemsUniformly r0 4 $ gismuList $ vocabularyWords vocabulary
            text = "Select definition for **" `T.append` (gismuText $ head chosenGismu) `T.append` "**"
            alternative gismu = gismuEnglishDefinition gismu
            correctAlternative = alternative $ head chosenGismu
            incorrectAlternatives = map alternative $ tail chosenGismu
        -- Exercise (TODO): match gismu with image
        -- Exercise (TODO): choose the correct image for a gismu

-- Exercise: recall gismu place structure
generateBasicGismuPlacesExercise :: Vocabulary -> StdGen -> Exercise
generateBasicGismuPlacesExercise vocabulary = combineFunctionsUniformly [f1, f2] where
    f1 :: StdGen -> Exercise
    f1 r0 = MatchingExercise text items where
        (chosenGismu, _) = chooseItemUniformly r0 . filter ((>=3) . length . gismuEnglishPlaces) . gismuList $ vocabularyWords vocabulary
        placeTags = map T.pack . map (\n -> 'x' : show n) $ [1..]
        placeKeywords = gismuEnglishPlaces chosenGismu
        text = "Identify place structure of **" `T.append` (gismuText chosenGismu) `T.append` "**"
        items = zip placeTags placeKeywords
    f2 :: StdGen -> Exercise
    f2 r0 = SingleChoiceExercise text (displayPlaceStructure correctAlternative) (map displayPlaceStructure incorrectAlternatives) False where
        (chosenGismu, r1) = chooseItemUniformly r0 . filter ((>=3) . length . gismuEnglishPlaces) . gismuList $ vocabularyWords vocabulary
        correctAlternative = gismuEnglishPlaces chosenGismu
        (incorrectAlternatives, _) = reorderSumtiPlaces correctAlternative r0
        text = "Select place structure of **" `T.append` (gismuText chosenGismu) `T.append` "**"

generateAdvancedGismuPlacesExercise :: Vocabulary -> StdGen -> Exercise
generateAdvancedGismuPlacesExercise vocabulary = combineFunctionsUniformly [f1, f2] where
    f1 :: StdGen -> Exercise
    f1 r0 = MatchingExercise text items where
        (chosenGismu, r1) = chooseItemUniformly r0 . filter ((>=3) . length . gismuEnglishPlaces) . gismuList $ vocabularyWords vocabulary
        placeGismu = map ((`T.append` (gismuText chosenGismu)) . (`T.append` " ") . displayPlacePrefix) [1..]
        placeKeywords = take 5 $ gismuEnglishPlaces chosenGismu
        text = "Identify the place structure of **" `T.append` (gismuText chosenGismu) `T.append` "**"
        items = zip placeGismu placeKeywords
    f2 :: StdGen -> Exercise
    f2 r0 = SingleChoiceExercise text (snd correctAlternative) (map snd incorrectAlternatives) False where
        (chosenGismu, r1) = chooseItemUniformly r0 . filter ((>=3) . length . gismuEnglishPlaces) . gismuList $ vocabularyWords vocabulary
        placeGismu = map ((`T.append` (gismuText chosenGismu)) . (`T.append` " ") . displayPlacePrefix) [1..]
        placeKeywords = take 5 $ gismuEnglishPlaces chosenGismu
        allAlternatives = zip placeGismu placeKeywords
        (correctAlternative, _) = chooseItemUniformly r1 $ allAlternatives
        incorrectAlternatives = filter (/= correctAlternative) allAlternatives
        text = "Select keyword for **" `T.append` (fst correctAlternative)

reorderSumtiPlaces :: [T.Text] -> StdGen -> ([[T.Text]], StdGen)
reorderSumtiPlaces [x1, x2, x3] r0 = (l, r0) where
       -- [x1, x2, x3]
    l = [ [x1, x3, x2] ]
reorderSumtiPlaces [x1, x2, x3, x4] r0 = (l, r0) where
       -- [x1, x2, x3, x4]
    l = [ [x1, x3, x2, x4]
        , [x1, x2, x4, x3]
        ] -- problematic (TOFIX): always take 2x(x2), 2x(x4)
reorderSumtiPlaces [x1, x2, x3, x4, x5] r0 = (l, r0) where
       -- [x1, x2, x3, x4, x5]
    l = [ [x1, x3, x2, x4, x5]
        , [x1, x3, x4, x2, x5]
        , [x1, x3, x2, x4, x5]
        {-, [x1, x2, x4, x3, x5]-}
        {-, [x1, x2, x3, x5, x4]-}
        --TODO: randomize
        ] -- problematic (TOFIX): ...

displayPlacePrefix :: Int -> T.Text
displayPlacePrefix 1 = "lo"
displayPlacePrefix 2 = "lo se"
displayPlacePrefix 3 = "lo te"
displayPlacePrefix 4 = "lo ve"
displayPlacePrefix 5 = "lo xe"

displayPlaceStructure :: [T.Text] -> T.Text
displayPlaceStructure placeKeywords = T.intercalate ", " $ zipWith T.append placeTags placeKeywords where
    placeTag n = "(x" ++ (show n) ++ ") "
    placeTags = map (T.pack . placeTag) [1..]

-- Exercise: tell grammatical class of a word
generateGrammaticalClassExercise :: Vocabulary -> StdGen -> Exercise
generateGrammaticalClassExercise vocabulary r0 = SingleChoiceExercise text correctAlternative incorrectAlternatives True where
    wordList = vocabularyWords vocabulary
    words "gismu" = map gismuText $ gismuList wordList
    words "cmavo" = map cmavoText $ cmavoList wordList
    words "cmevla" = cmevlaList wordList
    allAlternatives = filter (not . null . words) ["gismu", "cmavo", "cmevla"]
    (correctAlternative, r1) = assert (not . null $ allAlternatives) $ chooseItemUniformly r0 allAlternatives
    incorrectAlternatives = filter (/= correctAlternative) allAlternatives
    (word, _) = chooseItemUniformly r1 $ words correctAlternative
    text = "Choose the correct classification of **" `T.append` word `T.append` "**?"

-- Exercise: convert numbers
generateBasicNumberExercise :: StdGen -> Exercise
generateBasicNumberExercise = combineFunctionsUniformly [generateNumberToTextExercise, generateTextToNumberExercise]

generateNumberToTextExercise :: StdGen -> Exercise
generateNumberToTextExercise r0 =
    let (x, _) = first (`mod` 1000) $ random r0 :: (Integer, StdGen)
        v = \text -> case lojbanToNumber text of
            Nothing -> False
            Just x' -> x' == x
    in TypingExercise ("Number to text: " `T.append` (T.pack $ show x)) v

generateTextToNumberExercise :: StdGen -> Exercise
generateTextToNumberExercise r0 =
    let (x, _) = first (`mod` 999) $ random r0 :: (Integer, StdGen)
        v = \text -> case readMaybe (T.unpack text) of
            Nothing -> False
            Just x' -> x' == x
    in TypingExercise ("Text to number: " `T.append` (numberToSimpleLojban x)) v

-- Other exercises...
-- Exercise: given a portion of a sentence, choose its syntactical function
{-generateExercise6 :: StdGen -> Exercise-}
{-generateExercise6 r0 =-}
    {-let allAlternatives = ["bridi", "selbri", "sumti"]-}
        {-(correctAlternative, r1) = chooseItemUniformly r0 allAlternatives-}
        {-incorrectAlternatives = filter (/= correctAlternative) allAlternatives-}
        {-text = "Choose the synatical function of the highlighted text: "-}
    {-in SingleChoiceExercise text correctAlternative incorrectAlternatives-}
