{-# LANGUAGE OverloadedStrings #-}

module Lessons.Grammar.ExerciseGenerators
( Translation
, generateTranslationExercise
, generateGrammaticalClassExercise
, generateBridiJufraExercise
, generateLojbanBridiJufraExercise
, generateEnglishBridiJufraExercise
, generateSelbriIdentificationExercise
, generateEasyGismuPlacesExercise
, generateBasicNumberExercise
) where

import Core
import Lessons.Grammar.Vocabulary
import Lessons.Grammar.Sentences
import Lessons.Grammar.Number
import Util (replace, chooseItem, chooseItemUniformly, chooseItemsUniformly, combineFunctions, combineFunctionsUniformly)
import Text.Read (readMaybe)
import System.Random (StdGen, random)
import Control.Arrow (first)
import qualified Data.Text as T
import qualified Data.Map as M

-- Exercise: translate a sentence from English to Lojban
type Translation = (EnglishSentence, LojbanSentence)
type EnglishSentence = T.Text
type LojbanSentence = T.Text

generateTranslationExercise :: SentenceCannonicalizer -> [Translation] -> ExerciseGenerator
generateTranslationExercise cannonicalizer translations r0 = TypingExercise title (Just $ ExerciseSentence True english_sentence) validate lojban_sentence where
    ((english_sentence, lojban_sentence), r1) = chooseItemUniformly r0 translations
    title = "Translate this sentence"
    validate x = case cannonicalizer x of
        Left _ -> False
        Right x' -> case cannonicalizer (T.toLower lojban_sentence) of
            Left _ -> False
            Right lojban_sentence' -> (T.toLower x') == lojban_sentence'

-- Exercise: tell grammatical class of a word
generateGrammaticalClassExercise :: Vocabulary -> ExerciseGenerator
generateGrammaticalClassExercise vocabulary r0 = SingleChoiceExercise title sentence correctAlternative incorrectAlternatives True where
    wordList = vocabularyWords vocabulary
    words "gismu" = map gismuText $ gismuList wordList
    words "cmavo" = map cmavoText $ cmavoList wordList
    words "cmevla" = cmevlaList wordList
    allAlternatives = filter (not . null . words) ["gismu", "cmavo", "cmevla"]
    (correctAlternative, r1) = chooseItemUniformly r0 allAlternatives
    incorrectAlternatives = filter (/= correctAlternative) allAlternatives
    (word, _) = chooseItemUniformly r1 $ words correctAlternative
    title = "Classify <b>" `T.append` word `T.append` "</b>"
    sentence = Nothing

-- Exercise: jufra vs bridi
generateBridiJufraExercise :: Vocabulary -> SimpleBridiDisplayer -> ExerciseGenerator
generateBridiJufraExercise vocabulary displayBridi = combineFunctionsUniformly [generateEnglishBridiJufraExercise, generateLojbanBridiJufraExercise vocabulary displayBridi]

generateLojbanBridiJufraExercise :: Vocabulary -> SimpleBridiDisplayer -> ExerciseGenerator
generateLojbanBridiJufraExercise vocabulary displayBridi r0 = SingleChoiceExercise title sentence correctAlternative incorrectAlternatives True where
    chooseLojbanSentence :: T.Text -> StdGen -> (T.Text, StdGen)
    chooseLojbanSentence "only jufra" r0 = generateNonbridi vocabulary r0
    chooseLojbanSentence "bridi and jufra" r0 = displayBridi simpleBridi r1 where
        (simpleBridi, r1) = generateSimpleBridi vocabulary r0
    allAlternatives = ["only jufra", "bridi and jufra"]
    (correctAlternative, r1) = chooseItemUniformly r0 allAlternatives
    incorrectAlternatives = filter (/= correctAlternative) allAlternatives
    (sentenceText, _) = chooseLojbanSentence correctAlternative r1
    title = "Bridi or jufra?"
    sentence = Just $ ExerciseSentence True sentenceText

generateEnglishBridiJufraExercise :: ExerciseGenerator
generateEnglishBridiJufraExercise r0 = SingleChoiceExercise title sentence correctAlternative incorrectAlternatives True where
        allAlternatives = ["only jufra", "bridi and jufra"]
        (correctAlternative, r1) = chooseItemUniformly r0 allAlternatives
        incorrectAlternatives = filter (/= correctAlternative) allAlternatives
        (sentenceText, _) = chooseItemUniformly r1 $ englishSentences correctAlternative
        title = "Bridi or jufra?"
        sentence = Just . ExerciseSentence True $ sentenceText

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
    [ "I would like to see you."
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
    ]

-- Exercise: identify the selbri
generateSelbriIdentificationExercise :: Vocabulary -> SimpleBridiDisplayer -> ExerciseGenerator
generateSelbriIdentificationExercise vocabulary displayBridi r0 = SingleChoiceExercise title sentence correctAlternative incorrectAlternatives False where
    (bridi, r1) = generateSimpleBridi vocabulary r0
    correctAlternative = simpleBridiSelbri bridi
    incorrectAlternatives = take 4 $ simpleBridiSumti bridi
    title = "Identify the <b>selbri</b>"
    (sentenceText, _) = displayBridi bridi r1
    sentence = Just . ExerciseSentence True $ sentenceText

-- Exercise: tell gismu places of a sentence (TODO: typing exercises?)
generateEasyGismuPlacesExercise :: Dictionary -> Vocabulary -> SimpleBridiDisplayer -> ExerciseGenerator
generateEasyGismuPlacesExercise dictionary vocabulary displayBridi = combineFunctions [(0, f1), (3, f2), (0, f3), (5, f4)] where
    f1 r0 =
        let
            (bridi, r1) = generateActionBridi vocabulary r0
            placesEnglish = gismuEnglishPlaces $ (dictGismu dictionary) M.! (simpleBridiSelbri bridi)
            placesLojban = simpleBridiSumti $ bridi
            places = zip placesEnglish placesLojban
            title = "Match the places"
            (sentenceText, _) = displayBridi bridi r1
            sentence = Just . ExerciseSentence True $ sentenceText
        in MatchingExercise title sentence places
    f2 r0 =
        let
            (bridi, r1) = generateActionBridi vocabulary r0
            placesEnglish = gismuEnglishPlaces $ (dictGismu dictionary) M.! (simpleBridiSelbri bridi)
            placesLojban = simpleBridiSumti $ bridi
            places = zip placesEnglish placesLojban
            (place, r2) = chooseItemUniformly r1 places
            correctAlternative = snd place
            incorrectAlternatives = (simpleBridiSelbri bridi) : (filter (/= correctAlternative) . map snd $ places)
            title = "Select the " `T.append` "<b>" `T.append` (fst place) `T.append` "</b>"
            (sentenceText, _) = displayBridi bridi r2
            sentence = Just . ExerciseSentence True $ sentenceText
        in SingleChoiceExercise title sentence correctAlternative incorrectAlternatives False
    f3 r0 =
        let
            (bridi, r1) = generateActionBridi vocabulary r0
            placesNumeric = map (('x' `T.cons`) . T.pack . show) $ [1..]
            placesLojban = simpleBridiSumti $ bridi
            places = zip placesNumeric (replace "" "zo'e" placesLojban)
            title = "Match the places"
            (sentenceText, _) = displayBridi bridi r1
            sentence = Just . ExerciseSentence True $ sentenceText
        in MatchingExercise title sentence places
    f4 r0 =
        let
            (bridi, r1) = generateSimpleBridi vocabulary r0
            placesNumeric = map (('x' `T.cons`) . T.pack . show) $ [1..]
            placesLojban = simpleBridiSumti $ bridi
            places = zip placesNumeric (replace "" "zo'e" placesLojban)
            (place, r2) = chooseItemUniformly r1 places
            correctAlternative = snd place
            incorrectAlternatives = (simpleBridiSelbri bridi) : (filter (/= correctAlternative) . map snd $ places)
            title = "Select the <b>" `T.append` (fst place) `T.append` "</b>"
            (sentenceText, _) = displayBridi bridi r2
            sentence = Just . ExerciseSentence True $ sentenceText
        in SingleChoiceExercise title sentence correctAlternative incorrectAlternatives False

-- Exercise: convert numbers to and from lojban
generateBasicNumberExercise :: ExerciseGenerator
generateBasicNumberExercise = combineFunctionsUniformly [generateNumberToTextExercise, generateTextToNumberExercise]

generateNumberToTextExercise :: ExerciseGenerator
generateNumberToTextExercise r0 =
    let (x, _) = first (`mod` 1000) $ random r0 :: (Integer, StdGen)
        v = \text -> case lojbanToNumber text of
            Nothing -> False
            Just x' -> x' == x
    in TypingExercise ("Number to text: <b>" `T.append` (T.pack $ show x) `T.append` "</b>") Nothing v (numberToSimpleLojban x)

generateTextToNumberExercise :: ExerciseGenerator
generateTextToNumberExercise r0 =
    let (x, _) = first (`mod` 999) $ random r0 :: (Integer, StdGen)
        v = \text -> case readMaybe (T.unpack text) of
            Nothing -> False
            Just x' -> x' == x
    in TypingExercise ("Text to number: <b>" `T.append` (numberToSimpleLojban x) `T.append` "</b>") Nothing v (T.pack . show $ x)
