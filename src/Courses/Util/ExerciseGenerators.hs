{-# LANGUAGE OverloadedStrings #-}

module Courses.Util.ExerciseGenerators
( Translation
, TranslationGenerator
, generateTranslationExercise
, generateBlacklistedWordTranslationExercise
, generateRestrictedTranslationExercise
, simplifyTranslationGenerator
, generateGrammaticalClassExercise
, generateBridiJufraExercise
, generateLojbanBridiJufraExercise
, generateEnglishBridiJufraExercise
, generateBroadFillingBlanksExerciseByAlternatives
, generateNarrowFillingBlanksExerciseByAlternatives
, generateBroadFillingBlanksExerciseByExpression
, generateNarrowFillingBlanksExerciseByExpression
, generateSelbriIdentificationExercise
, generateContextualizedGismuPlacePositionExercise
, generateContextualizedGismuPlaceMeaningExercise
, generateIsolatedBrivlaPlacesExercise
, generateLexiconProvidingExercise
, generateBasicNumberExercise
) where

import Core
import Courses.Util.Vocabulary
import Courses.Util.Sentences
import Courses.Util.NumberTranslator
import Util (narrowTranslationGenerator, narrowTranslationGeneratorByExpression, isSubexpressionOf, replace, replaceFirstSubexpression, replaceSubexpression, chooseItem, chooseItemUniformly, chooseItemsUniformly, combineFunctions, combineFunctionsUniformly, containsWord)
import Text.Read (readMaybe)
import System.Random (StdGen, random)
import Control.Applicative (liftA2)
import Control.Arrow (first)
import Control.Exception (assert)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Map as M

-- Exercise: translate a sentence from English to Lojban
generateTranslationExercise :: SentenceCanonicalizer -> SentenceComparer -> TranslationGenerator -> ExerciseGenerator
generateTranslationExercise = generateRestrictedTranslationExercise "Translate this sentence" (\_ -> True)

generateBlacklistedWordTranslationExercise :: T.Text -> SentenceCanonicalizer -> SentenceComparer -> TranslationGenerator -> ExerciseGenerator
generateBlacklistedWordTranslationExercise blacklistedWord = generateRestrictedTranslationExercise (T.concat ["Translate without using \"", blacklistedWord, "\""]) (not . containsWord blacklistedWord)

generateRestrictedTranslationExercise :: T.Text -> (T.Text -> Bool) -> SentenceCanonicalizer -> SentenceComparer -> TranslationGenerator -> ExerciseGenerator
generateRestrictedTranslationExercise title validator canonicalizer sentenceComparer translationGenerator r0 = TypingExercise title [ExerciseSentence False english_sentence] (liftA2 (&&) validator validateAll) (head lojban_sentences) where
    (translation, r1) = translationGenerator r0
    (lojban_sentences, english_sentences) = translation
    (english_sentence, r2) = chooseItemUniformly r1 english_sentences
    validateAll typed_sentence = or $ map (validateSingle typed_sentence) lojban_sentences
    validateSingle typed_sentence lojban_sentence = case canonicalizer (T.toLower typed_sentence) of
        Left _ -> False
        Right typed_sentence' -> case canonicalizer (T.toLower lojban_sentence) of
            Left _ -> False
            Right lojban_sentence' -> typed_sentence' `sentenceComparer` lojban_sentence'

-- Simplifies Lojban translation
simplifyTranslation :: Translation -> Translation
simplifyTranslation (lojbanSentences, englishSentences) = (fmap simplifyBridi lojbanSentences, englishSentences)

-- Simplifies the Lojban translation produced by the generator
simplifyTranslationGenerator :: TranslationGenerator -> TranslationGenerator
simplifyTranslationGenerator translationGenerator r0 = (simplifyTranslation translation, r1) where
    (translation, r1) = translationGenerator r0

-- Exercise: tell grammatical class of a word
generateGrammaticalClassExercise :: Vocabulary -> ExerciseGenerator
generateGrammaticalClassExercise vocabulary r0 = SingleChoiceExercise title sentences correctAlternative incorrectAlternatives True where
    wordList = vocabularyWords vocabulary
    words "gismu" = map gismuText $ gismuList wordList
    words "cmavo" = map cmavoText $ cmavoList wordList
    words "cmevla" = cmevlaList wordList
    allAlternatives = filter (not . null . words) ["gismu", "cmavo", "cmevla"]
    (correctAlternative, r1) = chooseItemUniformly r0 allAlternatives
    incorrectAlternatives = filter (/= correctAlternative) allAlternatives
    (word, _) = chooseItemUniformly r1 $ words correctAlternative
    title = "Classify <b>" `T.append` word `T.append` "</b>"
    sentences = []

-- Exercise: jufra vs bridi
generateBridiJufraExercise :: Vocabulary -> SimpleBridiDisplayer -> ExerciseGenerator
generateBridiJufraExercise vocabulary displayBridi = combineFunctionsUniformly [generateEnglishBridiJufraExercise, generateLojbanBridiJufraExercise vocabulary displayBridi]

generateLojbanBridiJufraExercise :: Vocabulary -> SimpleBridiDisplayer -> ExerciseGenerator
generateLojbanBridiJufraExercise vocabulary displayBridi r0 = SingleChoiceExercise title sentences correctAlternative incorrectAlternatives True where
    chooseLojbanSentence :: T.Text -> StdGen -> (T.Text, StdGen)
    chooseLojbanSentence "only jufra" r0 = generateNonbridi vocabulary r0
    chooseLojbanSentence "bridi and jufra" r0 = displayBridi r1 simpleBridi where
        (simpleBridi, r1) = generateSimpleBridi vocabulary r0
    allAlternatives = ["only jufra", "bridi and jufra"]
    (correctAlternative, r1) = chooseItemUniformly r0 allAlternatives
    incorrectAlternatives = filter (/= correctAlternative) allAlternatives
    (sentenceText, _) = chooseLojbanSentence correctAlternative r1
    title = "Bridi or jufra?"
    sentences = [ExerciseSentence True sentenceText]

generateEnglishBridiJufraExercise :: ExerciseGenerator
generateEnglishBridiJufraExercise r0 = SingleChoiceExercise title sentences correctAlternative incorrectAlternatives True where
        allAlternatives = ["only jufra", "bridi and jufra"]
        (correctAlternative, r1) = chooseItemUniformly r0 allAlternatives
        incorrectAlternatives = filter (/= correctAlternative) allAlternatives
        (sentenceText, _) = chooseItemUniformly r1 $ englishSentences correctAlternative
        title = "Bridi or jufra?"
        sentences = [ExerciseSentence True sentenceText]

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

-- Exercise: fill in the blanks (by alternatives)
-- Expects a translation generator whose resulting sentences contain precisely one of the alternatives.

-- "Broad": this function chooses an arbitrary Lojban sentence, not necessarily the first in the Translation
generateBroadFillingBlanksExerciseByAlternatives :: [T.Text] -> TranslationGenerator -> ExerciseGenerator
generateBroadFillingBlanksExerciseByAlternatives alternatives translationGenerator r0 = SingleChoiceExercise title sentences correctAlternative incorrectAlternatives True where
    (translation, r1) = translationGenerator r0
    (sentenceText, r2) = chooseItemUniformly r1 (fst translation)
    correctAlternatives = filter (`isSubexpressionOf` sentenceText) $ alternatives
    correctAlternative = assert (length correctAlternatives == 1) $ head correctAlternatives
    incorrectAlternatives = filter (/= correctAlternative) alternatives
    title = "Fill in the blanks"
    redactedSentenceText = replaceFirstSubexpression correctAlternative "____" sentenceText
    sentences = [ExerciseSentence True redactedSentenceText]

-- "Narrow": this function always chooses the first (canonical) Lojban sentence from the Translation
generateNarrowFillingBlanksExerciseByAlternatives :: [T.Text] -> TranslationGenerator -> ExerciseGenerator
generateNarrowFillingBlanksExerciseByAlternatives alternatives translationGenerator = generateBroadFillingBlanksExerciseByAlternatives alternatives (narrowTranslationGenerator translationGenerator)

-- Exercise: fill in the blanks (by expression)

-- "Broad": this function chooses an arbitrary Lojban sentence, not necessarily the first in the Translation
-- TODO: create a proper exercise type instead of using "TypingExercise"
generateBroadFillingBlanksExerciseByExpression :: TranslationGeneratorByExpression -> ExerciseGenerator
generateBroadFillingBlanksExerciseByExpression translationGeneratorByExpression r0 = TypingExercise title sentences validator expression where
    ((expression, translationGenerator), r1) = chooseItemUniformly r0 translationGeneratorByExpression
    (translation, r2) = translationGenerator r1
    (lojbanSentenceText, r3) = chooseItemUniformly r2 (fst translation)
    (englishSentenceText, r4) = chooseItemUniformly r3 (snd translation)
    title = "Complete the translation"
    redactedLojbanSentenceText = replaceSubexpression expression "____" lojbanSentenceText
    sentences = [ExerciseSentence False englishSentenceText, ExerciseSentence True redactedLojbanSentenceText]
    validator = (== expression)

-- "Narrow": this function always chooses the first (canonical) Lojban sentence from the Translation
generateNarrowFillingBlanksExerciseByExpression :: TranslationGeneratorByExpression -> ExerciseGenerator
generateNarrowFillingBlanksExerciseByExpression translationGeneratorByExpression = generateBroadFillingBlanksExerciseByExpression (narrowTranslationGeneratorByExpression translationGeneratorByExpression)

-- Exercise: identify the selbri
generateSelbriIdentificationExercise :: Vocabulary -> SimpleBridiDisplayer -> ExerciseGenerator
generateSelbriIdentificationExercise vocabulary displayBridi r0 = SingleChoiceExercise title sentences correctAlternative incorrectAlternatives False where
    (bridi, r1) = generateSimpleBridi vocabulary r0
    correctAlternative = simpleBridiSelbri bridi
    incorrectAlternatives = take 4 $ simpleBridiSumti bridi
    title = "Identify the <b>selbri</b>"
    (sentenceText, _) = displayBridi r1 bridi
    sentences = [ExerciseSentence True sentenceText]

-- Exercises: tell gismu places of a sentence (TODO: typing exercises?)
generateContextualizedGismuPlaceMeaningExercise :: Dictionary -> Vocabulary -> SimpleBridiDisplayer -> ExerciseGenerator
generateContextualizedGismuPlaceMeaningExercise dictionary vocabulary displayBridi = combineFunctions [(0, f1), (1, f2)] where
    f1 r0 =
        let
            (bridi, r1) = generateActionBridi vocabulary r0
            placesEnglish = gismuEnglishPlaces $ (dictGismu dictionary) M.! (simpleBridiSelbri bridi)
            placesLojban = simpleBridiSumti $ bridi
            places = zip placesEnglish (replace "" "zo'e" placesLojban)
            title = "Match the places"
            (sentenceText, _) = displayBridi r1 bridi
            sentences = [ExerciseSentence True sentenceText]
        in MatchingExercise title sentences places
    f2 r0 =
        let
            (bridi, r1) = generateActionBridi vocabulary r0
            placesEnglish = gismuEnglishPlaces $ (dictGismu dictionary) M.! (simpleBridiSelbri bridi)
            placesLojban = simpleBridiSumti $ bridi
            places = zip placesEnglish (replace "" "zo'e" placesLojban)
            (place, r2) = chooseItemUniformly r1 places
            correctAlternative = snd place
            incorrectAlternatives = (simpleBridiSelbri bridi) : (filter (/= correctAlternative) . map snd $ places)
            title = "Select the " `T.append` "<b>" `T.append` (fst place) `T.append` "</b>"
            (sentenceText, _) = displayBridi r2 bridi
            sentences = [ExerciseSentence True sentenceText]
        in SingleChoiceExercise title sentences correctAlternative incorrectAlternatives False

generateContextualizedGismuPlacePositionExercise :: Dictionary -> Vocabulary -> SimpleBridiDisplayer -> ExerciseGenerator
generateContextualizedGismuPlacePositionExercise dictionary vocabulary displayBridi = combineFunctions [(0, f1), (1, f2)] where
    f1 r0 =
        let
            (bridi, r1) = generateActionBridi vocabulary r0
            placesNumeric = map (('x' `T.cons`) . T.pack . show) $ [1..]
            placesLojban = simpleBridiSumti $ bridi
            places = zip placesNumeric (replace "" "zo'e" placesLojban)
            title = "Match the places"
            (sentenceText, _) = displayBridi r1 bridi
            sentences = [ExerciseSentence True sentenceText]
        in MatchingExercise title sentences places
    f2 r0 =
        let
            (bridi, r1) = generateSimpleBridi vocabulary r0
            placesNumeric = map (('x' `T.cons`) . T.pack . show) $ [1..]
            placesLojban = simpleBridiSumti $ bridi
            places = zip placesNumeric (replace "" "zo'e" placesLojban)
            (place, r2) = chooseItemUniformly r1 places
            correctAlternative = snd place
            incorrectAlternatives = (simpleBridiSelbri bridi) : (filter (/= correctAlternative) . map snd $ places)
            title = "Select the <b>" `T.append` (fst place) `T.append` "</b>"
            (sentenceText, _) = displayBridi r2 bridi
            sentences = [ExerciseSentence True sentenceText]
        in SingleChoiceExercise title sentences correctAlternative incorrectAlternatives False

-- Exercise: tell brivla places using se/te/ve/xe
generateIsolatedBrivlaPlacesExercise :: Dictionary -> WordGenerator -> ExerciseGenerator
generateIsolatedBrivlaPlacesExercise dictionary brivlaGenerator r0 =
    let
        (brivla, r1) = brivlaGenerator r0
        placesLojban = map (\x -> x `T.append` " " `T.append` brivla `T.append` " ku") ["lo", "lo se", "lo te", "lo ve", "lo xe"]
        placesEnglish = retrieveBrivlaPlaces dictionary brivla
        places = zip placesLojban placesEnglish
        (place, _) = chooseItemUniformly r1 places
        correctAlternative = snd place
        incorrectAlternatives = filter (/= correctAlternative) . map snd $ places
        title = "Identify <b>" `T.append` (fst place) `T.append` "</b>"
        sentences = []
    in SingleChoiceExercise title sentences correctAlternative incorrectAlternatives False

-- Exercise: provide the lexicon
generateLexiconProvidingExercise :: T.Text -> Dictionary -> WordGenerator -> ExerciseGenerator
generateLexiconProvidingExercise lexiconCategory dictionary wordGenerator r0 = TypingExercise title sentences validator canonicalAnswer where
    (word, r1) = wordGenerator r0
    wordDefinition = dictValsiDefinition dictionary M.! word
    title = "Provide the " `T.append` lexiconCategory
    sentences = [ExerciseSentence True wordDefinition]
    validator = (== word)
    canonicalAnswer = word

-- Exercise: convert numbers to and from lojban
generateBasicNumberExercise :: ExerciseGenerator
generateBasicNumberExercise = combineFunctionsUniformly [generateNumberToTextExercise, generateTextToNumberExercise]

generateNumberToTextExercise :: ExerciseGenerator
generateNumberToTextExercise r0 =
    let (x, _) = first (`mod` 1000) $ random r0 :: (Integer, StdGen)
        v = \text -> case lojbanToNumber text of
            Nothing -> False
            Just x' -> x' == x
    in TypingExercise ("Number to text: <b>" `T.append` (T.pack $ show x) `T.append` "</b>") [] v (numberToSimpleLojban x)

generateTextToNumberExercise :: ExerciseGenerator
generateTextToNumberExercise r0 =
    let (x, _) = first (`mod` 999) $ random r0 :: (Integer, StdGen)
        v = \text -> case readMaybe (T.unpack text) of
            Nothing -> False
            Just x' -> x' == x
    in TypingExercise ("Text to number: <b>" `T.append` (numberToSimpleLojban x) `T.append` "</b>") [] v (T.pack . show $ x)

---------- Exercise ideas
-- 1) fill bridi with fa/fe/fi/fo/fu (this probably requires images for context)
