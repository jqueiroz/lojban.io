{-# LANGUAGE OverloadedStrings #-}

-- | This module provides utilities for constructing exercise generators.
module Courses.Framework.ExerciseGenerators
( generateTranslationExercise
, generateBlacklistedWordTranslationExercise
, generateRestrictedTranslationExercise
, generateGrammaticalClassExercise
, generateEnglishOrLojbanBridiJufraExercise
, generateLojbanBridiJufraExercise
, generateEnglishBridiJufraExercise
, generateBroadFillingBlanksExerciseByAlternatives
, generateNarrowFillingBlanksExerciseByAlternatives
, generateContextualizedBroadFillingBlanksExerciseByAlternatives
, generateContextualizedNarrowFillingBlanksExerciseByAlternatives
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
import Courses.Framework.NumberTranslator
import Courses.Framework.TranslationUtils (narrowTranslationGenerator, narrowTranslationGeneratorByExpression)
import Language.Lojban.Core
import Util (isSubexpressionOf, replace, replaceFirstSubexpression, replaceSubexpression, chooseItemUniformly, combineGenerators, combineGeneratorsUniformly, isWordOf)
import Text.Read (readMaybe)
import System.Random (StdGen, random)
import Control.Applicative (liftA2)
import Control.Arrow (first)
import Control.Exception (assert)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Map as M

-- | Exercise: translate a given sentence into Lojban.
generateTranslationExercise :: SentenceCanonicalizer -> SentenceComparer -> TranslationGenerator -> ExerciseGenerator
generateTranslationExercise = generateRestrictedTranslationExercise "Translate this sentence" (const True)

-- | Exercise: translate a given sentence into Lojban, with the restriction that a particular Lojban word cannot be used.
generateBlacklistedWordTranslationExercise :: T.Text -> SentenceCanonicalizer -> SentenceComparer -> TranslationGenerator -> ExerciseGenerator
generateBlacklistedWordTranslationExercise blacklistedWord = generateRestrictedTranslationExercise (T.concat ["Translate without using \"", blacklistedWord, "\""]) (not . isWordOf blacklistedWord)

-- | Exercise: translate a given sentence into Lojban, with some arbitrary (algorithmically specified) restriction concerning the user's solution.
generateRestrictedTranslationExercise :: T.Text -> (T.Text -> Bool) -> SentenceCanonicalizer -> SentenceComparer -> TranslationGenerator -> ExerciseGenerator
generateRestrictedTranslationExercise title validator canonicalizer sentenceComparer translationGenerator r0 = TypingExercise title [ExerciseSentence False english_sentence] (liftA2 (&&) validator validateAll) (head lojban_sentences) where
    (translation, r1) = translationGenerator r0
    (lojban_sentences, english_sentences) = translation
    (english_sentence, r2) = chooseItemUniformly r1 english_sentences
    validateAll typed_sentence = any (validateSingle typed_sentence) lojban_sentences
    validateSingle typed_sentence lojban_sentence =
        let lower_lojban_sentence = T.toLower lojban_sentence
            lower_typed_sentence = T.toLower typed_sentence
        in (lower_lojban_sentence == lower_typed_sentence) ||
            case canonicalizer lower_typed_sentence of
                Left _ -> False
                Right canonicalized_typed_sentence -> case canonicalizer lower_lojban_sentence of
                    Left _ -> False
                    Right canonicalized_lojban_sentence -> canonicalized_lojban_sentence `sentenceComparer` canonicalized_typed_sentence

-- | Exercise: tell grammatical class of a word (brivla, cmavo, or cmevla).
generateGrammaticalClassExercise :: Vocabulary -> ExerciseGenerator
generateGrammaticalClassExercise vocabulary r0 = SingleChoiceExercise title sentences correctAlternative incorrectAlternatives True where
    words "brivla" = vocabularyBrivlaList vocabulary
    words "cmavo" = vocabularyBrivlaList vocabulary
    words "cmevla" = vocabularyCmevlaList vocabulary
    allAlternatives = filter (not . null . words) ["gismu", "cmavo", "cmevla"]
    (correctAlternative, r1) = chooseItemUniformly r0 allAlternatives
    incorrectAlternatives = filter (/= correctAlternative) allAlternatives
    (word, _) = chooseItemUniformly r1 $ words correctAlternative
    title = "Classify <b>" `T.append` word `T.append` "</b>"
    sentences = []

-- | Exercise: decide whether an utterance (in either English or Lojban) is a bridi or merely a jufra.
generateEnglishOrLojbanBridiJufraExercise :: SimpleBridiGenerator -> TextGenerator -> SimpleBridiDisplayer -> ExerciseGenerator
generateEnglishOrLojbanBridiJufraExercise simpleBridiGenerator nonbridiGenerator displayBridi = combineGeneratorsUniformly [generateEnglishBridiJufraExercise, generateLojbanBridiJufraExercise simpleBridiGenerator nonbridiGenerator displayBridi]

-- | Exercise: decide whether a Lojban utterance is a bridi or merely a jufra.
generateLojbanBridiJufraExercise :: SimpleBridiGenerator -> TextGenerator -> SimpleBridiDisplayer -> ExerciseGenerator
generateLojbanBridiJufraExercise simpleBridiGenerator nonbridiGenerator displayBridi r0 = SingleChoiceExercise title sentences correctAlternative incorrectAlternatives True where
    chooseLojbanSentence :: T.Text -> StdGen -> (T.Text, StdGen)
    chooseLojbanSentence "only jufra" r0 = nonbridiGenerator r0
    chooseLojbanSentence "bridi and jufra" r0 = displayBridi r1 simpleBridi where
        (simpleBridi, r1) = simpleBridiGenerator r0
    allAlternatives = ["only jufra", "bridi and jufra"]
    (correctAlternative, r1) = chooseItemUniformly r0 allAlternatives
    incorrectAlternatives = filter (/= correctAlternative) allAlternatives
    (sentenceText, _) = chooseLojbanSentence correctAlternative r1
    title = "Bridi or jufra?"
    sentences = [ExerciseSentence True sentenceText]

-- | Exercise: decide whether an English utterance is a bridi or merely a jufra.
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

generateContextualizedBroadFillingBlanksExerciseByAlternatives :: [T.Text] -> TranslationGenerator -> ExerciseGenerator
generateContextualizedBroadFillingBlanksExerciseByAlternatives alternatives translationGenerator r0 = SingleChoiceExercise title sentences correctAlternative incorrectAlternatives True where
    (translation, r1) = translationGenerator r0
    (lojbanSentenceText, r2) = chooseItemUniformly r1 (fst translation)
    (englishSentenceText, r3) = chooseItemUniformly r2 (snd translation)
    correctAlternatives = filter (`isSubexpressionOf` lojbanSentenceText) $ alternatives
    correctAlternative = assert (length correctAlternatives == 1) $ head correctAlternatives
    incorrectAlternatives = filter (/= correctAlternative) alternatives
    title = "Complete the translation"
    redactedLojbanSentenceText = replaceFirstSubexpression correctAlternative "____" lojbanSentenceText
    sentences = [ExerciseSentence False englishSentenceText, ExerciseSentence True redactedLojbanSentenceText]

-- "Narrow": this function always chooses the first (canonical) Lojban sentence from the Translation
generateNarrowFillingBlanksExerciseByAlternatives :: [T.Text] -> TranslationGenerator -> ExerciseGenerator
generateNarrowFillingBlanksExerciseByAlternatives alternatives translationGenerator = generateBroadFillingBlanksExerciseByAlternatives alternatives (narrowTranslationGenerator translationGenerator)

generateContextualizedNarrowFillingBlanksExerciseByAlternatives :: [T.Text] -> TranslationGenerator -> ExerciseGenerator
generateContextualizedNarrowFillingBlanksExerciseByAlternatives alternatives translationGenerator = generateContextualizedBroadFillingBlanksExerciseByAlternatives alternatives (narrowTranslationGenerator translationGenerator)

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
generateSelbriIdentificationExercise :: SimpleBridiGenerator -> SimpleBridiDisplayer -> ExerciseGenerator
generateSelbriIdentificationExercise simpleBridiGenerator displayBridi r0 = SingleChoiceExercise title sentences correctAlternative incorrectAlternatives False where
    (bridi, r1) = simpleBridiGenerator r0
    correctAlternative = simpleBridiSelbri bridi
    incorrectAlternatives = take 4 $ simpleBridiSumti bridi
    title = "Identify the <b>selbri</b>"
    (sentenceText, _) = displayBridi r1 bridi
    sentences = [ExerciseSentence True sentenceText]

-- Exercises: tell gismu places of a sentence (TODO: typing exercises?)
generateContextualizedGismuPlaceMeaningExercise :: Dictionary -> SimpleBridiGenerator -> SimpleBridiDisplayer -> ExerciseGenerator
generateContextualizedGismuPlaceMeaningExercise dictionary simpleBridiGenerator displayBridi = combineGenerators [(1, f2)] where
    f2 r0 =
        let
            -- Generate bridi
            (bridi, r1) = simpleBridiGenerator r0
            -- Extract brivla places
            selbri = simpleBridiSelbri bridi
            placesEnglish = retrieveBrivlaPlaces dictionary selbri
            placesLojban = simpleBridiSumti bridi
            places = zip placesEnglish (replace "" "zo'e" placesLojban)
            -- Construct exercise
            (place, r2) = chooseItemUniformly r1 places
            correctAlternative = snd place
            incorrectAlternatives = (simpleBridiSelbri bridi) : (filter (/= correctAlternative) . map snd $ places)
            title = "Select the " `T.append` "<b>" `T.append` (fst place) `T.append` "</b>"
            (sentenceText, _) = displayBridi r2 bridi
            sentences = [ExerciseSentence True sentenceText]
        in SingleChoiceExercise title sentences correctAlternative incorrectAlternatives False

generateContextualizedGismuPlacePositionExercise :: Dictionary -> SimpleBridiGenerator -> SimpleBridiDisplayer -> ExerciseGenerator
generateContextualizedGismuPlacePositionExercise dictionary simpleBridiGenerator displayBridi = combineGenerators [(1, f2)] where
    f2 r0 =
        let
            (bridi, r1) = simpleBridiGenerator r0
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
generateIsolatedBrivlaPlacesExercise :: Dictionary -> [T.Text] -> ExerciseGenerator
generateIsolatedBrivlaPlacesExercise dictionary brivlaList r0 =
    let
        brivlaWithAtLeastTwoPlaces = filter ((>= 2) . length . retrieveBrivlaPlaces dictionary) brivlaList
        (brivla, r1) = chooseItemUniformly r0 brivlaWithAtLeastTwoPlaces
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
    wordDefinition = fromMaybe (error $ "Definition not found in dictionary: " ++ (T.unpack word)) (dictValsiDefinition dictionary M.!? word)
    title = "Provide the " `T.append` lexiconCategory
    sentences = [ExerciseSentence True wordDefinition]
    validator = (== word)
    canonicalAnswer = word

-- Exercise: convert numbers to and from lojban
generateBasicNumberExercise :: ExerciseGenerator
generateBasicNumberExercise = combineGeneratorsUniformly [generateNumberToTextExercise, generateTextToNumberExercise]

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
