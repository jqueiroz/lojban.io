{-# LANGUAGE OverloadedStrings #-}

-- | This module provides utilities for manipulating translations.
module Courses.Framework.TranslationUtils
( simplifyTerminatorsInTranslation
, simplifyTerminatorsInTranslationGenerator
, expandSentence
, expandSentences
, expandTranslation
, expandTranslationGenerator
, narrowTranslation
, narrowTranslationGenerator
, narrowTranslationGeneratorByExpression
) where

import Core
import Language.Lojban.Refinement (simplifyTerminatorsInSentence)
import Control.Monad (join)
import Control.Arrow ((***), second)
import qualified Data.Text as T

-- | Simplifies a 'Translation' by removing elidable terminators and/or replacing them with "cu" (see 'simplifyTerminatorsInSentence').
simplifyTerminatorsInTranslation :: Translation -> Translation
simplifyTerminatorsInTranslation (lojbanSentences, englishSentences) = (fmap simplifyTerminatorsInSentence lojbanSentences, englishSentences)

-- | Simplifies a 'TranslationGenerator' by removing elidable terminators and/or replacing them with "cu" (see 'simplifyTerminatorsInSentence').
simplifyTerminatorsInTranslationGenerator :: TranslationGenerator -> TranslationGenerator
simplifyTerminatorsInTranslationGenerator translationGenerator r0 = (simplifyTerminatorsInTranslation translation, r1) where
    (translation, r1) = translationGenerator r0

-- | Expands a list of sentences into a potentially larger list using syntax such as (x|y|z) or {x|y|z} (see 'expandSentence').
expandSentences :: [T.Text] -> [T.Text]
expandSentences = join . map expandSentence

-- | Expands a sentence into multiple sentences using syntax such as (x|y|z) or {x|y|z}.
--
-- Examples:
--
-- * "lo mlatu poi {ke'a} pinxe cu melbi" -> ["lo mlatu poi pinxe cu melbi", "lo mlatu poi ke'a pinxe cu melbi"]
-- * "lo ctuca (be|pe) mi" -> ["lo ctuca be mi", "lo ctuca pe mi"]
expandSentence :: T.Text -> [T.Text]
expandSentence sentence = map (T.unwords . T.words) (expandSentence' sentence) where
    expandSentence' :: T.Text -> [T.Text]
    expandSentence' sentence
        | (T.null sentence) =
            [ "" ]
        | (T.head sentence) == '(' = do
            let (expression, sentence') = ((T.drop 1) *** (T.drop 1)) $ T.breakOn ")" sentence
            expandedExpression <- expandExpression expression
            expandedSentence' <- expandSentence' sentence'
            return $ expandedExpression `T.append` expandedSentence'
        | (T.head sentence) == '{' = do
            let (expression, sentence') = ((T.drop 1) *** (T.drop 1)) $ T.breakOn "}" sentence
            expandedExpression <- "" : (expandExpression expression)
            expandedSentence' <- expandSentence' sentence'
            return $ expandedExpression `T.append` expandedSentence'
        | otherwise = do
            let (expression, sentence') = T.break (`elem` ['(', '{']) sentence
            expandedSentence' <- expandSentence' sentence'
            return $ expression `T.append` expandedSentence'
    expandExpression :: T.Text -> [T.Text]
    expandExpression = T.splitOn "|"

-- | Expands the Lojban sentences in a 'Translation' using syntax such as (x|y|z) or {x|y|z} (see 'expandSentence').
expandTranslation :: Translation -> Translation
expandTranslation (lojban_sentences, english_sentences) = (expandSentences lojban_sentences, english_sentences)

-- | Expands the Lojban sentences in a 'TranslationGenerator' using syntax such as (x|y|z) or {x|y|z} (see 'expandSentence').
expandTranslationGenerator :: TranslationGenerator -> TranslationGenerator
expandTranslationGenerator translationGenerator r0 = (expandTranslation translation, r1) where
    (translation, r1) = translationGenerator r0

-- | Returns a 'Translation' containing only the first (i.e., canonical) Lojban sentence.
--
-- This function discards all Lojban sentences except for the first one.
-- Useful if you have a 'Translation' that you would like to display to the user, but some of its
-- sentences in Lojban use words that have not yet been taught (perhaps you added them to ensure that
-- translations made by more advanced users are still accepted by the validator).
-- By convention, the first translation is expected to be suitable for presentation to the user.
narrowTranslation :: Translation -> Translation
narrowTranslation (lojban_sentences, english_sentences) = ([head lojban_sentences], english_sentences)

-- | Decorates a TranslationGenerator so that the resulting translation contains only the first (canonical) Lojban sentence (see 'narrowTranslation').
narrowTranslationGenerator :: TranslationGenerator -> TranslationGenerator
narrowTranslationGenerator translationGenerator = translationGenerator' where
    translationGenerator' :: TranslationGenerator
    translationGenerator' r0 = (narrowTranslation originalTranslation, r1) where
        (originalTranslation, r1) = translationGenerator r0

-- | Decorates a TranslationGeneratorByExpression so that the resulting translation contains only the first (canonical) Lojban sentence (see 'narrowTranslation').
narrowTranslationGeneratorByExpression :: TranslationGeneratorByExpression -> TranslationGeneratorByExpression
narrowTranslationGeneratorByExpression = map (second narrowTranslationGenerator)
