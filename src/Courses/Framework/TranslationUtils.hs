{-# LANGUAGE OverloadedStrings #-}

module Courses.Framework.TranslationUtils
( simplifyTranslation
, simplifyTranslationGenerator
, expandSentence
, expandSentences
, expandTranslation
, expandTranslationGenerator
) where

import Core
import Language.Lojban.Refinement (simplifyTerminatorsInSentence)
import Control.Monad (join)
import Control.Arrow ((***))
import qualified Data.Text as T

-- | Simplifies a 'Translation' by removing elidable terminators and/or replacing them with "cu" (see 'simplifyTerminatorsInSentence').
simplifyTranslation :: Translation -> Translation
simplifyTranslation (lojbanSentences, englishSentences) = (fmap simplifyTerminatorsInSentence lojbanSentences, englishSentences)

-- | Simplifies a 'TranslationGenerator' by removing elidable terminators and/or replacing them with "cu" (see 'simplifyTerminatorsInSentence').
simplifyTranslationGenerator :: TranslationGenerator -> TranslationGenerator
simplifyTranslationGenerator translationGenerator r0 = (simplifyTranslation translation, r1) where
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

-- | Expands the Lojban sentences in a 'Translations' using syntax such as (x|y|z) or {x|y|z} (see 'expandSentence').
expandTranslation :: Translation -> Translation
expandTranslation (lojban_sentences, english_sentences) = (expandSentences lojban_sentences, english_sentences)

-- | Expands the Lojban sentences in a 'TranslationsGenerator' using syntax such as (x|y|z) or {x|y|z} (see 'expandSentence').
expandTranslationGenerator :: TranslationGenerator -> TranslationGenerator
expandTranslationGenerator translationGenerator r0 = (expandTranslation translation, r1) where
    (translation, r1) = translationGenerator r0
