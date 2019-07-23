{-# LANGUAGE OverloadedStrings #-}

-- | This module establishes algorithmic strategies to be used throughout the course.
--
-- Currently, these strategies are used solely for evaluating student solutions to translation exercises.
-- First, both the student's response and the model response are normalized using a configurable sentence canonicalizer.
-- Then, the normalized responses are compared using a configurable sentence comparer.
-- If they match, the student's solution is considered correct.
module Courses.English.Grammar.Introduction.Strategies  where

import Core
import Language.Lojban.Core
import Language.Lojban.Canonicalization (extendedSentenceCanonicalizer)
import qualified Data.Text as T

-- | Default sentence comparer.
--
-- Decides whether two sentences are equivalent taking into account the following allowances:
--
-- * The descriptors "lo" and "le" are interchangeable.
-- * The generic abstractor "su'u" is exchangeable with the more specific abstractors "nu" and "du'u".
-- * Tenses are optional, and may be missing from one or both of the sentences (however, if tenses are specified in both sentences, they must match).
--
--     * TODO: this still needs to be implemented
--
-- * Association words ("pe", "ne", "po" and "po'e") are interchangeable.
-- * The quote delimiters "lu\/li'u" and "lo'u/le'u" are interchangeable.
--
--     * Conveniently, it is nonetheless enforced that "lu\/li'u" must only be used around grammatical text, as otherwise the canonicalization fails due to a parsing failure.
sentenceComparer :: SentenceComparer
sentenceComparer x y = (length xs == length ys) && (all wordComparer $ zip xs ys) where
    xs = T.words x
    ys = T.words y
    isAssociationWord :: T.Text -> Bool
    isAssociationWord x = x `elem` [ "pe", "ne", "po", "po'e" ]
    wordComparer :: (T.Text, T.Text) -> Bool
    wordComparer (x, y) = (wordComparer' x y) || (wordComparer' y x) || (isAssociationWord x && isAssociationWord y)
    wordComparer' "lo" "le" = True
    wordComparer' "lu" "lo'u" = True
    wordComparer' "li'u" "le'u" = True
    wordComparer' "nu" "su'u" = True
    wordComparer' "du'u" "su'u" = True
    wordComparer' x y = x == y

-- | Default sentence canonicalizer.
sentenceCanonicalizer :: SentenceCanonicalizer
sentenceCanonicalizer = extendedSentenceCanonicalizer
