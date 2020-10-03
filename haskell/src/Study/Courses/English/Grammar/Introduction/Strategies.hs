{-# LANGUAGE OverloadedStrings #-}

-- | This module establishes algorithmic strategies to be used throughout the course.
--
-- Currently, the only use for these strategies consists in evaluating student solutions to translation exercises.
-- The process is as follows:
--
-- 1. First, both the student's response and the model response are normalized using a configurable sentence canonicalizer.
-- 2. Then, the normalized responses are compared using a configurable sentence comparer.
-- 3. If they match, the student's solution is considered correct.
module Study.Courses.English.Grammar.Introduction.Strategies  where

import Core
import Language.Lojban.Core
import Language.Lojban.Canonicalization (extendedSentenceCanonicalizer)
import qualified Data.Text as T

-- | Default sentence comparer throughout the course.
--
-- Decides whether two sentences are equivalent taking into account the following allowances:
--
-- * The descriptors "lo" and "le" are interchangeable.
-- * The generic abstractor "su'u" is exchangeable with the more specific abstractors "nu", "du'u" and "ka".
-- * Tenses are optional, and may be missing from one or both of the sentences (however, if tenses are specified in both sentences, they must match).
--
--     * TODO: this still needs to be implemented
--
-- * Association words ("pe", "ne", "po" and "po'e") are interchangeable.
-- * The quote delimiters "lu\/li'u" and "lo'u/le'u" are interchangeable.
--
--     * Conveniently, it is nonetheless enforced that "lu\/li'u" must only be used around grammatical text, as otherwise the canonicalization fails (due to a parsing failure), and this yields a "mismatch" verdict.
sentenceComparer :: SentenceComparer
sentenceComparer x y = (length xs == length ys) && (all wordComparer $ zip xs ys) where
    xs = T.words x
    ys = T.words y
    isAssociationWord :: T.Text -> Bool
    isAssociationWord x = x `elem` [ "pe", "ne", "po", "po'e" ]
    isNonNameGadri x = x `elem` [ "lo", "le", "loi", "lei", "lo'e", "le'e" ]
    wordComparer :: (T.Text, T.Text) -> Bool
    wordComparer (x, y) = (wordComparer' x y) || (wordComparer' y x) || (isAssociationWord x && isAssociationWord y) || (isNonNameGadri x && isNonNameGadri y)
    wordComparer' "lu" "lo'u" = True
    wordComparer' "li'u" "le'u" = True
    wordComparer' "nu" "su'u" = True
    wordComparer' "du'u" "su'u" = True
    wordComparer' "ka" "su'u" = True
    wordComparer' x y = x == y

-- | Default sentence canonicalizer throughout the course.
sentenceCanonicalizer :: SentenceCanonicalizer
sentenceCanonicalizer = extendedSentenceCanonicalizer
