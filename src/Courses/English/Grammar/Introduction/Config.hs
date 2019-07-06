{-# LANGUAGE OverloadedStrings #-}

module Courses.English.Grammar.Introduction.Config  where

import Core
import Language.Lojban.Core
import Language.Lojban.Canonicalization (basicSentenceCanonicalizer)
import qualified Data.Text as T

-- Sentence comparer
sentenceComparer :: SentenceComparer
sentenceComparer x y = (length xs == length ys) && (all wordComparer $ zip xs ys) where
    xs = T.words x
    ys = T.words y
    wordComparer :: (T.Text, T.Text) -> Bool
    wordComparer (x, y) = (wordComparer' x y) || (wordComparer' y x)
    wordComparer' "nu" "su'u" = True
    wordComparer' "du'u" "su'u" = True
    wordComparer' x y = x == y

-- Sentence canonicalizer
sentenceCanonicalizer :: SentenceCanonicalizer
sentenceCanonicalizer = basicSentenceCanonicalizer
