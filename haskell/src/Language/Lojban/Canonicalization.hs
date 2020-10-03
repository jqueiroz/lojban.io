{-# LANGUAGE OverloadedStrings #-}

module Language.Lojban.Canonicalization
( basicSentenceCanonicalizer
, extendedSentenceCanonicalizer
, extractSimpleBridi
) where

import Language.Lojban.Core
import Language.Lojban.Canonicalization.Internals
import Util (replaceSubexpression)

-- | Basic general-purpose sentence canonicalizer.
basicSentenceCanonicalizer :: SentenceCanonicalizer
basicSentenceCanonicalizer = canonicalizeText

-- | Simple extension of 'basicSentenceCanonicalizer' which disregards (potentially important) semantic variations.
--
-- Currently, all it does is replace "da", "de", and "di" with "zo'e", essentially treating all these quantifiers as the equivalent.
extendedSentenceCanonicalizer :: SentenceCanonicalizer
extendedSentenceCanonicalizer = basicSentenceCanonicalizer . replaceQuantifiers where
    replaceQuantifiers = (replaceSubexpression "da" "zo'e") . (replaceSubexpression "de" "zo'e") . (replaceSubexpression "di" "zo'e")
