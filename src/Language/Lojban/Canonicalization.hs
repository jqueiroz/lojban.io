{-# LANGUAGE OverloadedStrings #-}

module Language.Lojban.Canonicalization
( basicSentenceCanonicalizer
, extendedSentenceCanonicalizer
, extractSimpleBridi
) where

import Language.Lojban.Core
import Language.Lojban.Canonicalization.Internals
import Util (replaceSubexpression)

basicSentenceCanonicalizer :: SentenceCanonicalizer
basicSentenceCanonicalizer = canonicalizeText

extendedSentenceCanonicalizer :: SentenceCanonicalizer
extendedSentenceCanonicalizer = basicSentenceCanonicalizer . replaceQuantifiers where
    replaceQuantifiers = (replaceSubexpression "da" "zo'e") . (replaceSubexpression "de" "zo'e") . (replaceSubexpression "di" "zo'e")
