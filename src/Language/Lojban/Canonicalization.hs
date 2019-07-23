module Language.Lojban.Canonicalization
( basicSentenceCanonicalizer
) where

import Language.Lojban.Core
import Language.Lojban.Canonicalization.Internals

basicSentenceCanonicalizer :: SentenceCanonicalizer
basicSentenceCanonicalizer = canonicalizeText
