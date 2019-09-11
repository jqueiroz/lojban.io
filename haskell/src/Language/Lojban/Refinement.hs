{-# LANGUAGE OverloadedStrings #-}

module Language.Lojban.Refinement
( simplifyTerminatorsInSentence
, simplifyTerminatorsInBridiDisplayer
) where

import Language.Lojban.Core
import Language.Lojban.Canonicalization (basicSentenceCanonicalizer)
import Control.Arrow ((***))
import System.Random (StdGen)
import Util (compose2)
import qualified Data.Text as T

-- * Terminator simplification
-- | Simplifies sentences by replacing terminators with "cu" whenever possible.
--
-- Example: "lo mlatu ku pinxe lo ladru ku" -> "lo mlatu cu pinxe lo ladru ku".
replaceElidableTerminatorsInSentence :: T.Text -> T.Text
replaceElidableTerminatorsInSentence t = f [] (T.words t) where
    originalCanonicalization = basicSentenceCanonicalizer t
    f :: [T.Text] -> [T.Text] -> T.Text
    f x [] = T.unwords x
    f x (y:ys) = if basicSentenceCanonicalizer (T.unwords $ x++("cu":ys)) == originalCanonicalization then f (x++["cu"]) ys else f (x++[y]) ys

-- | Simplifies sentences by removing redundant elidable terminators ("ku", "kei", etc.).
--
-- Example: "lo mlatu ku pinxe lo ladru ku" -> "lo mlatu ku pinxe lo ladru".
removeElidableTerminatorsInSentence :: T.Text -> T.Text
removeElidableTerminatorsInSentence t = f [] (T.words t) where
    originalCanonicalization = basicSentenceCanonicalizer t
    f :: [T.Text] -> [T.Text] -> T.Text
    f x [] = T.unwords x
    f x (y:ys) = if basicSentenceCanonicalizer (T.unwords $ x++ys) == originalCanonicalization then f x ys else f (x++[y]) ys

-- | Simplifies sentences by removing elidable terminators and/or replacing them with "cu".
--
-- Example: "lo mlatu ku pinxe lo ladru ku" -> "lo mlatu cu pinxe lo ladru".
simplifyTerminatorsInSentence :: T.Text -> T.Text
simplifyTerminatorsInSentence = removeElidableTerminatorsInSentence . replaceElidableTerminatorsInSentence

-- | Decorates the displayer so that the resulting bridi is simplified using 'simplifyTerminatorsInSentence'.
simplifyTerminatorsInBridiDisplayer :: SimpleBridiDisplayer -> SimpleBridiDisplayer
simplifyTerminatorsInBridiDisplayer bridiDisplayer = simplifySentence `compose2` bridiDisplayer where
    simplifySentence :: (T.Text, StdGen) -> (T.Text, StdGen)
    simplifySentence = simplifyTerminatorsInSentence *** id
