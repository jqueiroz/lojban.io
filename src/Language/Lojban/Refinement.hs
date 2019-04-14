{-# LANGUAGE OverloadedStrings #-}

module Language.Lojban.Refinement
( simplifyTerminatorsInSentence
, replaceElidableTerminatorsInSentence
, removeElidableTerminatorsInSentence
) where

import Language.Lojban.Canonicalization (basicSentenceCanonicalizer)
import qualified Data.Text as T

------------------------------------------------- Terminator ellisis
-- removeElidableTerminators :: ZG.Text -> Either String T.Text
-- removeElidableTerminators (ZG.LE (ZG.Init i) _ _ x _) =
-- removeElidableTerminators (ZG.Terms terms _) =
-- removeElidableTerminators (ZG.BridiTail (ZG.BRIVLA selbri) terms) = selbri `T.append` removeTerminators terms
-- removeElidableTerminators (ZG.BridiTail (ZG.GOhA selbri) terms) = selbri `T.append` removeTerminators terms
-- TODO: make this function way more efficient and use the following brute-force version only in unit tests -- nah, probably not needed

-- Replaces "ku" with "cu" whenever possible
replaceElidableTerminatorsInSentence :: T.Text -> T.Text
replaceElidableTerminatorsInSentence t = f [] (T.words t) where
    originalCanonicalization = basicSentenceCanonicalizer t
    f :: [T.Text] -> [T.Text] -> T.Text
    f x [] = T.unwords x
    f x (y:ys) = if basicSentenceCanonicalizer (T.unwords $ x++("cu":ys)) == originalCanonicalization then f (x++["cu"]) ys else f (x++[y]) ys

-- Removes redundant "ku", "kei", etc
removeElidableTerminatorsInSentence :: T.Text -> T.Text
removeElidableTerminatorsInSentence t = f [] (T.words t) where
    originalCanonicalization = basicSentenceCanonicalizer t
    f :: [T.Text] -> [T.Text] -> T.Text
    f x [] = T.unwords x
    f x (y:ys) = if basicSentenceCanonicalizer (T.unwords $ x++ys) == originalCanonicalization then f x ys else f (x++[y]) ys

simplifyTerminatorsInSentence :: T.Text -> T.Text
simplifyTerminatorsInSentence = removeElidableTerminatorsInSentence . replaceElidableTerminatorsInSentence
