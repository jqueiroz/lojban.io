{-# LANGUAGE OverloadedStrings #-}

module Language.Lojban.Presentation
( displayCanonicalBridi
, displayStandardSimpleBridi
, displayVariantSimpleBridi
, displayReorderedStandardSimpleBridi
) where

import Language.Lojban.Core
import Control.Arrow ((***))
import Control.Exception (assert)
import Util (replace, stripRight, chooseItemUniformly)
import qualified Data.Text as T
import System.Random (StdGen, mkStdGen)

-- The following function keeps trailing empty places, if present
swapSimpleBridiArguments :: String -> SimpleBridi -> SimpleBridi
swapSimpleBridiArguments particle (SimpleBridi xu selbri sumti extraSumti) = SimpleBridi xu selbri sumti''' extraSumti where
    sumti' = sumti ++ replicate 5 (T.pack "///")
    sumti'' = swapArguments particle sumti'
    sumti''' = replace (T.pack "///") (T.pack "") . stripRight "///" $ sumti''

swapArguments :: String -> [T.Text] -> [T.Text]
swapArguments "se" (a:b:cs) = (b:a:cs)
swapArguments "te" (a:b:c:ds) = (c:b:a:ds)
swapArguments "ve" (a:b:c:d:es) = (d:b:c:a:es)
swapArguments "xe" (a:b:c:d:e:fs) = (e:b:c:d:a:fs)

------------------------- ------------------------ Sentence displayers
-- TODO: use fa/fe/fi/fo/fu if convenient

prependXu :: Bool -> ([T.Text], StdGen) -> ([T.Text], StdGen)
prependXu True = ("xu":) *** id
prependXu False = id

buildSentenceDisplayer :: (StdGen -> SimpleBridi -> ([T.Text], StdGen)) -> SimpleBridiDisplayer
buildSentenceDisplayer sentenceDisplayer r0 simpleBridi = (T.unwords $ replace "" "zo'e" sentence, r1) where
    (sentence, r1) = sentenceDisplayer r0 simpleBridi

-- | Displays bridi in canonical form.
--
-- The main motivation for this function lies in determining bridi equivalence.
-- Essentially, two bridi are syntactically equivalent if and only if they yield the same representation after canonicalization.
displayCanonicalBridi :: SimpleBridi -> T.Text
displayCanonicalBridi = fst . displayStandardSimpleBridi (mkStdGen 42)

-- | Displays bridi in standard form: [x1] selbri x2 x3 x4 x5 (...) xn.
--
-- * Ellisis occurs in the first place and in the last places.
-- * All other missing places are filled with "zo'e".
displayStandardSimpleBridi :: StdGen -> SimpleBridi -> (T.Text, StdGen)
displayStandardSimpleBridi = buildSentenceDisplayer $ \r0 (SimpleBridi xu selbri sumti extraSumti) ->
    let
        (sumtiHead, sumtiTail) = splitAt 1 sumti
        sentence = (if sumtiHead == [""] then [] else sumtiHead) ++ [selbri] ++ (stripRight "" sumtiTail)
    in
        prependXu xu $
        (extraSumti ++ sentence, r0)

-- | Displays bridi with a random number of places before the selbri.
--
-- * Special case: if the first place is empty, then this function falls back to 'displayStandardSimpleBridi'.
-- * Ellisis occurs in the last places.
-- * All other missing places are filled with "zo'e".
--
-- Possible outputs are:
--
-- * selbri x1 x2 x3 x4 x5 (...) xn;
-- * x1 selbri x2 x3 x4 x5 (...) xn;
-- * x1 x2 selbri x3 x4 x5 (...) xn; and so on.
displayVariantSimpleBridi :: StdGen -> SimpleBridi -> (T.Text, StdGen)
displayVariantSimpleBridi = buildSentenceDisplayer $ \r0 (SimpleBridi xu selbri sumti extraSumti) ->
    let
        (sumtiHead, sumtiTail) = splitAt 1 sumti
    in
        prependXu xu $
        if sumtiHead == [""] then
            (selbri : (stripRight "" sumtiTail), r0)
        else
            let
                (beforeCount, r1) = chooseItemUniformly r0 [1..length sumti]
                (sumtiBefore, sumtiAfter) = splitAt beforeCount sumti
            in
                (extraSumti ++ sumtiBefore ++ [selbri] ++ sumtiAfter, r1)

-- The bridi is displayed as in 'displayStandardSimpleBridi', but if the x1 is missing then its position is used to hold the last place
--   * Exception: if there are more than five sumti places, then "displayStandardSimpleBridi" is used after all
displayPossiblyReorderedStandardSimpleBridi :: StdGen -> SimpleBridi -> (T.Text, StdGen)
displayPossiblyReorderedStandardSimpleBridi r0 bridi
    | length sumti <= 1 = displayStandardSimpleBridi r0 bridi
    | length sumti >= 6 = displayStandardSimpleBridi r0 bridi
    | head sumti == ""  = displayPossiblyReorderedStandardSimpleBridi' r0 bridi
    | otherwise         = displayStandardSimpleBridi r0 bridi
    where sumti = stripRight "" $ simpleBridiSumti bridi

displayPossiblyReorderedStandardSimpleBridi' :: StdGen -> SimpleBridi -> (T.Text, StdGen)
displayPossiblyReorderedStandardSimpleBridi' = buildSentenceDisplayer $ \r0 (SimpleBridi xu selbri sumti extraSumti) ->
    let
        particle = ["se", "te", "ve", "xe"] !! (length sumti - 2)
        sumti' = stripRight "" $ swapArguments particle sumti
        sentence = head sumti' : (T.pack particle) : selbri : (tail sumti')
    in
        assert (length sumti >= 2 && length sumti <= 5 && head sumti == "" && last sumti /= "") $
        prependXu xu $
        (extraSumti ++ sentence, r0)

-- The bridi is displayed with a single place swap
--   * Exception: if the first place is empty or there are fewer than two places, then this function behaves as 'displayStandardSimpleBridi'
displayReorderedStandardSimpleBridi :: StdGen -> SimpleBridi -> (T.Text, StdGen)
displayReorderedStandardSimpleBridi r0 bridi
    | length sumti <= 1 = displayStandardSimpleBridi r0 bridi
    | head sumti == ""  = displayPossiblyReorderedStandardSimpleBridi r0 bridi
    | otherwise         = displayReorderedStandardSimpleBridi' r0 bridi
    where sumti = stripRight "" $ simpleBridiSumti bridi

displayReorderedStandardSimpleBridi' :: StdGen -> SimpleBridi -> (T.Text, StdGen)
displayReorderedStandardSimpleBridi' = buildSentenceDisplayer $ \r0 (SimpleBridi xu selbri sumti extraSumti) ->
    let
        particles = take (length sumti - 1) ["se", "te", "ve", "xe"]
        (particle, r1) = chooseItemUniformly r0 particles
        sumti' = swapArguments particle sumti
        sentence = head sumti' : (T.pack particle) : selbri : tail sumti'
    in
        assert (length sumti >= 2 && head sumti /= "" && last sumti /= "") $
        prependXu xu $
        (extraSumti ++ sentence, r1)
