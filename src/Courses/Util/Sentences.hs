{-# LANGUAGE OverloadedStrings #-}

module Courses.Util.Sentences
( generateNonbridi
) where

import Core
import Util (generatorFromList)
import Data.List (intersect)
import System.Random (StdGen)
import qualified Data.Text as T

-- * Sentence generators
generateNonbridi :: Vocabulary -> StdGen -> (T.Text, StdGen)
generateNonbridi vocabulary = generatorFromList $ pronouns ++ nouns where
    pronouns = cmavoList `intersect` [ "mi", "do", "ti", "ta" ]
    nouns =
        if "lo" `elem` cmavoList then
            ["lo " `T.append` brivla `T.append` " ku" | brivla <- brivlaList]
        else
            []
    cmavoList = vocabularyCmavoList vocabulary
    brivlaList = vocabularyBrivlaList vocabulary
