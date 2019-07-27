{-# LANGUAGE OverloadedStrings #-}

module Courses.Util.Sentences
( generateNonbridi
) where

import Courses.Util.Vocabulary
import Util (chooseItem)
import System.Random (StdGen)
import qualified Data.Text as T

-- * Sentence generators
generateNonbridi :: Vocabulary -> StdGen -> (T.Text, StdGen)
generateNonbridi vocabulary r0 = chooseItem r0 . concatMap (getVocabularySumti vocabulary) $
    ["genericPersons", "semiGenericPersons", "animals", "genericPointable", "places", "subjects"]
