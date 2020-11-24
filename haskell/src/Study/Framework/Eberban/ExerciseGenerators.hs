{-# LANGUAGE OverloadedStrings #-}

-- | This module provides utilities for constructing exercise generators.
module Study.Framework.Eberban.ExerciseGenerators
( generateLexiconProvidingExercise
) where

import Core
import Language.Eberban.Core
import Util (combineGenerators, combineGeneratorsUniformly)
import System.Random (StdGen)
import qualified Data.Text as T
import qualified Data.Map as M

-- Exercise: provide the lexicon
generateLexiconProvidingExercise :: T.Text -> EntryGenerator -> ExerciseGenerator
generateLexiconProvidingExercise lexiconCategory entryGenerator r0 = TypingExercise title sentences validator canonicalAnswer where
    (entry, r1) = entryGenerator r0
    title = "Provide the " `T.append` lexiconCategory
    sentences = [ExerciseSentence True (entryEnglishLong entry)]
    validator attemptedSolution = (T.toLower $ attemptedSolution) == (T.toLower $ canonicalAnswer)
    canonicalAnswer = entryText entry
