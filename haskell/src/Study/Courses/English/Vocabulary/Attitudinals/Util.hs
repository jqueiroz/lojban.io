{-# LANGUAGE OverloadedStrings #-}

-- | This module defines utility functions for the course.
module Study.Courses.English.Vocabulary.Attitudinals.Util where

import Study.Courses.English.Vocabulary.Attitudinals.Model
import Util (chooseItemUniformly)
import Data.Maybe (catMaybes)
import System.Random (StdGen)
import qualified Data.Text as T

-- | Randomly picks one of the available modifiers for a given attitudinal (among "", "cu'i" and "nai").
randomlyPickAttitudinalModifier :: StdGen                       -- ^ A random number generator.
                                -> Attitudinal                  -- ^ An attitudinal.
                                -> (T.Text, T.Text, StdGen)     -- ^ A tuple consisting of
                                                                --
                                                                --     1. the selected attitudinal modifier;
                                                                --     2. the meaning of the corresponding modified attitudinal; and
                                                                --     3. a new random number generator.
randomlyPickAttitudinalModifier r0 attitudinal = (attitudinalModifier, meaningOfModifierAttitudinal, r1) where
    (meaningOfModifierAttitudinal, r1) = randomlyPickAttitudinalMeaning r0 attitudinal
    modifiedAttitudinalExpression = (attitudinalWord attitudinal) `T.append` attitudinalModifier
    attitudinalModifier
        | (Just meaningOfModifierAttitudinal) == (attitudinalNeutralMeaning attitudinal) = "cu'i"
        | (Just meaningOfModifierAttitudinal) == (attitudinalNegativeMeaning attitudinal) ="nai"
        | otherwise = ""

-- | Randomly picks one of the available meanings for a given attitudinal (positive, neutral or negative).
randomlyPickAttitudinalMeaning :: StdGen -> Attitudinal -> (T.Text, StdGen)
randomlyPickAttitudinalMeaning r0 attitudinal = chooseItemUniformly r0 $ catMaybes [Just $ attitudinalPositiveMeaning attitudinal, attitudinalNeutralMeaning attitudinal, attitudinalNegativeMeaning attitudinal]
