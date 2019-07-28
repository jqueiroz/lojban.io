{-# LANGUAGE OverloadedStrings #-}

-- | This module provides translations (and exercises relying on translations) for each of the course lessons.
module Courses.English.Grammar.Crash.Translations where

import Core
import Util (generatorFromList)

-- * Lesson 1: Basics 1

-- | Translations for the first lesson.
translations01 :: TranslationGenerator
translations01 = generatorFromList
    [ (["lo mlatu cu pinxe lo ladru"], ["Cats drink milk."])
    , (["lo plise cu kukte"], ["Apples are tasty."])
    , (["karce"], ["Car!"])
    , (["carvi"], ["It is raining."])
    ]

-- * Lesson 2: Basics 2

-- | Translations for the second lesson.
translations02 :: TranslationGenerator
translations02 = generatorFromList
    [ (["mi pinxe"], ["I drink."])
    , (["mi pinxe"], ["You drink."])
    , (["ti ladru"], ["This is some milk."])
    , (["(tu|ta) mlatu"], ["That is a cat."])
    , (["do citka lo plise"], ["You like apples."])
    ]
