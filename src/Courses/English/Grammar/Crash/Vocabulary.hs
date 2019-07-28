{-# LANGUAGE OverloadedStrings #-}

-- | This module defines vocabulary for the course.
module Courses.English.Grammar.Crash.Vocabulary where

import Core

-- | Vocabulary for the first lesson.
--
-- * Starting brivla: dunda, pelxu, zdani
-- * Starting cmavo: lo, cu
vocabulary01 :: Vocabulary
vocabulary01 = Vocabulary
    -- Brivla
    [
        "mlatu", "pinxe", "ladru", "plise", "kukte", "karce", "carvi"
    ]
    -- Cmavo
    [
        "lo", "cu"
    ]
    -- Cmene
    [
    ]

-- | Cumulative vocabulary up to the first lesson.
vocabulary01_cumulative :: Vocabulary
vocabulary01_cumulative = vocabulary01

-- | Vocabulary for the second lesson.
--
-- * New brivla: NONE.
-- * New cmavo: mi, do, ti, ta, tu.
vocabulary02 :: Vocabulary
vocabulary02 = Vocabulary
    -- Brivla
    [
    ]
    -- Cmavo
    [
        "mi", "do",
        "ti", "ta", "tu"
    ]
    -- Cmene
    [
    ]

-- | Cumulative vocabulary up to the second lesson.
vocabulary02_cumulative :: Vocabulary
vocabulary02_cumulative = vocabulary01_cumulative <> vocabulary02
