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
-- * New brivla: NONE
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

-- | Vocabulary for the third lesson.
--
-- * New brivla: stati, prenu, klama, nelci, zarci
-- * New cmavo: pa, re, ci, vo, mu, xa, ze, bi, so, no
vocabulary03 :: Vocabulary
vocabulary03 = Vocabulary
    -- Brivla
    [
        "stati", "prenu", "klama", "nelci", "zarci"
    ]
    -- Cmavo
    [
        "pa", "re", "ci", "vo", "mu", "xa", "ze", "bi", "so", "no"
    ]
    -- Cmene
    [
    ]

-- | Cumulative vocabulary up to the third lesson.
vocabulary03_cumulative :: Vocabulary
vocabulary03_cumulative = vocabulary02_cumulative <> vocabulary03
