{-# LANGUAGE TemplateHaskell #-}

-- | This module defines documents pertaining to the course, such as lesson materials and lesson plans.
module Courses.English.Vocabulary.Attitudinals.Documents where

import Data.FileEmbed (embedStringFile)
import qualified Text.Pandoc as P

-- * Lesson plans

-- | Plan for the first lesson.
plan1 :: P.Pandoc
Right plan1 = P.runPure $ P.readMarkdown P.def $ $(embedStringFile "courses/english/vocabulary/attitudinals/planning/01.md")
