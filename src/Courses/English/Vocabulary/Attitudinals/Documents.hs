{-# LANGUAGE TemplateHaskell #-}

-- | This module defines documents pertaining to the course, such as lesson materials and lesson plans.
module Courses.English.Vocabulary.Attitudinals.Documents where

import Data.FileEmbed (embedStringFile)
import qualified Text.Pandoc as P

-- * Lesson plans

-- | Plan for the first lesson.
plan1 :: P.Pandoc
Right plan1 = P.runPure $ P.readMarkdown P.def $ $(embedStringFile "courses/english/vocabulary/attitudinals/planning/01.md")

-- | Plan for the second lesson.
plan2 :: P.Pandoc
Right plan2 = P.runPure $ P.readMarkdown P.def $ $(embedStringFile "courses/english/vocabulary/attitudinals/planning/02.md")

-- | Plan for the third lesson.
plan3 :: P.Pandoc
Right plan3 = P.runPure $ P.readMarkdown P.def $ $(embedStringFile "courses/english/vocabulary/attitudinals/planning/03.md")
