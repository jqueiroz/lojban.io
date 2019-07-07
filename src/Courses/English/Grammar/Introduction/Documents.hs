{-# LANGUAGE TemplateHaskell #-}

-- | This module defines documents pertaining to the course, such as lesson materials and lesson plans.
module Courses.English.Grammar.Introduction.Documents where

import Data.FileEmbed (embedStringFile)
import qualified Data.Text as T
import qualified Text.Pandoc as P

-- * Lesson plans

-- | Plan for the first lesson.
plan1 :: P.Pandoc
Right plan1 = P.runPure $ P.readMarkdown P.def $ $(embedStringFile "courses/english/grammar/introduction/planning/01.md")

-- | Plan for the second lesson.
plan2 :: P.Pandoc
Right plan2 = P.runPure $ P.readMarkdown P.def $ $(embedStringFile "courses/english/grammar/introduction/planning/02.md")

-- | Plan for the third lesson.
plan3 :: P.Pandoc
Right plan3 = P.runPure $ P.readMarkdown P.def $ $(embedStringFile "courses/english/grammar/introduction/planning/03.md")

-- | Plan for the fourth lesson.
plan4 :: P.Pandoc
Right plan4 = P.runPure $ P.readMarkdown P.def $ $(embedStringFile "courses/english/grammar/introduction/planning/04.md")

-- | Plan for the fifth lesson.
plan5 :: P.Pandoc
Right plan5 = P.runPure $ P.readMarkdown P.def $ $(embedStringFile "courses/english/grammar/introduction/planning/05.md")

-- | Plan for the sixth lesson.
plan1to5 :: P.Pandoc
Right plan1to5 = P.runPure $ P.readMarkdown P.def $ T.pack ""

-- | Plan for the seventh lesson.
plan7 :: P.Pandoc
Right plan7 = P.runPure $ P.readMarkdown P.def $ $(embedStringFile "courses/english/grammar/introduction/planning/07.md")

-- | Plan for the eighth lesson.
plan8 :: P.Pandoc
Right plan8 = P.runPure $ P.readMarkdown P.def $ $(embedStringFile "courses/english/grammar/introduction/planning/08.md")

-- | Plan for the nineth lesson.
plan9 :: P.Pandoc
Right plan9 = P.runPure $ P.readMarkdown P.def $ $(embedStringFile "courses/english/grammar/introduction/planning/09.md")

-- | Plan for the tenth lesson.
plan10 :: P.Pandoc
Right plan10 = P.runPure $ P.readMarkdown P.def $ $(embedStringFile "courses/english/grammar/introduction/planning/10.md")

-- | Plan for the eleventh lesson.
plan11 :: P.Pandoc
Right plan11 = P.runPure $ P.readMarkdown P.def $ $(embedStringFile "courses/english/grammar/introduction/planning/11.md")

-- | Plan for the twelveth lesson.
plan7to11 :: P.Pandoc
Right plan7to11 = P.runPure $ P.readMarkdown P.def $ T.pack ""
