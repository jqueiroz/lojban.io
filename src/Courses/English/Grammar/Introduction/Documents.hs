{-# LANGUAGE TemplateHaskell #-}

module Courses.English.Grammar.Introduction.Documents where

import Data.FileEmbed (embedStringFile)
import qualified Data.Text as T
import qualified Text.Pandoc as P

------- Lesson plans
plan1 :: P.Pandoc
Right plan1 = P.runPure $ P.readMarkdown P.def $ $(embedStringFile "courses/english/grammar/introduction/planning/1.md")

plan2 :: P.Pandoc
Right plan2 = P.runPure $ P.readMarkdown P.def $ $(embedStringFile "courses/english/grammar/introduction/planning/2.md")

plan3 :: P.Pandoc
Right plan3 = P.runPure $ P.readMarkdown P.def $ $(embedStringFile "courses/english/grammar/introduction/planning/3.md")

plan4 :: P.Pandoc
Right plan4 = P.runPure $ P.readMarkdown P.def $ $(embedStringFile "courses/english/grammar/introduction/planning/4.md")

plan5 :: P.Pandoc
Right plan5 = P.runPure $ P.readMarkdown P.def $ $(embedStringFile "courses/english/grammar/introduction/planning/5.md")

plan1to5 :: P.Pandoc
Right plan1to5 = P.runPure $ P.readMarkdown P.def $ T.pack ""

plan7 :: P.Pandoc
Right plan7 = P.runPure $ P.readMarkdown P.def $ $(embedStringFile "courses/english/grammar/introduction/planning/7.md")

plan8 :: P.Pandoc
Right plan8 = P.runPure $ P.readMarkdown P.def $ $(embedStringFile "courses/english/grammar/introduction/planning/8.md")

plan9 :: P.Pandoc
Right plan9 = P.runPure $ P.readMarkdown P.def $ $(embedStringFile "courses/english/grammar/introduction/planning/9.md")

plan10 :: P.Pandoc
Right plan10 = P.runPure $ P.readMarkdown P.def $ $(embedStringFile "courses/english/grammar/introduction/planning/10.md")
