{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module defines documents pertaining to the course, such as lesson materials and lesson plans.
module Courses.English.Grammar.Introduction.Documents where

import Courses.Framework.DocumentBuilders (buildDocumentFromMarkdownCode)
import Data.FileEmbed (embedStringFile)
import qualified Text.Pandoc as P

-- * Lesson contents
-- | Lecture for the lesson.
lecture1 :: P.Pandoc
Right lecture1 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/grammar/introduction/lectures/01.md")

-- | Lecture for the lesson.
lecture2 :: P.Pandoc
Right lecture2 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/grammar/introduction/lectures/02.md")

-- | Lecture for the lesson.
lecture3 :: P.Pandoc
Right lecture3 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/grammar/introduction/lectures/03.md")

-- | Lecture for the lesson.
lecture4 :: P.Pandoc
Right lecture4 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/grammar/introduction/lectures/04.md")

-- | Lecture for the lesson.
lecture5 :: P.Pandoc
Right lecture5 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/grammar/introduction/lectures/05.md")

-- | Lecture for the lesson.
lecture6 :: P.Pandoc
Right lecture6 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/grammar/introduction/lectures/06.md")

-- | Lecture for the lesson.
lecture7 :: P.Pandoc
Right lecture7 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/grammar/introduction/lectures/07.md")

-- | Lecture for the lesson.
lecture1to7 :: P.Pandoc
Right lecture1to7 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/grammar/introduction/lectures/checkpoint_01to07.md")

-- | Lecture for the lesson.
lecture9 :: P.Pandoc
Right lecture9 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/grammar/introduction/lectures/09.md")

-- | Lecture for the lesson.
lecture10 :: P.Pandoc
Right lecture10 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/grammar/introduction/lectures/10.md")

-- | Lecture for the lesson.
lecture11 :: P.Pandoc
Right lecture11 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/grammar/introduction/lectures/11.md")

-- | Lecture for the lesson.
lecture12 :: P.Pandoc
Right lecture12 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/grammar/introduction/lectures/12.md")

-- | Lecture for the lesson.
lecture9to12 :: P.Pandoc
Right lecture9to12 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/grammar/introduction/lectures/checkpoint_09to12.md")

-- | Lecture for the lesson.
lecture14 :: P.Pandoc
Right lecture14 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/grammar/introduction/lectures/14.md")

-- | Lecture for the lesson.
lecture15 :: P.Pandoc
Right lecture15 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/grammar/introduction/lectures/15.md")

-- | Lecture for the lesson.
lecture16 :: P.Pandoc
Right lecture16 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/grammar/introduction/lectures/16.md")

-- | Lecture for the lesson.
lecture17 :: P.Pandoc
Right lecture17 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/grammar/introduction/lectures/17.md")

-- | Lecture for the lesson.
lecture18 :: P.Pandoc
Right lecture18 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/grammar/introduction/lectures/18.md")

-- | Lecture for the lesson.
lecture14to18 :: P.Pandoc
Right lecture14to18 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/grammar/introduction/lectures/checkpoint_14to18.md")

-- | Lecture for the lesson.
lecture20 :: P.Pandoc
Right lecture20 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/grammar/introduction/lectures/20.md")

-- | Lecture for the lesson.
lecture21 :: P.Pandoc
Right lecture21 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/grammar/introduction/lectures/21.md")

-- | Lecture for the lesson.
lecture22 :: P.Pandoc
Right lecture22 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/grammar/introduction/lectures/22.md")

-- | Lecture for the lesson.
lecture23 :: P.Pandoc
Right lecture23 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/grammar/introduction/lectures/23.md")

-- | Lecture for the lesson.
lecture24 :: P.Pandoc
Right lecture24 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/grammar/introduction/lectures/24.md")

-- | Lecture for the lesson.
lecture20to24 :: P.Pandoc
Right lecture20to24 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/grammar/introduction/lectures/checkpoint_20to24.md")

-- | Lecture for the lesson.
lecture26 :: P.Pandoc
Right lecture26 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/grammar/introduction/lectures/26.md")

-- * Lesson plans

-- | Plan for the lesson.
plan1 :: P.Pandoc
Right plan1 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/grammar/introduction/planning/01.md")

-- | Plan for the lesson.
plan2 :: P.Pandoc
Right plan2 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/grammar/introduction/planning/02.md")

-- | Plan for the lesson.
plan3 :: P.Pandoc
Right plan3 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/grammar/introduction/planning/03.md")

-- | Plan for the lesson.
plan4 :: P.Pandoc
Right plan4 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/grammar/introduction/planning/04.md")

-- | Plan for the lesson.
plan5 :: P.Pandoc
Right plan5 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/grammar/introduction/planning/05.md")

-- | Plan for the lesson.
plan6 :: P.Pandoc
Right plan6 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/grammar/introduction/planning/06.md")

-- | Plan for the lesson.
plan7 :: P.Pandoc
Right plan7 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/grammar/introduction/planning/07.md")

-- | Plan for the lesson.
plan1to7 :: P.Pandoc
Right plan1to7 = buildDocumentFromMarkdownCode ""

-- | Plan for the lesson.
plan9 :: P.Pandoc
Right plan9 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/grammar/introduction/planning/09.md")

-- | Plan for the lesson.
plan10 :: P.Pandoc
Right plan10 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/grammar/introduction/planning/10.md")

-- | Plan for the lesson.
plan11 :: P.Pandoc
Right plan11 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/grammar/introduction/planning/11.md")

-- | Plan for the lesson.
plan12 :: P.Pandoc
Right plan12 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/grammar/introduction/planning/12.md")

-- | Plan for the lesson.
plan9to12 :: P.Pandoc
Right plan9to12 = buildDocumentFromMarkdownCode ""

-- | Plan for the lesson.
plan14 :: P.Pandoc
Right plan14 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/grammar/introduction/planning/14.md")

-- | Plan for the lesson.
plan15 :: P.Pandoc
Right plan15 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/grammar/introduction/planning/15.md")

-- | Plan for the lesson.
plan16 :: P.Pandoc
Right plan16 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/grammar/introduction/planning/16.md")

-- | Plan for the lesson.
plan17 :: P.Pandoc
Right plan17 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/grammar/introduction/planning/17.md")

-- | Plan for the lesson.
plan18 :: P.Pandoc
Right plan18 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/grammar/introduction/planning/18.md")

-- | Plan for the lesson.
plan14to18 :: P.Pandoc
Right plan14to18 = buildDocumentFromMarkdownCode ""

-- | Plan for the lesson.
plan20 :: P.Pandoc
Right plan20 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/grammar/introduction/planning/20.md")

-- | Plan for the lesson.
plan21 :: P.Pandoc
Right plan21 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/grammar/introduction/planning/21.md")

-- | Plan for the lesson.
plan22 :: P.Pandoc
Right plan22 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/grammar/introduction/planning/22.md")

-- | Plan for the lesson.
plan23 :: P.Pandoc
Right plan23 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/grammar/introduction/planning/23.md")

-- | Plan for the lesson.
plan24 :: P.Pandoc
Right plan24 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/grammar/introduction/planning/24.md")

-- | Plan for the lesson.
plan20to24 :: P.Pandoc
Right plan20to24 = buildDocumentFromMarkdownCode ""

-- | Plan for the lesson.
plan26 :: P.Pandoc
Right plan26 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/grammar/introduction/planning/26.md")
