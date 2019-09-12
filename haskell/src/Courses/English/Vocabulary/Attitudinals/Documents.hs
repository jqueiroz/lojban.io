{-# LANGUAGE TemplateHaskell #-}

-- | This module defines documents pertaining to the course, such as lesson materials and lesson plans.
module Courses.English.Vocabulary.Attitudinals.Documents where

import Courses.Framework.DocumentBuilders (buildDocumentFromMarkdownCode)
import Data.FileEmbed (embedStringFile)
import qualified Text.Pandoc as P

-- * Lesson contents

-- | Lecture for the first lesson.
lecture1 :: P.Pandoc
Right lecture1 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/vocabulary/attitudinals/lectures/01.md")

-- | Lecture for the second lesson.
lecture2 :: P.Pandoc
Right lecture2 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/vocabulary/attitudinals/lectures/02.md")

-- | Lecture for the third lesson.
lecture3 :: P.Pandoc
Right lecture3 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/vocabulary/attitudinals/lectures/03.md")

-- * Lesson plans

-- | Plan for the first lesson.
plan1 :: P.Pandoc
Right plan1 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/vocabulary/attitudinals/planning/01.md")

-- | Plan for the second lesson.
plan2 :: P.Pandoc
Right plan2 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/vocabulary/attitudinals/planning/02.md")

-- | Plan for the third lesson.
plan3 :: P.Pandoc
Right plan3 = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/vocabulary/attitudinals/planning/03.md")
