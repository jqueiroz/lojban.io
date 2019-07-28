{-# LANGUAGE OverloadedStrings #-}

-- | This module defines documents pertaining to the course, such as lesson materials and lesson plans.
module Courses.English.Vocabulary.Brivla.Documents where

import Courses.Util.Documents (buildDocument)
import qualified Text.Pandoc as P

-- * Lesson contents

-- | Lecture for the corresponding lesson.
lecture01 :: P.Pandoc
Right lecture01 = buildDocument ""

-- | Lecture for the corresponding lesson.
lecture02 :: P.Pandoc
Right lecture02 = buildDocument ""

-- | Lecture for the corresponding lesson.
lecture03 :: P.Pandoc
Right lecture03 = buildDocument ""

-- | Lecture for the corresponding lesson.
lecture04 :: P.Pandoc
Right lecture04 = buildDocument ""

-- | Lecture for the corresponding lesson.
lecture05 :: P.Pandoc
Right lecture05 = buildDocument ""
