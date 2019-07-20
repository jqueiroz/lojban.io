-- | This module defines the course lessons.
module Courses.English.Vocabulary.Brivla.Lessons where

import Core
import Courses.English.Vocabulary.Brivla.Documents
import Courses.English.Vocabulary.Brivla.Exercises

-- * Lessons
-- TODO: rename: "Brivla 1--20", "Brivla 21--40", ...

-- * Lesson.
lesson01 :: Lesson
lesson01 = Lesson "Deck #1" exercises01 lecture01 plan01

-- * Lesson.
-- Conflicts:
--   * "ciska" vs "finti"
--   * "jundi" vs "zgana"
lesson02 :: Lesson
lesson02 = Lesson "Deck #2" exercises02 lecture02 plan02

-- * Lesson.
lesson03 :: Lesson
lesson03 = Lesson "Deck #3" exercises03 lecture03 plan03
