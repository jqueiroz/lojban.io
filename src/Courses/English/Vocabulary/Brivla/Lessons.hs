-- | This module defines the course lessons.
module Courses.English.Vocabulary.Brivla.Lessons where

import Core
import Courses.English.Vocabulary.Brivla.Documents
import Courses.English.Vocabulary.Brivla.Exercises

-- * Lessons
-- TODO: rename: "Brivla 1--20", "Brivla 21--40", ...

-- * Lesson.
lesson01 :: LessonBuilder
lesson01 dictionary = Lesson "Deck #1" (exercises01 dictionary) lecture01 plan01

-- * Lesson.
-- Conflicts:
--   * "ciska" vs "finti"
--   * "jundi" vs "zgana"
lesson02 :: LessonBuilder
lesson02 dictionary = Lesson "Deck #2" (exercises02 dictionary) lecture02 plan02

-- * Lesson.
lesson03 :: LessonBuilder
lesson03 dictionary = Lesson "Deck #3" (exercises03 dictionary) lecture03 plan03
