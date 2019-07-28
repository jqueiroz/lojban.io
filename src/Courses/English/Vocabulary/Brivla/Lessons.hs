-- | This module defines the course lessons.
module Courses.English.Vocabulary.Brivla.Lessons where

import Core
import Courses.English.Vocabulary.Brivla.Documents
import Courses.English.Vocabulary.Brivla.Exercises
import Courses.English.Vocabulary.Brivla.Vocabulary

-- * Lessons
-- TODO: rename: "Brivla 1--20", "Brivla 21--40", ...

-- * Lesson.
lesson01 :: Lesson
lesson01 = Lesson "Deck #1" exercises01 lecture01 Nothing (Just vocabulary01)

-- * Lesson.
-- Conflicts:
--   * "ciska" vs "finti"
--   * "jundi" vs "zgana"
lesson02 :: Lesson
lesson02 = Lesson "Deck #2" exercises02 lecture02 Nothing (Just vocabulary02)

-- * Lesson.
lesson03 :: Lesson
lesson03 = Lesson "Deck #3" exercises03 lecture03 Nothing (Just vocabulary03)

-- * Lesson.
lesson04 :: Lesson
lesson04 = Lesson "Deck #4" exercises04 lecture04 Nothing (Just vocabulary04)

-- * Lesson.
lesson05 :: Lesson
lesson05 = Lesson "Deck #5" exercises05 lecture05 Nothing (Just vocabulary05)
