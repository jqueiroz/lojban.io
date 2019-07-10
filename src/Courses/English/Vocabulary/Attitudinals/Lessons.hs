-- | This module defines the course lessons.
module Courses.English.Vocabulary.Attitudinals.Lessons where

import Core
import Courses.English.Vocabulary.Attitudinals.Documents
import Courses.English.Vocabulary.Attitudinals.Exercises

-- | First lesson: Pure emotions 1.
lesson1 :: LessonBuilder
lesson1 dictionary = Lesson "Pure emotions 1" exercises1 plan1
