-- | This module defines the course lessons.
module Courses.English.Vocabulary.Attitudinals.Lessons where

import Core
import Courses.English.Vocabulary.Attitudinals.Documents
import Courses.English.Vocabulary.Attitudinals.Exercises

-- | First lesson: Pure emotions 1.
lesson1 :: LessonBuilder
lesson1 dictionary = Lesson "Pure emotions 1" exercises1 plan1

-- | Second lesson: Propositional emotions 1.
lesson2 :: LessonBuilder
lesson2 dictionary = Lesson "Propositional emotions 1" exercises2 plan2

-- | Third lesson: Attitudinal modifiers 1
lesson3 :: LessonBuilder
lesson3 dictionary = Lesson "Attitudinal modifiers 1" exercises3 plan3
