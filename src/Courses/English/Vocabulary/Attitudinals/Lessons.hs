-- | This module defines the course lessons.
module Courses.English.Vocabulary.Attitudinals.Lessons where

import Core
import Courses.English.Vocabulary.Attitudinals.Documents
import Courses.English.Vocabulary.Attitudinals.Exercises

-- | First lesson: Pure emotions 1.
lesson1 :: Lesson
lesson1 = Lesson "Pure emotions 1" exercises1 lecture1 (Just plan1) Nothing

-- | Second lesson: Propositional emotions 1.
lesson2 :: Lesson
lesson2 = Lesson "Propositional emotions 1" exercises2 lecture2 (Just plan2) Nothing

-- | Third lesson: Attitudinal modifiers 1
lesson3 :: Lesson
lesson3 = Lesson "Attitudinal modifiers 1" exercises3 lecture3 (Just plan3) Nothing
