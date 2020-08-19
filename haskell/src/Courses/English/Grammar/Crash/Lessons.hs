{-# LANGUAGE OverloadedStrings #-}

-- | This module defines the course lessons.
module Courses.English.Grammar.Crash.Lessons where

import Core
import Courses.English.Grammar.Crash.Documents
import Courses.English.Grammar.Crash.Exercises
import Courses.English.Grammar.Crash.Vocabulary

-- | Lesson.
lesson01 :: Lesson
lesson01 = Lesson "The simplest sentence" exercises01 (Just lecture01) Nothing (Just vocabulary01_cumulative)

-- | Lesson.
lesson02 :: Lesson
lesson02 = Lesson "Pronouns" exercises02 (Just lecture02) Nothing (Just vocabulary02_cumulative)

-- | Numbers.
lesson03 :: Lesson
lesson03 = Lesson "Numbers" exercises03 (Just lecture03) Nothing (Just vocabulary03_cumulative)
