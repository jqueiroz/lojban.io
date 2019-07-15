-- | This module defines the course lessons.
module Courses.English.Grammar.Introduction.Lessons  where

import Core
import Courses.English.Grammar.Introduction.Documents
import Courses.English.Grammar.Introduction.Exercises

-- | First lesson: Basics 1.
lesson1 :: LessonBuilder
lesson1 dictionary = Lesson "Basics 1" (exercises1 dictionary) lecture1 plan1

-- | Second lesson: Basics 2.
lesson2 :: LessonBuilder
lesson2 dictionary = Lesson "Basics 2" (exercises2 dictionary) lecture2 plan2

-- | Third lesson: Basics 3.
lesson3 :: LessonBuilder
lesson3 dictionary = Lesson "Basics 3" (exercises3 dictionary) lecture3 plan3

-- | Fourth lesson: Questions 1.
lesson4 :: LessonBuilder
lesson4 dictionary = Lesson "Questions 1" (exercises4 dictionary) lecture4 plan4

-- | Fifth lesson: Abstractions 1.
lesson5 :: LessonBuilder
lesson5 dictionary = Lesson "Abstractions 1" (exercises5 dictionary) lecture5 plan5

-- | Sixth lesson: Terminator elision.
lesson6 :: LessonBuilder
lesson6 dictionary = Lesson "Terminator elision" (exercises6 dictionary) lecture6 plan6

-- | Seventh lesson: Checkpoint (Lessons 1--6).
checkpoint1to6 :: LessonBuilder
checkpoint1to6 dictionary = Lesson "Checkpoint: Lessons 1–6" (exercises1to6 dictionary) lecture1to6 plan1to6

-- | Eighth lesson: Relative clauses.
lesson8 :: LessonBuilder
lesson8 dictionary = Lesson "Relative clauses" (exercises8 dictionary) lecture8 plan8

-- | Nineth lesson: Linked sumti.
lesson9 :: LessonBuilder
lesson9 dictionary = Lesson "Linked sumti" (exercises9 dictionary) lecture9 plan9

-- | Tenth lesson: Sumtcita
lesson10 :: LessonBuilder
lesson10 dictionary = Lesson "Sumtcita" (exercises10 dictionary) lecture10 plan10

-- | Eleventh lesson: Tenses 1
lesson11 :: LessonBuilder
lesson11 dictionary = Lesson "Tenses 1" (exercises11 dictionary) lecture11 plan11

-- | Twelveth lesson: Tanru
lesson12 :: LessonBuilder
lesson12 dictionary = Lesson "Tanru 1" (exercises12 dictionary) lecture12 plan12

-- | Thirteenth lesson: Checkpoint (Lessons 8--12)
checkpoint8to12 :: LessonBuilder
checkpoint8to12 dictionary = Lesson "Checkpoint: Lessons 8-12" (exercises8to12 dictionary) lecture8to12 plan8to12

-- | Fourteenth lesson: Quotations 1
lesson14 :: LessonBuilder
lesson14 dictionary = Lesson "Quotations 1" (exercises14 dictionary) lecture14 plan14

-- | Fifteenth lesson: Relative phrases
lesson15 :: LessonBuilder
lesson15 dictionary = Lesson "Relative phrases" (exercises15 dictionary) lecture15 plan15

-- | Sixteenth lesson: Logical connectives 1
lesson16 :: LessonBuilder
lesson16 dictionary = Lesson "Logical connectives 1" (exercises16 dictionary) lecture16 plan16
