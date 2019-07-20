-- | This module defines the course lessons.
module Courses.English.Grammar.Introduction.Lessons  where

import Core
import Courses.English.Grammar.Introduction.Documents
import Courses.English.Grammar.Introduction.Exercises

-- | First lesson: Basics 1.
lesson1 :: Lesson
lesson1 = Lesson "Basics 1" exercises1 lecture1 plan1

-- | Second lesson: Basics 2.
lesson2 :: Lesson
lesson2 = Lesson "Basics 2" exercises2 lecture2 plan2

-- | Third lesson: Basics 3.
lesson3 :: Lesson
lesson3 = Lesson "Basics 3" exercises3 lecture3 plan3

-- | Fourth lesson: Questions 1.
lesson4 :: Lesson
lesson4 = Lesson "Questions 1" exercises4 lecture4 plan4

-- | Fifth lesson: Abstractions 1.
lesson5 :: Lesson
lesson5 = Lesson "Abstractions 1" exercises5 lecture5 plan5

-- | Sixth lesson: Terminator elision.
lesson6 :: Lesson
lesson6 = Lesson "Terminator elision" exercises6 lecture6 plan6

-- | Seventh lesson: Checkpoint (Lessons 1--6).
checkpoint1to6 :: Lesson
checkpoint1to6 = Lesson "Checkpoint: Lessons 1–6" exercises1to6 lecture1to6 plan1to6

-- | Eighth lesson: Relative clauses.
lesson8 :: Lesson
lesson8 = Lesson "Relative clauses" exercises8 lecture8 plan8

-- | Nineth lesson: Linked sumti.
lesson9 :: Lesson
lesson9 = Lesson "Linked sumti" exercises9 lecture9 plan9

-- | Tenth lesson: Sumtcita
lesson10 :: Lesson
lesson10 = Lesson "Sumtcita" exercises10 lecture10 plan10

-- | Eleventh lesson: Tenses 1
lesson11 :: Lesson
lesson11 = Lesson "Tenses 1" exercises11 lecture11 plan11

-- | Twelveth lesson: Tanru
lesson12 :: Lesson
lesson12 = Lesson "Tanru 1" exercises12 lecture12 plan12

-- | Thirteenth lesson: Checkpoint (Lessons 8--12)
checkpoint8to12 :: Lesson
checkpoint8to12 = Lesson "Checkpoint: Lessons 8-12" exercises8to12 lecture8to12 plan8to12

-- | Fourteenth lesson: Quotations 1
lesson14 :: Lesson
lesson14 = Lesson "Quotations 1" exercises14 lecture14 plan14

-- | Fifteenth lesson: Relative phrases
lesson15 :: Lesson
lesson15 = Lesson "Relative phrases" exercises15 lecture15 plan15

-- | Sixteenth lesson: Logical connectives 1
lesson16 :: Lesson
lesson16 = Lesson "Logical connectives 1" exercises16 lecture16 plan16
