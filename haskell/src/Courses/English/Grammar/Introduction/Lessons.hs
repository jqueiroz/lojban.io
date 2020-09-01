{-# LANGUAGE OverloadedStrings #-}

-- | This module defines the course lessons.
module Courses.English.Grammar.Introduction.Lessons  where

import Core
import Courses.English.Grammar.Introduction.Documents
import Courses.English.Grammar.Introduction.Exercises
import Courses.English.Grammar.Introduction.Vocabulary

-- | Lesson: Basics 1.
lesson1 :: Lesson
lesson1 = Lesson "Basics 1" exercises1 (Just lecture1) (Just plan1) (Just vocabulary1_cumulative)

-- | Lesson: Basics 2.
lesson2 :: Lesson
lesson2 = Lesson "Basics 2" exercises2 (Just lecture2) (Just plan2) (Just vocabulary2_cumulative)

-- | Lesson: Basics 3.
lesson3 :: Lesson
lesson3 = Lesson "Basics 3" exercises3 (Just lecture3) (Just plan3) (Just vocabulary3_cumulative)

-- | Lesson: Tanru 1.
lesson4 :: Lesson
lesson4 = Lesson "Tanru 1" exercises4 (Just lecture4) (Just plan4) (Just vocabulary4_cumulative)

-- | Lesson: Questions 1.
lesson5 :: Lesson
lesson5 = Lesson "Questions 1" exercises5 (Just lecture5) (Just plan5) (Just vocabulary5_cumulative)

-- | Lesson: Abstractions 1.
lesson6 :: Lesson
lesson6 = Lesson "Abstractions 1" exercises6 (Just lecture6) (Just plan6) (Just vocabulary6_cumulative)

-- | Lesson: Terminator elision.
lesson7 :: Lesson
lesson7 = Lesson "Terminator elision" exercises7 (Just lecture7) (Just plan7) (Just vocabulary7_cumulative)

-- | Lesson: Checkpoint (Lessons 1--7).
checkpoint1to7 :: Lesson
checkpoint1to7 = Lesson "Checkpoint: Lessons 1–7" exercises1to7 (Just lecture1to7) (Just plan1to7) (Just vocabulary7_cumulative)

-- | Lesson: Relative clauses.
lesson9 :: Lesson
lesson9 = Lesson "Relative clauses" exercises9 (Just lecture9) (Just plan9) (Just vocabulary9_cumulative)

-- | Lesson: Linked sumti.
lesson10 :: Lesson
lesson10 = Lesson "Linked sumti" exercises10 (Just lecture10) (Just plan10) (Just vocabulary10_cumulative)

-- | Lesson: Sumtcita
lesson11 :: Lesson
lesson11 = Lesson "Sumtcita" exercises11 (Just lecture11) (Just plan11) (Just vocabulary11_cumulative)

-- | Lesson: Tenses 1
lesson12 :: Lesson
lesson12 = Lesson "Tenses 1" exercises12 (Just lecture12) (Just plan12) (Just vocabulary12_cumulative)

-- | Lesson: Checkpoint (Lessons 9--12)
checkpoint9to12 :: Lesson
checkpoint9to12 = Lesson "Checkpoint: Lessons 9-12" exercises9to12 (Just lecture9to12) (Just plan9to12) (Just vocabulary12_cumulative)

-- | Lesson: Quotations 1
lesson14 :: Lesson
lesson14 = Lesson "Quotations 1" exercises14 (Just lecture14) (Just plan14) (Just vocabulary14_cumulative)

-- | Lesson: Relative phrases
lesson15 :: Lesson
lesson15 = Lesson "Relative phrases" exercises15 (Just lecture15) (Just plan15) (Just vocabulary15_cumulative)

-- | Lesson: Logical connectives 1
lesson16 :: Lesson
lesson16 = Lesson "Logical connectives 1" exercises16 (Just lecture16) (Just plan16) (Just vocabulary16_cumulative)

-- | Lesson: Negation 1
lesson17 :: Lesson
lesson17 = Lesson "Negation 1" exercises17 (Just lecture17) (Just plan17) (Just vocabulary17_cumulative)

-- | Lesson: Misc 1
lesson18 :: Lesson
lesson18 = Lesson "Misc. 1" exercises18 (Just lecture18) (Just plan18) (Just vocabulary18_cumulative)

-- | Lesson: Checkpoint (Lessons 14--18)
checkpoint14to18 :: Lesson
checkpoint14to18 = Lesson "Checkpoint: Lessons 14-18" exercises14to18 (Just lecture14to18) (Just plan14to18) (Just vocabulary18_cumulative)
