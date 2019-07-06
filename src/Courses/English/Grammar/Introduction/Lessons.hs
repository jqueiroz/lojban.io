module Courses.English.Grammar.Introduction.Lessons  where

import Core
import Courses.English.Grammar.Introduction.Documents
import Courses.English.Grammar.Introduction.Exercises

-------- Lessons
lesson1 :: LessonBuilder
lesson1 dictionary = Lesson "Basics 1" (exercises1 dictionary) plan1

lesson2 :: LessonBuilder
lesson2 dictionary = Lesson "Basics 2" (exercises2 dictionary) plan2

lesson3 :: LessonBuilder
lesson3 dictionary = Lesson "Questions 1" (exercises3 dictionary) plan3

lesson4 :: LessonBuilder
lesson4 dictionary = Lesson "Abstractions 1" (exercises4 dictionary) plan4

lesson5 :: LessonBuilder
lesson5 dictionary = Lesson "Terminator elision" (exercises5 dictionary) plan5

checkpoint1to5 :: LessonBuilder
checkpoint1to5 dictionary = Lesson "Checkpoint: Lessons 1–5" (exercises1to5 dictionary) plan1to5

lesson7 :: LessonBuilder
lesson7 dictionary = Lesson "Relative clauses" (exercises7 dictionary) plan7

lesson8 :: LessonBuilder
lesson8 dictionary = Lesson "Linked sumti" (exercises8 dictionary) plan8

lesson9 :: LessonBuilder
lesson9 dictionary = Lesson "Sumtcita" (exercises9 dictionary) plan9

lesson10 :: LessonBuilder
lesson10 dictionary = Lesson "Tenses 1" (exercises10 dictionary) plan10
