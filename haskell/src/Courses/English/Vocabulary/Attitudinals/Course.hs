{-# LANGUAGE OverloadedStrings #-}

-- | This modules exposes the overall "Attitudinals" course.
module Courses.English.Vocabulary.Attitudinals.Course (course) where

import Core
import Courses.English.Vocabulary.Attitudinals.Lessons
import Language.Lojban.Dictionaries (englishDictionary)

-- | Course style.
style :: CourseStyle
style = CourseStyle color1 iconUrl where
    color1 = Just
        "hsl(274, 34%, 30%)"
    -- Icon url
    iconUrl = Just
        -- Source: https://www.flaticon.com/free-icon/happy_187134
        "https://image.flaticon.com/icons/svg/187/187134.svg"

-- | Course: Attitudinals.
course :: Course
course = Course "attitudinals_eng" title style englishDictionary lessons where
    title = "Attitudinals (WIP)"
    lessons = [lesson1, lesson2, lesson3]

-- Interesting exercise: "Rewrite using attitudinals"
-- Example: "mi djica lo nu mi citka" -> ".au mi citka"
