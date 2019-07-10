{-# LANGUAGE OverloadedStrings #-}

-- | This modules exposes the overall "Attitudinals" course.
module Courses.English.Vocabulary.Attitudinals.Course (course) where

import Core
import Courses.English.Vocabulary.Attitudinals.Lessons

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
course :: CourseBuilder
course = createCourseBuilder title style lessons where
    title = "Attitudinals"
    lessons = [lesson1, lesson2]

-- Interesting exercise: "Rewrite using attitudinals"
-- Example: "mi djica lo nu mi citka" -> ".au mi citka"