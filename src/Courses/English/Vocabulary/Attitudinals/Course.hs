{-# LANGUAGE OverloadedStrings #-}

-- | This modules exposes the overall "Attitudinals" course.
module Courses.English.Vocabulary.Attitudinals.Course (course) where

import Core
import Courses.English.Vocabulary.Attitudinals.Lessons

-- | Course style.
style :: CourseStyle
style = CourseStyle color1 iconUrl where
    color1 = Just
        "#7ac70c"
    -- Icon url
    iconUrl = Just
        -- Source: https://www.flaticon.com/premium-icon/swear_774512#term=word&page=1&position=64
        "https://www.flaticon.com/premium-icon/icons/svg/774/774512.svg"

-- | Course: Attitudinals.
course :: CourseBuilder
course = createCourseBuilder title style lessons where
    title = "Attitudinals"
    lessons = [lesson1]

-- Interesting exercise: "Rewrite using attitudinals"
-- Example: "mi djica lo nu mi citka" -> ".au mi citka"
