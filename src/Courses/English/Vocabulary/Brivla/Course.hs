{-# LANGUAGE OverloadedStrings #-}

-- | This modules exposes the overall "Brivla" course.
module Courses.English.Vocabulary.Brivla.Course (course) where

import Core
import Courses.English.Vocabulary.Brivla.Lessons

-- | Course style.
style :: CourseStyle
style = CourseStyle color1 iconUrl where
    color1 = Just
        "hsl(85, 89%, 30%)"
    -- Icon url
    iconUrl = Just
        -- Source: https://www.flaticon.com/premium-icon/swear_774512#term=word&page=1&position=64
        "https://www.flaticon.com/premium-icon/icons/svg/774/774512.svg"

-- | Course: Brivla.
course :: Course
course = Course title style lessons where
    title = "Common brivla"
    lessons = [lesson01, lesson02, lesson03]
