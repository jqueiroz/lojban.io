{-# LANGUAGE OverloadedStrings #-}

-- | This modules exposes the overall "Brivla" course.
module Courses.English.Vocabulary.Brivla.Course (course) where

import Core
import Courses.English.Vocabulary.Brivla.Lessons
import Language.Lojban.Dictionaries (englishDictionary)

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
course = Course "brivla_eng" title shortDescription longDescription credits style englishDictionary lessons where
    title = "Common brivla"
    shortDescription = "Learn the most commonly used brivla, while also developing your comprehension skills."
    longDescription = Nothing
    credits = Nothing
    lessons = [lesson01, lesson02, lesson03, lesson04, lesson05]
