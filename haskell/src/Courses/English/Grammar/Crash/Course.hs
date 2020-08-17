{-# LANGUAGE OverloadedStrings #-}

-- | This modules exposes the overall "The Crash Course" course.
module Courses.English.Grammar.Crash.Course (course) where

import Core
import Courses.English.Grammar.Crash.Lessons
import Language.Lojban.Dictionaries (englishDictionary)

-- | Course style.
style :: CourseStyle
style = CourseStyle color1 iconUrl where
    -- Color1
    color1 = Just
        "hsl(0, 0%, 30%)"
    -- Icon url
    iconUrl = Just
        -- Source: https://www.flaticon.com/free-icon/jigsaw_993723#term=jigsaw&page=1&position=3
        "https://image.flaticon.com/icons/svg/993/993723.svg"

-- | Course: The Crash Course.
course :: Course
course = Course "crash-course_eng" title shortDescription longDescription style englishDictionary lessons where
    title = "The Crash Course (WIP)"
    shortDescription = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor."
    longDescription = Nothing
    lessons = [lesson01, lesson02, lesson03]
