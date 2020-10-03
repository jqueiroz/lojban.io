{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This modules exposes the overall "Attitudinals" course.
module Courses.English.Vocabulary.Attitudinals.Course (course) where

import Core
import Courses.English.Vocabulary.Attitudinals.Lessons
import Language.Lojban.Dictionaries (englishDictionary)
import Study.Framework.DocumentBuilders (buildDocumentFromMarkdownCode)
import qualified Text.Pandoc as P
import Data.FileEmbed (embedStringFile)

-- | Course style.
style :: CourseStyle
style = CourseStyle color1 iconUrl where
    color1 = Just
        "hsl(274, 34%, 30%)"
    -- Icon url
    iconUrl = Just
        -- Source: https://www.flaticon.com/free-icon/happy_187134
        "https://image.flaticon.com/icons/svg/187/187134.svg"

-- | Course description.
longDescription :: P.Pandoc
Right longDescription = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/vocabulary/attitudinals/description.md")

-- | Course credits.
credits :: P.Pandoc
Right credits = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/vocabulary/attitudinals/credits.md")

-- | Course: Attitudinals.
course :: Course
course = Course "attitudinals_eng" title shortDescription (Just longDescription) (Just credits) style englishDictionary lessons where
    title = "Attitudinals (pre-alpha)"
    --shortDescription = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor."
    shortDescription = "This course is still under construction. Please check back later."
    lessons = [lesson1, lesson2, lesson3]

-- Interesting exercise: "Rewrite using attitudinals"
-- Example: "mi djica lo nu mi citka" -> ".au mi citka"
