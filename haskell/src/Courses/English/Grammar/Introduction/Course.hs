{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This modules exposes the overall "Introduction to Grammar" course.
module Courses.English.Grammar.Introduction.Course (course) where

import Core
import Courses.English.Grammar.Introduction.Lessons
import Language.Lojban.Dictionaries (englishDictionary)
import Courses.Framework.DocumentBuilders (buildDocumentFromMarkdownCode)
import qualified Text.Pandoc as P
import Data.FileEmbed (embedStringFile)

-- introduce djica alongside questions: "I want you to be happy" / "Do you want me to be happy?" / "What do you want?" / "Who wants you to be happy" / "Who do you want to be happy?"
-- TODO: remove the translations that make the least sense (in progress...)

-- Considerations
-- TODO: also accept "ma'a" (and similar terms) whenever "mi" is accepted

-------- Tanru
-- useful gismu: sutra, pelxu
-- lo melbi prenu, lo sutra mlatu, lo sutra gerku, lo gleki prenu, lo melbi prenu, mi mutce gleki
-------- Questions
-- Are you talking about the donation? (lo ka dunda)
-- Who wants to talk to me?
-- Who do you think you are talking to? (?)


-- Reminder: from Lesson 4 onwards, mix propositions and questions

-- | Course description.
longDescription :: P.Pandoc
Right longDescription = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/grammar/introduction/description.md")

-- | Course credits.
credits :: P.Pandoc
Right credits = buildDocumentFromMarkdownCode $(embedStringFile "resources/courses/english/grammar/introduction/credits.md")

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

        -- Source: https://www.flaticon.com/free-icon/jigsaw_993686
        --"https://image.flaticon.com/icons/svg/993/993686.svg"

        -- Source: https://www.flaticon.com/free-icon/puzzle_755205
        --"https://image.flaticon.com/icons/svg/755/755205.svg"

-- | Course: Introduction to Grammar.
course :: Course
course = Course "grammar-intro_eng" title shortDescription (Just longDescription) (Just credits) style englishDictionary lessons where
    --title = "Introduction to Grammar (alpha)"
    title = "Getting started with Lojban (alpha)"
    --shortDescription = "Get started with Lojban, and grasp beginner to intermediate concepts of the language."
    shortDescription = "Get started with Lojban, and assimilate beginner to intermediate concepts of the language."
    lessons = [lesson1, lesson2, lesson3, lesson4, lesson5, lesson6, lesson7, checkpoint1to7, lesson9, lesson10, lesson11, lesson12, checkpoint9to12, lesson14, lesson15, lesson16, lesson17, lesson18]
