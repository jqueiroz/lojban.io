{-# LANGUAGE OverloadedStrings #-}

-- | This modules exposes the 'CourseStore'.
module Study.Courses.CourseStore (courseStore) where

import Core
import qualified Study.Courses.English.Grammar.Introduction.Course as Introduction
import qualified Study.Courses.English.Grammar.Crash.Course as Crash
import qualified Study.Courses.English.Vocabulary.Brivla.Course as Brivla
import qualified Study.Courses.English.Vocabulary.Attitudinals.Course as Attitudinals
import qualified Data.Map as M

-- | Course store.
courseStore :: CourseStore
courseStore = CourseStore collectionsMap coursesMap where
    collectionsList = [ collectionEnglishGrammar, collectionEnglishVocabulary ]
    collectionsMap = M.fromList $ map (\collection -> (courseCollectionId collection, collection)) collectionsList
    coursesList = [ Introduction.course, Crash.course, Brivla.course, Attitudinals.course ]
    coursesMap = M.fromList $ map (\course -> (courseId course, course)) coursesList

-- | Collection: grammar courses in English.
collectionEnglishGrammar :: CourseCollection
collectionEnglishGrammar = CourseCollection collectionId collectionCourses where
    collectionId = "grammar_eng"
    collectionCourses =
        [ Introduction.course
        , Crash.course
        ]

-- | Collection: vocabulary courses in English.
collectionEnglishVocabulary :: CourseCollection
collectionEnglishVocabulary = CourseCollection collectionId collectionCourses where
    collectionId = "vocabulary_eng"
    collectionCourses =
        [ Brivla.course
        , Attitudinals.course
        ]
