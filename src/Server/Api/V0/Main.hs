{-# LANGUAGE OverloadedStrings #-}

module Server.Api.V0.Main (handleRoot) where

import Core
import Courses.CourseStore
import Control.Monad (msum)
import Server.Util (forceSlash)
import Server.Api.V0.Serializers (serializeCourse)
import Happstack.Server
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Aeson as A

handleRoot :: ServerPart Response
handleRoot = msum
    [ forceSlash $ ok . toResponse $ A.encode $ A.object [("success", A.Bool True)]
    , dir "course" $ path handleCourse
    ]

handleCourse :: T.Text -> ServerPart Response
handleCourse courseId =
    let courseLookup = M.lookup courseId (courseStoreCourses courseStore)
    in case courseLookup of
        Nothing -> notFound . toResponse $ ("" :: T.Text)
        Just course -> ok . toResponse . A.encode $ serializeCourse course
