{-# LANGUAGE DeriveGeneric #-}

module Server.Api.V0.Contract where

import qualified Data.Aeson as A
import qualified Data.Text as T
import GHC.Generics

data Course = Course
    { title :: T.Text
    , dictionaryId :: T.Text
    , style :: CourseStyle
    } deriving (Generic, Show)

data CourseStyle = CourseStyle
    { color1 :: Maybe String
    , iconUrl :: Maybe String
    } deriving (Generic, Show)

instance A.ToJSON Course where
    toEncoding = A.genericToEncoding A.defaultOptions

instance A.ToJSON CourseStyle where
    toEncoding = A.genericToEncoding A.defaultOptions
