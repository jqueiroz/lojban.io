{-# LANGUAGE OverloadedStrings #-}

module Server.Modules
( displayModule
) where

import Core
import Control.Monad (when, forM_)
import Data.Maybe (isJust, fromJust, fromMaybe)
import qualified Text.Blaze as B
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

displayModule :: String -> [(String, Course)] -> H.Html
displayModule title courseList = do
    H.div B.! A.class_ (H.stringValue "module") $ do
        H.h1 $ H.toHtml title
        H.div B.! A.class_ (H.stringValue "course-list") $ do
            forM_ courseList displayCourse

displayCourse :: (String, Course) -> H.Html
displayCourse (url, course) = do
    let style = courseStyle course
    let backgroundColor = fromMaybe "#ddd" (courseStyleColor1 style)
    H.div B.! A.class_ (H.stringValue "course") B.! A.style (H.stringValue $ "background-color: " ++ backgroundColor ++ ";") $ do
        when (isJust $ courseStyleIconUrl style) $ do
            H.img B.! A.class_ (H.stringValue "course-icon") B.! A.src (H.stringValue $ fromJust $ courseStyleIconUrl style)
        H.div B.! A.class_ (H.stringValue "course-title") $ do
            H.a B.! A.href (H.stringValue url) $ H.toHtml (courseTitle course)
