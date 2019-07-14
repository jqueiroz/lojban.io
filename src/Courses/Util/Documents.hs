{-# LANGUAGE OverloadedStrings #-}

module Courses.Util.Documents
( buildDocument
) where

import qualified Data.Text as T
import qualified Text.Pandoc as P

buildDocument :: T.Text -> Either P.PandocError P.Pandoc
buildDocument code = P.runPure $ P.readMarkdown P.def
    { P.readerStripComments = True
    , P.readerExtensions = P.extensionsFromList [ P.Ext_raw_html, P.Ext_subscript ]
    } $ preprocessCode code

preprocessCode :: T.Text -> T.Text
preprocessCode = (T.replace "_1" "~1~") . (T.replace "_2" "~2~") . (T.replace "_3" "~3~") . (T.replace "_4" "~4~") . (T.replace "_5" "~5~")
