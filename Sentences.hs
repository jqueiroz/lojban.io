{-# LANGUAGE OverloadedStrings #-}
module Sentences where

import Core
import Util (replace, stripRight)
import qualified Data.Text as T

data SimpleBridi = SimpleBridi
    { simpleBridiSelbri :: T.Text
    , simpleBridiSumti :: [T.Text]
    } deriving (Show)

displaySimpleBridi :: SimpleBridi -> T.Text
displaySimpleBridi (SimpleBridi selbri sumti) = T.unwords . replace "" "zo'e" . stripRight "" $ sumti' ++ [selbri] ++ sumti'' where
    (sumti', sumti'') = splitAt 1 sumti

-- TODO: other display modes (place some sumti before the selbri, introduce fa/fe/fi/fo/fu, introduce se/te/ve/..., etc.)
-- TODO: create functions that use the variant bridi structure (eg. x1 x2 selbri x3 ...), different ways of skipping (fa/fe/... vs se/te/...) according to some randomness, perhaps even unnecessarily sometimes (there should be parameters to control the preferences between fa/fe/... and se/te/... and the level of unnecessary use of fa/fe/..., variant bridi structure, etc.)
