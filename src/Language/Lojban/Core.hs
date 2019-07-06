module Language.Lojban.Core
( SimpleBridi (..)
, SimpleBridiDisplayer
) where

import qualified Data.Text as T
import System.Random (StdGen)

data SimpleBridi = SimpleBridi
    { simpleBridiXu :: Bool
    , simpleBridiSelbri :: T.Text
    , simpleBridiSumti :: [T.Text]
    , simpleBridiExtraSumti :: [T.Text]
    } deriving (Show, Eq)

type SimpleBridiDisplayer = StdGen -> SimpleBridi -> (T.Text, StdGen)
