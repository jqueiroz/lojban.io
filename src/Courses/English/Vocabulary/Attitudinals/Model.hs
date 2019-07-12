module Courses.English.Vocabulary.Attitudinals.Model where

import System.Random (StdGen)
import qualified Data.Text as T

data AttitudinalType = PureEmotion | PropositionalEmotion deriving (Eq)

data Attitudinal = Attitudinal
    { attitudinalWord :: T.Text
    , attitudinalType :: AttitudinalType
    , attitudinalPositiveMeaning :: T.Text
    , attitudinalNeutralMeaning :: Maybe T.Text
    , attitudinalNegativeMeaning :: Maybe T.Text
    }

instance Eq Attitudinal where
    x == y = (attitudinalWord x) == (attitudinalWord y)

type AttitudinalGenerator = StdGen -> (Attitudinal, StdGen)
