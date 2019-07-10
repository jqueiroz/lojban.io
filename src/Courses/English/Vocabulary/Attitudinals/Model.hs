module Courses.English.Vocabulary.Attitudinals.Model where

import System.Random (StdGen)
import qualified Data.Text as T

data AttitudinalType = PureEmotion | PropositionalEmotion

data Attitudinal = Attitudinal
    { attitudinalWord :: T.Text
    , attitudinalType :: AttitudinalType
    , attitudinalPositiveMeaning :: T.Text
    , attitudinalNeutralMeaning :: Maybe T.Text
    , attitudinalNegativeMeaning :: Maybe T.Text
    }

type AttitudinalGenerator = StdGen -> (Attitudinal, StdGen)
