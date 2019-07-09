module Courses.English.Vocabulary.Attitudinals.Model where

import qualified Data.Text as T

data AttitudinalType = PureEmotion | PropositionalEmotion

data Attitudinal = Attitudinal
    { attitudinalWord :: T.Text
    , attitudinalType :: AttitudinalType
    , attitudinalPositiveMeaning :: T.Text
    , attitudinalNeutralMeaning :: Maybe T.Text
    , attitudinalNegativeMeaning :: Maybe T.Text
    }
