{-# LANGUAGE OverloadedStrings #-}

module Study.Framework.ExerciseGenerators.Tests
( tests
) where

import Study.Framework.ExerciseGenerators
import Study.Framework.TranslationUtils (expandSentence)
import Test.Hspec
import Data.List (sort)
import qualified Data.Text as T

validateSentenceExpansion :: T.Text -> [T.Text] -> IO ()
validateSentenceExpansion originalSentence expectedSentences = (sort expectedSentences) `shouldBe` (sort $ expandSentence originalSentence)

sentenceExpansionTests :: Spec
sentenceExpansionTests = do
    describe "Sentence expansion" $ do
      it "supports '{}'" $ do
          validateSentenceExpansion "mi tavla {do}" ["mi tavla", "mi tavla do"]
          validateSentenceExpansion "mi tavla {mi|do}" ["mi tavla", "mi tavla mi", "mi tavla do"]
          validateSentenceExpansion "mi tavla {mi|do|mi'o}" ["mi tavla", "mi tavla mi", "mi tavla do", "mi tavla mi'o"]
          validateSentenceExpansion "mi nelci lo cmene {be do be'o|pe do ge'u} noi melbi" ["mi nelci lo cmene noi melbi", "mi nelci lo cmene be do be'o noi melbi", "mi nelci lo cmene pe do ge'u noi melbi"]
      it "supports '()'" $ do
          validateSentenceExpansion "mi tavla (do)" ["mi tavla do"]
          validateSentenceExpansion "mi tavla (mi|do)" ["mi tavla mi", "mi tavla do"]
          validateSentenceExpansion "mi tavla (mi|do|mi'o)" ["mi tavla mi", "mi tavla do", "mi tavla mi'o"]
          validateSentenceExpansion "mi nelci lo cmene (be do be'o|pe do ge'u) noi melbi" ["mi nelci lo cmene be do be'o noi melbi", "mi nelci lo cmene pe do ge'u noi melbi"]

tests = sentenceExpansionTests
