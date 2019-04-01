{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import Courses.Util.Sentences (basicSentenceCanonicalizer)
import Data.Either
import qualified Data.Text as T

validateSentences :: [(T.Text, T.Text)] -> IO ()
validateSentences sentences = map basicSentenceCanonicalizer originalSentences `shouldBe` canonicalSentences where
  originalSentences = map fst sentences
  canonicalSentences = map (Right . snd) sentences

main :: IO ()
main = hspec $ do
    describe "Basic sentence canonicalizer" $ do
      it "supports SE" $ do
        validateSentences
            [ ("mi se nelci lo mlatu", "lo mlatu ku nelci mi")
            , ("lo mlatu cu te tavla", "tavla zo'e lo mlatu ku")
            ]
      it "supports FA" $ do
        validateSentences
            [ ("mi tavla fi lo mlatu", "mi tavla zo'e lo mlatu ku")
            , ("mi ctuca fo lo bangu", "mi ctuca zo'e zo'e lo bangu ku")
            ]
      it "supports BE" $ do
        validateSentences
            [ ("mi nelci lo cmene be mi", "mi nelci lo cmene be mi be'o ku")
            ]
      it "supports NOI" $ do
        validateSentences
            [ ("mi citka lo se dunda ku poi plise", "mi citka lo se dunda ku poi plise ku'o")
            , ("mi nelci lo mlatu poi melbi", "mi nelci lo mlatu ku poi melbi ku'o")
            ]
      it "supports XU" $ do
        validateSentences
            [ ("xu do nelci lo mlatu", "xu do nelci lo mlatu ku")
            ]
      --it "supports reordered XU" $ do
        --validateSentences
            --[ ("do nelci xu lo mlatu", "xu do nelci lo mlatu ku")
            --]
