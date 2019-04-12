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
            , ("mi tavla lo vecnu be lo zdani bei do", "mi tavla lo vecnu be lo zdani ku bei do be'o ku")
            , ("lo tavla be zo'e bei do cu melbi", "lo tavla be zo'e bei do be'o ku melbi")
            , ("lo tavla be zo'e bei do ku melbi", "lo tavla be zo'e bei do be'o ku melbi")
            ]
      it "supports NOI" $ do
        validateSentences
            [ ("mi citka lo se dunda ku poi plise", "mi citka lo se dunda ku poi plise ku'o")
            , ("mi nelci lo mlatu poi melbi", "mi nelci lo mlatu ku poi melbi ku'o")
            , ("mi tavla lo prenu poi nupre do", "mi tavla lo prenu ku poi nupre do ku'o")
            , ("mi djuno lo du'u lo gerku poi do dunda ke'a mi cu pendo", "mi djuno lo du'u lo gerku ku poi do dunda ke'a mi ku'o pendo kei ku")
            ]
      it "supports XU" $ do
        validateSentences
            [ ("xu do nelci lo mlatu", "xu do nelci lo mlatu ku")
            ]
      --it "supports reordered XU" $ do
        --validateSentences
            --[ ("do nelci xu lo mlatu", "xu do nelci lo mlatu ku")
            --]
      it "supports tenses" $ do
        validateSentences
            [ ("lo prenu pu dunda lo mlatu", "pu ku lo prenu ku dunda lo mlatu ku")
            , ("pu ku lo prenu cu dunda lo mlatu", "pu ku lo prenu ku dunda lo mlatu ku")
            ]
      it "supports sumtcita" $ do
        validateSentences
            [ ("mi fanva fi'o se pilno fe'u lo skami", "fi'o se pilno fe'u lo skami ku mi fanva")
            , ("mi fanva fi'o se pilno fe'u lo skami ta", "fi'o se pilno fe'u lo skami ku mi fanva ta")
            , ("mi fanva se pi'o lo skami", "se pi'o lo skami ku mi fanva")
            , ("mi fanva se pi'o lo skami ta", "se pi'o lo skami ku mi fanva ta")
            , ("lo prenu zu'e klama lo zarci", "zu'e ku lo prenu ku klama lo zarci ku")
            ]
      it "supports complex sentences" $ do
        validateSentences
            [ ("mi nupre lo nu pilno lo skami poi do dunda ke'a mi", "mi nupre lo nu pilno lo skami ku poi do dunda ke'a mi ku'o kei ku")
            ]
