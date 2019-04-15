{-# LANGUAGE OverloadedStrings #-}

module Language.Canonicalization
( validateCanonicalization
) where

import Language.Lojban.Core
import Language.Lojban.Parsing (parse)
import Language.Lojban.Canonicalization.Internals
import Test.Hspec
import Data.Either
import qualified Data.Text as T

validateBridiRetrieval :: T.Text -> SimpleBridi -> IO ()
validateBridiRetrieval text expectedSimpleBridi = simpleBridi `shouldBe` (Right expectedSimpleBridi) where
    simpleBridi = (parse text) >>= canonicalizeText

validateEquivalentSentences :: T.Text -> [T.Text] -> IO ()
validateEquivalentSentences canonicalSentence originalSentences = lhs `shouldBe` rhs where
    lhs = map basicSentenceCanonicalizer (canonicalSentence : originalSentences)
    rhs = replicate (length lhs) $ Right canonicalSentence

validateSentences :: [(T.Text, T.Text)] -> IO ()
validateSentences sentences = map basicSentenceCanonicalizer originalSentences `shouldBe` canonicalSentences where
  originalSentences = map fst sentences
  canonicalSentences = map (Right . snd) sentences

validateCanonicalization :: IO ()
validateCanonicalization = hspec $ do
    describe "Bridi retrieval" $ do
      -- (without-x1) nelci
      it "supports 'nelci'" $ do
        validateBridiRetrieval
            "nelci" $
            SimpleBridi False "nelci" [] []
      it "supports 'se nelci'" $ do
        validateBridiRetrieval
            "se nelci" $
            SimpleBridi False "nelci" [] []
      it "supports 'nelci do'" $ do
        validateBridiRetrieval
            "nelci do" $
            SimpleBridi False "nelci" ["", "do"] []
      it "supports 'se nelci do'" $ do
        validateBridiRetrieval
            "se nelci do" $
            SimpleBridi False "nelci" ["do"] []
      -- (without-x1) go'i
      it "supports 'go'i'" $ do
        validateBridiRetrieval
            "go'i" $
            SimpleBridi False "go'i" [] []
      it "supports 'se go'i'" $ do
        validateBridiRetrieval
            "se go'i" $
            SimpleBridi False "go'i" [] []
      it "supports 'go'i do'" $ do
        validateBridiRetrieval
            "go'i do" $
            SimpleBridi False "go'i" ["", "do"] []
      it "supports 'se go'i do'" $ do
        validateBridiRetrieval
            "se go'i do" $
            SimpleBridi False "go'i" ["do"] []
      -- (without-x1) pu nelci
      it "supports 'pu nelci'" $ do
        validateBridiRetrieval
            "pu nelci" $
            SimpleBridi False "nelci" [] ["pu ku"]
      it "supports 'pu se nelci'" $ do
        validateBridiRetrieval
            "pu se nelci" $
            SimpleBridi False "nelci" [] ["pu ku"]
      it "supports 'pu ca ba nelci'" $ do
        validateBridiRetrieval
            "pu ca ba nelci" $
            SimpleBridi False "nelci" [] ["pu ku", "ca ku", "ba ku"]
      it "supports 'pu ca ba se nelci'" $ do
        validateBridiRetrieval
            "pu ca ba se nelci" $
            SimpleBridi False "nelci" [] ["pu ku", "ca ku", "ba ku"]
      -- (without-x1) pu go'i
      it "supports 'pu go'i'" $ do
        validateBridiRetrieval
            "pu go'i" $
            SimpleBridi False "go'i" [] ["pu ku"]
      it "supports 'pu se go'i'" $ do
        validateBridiRetrieval
            "pu se go'i" $
            SimpleBridi False "go'i" [] ["pu ku"]
      it "supports 'pu ca ba go'i'" $ do
        validateBridiRetrieval
            "pu ca ba go'i" $
            SimpleBridi False "go'i" [] ["pu ku", "ca ku", "ba ku"]
      it "supports 'pu ca ba se go'i'" $ do
        validateBridiRetrieval
            "pu ca ba se go'i" $
            SimpleBridi False "go'i" [] ["pu ku", "ca ku", "ba ku"]
      -- (with-x1) nelci
      it "supports 'mi nelci'" $ do
        validateBridiRetrieval
            "mi nelci" $
            SimpleBridi False "nelci" ["mi"] []
      it "supports 'mi se nelci'" $ do
        validateBridiRetrieval
            "mi se nelci" $
            SimpleBridi False "nelci" ["", "mi"] []
      it "supports 'mi nelci do'" $ do
        validateBridiRetrieval
            "mi nelci do" $
            SimpleBridi False "nelci" ["mi", "do"] []
      it "supports 'mi se nelci do'" $ do
        validateBridiRetrieval
            "mi se nelci do" $
            SimpleBridi False "nelci" ["do", "mi"] []
      -- (with-x1) pu nelci
      -- TODO: make the following three tests work
      --it "supports 'mi pu ca nelci'" $ do
        --validateBridiRetrieval
            --"mi pu ca nelci" $
            --SimpleBridi False "nelci" ["mi"] ["pu ku", "ca ku"]
      --it "supports 'mi pu ku ca nelci'" $ do
        --validateBridiRetrieval
            --"mi pu ku ca nelci" $
            --SimpleBridi False "nelci" ["mi"] ["pu ku", "ca ku"]
      it "supports 'mi pu ku ca ku nelci'" $ do
        validateBridiRetrieval
            "mi pu ku ca ku nelci" $
            SimpleBridi False "nelci" ["mi"] ["pu ku", "ca ku"]
      it "supports 'pu ku ca ku mi nelci'" $ do
        validateBridiRetrieval
            "pu ku ca ku mi nelci" $
            SimpleBridi False "nelci" ["mi"] ["pu ku", "ca ku"]
      it "supports 'mi pu ku ca ku nelci do'" $ do
        validateBridiRetrieval
            "mi pu ku ca ku nelci do" $
            SimpleBridi False "nelci" ["mi", "do"] ["pu ku", "ca ku"]
      it "supports 'mi pu ku ca ku se nelci do'" $ do
        validateBridiRetrieval
            "mi pu ku ca ku se nelci do" $
            SimpleBridi False "nelci" ["do", "mi"] ["pu ku", "ca ku"]
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
        validateEquivalentSentences
            "pu ku lo prenu ku dunda lo mlatu ku"
            [ "lo prenu pu dunda lo mlatu"
            , "pu ku lo prenu cu dunda lo mlatu"
            , "pu ku lo prenu ku dunda lo mlatu"
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
