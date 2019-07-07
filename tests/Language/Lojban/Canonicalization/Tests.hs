{-# LANGUAGE OverloadedStrings #-}

module Language.Lojban.Canonicalization.Tests
( validateCanonicalization
) where

import Language.Lojban.Core
import Language.Lojban.Parsing (parse)
import Language.Lojban.Canonicalization.Internals
import Test.Hspec
import Data.Either
import qualified Data.Text as T

validateSimpleBridiRetrieval :: T.Text -> SimpleBridi -> IO ()
validateSimpleBridiRetrieval text expectedSimpleBridi = simpleBridi `shouldBe` (Right expectedSimpleBridi) where
    simpleBridi = (parse text) >>= retrieveSimpleBridi

validateTermCanonicalization :: T.Text -> T.Text -> IO ()
validateTermCanonicalization term expectedCanonicalizedTerm = canonicalizedTerm `shouldBe` (Right expectedCanonicalizedTerm) where
    canonicalizedTerm = basicSentenceCanonicalizer term

validateEquivalentSentences :: T.Text -> [T.Text] -> IO ()
validateEquivalentSentences canonicalSentence originalSentences = lhs `shouldBe` rhs where
    lhs = map basicSentenceCanonicalizer (canonicalSentence : originalSentences)
    rhs = replicate (length lhs) $ Right canonicalSentence

validateSentences :: [(T.Text, T.Text)] -> IO ()
validateSentences sentences = map basicSentenceCanonicalizer (originalSentences ++ canonicalSentences) `shouldBe` map Right (canonicalSentences ++ canonicalSentences) where
  originalSentences = map fst sentences
  canonicalSentences = map snd sentences

validateCanonicalization :: IO ()
validateCanonicalization = hspec $ do
    describe "SimpleBridi retrieval" $ do
      -- (without-x1) nelci
      it "supports 'nelci'" $ do
        validateSimpleBridiRetrieval
            "nelci" $
            SimpleBridi False "nelci" [] []
      it "supports 'se nelci'" $ do
        validateSimpleBridiRetrieval
            "se nelci" $
            SimpleBridi False "nelci" [] []
      it "supports 'nelci do'" $ do
        validateSimpleBridiRetrieval
            "nelci do" $
            SimpleBridi False "nelci" ["", "do"] []
      it "supports 'se nelci do'" $ do
        validateSimpleBridiRetrieval
            "se nelci do" $
            SimpleBridi False "nelci" ["do"] []
      -- (without-x1) go'i
      it "supports 'go'i'" $ do
        validateSimpleBridiRetrieval
            "go'i" $
            SimpleBridi False "go'i" [] []
      it "supports 'se go'i'" $ do
        validateSimpleBridiRetrieval
            "se go'i" $
            SimpleBridi False "go'i" [] []
      it "supports 'go'i do'" $ do
        validateSimpleBridiRetrieval
            "go'i do" $
            SimpleBridi False "go'i" ["", "do"] []
      it "supports 'se go'i do'" $ do
        validateSimpleBridiRetrieval
            "se go'i do" $
            SimpleBridi False "go'i" ["do"] []
      -- (without-x1) pu nelci
      it "supports 'pu nelci'" $ do
        validateSimpleBridiRetrieval
            "pu nelci" $
            SimpleBridi False "nelci" [] ["pu ku"]
      it "supports 'pu se nelci'" $ do
        validateSimpleBridiRetrieval
            "pu se nelci" $
            SimpleBridi False "nelci" [] ["pu ku"]
      it "supports 'pu ca ba nelci'" $ do
        validateSimpleBridiRetrieval
            "pu ca ba nelci" $
            SimpleBridi False "nelci" [] ["pu ku", "ca ku", "ba ku"]
      it "supports 'pu ca ba se nelci'" $ do
        validateSimpleBridiRetrieval
            "pu ca ba se nelci" $
            SimpleBridi False "nelci" [] ["pu ku", "ca ku", "ba ku"]
      -- (without-x1) pu ku nelci
      it "supports 'pu ku nelci'" $ do
        validateSimpleBridiRetrieval
            "pu ku nelci" $
            SimpleBridi False "nelci" [] ["pu ku"]
      it "supports 'pu ku se nelci'" $ do
        validateSimpleBridiRetrieval
            "pu ku se nelci" $
            SimpleBridi False "nelci" [] ["pu ku"]
      -- (without-x1) pu go'i
      it "supports 'pu go'i'" $ do
        validateSimpleBridiRetrieval
            "pu go'i" $
            SimpleBridi False "go'i" [] ["pu ku"]
      it "supports 'pu se go'i'" $ do
        validateSimpleBridiRetrieval
            "pu se go'i" $
            SimpleBridi False "go'i" [] ["pu ku"]
      it "supports 'pu ca ba go'i'" $ do
        validateSimpleBridiRetrieval
            "pu ca ba go'i" $
            SimpleBridi False "go'i" [] ["pu ku", "ca ku", "ba ku"]
      it "supports 'pu ca ba se go'i'" $ do
        validateSimpleBridiRetrieval
            "pu ca ba se go'i" $
            SimpleBridi False "go'i" [] ["pu ku", "ca ku", "ba ku"]
      -- (without-x1) pu ku go'i
      it "supports 'pu ku go'i'" $ do
        validateSimpleBridiRetrieval
            "pu ku go'i" $
            SimpleBridi False "go'i" [] ["pu ku"]
      it "supports 'pu ku se go'i'" $ do
        validateSimpleBridiRetrieval
            "pu ku se go'i" $
            SimpleBridi False "go'i" [] ["pu ku"]
      -- (with-x1) nelci
      it "supports 'mi nelci'" $ do
        validateSimpleBridiRetrieval
            "mi nelci" $
            SimpleBridi False "nelci" ["mi"] []
      it "supports 'mi se nelci'" $ do
        validateSimpleBridiRetrieval
            "mi se nelci" $
            SimpleBridi False "nelci" ["", "mi"] []
      it "supports 'mi nelci do'" $ do
        validateSimpleBridiRetrieval
            "mi nelci do" $
            SimpleBridi False "nelci" ["mi", "do"] []
      it "supports 'mi se nelci do'" $ do
        validateSimpleBridiRetrieval
            "mi se nelci do" $
            SimpleBridi False "nelci" ["do", "mi"] []
      -- (with-x1) pu nelci
      it "supports 'mi pu nelci'" $ do
        validateSimpleBridiRetrieval
            "mi pu nelci" $
            SimpleBridi False "nelci" ["mi"] ["pu ku"]
      it "supports 'mi pu se nelci'" $ do
        validateSimpleBridiRetrieval
            "mi pu se nelci" $
            SimpleBridi False "nelci" ["", "mi"] ["pu ku"]
      it "supports 'mi pu nelci do'" $ do
        validateSimpleBridiRetrieval
            "mi pu nelci do" $
            SimpleBridi False "nelci" ["mi", "do"] ["pu ku"]
      it "supports 'mi pu se nelci do'" $ do
        validateSimpleBridiRetrieval
            "mi pu se nelci do" $
            SimpleBridi False "nelci" ["do", "mi"] ["pu ku"]
      it "supports 'mi pu ca nelci'" $ do
        validateSimpleBridiRetrieval
            "mi pu ca nelci" $
            SimpleBridi False "nelci" ["mi"] ["pu ku", "ca ku"]
      it "supports 'mi pu ca se nelci'" $ do
        validateSimpleBridiRetrieval
            "mi pu ca se nelci" $
            SimpleBridi False "nelci" ["", "mi"] ["pu ku", "ca ku"]
      it "supports 'mi pu ca nelci do'" $ do
        validateSimpleBridiRetrieval
            "mi pu ca nelci do" $
            SimpleBridi False "nelci" ["mi", "do"] ["pu ku", "ca ku"]
      it "supports 'mi pu ca se nelci do'" $ do
        validateSimpleBridiRetrieval
            "mi pu ca se nelci do" $
            SimpleBridi False "nelci" ["do", "mi"] ["pu ku", "ca ku"]
      -- (with-x1) pu ku nelci
      -- TODO: make the following test work
      --it "supports 'mi pu ku ca nelci'" $ do
        --validateSimpleBridiRetrieval
            --"mi pu ku ca nelci" $
            --SimpleBridi False "nelci" ["mi"] ["pu ku", "ca ku"]
      it "supports 'mi pu ku ca ku nelci'" $ do
        validateSimpleBridiRetrieval
            "mi pu ku ca ku nelci" $
            SimpleBridi False "nelci" ["mi"] ["pu ku", "ca ku"]
      it "supports 'pu ku ca ku mi nelci'" $ do
        validateSimpleBridiRetrieval
            "pu ku ca ku mi nelci" $
            SimpleBridi False "nelci" ["mi"] ["pu ku", "ca ku"]
      it "supports 'mi pu ku ca ku nelci do'" $ do
        validateSimpleBridiRetrieval
            "mi pu ku ca ku nelci do" $
            SimpleBridi False "nelci" ["mi", "do"] ["pu ku", "ca ku"]
      it "supports 'mi pu ku ca ku se nelci do'" $ do
        validateSimpleBridiRetrieval
            "mi pu ku ca ku se nelci do" $
            SimpleBridi False "nelci" ["do", "mi"] ["pu ku", "ca ku"]
    describe "Term canonicalization" $ do
      it "supports 'lo prenu'" $ do
        validateTermCanonicalization "lo prenu" "lo prenu ku"
      it "supports 'lo prenu ku'" $ do
        validateTermCanonicalization "lo prenu ku" "lo prenu ku"
      it "supports 'lo pu prenu'" $ do
        validateTermCanonicalization "lo pu prenu" "lo pu prenu ku"
      it "supports 'lo pu prenu ku'" $ do
        validateTermCanonicalization "lo pu prenu ku" "lo pu prenu ku"
      it "supports 'lo gleki prenu ku'" $ do
        validateTermCanonicalization "lo gleki prenu ku" "lo gleki prenu ku"
      it "supports 'lo gleki prenu'" $ do
        validateTermCanonicalization "lo gleki prenu" "lo gleki prenu ku"
      it "supports 'lo gleki te dunda ku'" $ do
        validateTermCanonicalization "lo gleki te dunda ku" "lo gleki te dunda ku"
      it "supports 'lo gleki te dunda'" $ do
        validateTermCanonicalization "lo gleki te dunda" "lo gleki te dunda ku"
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
        -- not followed by arguments; not containing prefix tag
        validateEquivalentSentences
            "fi'o zukte fe'u ku lo prenu ku klama lo zarci ku"
            [ "lo prenu fi'o zukte fe'u klama lo zarci"
            , "fi'o zukte ku lo prenu cu klama lo zarci"
            --, "fi'o zukte lo prenu cu klama lo zarci" -- should this work???
            , "zu'e ku lo prenu cu klama lo zarci"
            , "lo prenu zu'e klama lo zarci"
            ]
        -- not followed by arguments; containing prefix tag (note: semantically nonsensical)
        validateEquivalentSentences
            "fi'o se zukte fe'u ku lo prenu ku klama lo zarci ku"
            [ "lo prenu fi'o se zukte fe'u klama lo zarci"
            , "fi'o se zukte ku lo prenu cu klama lo zarci"
            , "se zu'e ku lo prenu cu klama lo zarci"
            , "lo prenu se zu'e klama lo zarci"
            ]
        -- followed by argument; not containing prefix tag (note: semantically nonsensical)
        validateEquivalentSentences
            "fi'o pilno fe'u lo skami ku mi fanva"
            [ "mi fanva fi'o pilno fe'u lo skami"
            , "mi fi'o pilno fe'u lo skami ku fanva"
            , "mi fi'o pilno fe'u lo skami cu fanva"
            , "mi fanva pi'o lo skami"
            , "pi'o lo skami ku mi fanva"
            ]
        validateEquivalentSentences
            "fi'o pilno fe'u lo skami ku mi fanva ta"
            [ "mi fanva ta fi'o pilno fe'u lo skami"
            , "mi fanva fi'o pilno fe'u lo skami ta"
            , "mi fi'o pilno fe'u lo skami ku fanva ta"
            , "mi fi'o pilno fe'u lo skami cu fanva ta"
            , "fi'o pilno fe'u lo skami ku mi fanva ta"
            , "mi fanva ta pi'o lo skami"
            , "mi fanva pi'o lo skami ta"
            , "mi pi'o lo skami ku fanva ta"
            , "mi pi'o lo skami cu fanva ta"
            , "pi'o lo skami ku mi fanva ta"
            ]
        -- followed by argument; containing prefix tag
        validateEquivalentSentences
            "fi'o se pilno fe'u lo skami ku mi fanva"
            [ "mi fanva fi'o se pilno fe'u lo skami"
            , "mi fi'o se pilno fe'u lo skami ku fanva"
            , "mi fi'o se pilno fe'u lo skami cu fanva"
            , "mi fanva se pi'o lo skami"
            , "se pi'o lo skami ku mi fanva"
            ]
        validateEquivalentSentences
            "fi'o se pilno fe'u lo skami ku mi fanva ta"
            [ "mi fanva ta fi'o se pilno fe'u lo skami"
            , "mi fanva fi'o se pilno fe'u lo skami ta"
            , "mi fi'o se pilno fe'u lo skami ku fanva ta"
            , "mi fi'o se pilno fe'u lo skami cu fanva ta"
            , "fi'o se pilno fe'u lo skami ku mi fanva ta"
            , "mi fanva ta se pi'o lo skami"
            , "mi fanva se pi'o lo skami ta"
            , "mi se pi'o lo skami ku fanva ta"
            , "mi se pi'o lo skami cu fanva ta"
            , "se pi'o lo skami ku mi fanva ta"
            ]
      it "supports complex sentences" $ do
        validateSentences
            [ ("mi nupre lo nu pilno lo skami poi do dunda ke'a mi", "mi nupre lo nu pilno lo skami ku poi do dunda ke'a mi ku'o kei ku")
            ]
