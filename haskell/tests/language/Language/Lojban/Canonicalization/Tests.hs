{-# LANGUAGE OverloadedStrings #-}

module Language.Lojban.Canonicalization.Tests
( validateCanonicalization
) where

import Language.Lojban.Core
import Language.Lojban.Parsing (parseText)
import Language.Lojban.Canonicalization
import Language.Lojban.Canonicalization.Internals
import Test.Hspec
import Data.Either
import qualified Data.Text as T

validateSimpleBridiRetrieval :: T.Text -> SimpleBridi -> IO ()
validateSimpleBridiRetrieval text expectedSimpleBridi = simpleBridi `shouldBe` (Right expectedSimpleBridi) where
    simpleBridi = (parseText text) >>= retrieveSimpleBridi

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

validateEquivalentTerms :: T.Text -> [T.Text] -> IO ()
validateEquivalentTerms = validateEquivalentSentences

validateTerms :: [(T.Text, T.Text)] -> IO ()
validateTerms = validateSentences

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
        validateEquivalentTerms
            "lo prenu ku"
            [ "lo prenu"
            ]
      it "supports 'lo pu prenu'" $ do
        validateEquivalentTerms
            "lo pu prenu ku"
            [ "lo pu prenu"
            ]
      it "supports 'lo gleki prenu'" $ do
        validateEquivalentTerms
            "lo gleki prenu ku"
            [ "lo gleki prenu"
            ]
      it "supports 'lo gleki te dunda'" $ do
        validateEquivalentTerms
            "lo gleki te dunda ku"
            [ "lo gleki te dunda"
            ]
      it "supports 'lo selprami' and 'lo selpa'i'" $ do
        validateEquivalentTerms
            "lo se prami ku"
            [ "lo se prami"
            , "lo selprami"
            , "lo selpa'i"
            ]
      it "supports 'lo tertavla' and 'lo terta'a'" $ do
        validateEquivalentTerms
            "lo te tavla ku"
            [ "lo te tavla"
            , "lo tertavla"
            , "lo terta'a"
            ]
      -- TODO: fix the following canonicalization
      --it "supports 'lo tertavydu'a'" $ do
        --validateEquivalentTerms
            --"lo tertavydu'a ku"
            --[ "lo tertavydu'a ku"
            --, "lo tertavydu'a"
            --]
      it "supports 'lo zdani pe mi'" $ do
        validateEquivalentTerms
            "lo zdani ku pe mi ge'u"
            [ "lo zdani pe mi"
            , "lo mi zdani"
            ]
      it "supports 'mi .a do'" $ do
        validateEquivalentTerms
            "mi .a do"
            [
            ]
      it "supports 'mi .a lo gerku'" $ do
        validateEquivalentTerms
            "mi .a lo gerku ku"
            [ "mi .a lo gerku"
            ]
      it "supports 'mi .e do'" $ do
        validateEquivalentTerms
            "mi .e do"
            [
            ]
      it "supports 'mi .e lo gerku'" $ do
        validateEquivalentTerms
            "mi .e lo gerku ku"
            [ "mi .e lo gerku"
            ]
      -- TODO: fix the following canonicalization
      --it "supports 'lo zdani po'e mi'" $ do
        --validateEquivalentTerms
            --"lo zdani ku po'e mi ge'u"
            --[ "lo zdani po'e mi"
            --]
      it "supports 'zo prenu'" $ do
        validateEquivalentTerms
            "lo'u prenu le'u"
            [ "zo prenu"
            ]
      it "supports 'lu mi tavla do'" $ do
        validateEquivalentTerms
            "lu mi tavla do li'u"
            [ "lu mi tavla do"
            ]
      it "supports 'lo'u mi je tavla le'u'" $ do
        validateEquivalentTerms
            "lo'u mi je tavla le'u"
            [
            ]
      it "supports 'lo na'e prenu'" $ do
        validateEquivalentTerms
            "lo na'e prenu ku"
            [ "lo na'e prenu"
            ]
      it "supports 'lo to'e prenu'" $ do
        validateEquivalentTerms
            "lo to'e prenu ku"
            [ "lo to'e prenu"
            ]
      it "supports 'lo cmalu zdani'" $ do
        validateEquivalentTerms
            "lo cmalu zdani ku"
            [ "lo cmalu zdani"
            ]
      it "supports 'lo re mlatu'" $ do
        validateEquivalentTerms
            "lo re mlatu ku"
            [ "lo re mlatu"
            ]
      it "supports 'lo ci te vecnu'" $ do
        validateEquivalentTerms
            "lo ci te vecnu ku"
            [ "lo ci te vecnu"
            ]
      it "supports 'lo zu'a gerku'" $ do
        validateEquivalentTerms
            "lo zu'a gerku ku"
            [ "lo zu'a gerku"
            , "lozu'a gerku"
            ]
      it "supports 'lo vi gerku'" $ do
        validateEquivalentTerms
            "lo vi gerku ku"
            [ "lo vi gerku"
            , "lovi gerku"
            ]
      it "supports 'lo zu'a vi gerku'" $ do
        validateEquivalentTerms
            "lo zu'a vi gerku ku"
            [ "lo zu'a vi gerku"
            , "lo zu'avi gerku"
            , "lozu'avi gerku"
            ]
      it "supports 'lo pareci mlatu'" $ do
        validateEquivalentTerms
            "lo pareci mlatu ku"
            [ "lo pa re ci mlatu"
            , "lo pare ci mlatu"
            , "lo pa reci mlatu"
            , "lo pareci mlatu"
            ]
      -- TODO: support the following construct
      --it "supports 'lo prenu be bu'u lo zdani'" $ do
        --validateEquivalentTerms
            --"lo prenu be bu'u lo zdani ku"
            --[ "lo prenu be bu'u lo zdani ku"
            --, "lo bu'u lo zdani ku prenu ku"
            --]
    describe "Basic sentence canonicalizer" $ do
      it "supports SE" $ do
        validateSentences
            [ ("mi se nelci lo mlatu", "lo mlatu ku nelci mi")
            , ("lo mlatu cu te tavla", "tavla zo'e lo mlatu ku")
            , ("se tavla", "tavla")
            ]
      it "supports FA" $ do
        validateSentences
            [ ("tavla fi lo mlatu", "tavla zo'e lo mlatu ku")
            , ("mi tavla fi lo mlatu", "mi tavla zo'e lo mlatu ku")
            , ("mi ctuca fo lo bangu", "mi ctuca zo'e zo'e lo bangu ku")
            ]
      it "supports BE" $ do
        validateSentences
            [ ("mi nelci lo cmene be mi", "mi nelci lo cmene be mi be'o ku")
            , ("mi tavla lo vecnu be lo zdani bei do", "mi tavla lo vecnu be lo zdani ku bei do be'o ku")
            , ("lo tavla be zo'e bei do cu melbi", "lo tavla be zo'e bei do be'o ku melbi")
            , ("lo tavla be zo'e bei do ku melbi", "lo tavla be zo'e bei do be'o ku melbi")
            ]
        validateEquivalentSentences
            "lo dunda be zo'e bei lo prenu ku be'o ku"
            [ "lo dunda be zo'e bei lo prenu"
            , "lo dunda bezo'ebei lo prenu"
            , "lo dunda be fi lo prenu"
            , "lo dunda befi lo prenu"
            ]
      it "supports apostrophes" $ do
        validateSentences
            [ ("mi nelci lo cmene be mi be’o", "mi nelci lo cmene be mi be'o ku")
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
            , ("xu tavla fi mi", "xu tavla zo'e mi")
            ]
      it "supports NA" $ do
        validateSentences
            [ ("mi na cusku", "na ku mi cusku")
            , ("lo prenu cu na gleki", "na ku lo prenu ku gleki")
            , ("lo prenu na gleki", "na ku lo prenu ku gleki")
            , ("lo ba te vecnu cu na gleki", "na ku lo ba te vecnu ku gleki")
            , ("mi na pu mlatu", "pu ku na ku mi mlatu")
            , ("mi pu na mlatu", "pu ku na ku mi mlatu")
            ]
      it "supports BPFK-2016-03-15" $ do
        validateSentences
            [ ("lo nu broda ba brode", "ba ku lo nu broda kei ku brode")
            ]
      it "supports NAhE" $ do
        validateSentences
            [ ("mi na'e prenu", "mi na'e prenu")
            , ("mi na'e nelci do", "mi na'e nelci do")
            , ("mi to'e prenu", "mi to'e prenu")
            , ("mi to'e nelci do", "mi to'e nelci do")
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
      it "supports tanru" $ do
        validateSentences
            [ ("mutce melbi", "mutce melbi")
            , ("pu mutce melbi", "pu ku mutce melbi")
            , ("se mutce melbi", "se mutce melbi")
            , ("mutce se melbi", "mutce se melbi") -- ideally should return "mutce melbi", but this has not yet been implemented
            , ("lo mlatu cu mutce melbi", "lo mlatu ku mutce melbi")
            , ("lo mlatu pu mutce melbi", "pu ku lo mlatu ku mutce melbi")
            ]
      it "supports jai" $ do
        validateSentences
            [ ("do jai lerci", "do jai lerci")
            , ("mi tavla do noi jai lerci", "mi tavla do noi jai lerci ku'o")
            , ("do jai mukti lo nu ctuca kei mi", "do jai mukti lo nu ctuca kei ku mi")
            , ("lo prenu cu jai lerci", "lo prenu ku jai lerci")
            , ("jai se lerci do", "do jai lerci")
            , ("do jai se djica mi", "mi jai djica do") -- TODO: incorrect! should actually be "mi djica tu'a do" (or at least "do jai se djica mi"), but that's acceptable for now
            , ("do se jai djica mi", "mi jai djica do")
            ]
      it "supports tu'a" $ do
        validateSentences
            [ ("tu'a do lerci", "tu'a do lerci")
            , ("se lerci tu'a do", "tu'a do lerci")
            , ("mi djica tu'a do", "mi djica tu'a do")
            , ("xu do cusku tu'a lo plise", "xu do cusku tu'a lo plise ku")
            , ("do gasnu tu'a lo skami", "do gasnu tu'a lo skami ku")
            ]
      it "supports BAI" $ do
        validateSentences
            [ ("gau mi cmene lo mlatu", "fi'o gasnu fe'u mi cmene lo mlatu ku")
            , ("cmene lo mlatu gau mi", "fi'o gasnu fe'u mi cmene lo mlatu ku")
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
            , ("lo nu mu'i ma dunda kei ku te tavla fi do", "do tavla zo'e lo nu fi'o mukti fe'u ma dunda kei ku")
            ]
