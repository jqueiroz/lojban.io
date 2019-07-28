{-# LANGUAGE OverloadedStrings #-}

module Courses.Util.Documents
( buildDocument
, buildVocabularyDocument
) where

import Core
import Language.Lojban.Core
import qualified Data.Text as T
import qualified Text.Pandoc as P

buildDocument :: T.Text -> Either P.PandocError P.Pandoc
buildDocument code = P.runPure $ P.readMarkdown P.def
    { P.readerStripComments = True
    , P.readerExtensions = P.extensionsFromList [ P.Ext_raw_html, P.Ext_subscript ]
    } $ preprocessCode code

preprocessCode :: T.Text -> T.Text
preprocessCode = (T.replace "_1" "~1~") . (T.replace "_2" "~2~") . (T.replace "_3" "~3~") . (T.replace "_4" "~4~") . (T.replace "_5" "~5~")

buildVocabularyDocument :: Dictionary -> Vocabulary -> P.Pandoc
buildVocabularyDocument dictionary vocabulary = document where
    Right document = buildDocument code
    -- Code
    code = T.concat $ [] ++
        if null brivlaList then [] else ["### Brivla\n", brivlaCode, "\n\n"] ++
        if null cmavoList then [] else ["### Cmavo\n", cmavoCode, "\n\n"]
    cmavoCode = handleWordList cmavoList
    brivlaCode = handleWordList brivlaList
    -- Word lists
    cmavoList = vocabularyCmavoList vocabulary
    brivlaList = vocabularyBrivlaList vocabulary
    -- Hanndle word list
    handleWordList :: [T.Text] -> T.Text
    handleWordList = T.intercalate "\n\n" . map handleWord
    -- Handle word
    handleWord :: T.Text -> T.Text
    handleWord word = T.concat ["<span class=\"definition-head\">", word, "</span> ", dictLookupValsiDefinition dictionary word]
