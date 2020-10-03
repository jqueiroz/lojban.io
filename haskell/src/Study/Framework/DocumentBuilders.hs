{-# LANGUAGE OverloadedStrings #-}

-- | This module provides utilities for building Pandoc documents.
module Study.Framework.DocumentBuilders
( buildDocumentFromMarkdownCode
, buildGlossaryDocument
) where

import Core
import Language.Lojban.Core
import qualified Data.Text as T
import qualified Text.Pandoc as P

-- | Given Markdown code, produces a Pandoc document.
buildDocumentFromMarkdownCode :: T.Text -> Either P.PandocError P.Pandoc
buildDocumentFromMarkdownCode code = P.runPure $ P.readMarkdown P.def
    { P.readerStripComments = True
    , P.readerExtensions = P.extensionsFromList [ P.Ext_raw_html, P.Ext_markdown_in_html_blocks, P.Ext_subscript, P.Ext_simple_tables, P.Ext_multiline_tables, P.Ext_pipe_tables ]
    } $ preprocessMarkdownCode code

-- | Proprocess Markdown code to handle subscripts.
preprocessMarkdownCode :: T.Text -> T.Text
preprocessMarkdownCode = (T.replace "x_1" "x~1~") . (T.replace "x_2" "x~2~") . (T.replace "x_3" "x~3~") . (T.replace "x_4" "x~4~") . (T.replace "x_5" "x~5~")

-- | Given a dictionary and a vocabulary, produces a glossary in the form of a Pandoc document.
buildGlossaryDocument :: Dictionary -> Vocabulary -> P.Pandoc
buildGlossaryDocument dictionary vocabulary = document where
    Right document = buildDocumentFromMarkdownCode code
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
