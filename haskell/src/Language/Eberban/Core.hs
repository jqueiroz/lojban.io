module Language.Eberban.Core
( Dictionary (..)
, Entry (..)
, dictFindEntry
) where

import qualified Data.Text as T
import qualified Data.Map as M

-- * Lexicon
data Dictionary = Dictionary
    { dictIdentifier :: T.Text
    , dictEntries :: M.Map T.Text Entry
    } deriving (Show)

data Entry = Entry
    { entryText :: T.Text
    , entryFamily :: T.Text
    , entrySignature :: Maybe T.Text
    , entryEnglishShort :: T.Text
    , entryEnglishLong :: T.Text
    , entryLojbanSimilar :: Maybe T.Text
    } deriving (Show)

instance Eq Entry where
    x == y = (entryText x) == (entryText y)

-- * Helper functions

dictFindEntry :: Dictionary -> T.Text -> Maybe Entry
dictFindEntry dictionary key = (dictEntries dictionary) M.!? key
