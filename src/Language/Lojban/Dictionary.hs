module Language.Lojban.Dictionary
( englishDictionary
) where

import Core
import DictionaryLoader (loadDictionary)

englishDictionary :: Dictionary
englishDictionary = loadDictionary
