{-# LANGUAGE TypeFamilies, QuasiQuotes, PatternGuards #-}

module Language.Eberban.Parser.Experimental.Parser where

import Prelude hiding (Word)
import Text.Papillon
import Data.Maybe

parse :: String -> Either String String
parse src
    | Right (r, _) <- parsed = Right r
    | Left l <- parsed = Left $ showParseError l
    where
        parsed = runError $ gerna_textAll $ gerna_parse src

showParseError :: ParseError (Pos String) Gerna_Derivs -> String
showParseError pe =
    unwords (map (showReading d) ns) ++ (if null ns then "" else " ") ++
    m ++ c ++ " at position: " ++ show p
    where
        [c, m, _] = ($ pe) `map` [peCode, peMessage, peComment]
        ns = peReading pe
        d = peDerivs pe
        p = pePositionS pe

showReading :: Gerna_Derivs -> String -> String
showReading d n
    | n == "char", Right (c, _) <- runError $ gerna_char d = show c
    | otherwise = "yet: " ++ n

[papillon|

prefix: "gerna_"

textAll :: (String) = 'a' { "some string" }

|]
