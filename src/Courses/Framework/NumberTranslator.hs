{-# LANGUAGE OverloadedStrings #-}

module Courses.Framework.NumberTranslator
( numberToSimpleLojban
, lojbanToNumber
) where

import Text.Read (readMaybe)
import Control.Applicative ((<$>), (<*>))
import qualified Data.Text as T

-- Digit
digitToLojban :: Char -> T.Text
digitToLojban '0' = "no"
digitToLojban '1' = "pa"
digitToLojban '2' = "re"
digitToLojban '3' = "ci"
digitToLojban '4' = "vo"
digitToLojban '5' = "mu"
digitToLojban '6' = "xa"
digitToLojban '7' = "ze"
digitToLojban '8' = "bi"
digitToLojban '9' = "so"

lojbanToDigit :: T.Text -> Maybe Char
lojbanToDigit "no" = Just '0'
lojbanToDigit "pa" = Just '1'
lojbanToDigit "re" = Just '2'
lojbanToDigit "ci" = Just '3'
lojbanToDigit "vo" = Just '4'
lojbanToDigit "mu" = Just '5'
lojbanToDigit "xa" = Just '6'
lojbanToDigit "ze" = Just '7'
lojbanToDigit "bi" = Just '8'
lojbanToDigit "so" = Just '9'
lojbanToDigit _ = Nothing

-- Text
numberToSimpleLojban :: Integer -> T.Text
numberToSimpleLojban = T.concat . map digitToLojban . show

lojbanToNumber :: T.Text -> Maybe Integer
lojbanToNumber t =
    case sequence subnumbers of
        Just subnumbers' -> readMaybe $ T.unpack $ T.concat subnumbers'
        Nothing -> Nothing
    where subnumbers = map (fmap $ T.justifyRight 3 '0') $ map simpleLojbanToNumberText $ T.splitOn "ki'o" t

simpleLojbanToNumber :: T.Text -> Maybe Integer
simpleLojbanToNumber t =
    case x of
        Just x' -> readMaybe $ T.unpack x'
        Nothing -> Nothing
    where x = simpleLojbanToNumberText t

simpleLojbanToNumberText :: T.Text -> Maybe T.Text
simpleLojbanToNumberText "" = Just ""
simpleLojbanToNumberText text =
    T.cons <$> lojbanToDigit prefix <*> simpleLojbanToNumberText suffix
    where (prefix, suffix) = T.splitAt 2 text
