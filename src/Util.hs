{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Util where

import Core
import Control.Arrow (second)
import Control.Applicative (liftA2)
import System.Random (StdGen, random, mkStdGen, split)
import qualified Data.Text as T
import Data.List (group, sort, intersperse)
import System.Random.Shuffle (shuffle')

-- Domain-specific

-- Returns a Translation containing only the first (canonical) Lojban sentence
narrowTranslation :: Translation -> Translation
narrowTranslation (lojban_sentences, english_sentences) = ([head lojban_sentences], english_sentences)

-- Returns a TranslationGenerator containing only the first (canonical) Lojban sentence
narrowTranslationGenerator :: TranslationGenerator -> TranslationGenerator
narrowTranslationGenerator translationGenerator = translationGenerator' where
    translationGenerator' :: TranslationGenerator
    translationGenerator' r0 = (narrowTranslation originalTranslation, r1) where
        (originalTranslation, r1) = translationGenerator r0

-- Returns a TranslationGeneratorByExpression containing only the first (canonical) Lojban sentence
narrowTranslationGeneratorByExpression :: TranslationGeneratorByExpression -> TranslationGeneratorByExpression
narrowTranslationGeneratorByExpression = map (second narrowTranslationGenerator)

-- Function manipulation
compose2 :: (t1 -> t2) -> (t3 -> t4 -> t1) -> t3 -> t4 -> t2
compose2 f g x y = f (g x y)

-- List manipulation
stripLeft :: (Eq a) => a -> [a] -> [a]
stripLeft x = dropWhile (==x)

stripRight :: (Eq a) => a -> [a] -> [a]
stripRight x = reverse . stripLeft x . reverse

strip :: (Eq a) => a -> [a] -> [a]
strip x = stripLeft x . stripRight x

replace :: (Eq a) => a -> a -> [a] -> [a]
replace x y = map r where
    r z = if z == x then y else z

filterSnd :: (b -> Bool) -> [(a, b)] -> [(a, b)]
filterSnd f = filter (f . snd)

filterOutWords :: [T.Text] -> [(Int, T.Text)] -> [(Int, T.Text)]
filterOutWords forbiddenWords expressions = foldr filterOutWord expressions forbiddenWords

filterOutWord :: T.Text -> [(Int, T.Text)] -> [(Int, T.Text)]
filterOutWord forbiddenWord = filterSnd $ not . (isSubexpressionOf forbiddenWord)

sortUniq :: (Ord a) => [a] -> [a]
sortUniq = (map head) . group . sort

headOrDefault :: a -> [a] -> a
headOrDefault d [] = d
headOrDefault _ (h:_) = h

isContiguousSequence :: (Integral a) => [a] -> Bool
isContiguousSequence xs = all (== 1) $ zipWith (-) (tail xs) xs

infixr 5 ?:
(?:) :: (Eq a) => a -> [a] -> [a]
x ?: xs
    | x `elem` xs = xs
    | otherwise   = x:xs

concatET :: [Either String T.Text] -> Either String T.Text
concatET = foldl1 (liftA2 T.append)

unwordsET :: [Either String T.Text] -> Either String T.Text
unwordsET = concatET . intersperse (Right $ T.pack " ")

-- String manipulation
substr :: Int -> Int -> T.Text -> T.Text
substr beg end = T.drop beg . T.take end

subfield :: Int -> Int -> T.Text -> T.Text
subfield beg end = T.strip . substr beg end

isWordOf :: T.Text -> T.Text -> Bool
isWordOf word = (word `elem`) . T.words

isSubexpressionOf :: T.Text -> T.Text -> Bool
isSubexpressionOf expr text = (expr == text) || (prefixExpr `T.isPrefixOf` text) || (suffixExpr `T.isSuffixOf` text) || (infixExpr `T.isInfixOf` text) where
    prefixExpr = expr `T.append` " "
    suffixExpr = " " `T.append` expr
    infixExpr = T.concat [" ", expr, " "]

replaceFirstSubstring :: T.Text -> T.Text -> T.Text -> T.Text
replaceFirstSubstring old new text =
    let
        components = T.splitOn old text
    in
        if null components then
            text
        else
            (head components) `T.append` new `T.append` (T.intercalate old (tail components))

replaceFirstSubexpression :: T.Text -> T.Text -> T.Text -> T.Text
replaceFirstSubexpression old new = T.drop 1 . T.dropEnd 1 . replaceFirstSubstring (" " `T.append` old `T.append` " ") (" " `T.append` new `T.append` " ") . (`T.append` " ") . (" " `T.append`)

replaceSubexpression :: T.Text -> T.Text -> T.Text -> T.Text
replaceSubexpression old new = T.drop 1 . T.dropEnd 1 . T.replace (" " `T.append` old `T.append` " ") (" " `T.append` new `T.append` " ") . (`T.append` " ") . (" " `T.append`)

-- Random (TODO: assert that sum > 0)
shuffleList :: StdGen -> [a] -> [a]
shuffleList r0 xs = shuffle' xs (length xs) r0

shuffle :: StdGen -> [a] -> ([a], StdGen)
shuffle r0 xs = (shuffle' xs (length xs) r1, r2) where
    (r1, r2) = split r0

chooseItem :: StdGen -> [(Int, a)] -> (a, StdGen)
chooseItem r0 [] = error "choosing item from empty list"
chooseItem r0 xs = (f 0 xs, r1) where
    (val, r1) = (random r0)
    pos = val `mod` (sum $ map fst xs)
    f acc ((n, x):xs)
        | (acc+n) > pos = x
        | otherwise = f (acc+n) xs

chooseItemUniformly :: StdGen -> [a] -> (a, StdGen)
chooseItemUniformly r0 xs = chooseItem r0 $ map (1,) xs

chooseItemsUniformly :: (Eq a) => StdGen -> Int -> [a] -> ([a], StdGen)
chooseItemsUniformly r0 0 _ = ([], r0)
chooseItemsUniformly r0 _ [] = error "not enough items to choose from"
chooseItemsUniformly r0 q xs = (y:ys, r2) where
    (y, r1) = chooseItemUniformly r0 xs
    (ys, r2) = chooseItemsUniformly r1 (q-1) (filter (/= y) xs)

generatorFromSingleton :: a -> StdGen -> (a, StdGen)
generatorFromSingleton x r0 = (x, r0)

generatorFromList :: [a] -> StdGen -> (a, StdGen)
generatorFromList = flip chooseItemUniformly

generatorFromWeightedList :: [(Int, a)] -> StdGen -> (a, StdGen)
generatorFromWeightedList = flip chooseItem

-- combineSimpleFunctions :: [(Int, a)] -> (StdGen -> a)

combineFunctions :: [(Int, StdGen -> a)] -> (StdGen -> a)
combineFunctions fs r0 =
    let (f, r1) = chooseItem r0 fs
    in f r1

combineFunctionsUniformly :: [StdGen -> a] -> (StdGen -> a)
combineFunctionsUniformly fs = combineFunctions $ map (1,) fs

mapRandom :: StdGen -> (StdGen -> a -> (b, StdGen)) -> [a] -> ([b], StdGen)
mapRandom r0 _ [] = ([], r0)
mapRandom r0 f (x:xs) = (f_x : f_xs, r2) where
    (f_x, r1) = f r0 x
    (f_xs, r2) = mapRandom r1 f xs

-- Tests
testChooseItem :: Int -> T.Text
testChooseItem x = fst $ chooseItem (mkStdGen x) [(5, "a"), (2, "b"), (3, "c"), (10, "d")]

testChooseItemUniformly :: Int -> T.Text
testChooseItemUniformly x = fst $ chooseItemUniformly (mkStdGen x) ["a", "b", "c", "d"]

testChooseFunctionUniformly :: Int -> T.Text
testChooseFunctionUniformly x = combineFunctionsUniformly [const "a", const "b", const "c"] (mkStdGen x)

-- Old implementations

{-chooseItemUniformly :: StdGen -> [a] -> (a, StdGen)-}
{-chooseItemUniformly r0 [] = error "choosing item from empty list"-}
{-chooseItemUniformly r0 list =-}
    {-let len = length list-}
        {-(p, r1) = random r0-}
        {-x = list !! (p `mod` len)-}
    {-in (x, r1)-}

{-chooseFunctionUniformly :: [(StdGen -> a)] -> (StdGen -> a)-}
{-chooseFunctionUniformly [] r0 = error "choosing function from empty list"-}
{-chooseFunctionUniformly fs r0 =-}
    {-let len = length fs-}
        {-(f, r1) = chooseItemUniformly r0 fs-}
    {-in f r1-}

curryUniformly :: (a -> b) -> [a] -> (StdGen -> b)
curryUniformly f [] r0 = error "choosing argument from empty list"
curryUniformly f xs r0 =
    let len = length xs
        (p, r1) = random r0
        x = xs !! (p `mod` len)
    in f x
