{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Language.Lojban.Canonicalization.Internals
( StructuredSelbri
, StructuredTerm
, ExtraTerm
, StructuredBridi
, basicSentenceCanonicalizer
, canonicalizeParsedText
, canonicalizeParsedBridi
, canonicalizeParsedTerm
, retrieveSimpleBridi
) where

import Language.Lojban.Core
import Language.Lojban.Parsing (parse)
import Language.Lojban.Presentation (displayCanonicalBridi)
import Util (headOrDefault, isContiguousSequence, concatET, unwordsET)
import Control.Applicative (liftA2)
import Control.Exception (assert)
import Control.Monad (mplus)
import Data.List (partition)
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Language.Lojban.Parser.ZasniGerna as ZG

------------------------- ----------------------- Sentence canonicalizers
--TODO: check whether se/te/ve/xe are left-associative or right-associative
--ZasniGerna documentation: https://hackage.haskell.org/package/zasni-gerna-0.0.7/docs/Language-Lojban-Parser-ZasniGerna.html
--TODO: support tanru

---------- Types
type StructuredSelbri = ZG.Text
type StructuredTerm = ZG.Text
type ExtraTerm = ZG.Text
type StructuredBridi = (StructuredSelbri, [(Int, StructuredTerm)], [ExtraTerm])

---------- Handle place tags (fa/fe/fi/fo/fu)
handlePlaceTags :: StructuredBridi -> Either String StructuredBridi
handlePlaceTags (selbri, [], extraTerms) = Right $ (selbri, [], extraTerms)
handlePlaceTags (selbri, terms, extraTerms) = assert (isContiguousSequence $ map fst terms) $ Right (selbri, f firstPosition terms, extraTerms) where
    firstPosition = fst $ head terms
    f :: Int -> [(Int, StructuredTerm)] -> [(Int, StructuredTerm)]
    f _ [] = []
    f defaultPosition (h:t) = let (tag, term) = retrieveTag (snd h)
                                  position = case tag of Just x -> retrievePosition x; Nothing -> defaultPosition
                              in (position, term) : f (position+1) t
    retrievePosition :: String -> Int
    retrievePosition "fa" = 1
    retrievePosition "fe" = 2
    retrievePosition "fi" = 3
    retrievePosition "fo" = 4
    retrievePosition "fu" = 5
    retrieveTag :: ZG.Text -> (Maybe String, ZG.Text)
    retrieveTag (ZG.Tag (ZG.FA x) y) = (Just x, y)
    retrieveTag x = (Nothing, x)

---------- Handle place permutations (se/te/ve/xe)
swapTerms :: Int -> Int -> [(Int, StructuredTerm)] -> [(Int, StructuredTerm)]
swapTerms x y terms = assert (x /= y) $ map f terms where
    f (k, t) = (if k == x then y else if k == y then x else k, t)
swapTerms2 :: String -> [(Int, StructuredTerm)] -> [(Int, StructuredTerm)]
swapTerms2 "se" = swapTerms 1 2
swapTerms2 "te" = swapTerms 1 3
swapTerms2 "ve" = swapTerms 1 4
swapTerms2 "xe" = swapTerms 1 5

handlePlacePermutations :: StructuredBridi -> Either String StructuredBridi
handlePlacePermutations (ZG.BRIVLA brivla, terms, extraTerms) = Right $ (ZG.BRIVLA brivla, terms, extraTerms)
handlePlacePermutations (ZG.GOhA brivla, terms, extraTerms) = Right $ (ZG.GOhA brivla, terms, extraTerms)
handlePlacePermutations (ZG.Prefix (ZG.SE x) y, terms, extraTerms) = do
    (selbri, terms2, extraTerms) <- handlePlacePermutations (y, terms, extraTerms)
    return $ (selbri, swapTerms2 x terms2, extraTerms)
handlePlacePermutations x = Left $ "unrecognized pattern in function handlePlacePermutations: " ++ show x

---------- Append extra tag to structured bridi
appendExtraTagToStructuredBridi :: ZG.Text -> StructuredBridi -> StructuredBridi
appendExtraTagToStructuredBridi tag (x, y, z) = (x, y, tag : z)

---------- Construct structured bridi from terms
constructStructuredBridiFromTerms :: StructuredSelbri -> [StructuredTerm] -> StructuredBridi
constructStructuredBridiFromTerms selbri terms = (selbri, (zip [1..] mainTerms), extraTerms) where
    isExtraTerm :: ZG.Text -> Bool
    isExtraTerm (ZG.TagKU (ZG.BAI x) _) = True
    isExtraTerm (ZG.TagKU (ZG.FIhO x y z) _) = True
    isExtraTerm (ZG.TagKU (ZG.PrefixTag x y) a) = isExtraTerm (ZG.TagKU y a)
    isExtraTerm (ZG.Tag (ZG.BAI x) _) = True
    isExtraTerm (ZG.Tag (ZG.FIhO x y z) _) = True
    isExtraTerm (ZG.Tag (ZG.PrefixTag x y) a) = isExtraTerm (ZG.Tag y a)
    isExtraTerm _ = False
    (extraTerms, mainTerms) = partition isExtraTerm terms

---------- Retrieve structured bridi
retrieveStructuredBridi :: ZG.Text -> Either String StructuredBridi
------- without x1
-- pu prami / pu se prami / pu ca ba prami / pu ca ba se prami (also pu go'i / pu se go'i / ...)
retrieveStructuredBridi (ZG.Tag x y) = appendExtraTagToStructuredBridi (ZG.TagKU x (ZG.Term "ku")) <$> retrieveStructuredBridi y
-- pu prami do / pu se prami do / pu ca ba prami do / pu ca ba se prami do (also pu go'i do / pu se go'i do / ...)
retrieveStructuredBridi (ZG.BridiTail (ZG.Tag x y) z) = appendExtraTagToStructuredBridi (ZG.TagKU x (ZG.Term "ku")) <$> retrieveStructuredBridi (ZG.BridiTail y z)
-- prami
retrieveStructuredBridi (ZG.BRIVLA brivla) = Right $ (ZG.BRIVLA brivla, [], [])
-- go'i
retrieveStructuredBridi (ZG.GOhA brivla) = Right $ (ZG.GOhA brivla, [], [])
-- se prami / se go'i
retrieveStructuredBridi (ZG.Prefix x y) = Right $ (ZG.Prefix x y, [], [])
-- prami do / se prami do (also go'i do / se go'i do)
retrieveStructuredBridi (ZG.BridiTail selbri (ZG.Terms terms _)) = Right $ (selbri, zip [2..] terms, [])
------- with x1
-- mi prami / mi pu ku ca ku prami
retrieveStructuredBridi (ZG.Bridi (ZG.Terms terms _) (ZG.BRIVLA brivla)) = Right $ constructStructuredBridiFromTerms (ZG.BRIVLA brivla) terms
retrieveStructuredBridi (ZG.Bridi (ZG.Terms terms terms_t) (ZG.Tag x y)) = appendExtraTagToStructuredBridi (ZG.TagKU x (ZG.Term "ku")) <$> retrieveStructuredBridi (ZG.Bridi (ZG.Terms terms terms_t) y)
-- mi go'i / mi pu ku ca ku go'i
retrieveStructuredBridi (ZG.Bridi (ZG.Terms terms _) (ZG.GOhA brivla)) = Right $ constructStructuredBridiFromTerms (ZG.GOhA brivla) terms
-- mi se prami / mi pu ku ca ku se prami
retrieveStructuredBridi (ZG.Bridi (ZG.Terms terms _) (ZG.Prefix x y)) = Right $ constructStructuredBridiFromTerms (ZG.Prefix x y) terms
-- mi pu ku ca ku prami do / mi pu ku ca ku se prami do
retrieveStructuredBridi (ZG.Bridi (ZG.Terms terms1 terms1_t) (ZG.BridiTail (ZG.Tag x y) z)) = appendExtraTagToStructuredBridi (ZG.TagKU x (ZG.Term "ku")) <$> retrieveStructuredBridi (ZG.Bridi (ZG.Terms terms1 terms1_t) (ZG.BridiTail y z))
-- mi prami do / mi se prami do 
retrieveStructuredBridi (ZG.Bridi (ZG.Terms terms1 _) (ZG.BridiTail selbri (ZG.Terms terms2 _))) = Right $ constructStructuredBridiFromTerms selbri (terms1 ++ terms2)
------- invalid
retrieveStructuredBridi x = Left $ "unrecognized pattern in function retrieveStructuredBridi: " ++ show x

---------- Convert structured bridi to simple bridi
-- The structured bridi must already have correct place structure (no place tags, no place reordering)
convertStructuredBridi :: Bool -> StructuredBridi -> Either String SimpleBridi
convertStructuredBridi xu (selbri, terms, extraTerms) = do
    selbri2 <- convertStructuredSelbri selbri
    terms2 <- convertStructuredTerms terms
    extraTerms2 <- convertExtraTerms extraTerms
    return $ SimpleBridi xu selbri2 terms2 extraTerms2

convertStructuredSelbri :: StructuredSelbri -> Either String T.Text
convertStructuredSelbri (ZG.BRIVLA brivla) = Right $ T.pack brivla
convertStructuredSelbri (ZG.GOhA brivla) = Right $ T.pack brivla
convertStructuredSelbri (ZG.Prefix (ZG.SE x) y) = concatET [Right $ T.pack x, Right $ T.pack " ", convertStructuredSelbri y]
convertStructuredSelbri x = Left $ "Unrecognized pattern for structured selbri: " ++ show x

convertStructuredTerms :: [(Int, StructuredTerm)] -> Either String [T.Text]
convertStructuredTerms terms = do
    let terms2 = map (fmap convertStructuredTerm) terms :: [(Int, Either String T.Text)]
    let terms3 = map (\(i, v) -> (i,) <$> v) terms2 :: [Either String (Int, T.Text)]
    terms4 <- foldr (liftA2 (:)) (Right []) terms3 :: Either String [(Int, T.Text)]
    let terms5 = filter ((/= "zo'e") . snd) terms4 :: [(Int, T.Text)]
    let lastTermNumber = if null terms5 then 0 else maximum (map fst terms5)
    let retrieveTerm i = headOrDefault (T.pack "") $ map snd $ filter ((== i) . fst) terms5
    return $ map retrieveTerm [1..lastTermNumber]

convertLinkargs :: ZG.Linkargs -> Either String T.Text
convertLinkargs (ZG.BE (ZG.Init x) y _) = concatET [Right $ T.pack x, Right $ T.pack " ", convertStructuredTerm y, Right $ T.pack " be'o"]
convertLinkargs (ZG.BEI (ZG.Init x) y z _) = concatET [Right $ T.pack x, Right $ T.pack " ", convertStructuredTerm y, Right $ T.pack " ", unwordsET beiArguments, Right $ T.pack " be'o"] where
    beiArguments :: [Either String T.Text]
    beiArguments = map convertArgument z
    convertArgument :: (ZG.Separator, ZG.Text) -> Either String T.Text
    convertArgument (ZG.Sep x, y) = concatET [Right $ T.pack x, Right $ T.pack " ", convertStructuredTerm y]
-- TODO: handle InitF

convertInitiator :: ZG.Initiator -> Either String T.Text
convertInitiator (ZG.Init x) = Right $ T.pack x
-- TODO: InitF, BInit, BInitF

convertRelative :: ZG.Relative -> Either String T.Text
convertRelative (ZG.NOI x y _) = concatET [convertInitiator x, Right $ T.pack " ", convertBridi y, Right $ " ku'o"]
convertRelative x = Left $ "Unrecognized pattern for convertRelative: " ++ show x

convertStructuredTerm :: StructuredTerm -> Either String T.Text
convertStructuredTerm (ZG.KOhA x) = Right $ T.pack x
convertStructuredTerm (ZG.Link x y) = concatET [convertStructuredTerm x, Right $ T.pack " ", convertLinkargs y]
convertStructuredTerm (ZG.BRIVLA x) = Right $ T.pack x
convertStructuredTerm (ZG.Tag (ZG.BAI x) y) = concatET [Right $ T.pack x, Right $ T.pack " ",  convertStructuredTerm y]
convertStructuredTerm (ZG.Rel x y) = concatET [convertStructuredTerm x, Right $ T.pack " ", convertRelative y]
convertStructuredTerm (ZG.GOhA x) = Right $ T.pack x
convertStructuredTerm (ZG.Prefix (ZG.SE x) y) = insertPrefix <$> convertStructuredTerm y where
    insertPrefix = ((T.pack $ x ++ " ") `T.append`)
convertStructuredTerm (ZG.NU (ZG.Init x) y w) = convertStructuredTerm (ZG.NU (ZG.InitF x ZG.NF) y w)
convertStructuredTerm (ZG.NU (ZG.InitF x y) w z) = insertPrefix . insertSuffix <$> canonicalizeParsedBridi (y, w, z) where
    insertPrefix = ((T.pack $ x ++ " ") `T.append`)
    insertSuffix = (`T.append` " kei")
convertStructuredTerm (ZG.LE (ZG.Init x) ZG.NR ZG.NQ (ZG.Rel y z) t) = convertStructuredTerm $ ZG.Rel (ZG.LE (ZG.Init x) ZG.NR ZG.NQ y t) z
convertStructuredTerm (ZG.LE (ZG.Init x) ZG.NR ZG.NQ y _) = insertPrefix . insertSuffix <$> convertStructuredTerm y where
    insertPrefix = ((T.pack $ x ++ " ") `T.append`)
    insertSuffix = (`T.append` " ku")
convertStructuredTerm x = Left $ "Unrecognized pattern for structured term: " ++ show x

convertExtraTerms :: [ExtraTerm] -> Either String [T.Text]
convertExtraTerms = mapM convertExtraTerm . expandExtraTerms

expandExtraTerms :: [ExtraTerm] -> [ExtraTerm]
expandExtraTerms = concatMap expandTerm where
    expandTerm :: ExtraTerm -> [ExtraTerm]
    expandTerm (ZG.TagKU (ZG.TTags tags) term) = map (`ZG.TagKU` term) tags
    expandTerm x = [x]

convertExtraTerm :: ExtraTerm -> Either String T.Text
convertExtraTerm (ZG.TagKU (ZG.FIhO (ZG.Init _) y _) _) = concatET [Right $ T.pack "fi'o ", convertStructuredSelbri y, Right $ T.pack " fe'u ku"]
convertExtraTerm (ZG.Tag (ZG.FIhO (ZG.Init _) y _) text) = concatET [Right $ T.pack "fi'o ", convertStructuredSelbri y, Right $ T.pack " fe'u ", convertStructuredTerm text]
convertExtraTerm (ZG.TagKU (ZG.PrefixTag (ZG.SE x) (ZG.BAI y)) z) = case expandBai y of
    Just y' -> convertExtraTerm $ ZG.TagKU (ZG.FIhO (ZG.Init "fi'o") (ZG.Prefix (ZG.SE x) (ZG.BRIVLA y')) (ZG.Term "fe'u")) z
    Nothing -> concatET [Right $ T.pack x, Right $ T.pack " ", convertExtraTerm (ZG.TagKU (ZG.BAI y) z)]
convertExtraTerm (ZG.Tag (ZG.PrefixTag (ZG.SE x) (ZG.BAI y)) z) = case expandBai y of
    Just y' -> convertExtraTerm $ ZG.Tag (ZG.FIhO (ZG.Init "fi'o") (ZG.Prefix (ZG.SE x) (ZG.BRIVLA y')) (ZG.Term "fe'u")) z
    Nothing -> concatET [Right $ T.pack x, Right $ T.pack " ", convertExtraTerm (ZG.Tag (ZG.BAI y) z)]
convertExtraTerm (ZG.TagKU (ZG.BAI x) y) = case expandBai x of
    Just x' -> convertExtraTerm $ ZG.TagKU (ZG.FIhO (ZG.Init "fi'o") (ZG.BRIVLA x') (ZG.Term "fe'u")) y
    Nothing -> concatET [Right $ T.pack x, Right $ T.pack " ku"]
convertExtraTerm (ZG.Tag (ZG.BAI x) text) = case expandBai x of
    Just x' -> convertExtraTerm $ ZG.Tag (ZG.FIhO (ZG.Init "fi'o") (ZG.BRIVLA x') (ZG.Term "fe'u")) text
    Nothing -> concatET [Right $ T.pack x, Right $ T.pack " ", convertStructuredTerm text]
convertExtraTerm x = Left $ "Unrecognized pattern for convertExtraTerm: " ++ show x

-- TODO: add all BAI
compressedBai :: M.Map String String
compressedBai = M.fromList
    [ ("pi'o", "pilno")
    , ("zu'e", "zukte")
    , ("mu'i", "mukti")
    , ("gau", "gasnu")
    ]

expandBai :: String -> Maybe String
expandBai = (`M.lookup` compressedBai)

---------- Canonicalization
--TODO: canonicalize "do xu ciska" -> "xu do ciska"
basicSentenceCanonicalizer :: T.Text -> Either String T.Text
basicSentenceCanonicalizer sentence = parse sentence >>= canonicalizeParsedText

canonicalizeParsedText :: (ZG.Free, ZG.Text, ZG.Terminator) -> Either String T.Text
canonicalizeParsedText parsedText = (canonicalizeParsedBridi parsedText) `mplus` (canonicalizeParsedTerm parsedText)

canonicalizeParsedBridi :: (ZG.Free, ZG.Text, ZG.Terminator) -> Either String T.Text
canonicalizeParsedBridi parsedBridi = displayCanonicalBridi <$> (retrieveSimpleBridi parsedBridi)

retrieveSimpleBridi :: (ZG.Free, ZG.Text, ZG.Terminator) -> Either String SimpleBridi
retrieveSimpleBridi (free, text, terminator) = retrieveStructuredBridi text >>= handlePlaceTags >>= handlePlacePermutations >>= convertStructuredBridi xu where
    xu = hasXu free

canonicalizeParsedTerm :: (ZG.Free, ZG.Text, ZG.Terminator) -> Either String T.Text
canonicalizeParsedTerm (free, ZG.Terms [term] _, terminator) = convertStructuredTerm term

hasXu :: ZG.Free -> Bool
hasXu (ZG.UI x) = x == "xu"
hasXu (ZG.UIF x y) = (x == "xu") || hasXu y
hasXu (ZG.BUIF x y z) = hasXu z
hasXu (ZG.DOIF x y) = hasXu y
hasXu (ZG.BDOIF x y z) = hasXu z
hasXu (ZG.COIF x y) = hasXu y
hasXu (ZG.BCOIF x y z) = hasXu z
hasXu (ZG.COIs xs y) = any hasXu xs
hasXu (ZG.Vocative xs y z) = any hasXu xs
hasXu _ = False


convertBridi :: ZG.Text -> Either String T.Text
convertBridi text = canonicalizeParsedBridi (ZG.NF, text, ZG.NT)
