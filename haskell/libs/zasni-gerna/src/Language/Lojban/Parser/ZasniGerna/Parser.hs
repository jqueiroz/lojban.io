{-# LANGUAGE TypeFamilies, QuasiQuotes, PatternGuards, TupleSections #-}

module Language.Lojban.Parser.ZasniGerna.Parser where

import Prelude hiding (Word)
import Text.Papillon
import Data.Maybe

parse :: String -> Either String (Free, Text, Terminator)
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

maybeCons :: Maybe a -> [a] -> [a]
maybeCons mx xs = maybe xs (: xs) mx

data Text
	= NIhO Text [(Separator, Text)]
	| I Text [(Separator, Text)]
	| ICon Text [(Separator, Connective, Text)]
	| IBO Text [(Separator, Connective, BO, Text)]
	| BridiTail Text Text
	| Con Text [(Connective, Text)]
	| ConBO Text [(Connective, BO, Text)]
	| Gihek Text [(Connective, Text, Text)]
	| GihekBO Text [(Connective, BO, Text, Text)]
	| TanruCO Text [(CO, Text)]
	| Tanru [Text]
	| TanruBO Text [(BO, Text)]
	| Q Mex Text
	| QS Mex Text Terminator Relative
	| Gek Connective Text Connective Text
	| LE Initiator Relative Mex Text Terminator
	| Terms [Text] Terminator
	| Bridi Text Text
	| ZOhU Text Text
	| Tag Tag Text
	| Tags [Tag] Text
	| TagKU Tag Terminator
	| Rel Text Relative
	| RelSelbri Relative Text
	| CEI Text Initiator Text
	| BRIVLA String
	| BRIVLAF String Free
	| BBRIVLA [String] String
	| BBRIVLAF [String] String Free
	| CMEVLA String
	| CMEVLAF String Free
	| BCMEVLA [String] String
	| BCMEVLAF [String] String Free
	| KOhA String
	| KOhAF String Free
	| BKOhA [String] String
	| BKOhAF [String] String Free
	| GOhA String
	| GOhAF String Free
	| BGOhA [String] String
	| BGOhAF [String] String Free
	| MOI Mex Suffix
	| MEMOI Text Suffix
	| MESumti Initiator Text Terminator
	| MEMex Initiator Mex Terminator
	| MEJoik Initiator Connective Terminator
	| NU Initiator Text Terminator
	| KE Initiator Text Terminator
	| Prefix Prefix Text
	| Link Text Linkargs
	| LI Initiator Mex Terminator
	| LU Initiator Text Terminator
	| LAhE Initiator Relative Text Terminator
	| NAhEBO Prefix BO Relative Text Terminator
	| Clause Word
	| ClauseF Word Free
	| BClause [String] Word
	| BClauseF [String] Word Free
	| Lergum [Lerfu] Terminator
	| TUhE Initiator Text Terminator
	deriving Show

data Relative
	= GOI Initiator Text Terminator
	| NOI Initiator Text Terminator
	| ZIhE Relative [(Separator, Relative)]
	| RelSumti Text
	| VR VUhO Relative
	| NR
	deriving Show

data Linkargs
	= BE Initiator Text Terminator
	| BEI Initiator Text [(Separator, Text)] Terminator
	deriving Show

data Tag
	= BAI String
	| BAIF String Free
	| BBAI [String] String
	| BBAIF [String] String Free
	| FA String
	| FAF String Free
	| BFA [String] String
	| BFAF [String] String Free
	| NA String
	| NAF String Free
	| BNA [String] String
	| BNAF [String] String Free
	| TTags [Tag]
	| TTagCons Tag [(Connective, Tag)]
	| MexROI Mex Suffix
	| FIhO Initiator Text Terminator
	| PrefixTag Prefix Tag
	deriving Show

data Mex
	= MRel Mex Relative
	| MJoik Mex [(Connective, Mex)]
	| Ms [Mex] Terminator
	| P1 String
	| P1F String Free
	| BP1 [String] String
	| BP1F [String] String Free
	| Stub String
	| NQ
	| Lerfu Word
	| LerfuF Word Free
	| BLerfu [String] Word
	| BLerfuF [String] Word Free
	| NIhE Initiator Text Terminator
	| MOhE Initiator Text Terminator
	| VEI Initiator Mex Terminator
	| MLAhE Initiator Mex Terminator
	| MNAhEBO Prefix BO Mex Terminator
	deriving Show

type Lerfu = Mex

data Connective
	= GA String
	| GAF String Free
	| BGA [String] String
	| BGAF [String] String Free
	| GI String
	| GIF String Free
	| BGI [String] String
	| BGIF [String] String Free
	| JOI String
	| JOIF String Free
	| BJOI [String] String
	| BJOIF [String] String Free
	| GIhA String
	| GIhAF String Free
	| BGIhA [String] String
	| BGIhAF [String] String Free
	| JOIGI Connective Connective
	| NACon Tag Connective
	| SECon Prefix Connective
	| NASECon Tag Prefix Connective
	| TAGGI Tag Connective
	| NC
	| GIJA Connective Connective
	deriving Show

data Initiator
	= Init String
	| InitF String Free
	| BInit [String] String
	| BInitF [String] String Free
	deriving Show

data Terminator
	= Term String
	| TermF String Free
	| BTerm [String] String
	| BTermF [String] String Free
	| NT
	deriving Show

data Separator
	= Sep String
	| SepF String Free
	| BSep [String] String
	| BSepF [String] String Free
	deriving Show

data Prefix
	= NAhE String
	| NAhEF String Free
	| BNAhE [String] String
	| BNAhEF[String] String Free
	| SE String
	| SEF String Free
	| BSE [String] String
	| BSEF [String] String Free
	| JAI String
	| JAIF String Free
	| BJAI [String] String
	| BJAIF [String] String Free
	| JAITag Prefix Tag
	| XI String
	| XIF String Free
	| BXI [String] String
	| BXIF [String] String Free
	deriving Show

data BO = BO String
	| BOF String Free
	| BBO [String] String
	| BBOF [String] String Free
	| TagBO Tag BO
	deriving Show

data CO	= CO String
	| COF String Free
	| BCO [String] String
	| BCOF [String] String Free
	deriving Show

data Suffix
	= Suffix String
	| SuffixF String Free
	| BSuffix [String] String
	| BSuffixF [String] String Free
	deriving Show

data VUhO
	= VUhO String
	| VUhOF String Free
	| BVUhO [String] String
	| BVUhOF [String] String Free
	deriving Show

data Free
	= UI String
	| UIF String Free
	| BUI [String] String
	| BUIF [String] String Free
	| DOI String
	| DOIF String Free
	| BDOI [String] String
	| BDOIF [String] String Free
	| COI String
	| COIF String Free
	| BCOI [String] String
	| BCOIF [String] String Free
	| COIs [Free] Terminator
	| Vocative [Free] Text Terminator
	| FXI Prefix Mex
	| MAI Mex Suffix
	| SEI Initiator Text Terminator
	| TO Initiator Text Terminator
	| NF
	deriving Show

data Word
	= BUStr Word [([BU], [ZEI])] [BU]
	| ZEIStr Word [([ZEI], [BU])] [ZEI]
	| ZO String
	| LOhU [String]
	| ZOI String String String
	| Word String
	deriving Show

data ZEI = ZEI String deriving Show
data BU = BU deriving Show

baheFree ::
	(a -> c) -> (a -> f -> c) -> ([b] -> a -> c) -> ([b] -> a -> f -> c) ->
	[b] -> a -> Maybe f -> c
baheFree a _ _ _ [] x Nothing = a x
baheFree _ af _ _ [] x (Just f) = af x f
baheFree _ _ ba _ b x Nothing = ba b x
baheFree _ _ _ baf b x (Just f) = baf b x f

data Either3 a b c = E1 a | E2 b | E3 c deriving Show

either3 :: Either3 a b c -> (a -> d) -> (b -> d) -> (c -> d) -> d
either3 (E1 x) f _ _ = f x
either3 (E2 y) _ f _ = f y
either3 (E3 z) _ _ f = f z

mkSelbri :: [Text] -> [(CO, Text)] -> Maybe Relative ->
	Maybe (Initiator, Text) -> Text
mkSelbri [s] [] rel cei = let
	b = maybe s (Rel s) rel in
	maybe b (uncurry $ CEI b) cei
mkSelbri [s] cs rel cei = let
	b = TanruCO s cs
	b' = maybe b (Rel b) rel in
	maybe b' (uncurry $ CEI b') cei
mkSelbri ss [] rel cei = let
	b = Tanru ss
	b' = maybe b (Rel b) rel in
	maybe b' (uncurry $ CEI b') cei
mkSelbri ss cs rel cei = let
	b = Tanru ss
	b' = TanruCO b cs
	b'' = maybe b' (Rel b) rel in
	maybe b'' (uncurry $ CEI b'') cei

mkBridiTail :: [Tag] -> Connective -> Text -> Connective -> Text -> [Text] ->
	Maybe Terminator -> Text
mkBridiTail [] ge b1 gi b2 [] Nothing = Gek ge b1 gi b2
mkBridiTail [] ge b1 gi b2 terms vau =
	BridiTail (Gek ge b1 gi b2) $ Terms terms $ fromMaybe NT vau
mkBridiTail tag ge b1 gi b2 [] Nothing = Tags tag $ Gek ge b1 gi b2
mkBridiTail tag ge b1 gi b2 terms vau = Tags tag $
	BridiTail (Gek ge b1 gi b2) $ Terms terms $ fromMaybe NT vau

mkGihekBO :: Connective -> Maybe Tag -> BO -> Text -> [Text] -> Maybe Terminator ->
	(Connective, BO, Text, Text)
mkGihekBO g tag bo b terms v =
	(g, maybe bo (flip TagBO bo) tag, b, Terms terms $ fromMaybe NT v)

mkMex1 :: [Mex] -> Maybe Terminator -> Mex
mkMex1 [m] Nothing = m
mkMex1 ms v = Ms ms $ fromMaybe NT v

mkConnective :: Maybe Tag -> Maybe Prefix -> Connective -> Connective
mkConnective (Just n) (Just s) c = NASECon n s c
mkConnective (Just n) _ c = NACon n c
mkConnective _ (Just s) c = SECon s c
mkConnective _ _ c = c

mkTags :: [Tag] -> Tag
mkTags [t] = t
mkTags ts = TTags ts

mkTagCons :: [Tag] -> [(Connective, Tag)] -> Tag
mkTagCons [t] [] = t
mkTagCons [t] cts = TTagCons t cts
mkTagCons ts [] = TTags ts
mkTagCons ts cts = TTagCons (TTags ts) cts

mkVocative :: [Free] -> Maybe Text -> Maybe Terminator -> Free
mkVocative [c] Nothing Nothing = c
mkVocative cs (Just s) dohu = Vocative cs s $ fromMaybe NT dohu
mkVocative cs _ dohu = COIs cs $ fromMaybe NT dohu

[papillon|

prefix: "gerna_"

textAll :: (Free, Text, Terminator) = r:text _:space* !_	{ r }

-- ****** A. GRAMMAR ******

-- 1. Text Paragraphs Statement

text :: (Free, Text, Terminator)
	= _:words_SU* _:SI_tail* ps:post p:paragraphs f:(f':FAhO_ _:(_)* { f' })?
	{ (fromMaybe NF ps, p, fromMaybe NT f) }

paragraphs :: Text = p:paragraph ps:(n:NIhO_ p':paragraph { (n, p') })*
	{ if null ps then p else NIhO p ps }

paragraph :: Text = s:statement is:(i:I_ s':statement { (i, s') })*
	{ if null is then s else I s is }

statement :: Text = s:statement_1 is:
	( i:I_ j:joik s':statement_1		{ (i, j, s') }
 )*
	{ if null is then s else ICon s is }

statement_1 :: Text = s:sentence is:
	( i:I_ j:joik? t:tag? b:BO_ s':sentence
		{ (i, fromMaybe NC j, maybe b (flip TagBO b) t, s') }
 )*	{ if null is then s else IBO s is }

-- 2. Sentence Bridi

sentence :: Text
	= tc:(t:term+ c:CU_? { Terms t $ fromMaybe NT c })?  b:bridi_tail
		{ maybe b (flip Bridi b) tc }
	/ te:TUhE_ p:paragraphs tu:TUhU_		{ TUhE te p tu }
	/ ge:gek s1:sentence gi:GI_ s2:sentence		{ Gek ge s1 gi s2 }
	/ t:term+ z:ZOhU_ s:sentence			{ ZOhU (Terms t z) s }
	/ t:term* v:VAU_?				{ Terms t $ fromMaybe NT v }

bridi_tail :: Text = b:bridi_tail_1 gb:
	( g:gihek b':bridi_tail_1 t:term* v:VAU_?
		{ (g, b', Terms t $ fromMaybe NT v) }
 )*	{ if null gb then b else Gihek b gb }

bridi_tail_1 :: Text = b:bridi_tail_2 gb:
	( g:gihek tg:tag? bo:BO_ b':bridi_tail_2 t:term* v:VAU_?
		{ mkGihekBO g tg bo b' t v } )*
	{ if null gb then b else GihekBO b gb }

bridi_tail_2 :: Text
	= s:selbri t:term* v:VAU_?
		{ if null t && isNothing v then s else
			BridiTail s $ Terms t $ fromMaybe NT v }
	/ tn:(t:tag { t } / n:NA_ { n })* ge:gek b1:bridi_tail gi:GI_
		b2:bridi_tail t':term* v:VAU_?
		{ mkBridiTail tn ge b1 gi b2 t' v }

-- 3. Term Sumti

term :: Text
	= f:FA_? s:sumti				{ maybe s (flip Tag s) f }
	/ t:tag? s:sumti				{ maybe s (flip Tag s) t }
	/ f:FA_ k:KU_?					{ TagKU f $ fromMaybe NT k }
	/ t:tag !_:selbri k:KU_?					{ TagKU t $ fromMaybe NT k }
	/ n:NA_ k:KU_?					{ TagKU n $ fromMaybe NT k }
	/ ge:gek t1:term+ v1:VAU_? gi:GI_ t2:term+ v2:VAU_?
		{ Gek ge (Terms t1 $ fromMaybe NT v1)
			gi (Terms t2 $ fromMaybe NT v2) }

sumti :: Text = s1:sumti_1 js:(j:joik s2:sumti_1 { (j, s2) })*
	vr:(v:VUhO_ r:rels { VR v r })?
	{ if null js then s1 else let c = Con s1 js in
		maybe c (Rel c) vr }

sumti_1 :: Text = s1:sumti_2 jss:
	( j:joik t:tag? bw:BO_ s2:sumti_2
		{ (j, maybe bw (\t' -> TagBO t' bw) t, s2) }
 )*	{ if null jss then s1 else ConBO s1 jss }

sumti_2 :: Text
	= q:quantifier? s:bare_sumti			{ maybe s (flip Q s) q }
	/ q:quantifier s:selbri k:KU_? r:rels?		{ QS q s (fromMaybe NT k)
								(fromMaybe NR r) }
	/ ge:gek s1:sumti gi:GI_ s2:sumti		{ Gek ge s1 gi s2 }

bare_sumti :: Text = s:
	( d:description				{ d }
	/ li:LI_ m:mex lo:LOhO_?		{ LI li m $ fromMaybe NT lo }
	/ z:ZO_word_				{ z }
	/ lu:LU_ p:paragraphs li:LIhU_?		{ LU lu p $ fromMaybe NT li }
	/ l:LOhU_words_LEhU_			{ l }
	/ z:ZOI_anything_			{ z }
	/ k:KOhA_				{ k }
	/ !_:tag !_:selbri ls:(l:lerfu { l })+ b:BOI_?
						{ Lergum ls $ fromMaybe NT b }
	/ !_:tag !_:selbri
		ln:(l:LAhE_ { Left l } / n:NAhE_ b:BO_ { Right (n, b) })
		r:rels? s':sumti lu:LUhU_?	{ either
							(\l -> LAhE l
								(fromMaybe NR r)
								s'
								(fromMaybe NT lu))
							(\(n, b) -> NAhEBO n b
								(fromMaybe NR r)
								s'
								(fromMaybe NT lu))
							ln }
 ) r':rels?						{ maybe s (Rel s) r' }

description :: Text = l:LE_ rs:(r:rels { r } / s':bare_sumti { RelSumti s' })?
	q:quantifier? ss:(s:selbri { s } / s':bare_sumti { s' }) k:KU_?
	{ LE l (fromMaybe NR rs) (fromMaybe NQ q) ss (fromMaybe NT k) }

-- 4. Mex

quantifier :: Mex
	= !_:bare_sumti !_:selbri m:mex r:rels?		{ maybe m (MRel m) r }

mex :: Mex = m:mex_1 jm:(j:joik m':mex_1 { (j, m') })*
	{ if null jm then m else MJoik m jm }

mex_1 :: Mex
	= p:PA_+ b:BOI_?				{ mkMex1 p b }
	/ l:lerfu+ b:BOI_?				{ mkMex1 l b }
	/ n:NIhE_ s:selbri t:TEhU_?		{ NIhE n s $ fromMaybe NT t }
	/ m:MOhE_ s:sumti t:TEhU_?		{ MOhE m s $ fromMaybe NT t }
	/ vei:VEI_ m:mex veho:VEhO_?		{ VEI vei m $ fromMaybe NT veho }
	/ lnb:(l:LAhE_ { Left l } / n:NAhE_ b:BO_ { Right (n, b) })
		m:mex luhu:LUhU_?
	{ either
		(\l -> MLAhE l m $ fromMaybe NT luhu)
		(\(n, b) -> MNAhEBO n b m $ fromMaybe NT luhu)
		lnb }

lerfu :: Lerfu
	= b:BY_						{ b }
	/ b:word_BU_					{ b }

-- 5. Relative

rels :: Relative = r:rel rs:(z:ZIhE_ r':rel { (z, r') })*
	{ if null rs then r else ZIhE r rs }

rel :: Relative
	= goi:GOI_ t:term gehu:GEhU_?		{ GOI goi t $ fromMaybe NT gehu }
	/ n:NOI_ s:sentence k:KUhO_?		{ NOI n s $ fromMaybe NT k }

-- 6. Selbri Tanru unit

selbri :: Text
	= sl:selbri_1+ cs:(c:CO_ s:selbri_1 { (c, s) })* r:rels?
		cei:(c:CEI_ s':selbri { (c, s') })?	{ mkSelbri sl cs r cei }
	/ t:tag s:selbri				{ Tag t s }
	/ n:NA_ s:selbri				{ Tag n s }

selbri_1 :: Text = s:selbri_2 js:(j:joik s2:selbri_2 { (j, s2) })*
	{ if null js then s else Con s js }

selbri_2 :: Text = s:selbri_3 jbs:
	( j:joik t:tag? b:BO_ s':selbri_3
		{ (j, maybe b (\t' -> TagBO t' b) t, s') }
 )*	{ if null jbs then s else ConBO s jbs }

selbri_3 :: Text = t:tanru_unit bts:(b:BO_ t':tanru_unit { (b, t') })*
	{ if null bts then t else TanruBO t bts }

tanru_unit :: Text
	= t:tanru_unit_1	l:linkargs?		{ maybe t (Link t) l }

tanru_unit_1 :: Text
	= b:BRIVLA_					{ b }
	/ z:word_ZEI_word_				{ z }
	/ c:CMEVLA_					{ c }
	/ g:GOhA_					{ g }
	/ m:mex mo:MOI_					{ MOI m mo }
	/ me:ME_ smj:(s:sumti { E1 s } / m:mex { E2 m } / j:joik { E3 j })
		mehu:MEhU_? moi:MOI_?
		{ let b = either3 smj
			(\s -> MESumti me s $ fromMaybe NT mehu)
			(\m -> MEMex me m $ fromMaybe NT mehu)
			(\j -> MEJoik me j $ fromMaybe NT mehu) in
			maybe b (MEMOI b) moi }
	/ n:NU_ s:sentence k:KEI_?			{ NU n s $ fromMaybe NT k }
	/ ke:KE_ s:selbri k:KEhE_?			{ KE ke s $ fromMaybe NT k }
	/ p:
		( s:SE_				{ s }
		/ j:JAI_ t:tag?			{ maybe j (JAITag j) t }
		/ n:NAhE_			{ n }
	 ) t':tanru_unit_1				{ Prefix p t' }

-- 7. Link args

linkargs :: Linkargs
	= be:BE_ t:term bts:(b:BEI_ t':term { (b, t') })* beho:BEhO_?
	{ if null bts then BE be t $ fromMaybe NT beho
		else BEI be t bts $ fromMaybe NT beho }

-- 8. Connective

joik :: Connective
	= n:NA_? s:SE_? j:JOI_				{ mkConnective n s j }

gihek :: Connective
	= n:NA_? s:SE_? g:GIhA_				{ mkConnective n s g }
	/ n:NA_? s:SE_? gi:GI_ j:JOI_
		{ mkConnective n s (GIJA gi j) }

gek :: Connective
	= s:SE_? g:GA_		{ maybe g (flip SECon g) s }
	/ s:SE_? j:JOI_ g:GI_	{ maybe (JOIGI j g) (flip SECon $ JOIGI j g) s }
	/ t:tag_unit+ jt:(j:joik t':tag_unit+ { (j, mkTags t') })* g:GI_
				{ TAGGI (mkTagCons t jt) g }

-- 9. Tense Modal

tag :: Tag
	= t:tag_unit+ jt:(j:joik t':tag_unit+ { (j, mkTags t') })* !_:GI_
	{ mkTagCons t jt }

tag_unit :: Tag
	= b:BAI_					{ b }
	/ m:mex r:ROI_					{ MexROI m r }
	/ fi:FIhO_ s:selbri fe:FEhU_?			{ FIhO fi s $
								fromMaybe NT fe }
	/ p:(n:NAhE_ { n } / s:SE_ { s }) t:tag_unit	{ PrefixTag p t }

-- 10. Free modifier

-- stub
free :: Free
	= u:UI_						{ u }
	/ x:XI_ m:mex_1					{ FXI x m }
	/ m:mex_1 mai:MAI_				{ MAI m mai }
	/ sei:SEI_ tc:(t:term+ c:CU_? { Terms t $ fromMaybe NT c })?
		s:selbri sehu:SEhU_?
		{ SEI sei (maybe s (flip Bridi s) tc) $ fromMaybe NT sehu }
	/ to:TO_ p:paragraphs toi:TOI_?		{ TO to p $ fromMaybe NT toi }
	/ v:vocative					{ v }

vocative :: Free
	= cd:(c:COI_+ d:DOI_? { c ++ maybeToList d } / d:DOI_ { [d] })
		rss:(r:rels? s:selbri { Just $ maybe s (flip RelSelbri s) r } /
			s:sumti? { s }) dohu:DOhU_?
	{ mkVocative cd rss dohu }

-- ****** B. LOW LEVEL GRAMMAR ******

--- 11. SELMAhO ---

BRIVLA_ :: Text = pr:pre b:BRIVLA ps:post		{ baheFree BRIVLA BRIVLAF
								BBRIVLA BBRIVLAF
								pr b ps }

CMEVLA_ :: Text = pr:pre c:CMEVLA ps:post		{ baheFree CMEVLA CMEVLAF
								BCMEVLA BCMEVLAF
								pr c ps }

BAI_ :: Tag = pr:pre b:BAI ps:post			{ baheFree BAI BAIF
								BBAI BBAIF
								pr b ps }

BE_ :: Initiator = pr:pre b:BE ps:post			{ baheFree Init InitF
								BInit BInitF
								pr b ps }

BEI_ :: Separator = pr:pre b:BEI ps:post		{ baheFree Sep SepF
								BSep BSepF
								pr b ps }

BEhO_ :: Terminator = pr:pre b:BEhO ps:post		{ baheFree Term TermF
								BTerm BTermF
								pr b ps }

BO_ :: BO = pr:pre b:BO ps:post				{ baheFree BO BOF
								BBO BBOF
								pr b ps }

BOI_ :: Terminator = pr:pre b:BOI ps:post		{ baheFree Term TermF
								BTerm BTermF
								pr b ps }

BY_ :: Lerfu = pr:pre b:BY ps:lerfu_post		{ baheFree Lerfu LerfuF
								BLerfu BLerfuF
								pr (Word b) ps }

CEI_ :: Initiator = pr:pre c:CEI ps:post		{ baheFree Init InitF
								BInit BInitF
								pr c ps }

CO_ :: CO = pr:pre c:CO ps:post				{ baheFree CO COF
								BCO BCOF
								pr c ps }

COI_ :: Free = pr:pre c:COI ps:vocative_post		{ baheFree COI COIF
								BCOI BCOIF
								pr c ps }

CU_ :: Terminator = pr:pre c:CU ps:post			{ baheFree Term TermF
								BTerm BTermF
								pr c ps }

DOI_ :: Free = pr:pre d:DOI ps:post			{ baheFree DOI DOIF
								BDOI BDOIF
								pr d ps }

DOhU_ :: Terminator = pr:pre d:DOhU ps:post		{ baheFree Term TermF
								BTerm BTermF
								pr d ps }

FA_ :: Tag = pr:pre f:FA ps:post			{ baheFree FA FAF
								BFA BFAF
								pr f ps }

FAhO_ :: Terminator = pr:pre f:FAhO ps:post		{ baheFree Term TermF
								BTerm BTermF
								pr f ps }

FIhO_ :: Initiator = pr:pre f:FIhO ps:post		{ baheFree Init InitF
								BInit BInitF
								pr f ps }

FEhU_ :: Terminator = pr:pre f:FEhU ps:post		{ baheFree Term TermF
								BTerm BTermF
								pr f ps }

GA_ :: Connective = pr:pre g:GA ps:post			{ baheFree GA GAF
								BGA BGAF
								pr g ps }

GEhU_ :: Terminator = pr:pre g:GEhU ps:post		{ baheFree Term TermF
								BTerm BTermF
								pr g ps }

GI_ :: Connective = pr:pre g:GI ps:post			{ baheFree GI GIF
								BGI BGIF
								pr g ps }

GIhA_ :: Connective = pr:pre g:GIhA ps:post		{ baheFree GIhA GIhAF
								BGIhA BGIhAF
								pr g ps }

GOI_ :: Initiator = pr:pre g:GOI ps:post		{ baheFree Init InitF
								BInit BInitF
								pr g ps }

GOhA_ :: Text = pr:pre g:GOhA ps:post			{ baheFree GOhA GOhAF
								BGOhA BGOhAF
								pr g ps }

I_ :: Separator = pr:pre i:SelmahoI ps:post		{ baheFree Sep SepF
								BSep BSepF
								pr i ps }

JAI_ :: Prefix = pr:pre j:JAI ps:post			{ baheFree JAI JAIF
								BJAI BJAIF
								pr j ps }

JOI_ :: Connective = pr:pre j:JOI ps:post		{ baheFree JOI JOIF
								BJOI BJOIF
								pr j ps }

KE_ :: Initiator = pr:pre k:KE ps:post			{ baheFree Init InitF
								BInit BInitF
								pr k ps }

KEhE_ :: Terminator = pr:pre k:KEhE ps:post		{ baheFree Term TermF
								BTerm BTermF
								pr k ps }

KEI_ :: Terminator = pr:pre k:KEI ps:post		{ baheFree Term TermF
								BTerm BTermF
								pr k ps }

KOhA_ :: Text = pr:pre k:KOhA ps:post			{ baheFree KOhA KOhAF
								BKOhA BKOhAF
								pr k ps }

KU_ :: Terminator = pr:pre k:KU ps:post			{ baheFree Term TermF
								BTerm BTermF
								pr k ps }

KUhO_ :: Terminator = pr:pre k:KUhO ps:post		{ baheFree Term TermF
								BTerm BTermF
								pr k ps }

LAhE_ :: Initiator = pr:pre l:LAhE ps:post		{ baheFree Init InitF
								BInit BInitF
								pr l ps }

LE_ :: Initiator = pr:pre l:LE ps:post			{ baheFree Init InitF
								BInit BInitF
								pr l ps }

LI_ :: Initiator = pr:pre l:LI ps:post			{ baheFree Init InitF
								BInit BInitF
								pr l ps }

LIhU_ :: Terminator = pr:pre l:LIhU ps:post		{ baheFree Term TermF
								BTerm BTermF
								pr l ps }

LOhO_ :: Terminator = pr:pre l:LOhO ps:post		{ baheFree Term TermF
								BTerm BTermF
								pr l ps }

LU_ :: Initiator = pr:pre l:LU ps:post			{ baheFree Init InitF
								BInit BInitF
								pr l ps }

LUhU_ :: Terminator = pr:pre l:LUhU ps:post		{ baheFree Term TermF
								BTerm BTermF
								pr l ps }

MAI_ :: Suffix = pr:pre m:MAI ps:post			{ baheFree Suffix SuffixF
								BSuffix BSuffixF
								pr m ps }

ME_ :: Initiator = pr:pre m:ME ps:post			{ baheFree Init InitF
								BInit BInitF
								pr m ps }

MEhU_ :: Terminator = pr:pre m:MEhU ps:post		{ baheFree Term TermF
								BTerm BTermF
								pr m ps }

MOI_ :: Suffix = pr:pre m:MOI ps:post			{ baheFree Suffix SuffixF
								BSuffix BSuffixF
								pr m ps }

MOhE_ :: Initiator = pr:pre m:MOhE ps:post		{ baheFree Init InitF
								BInit BInitF
								pr m ps }

NA_ :: Tag = pr:pre n:NA ps:post			{ baheFree NA NAF
								BNA BNAF
								pr n ps }

NAhE_ :: Prefix = pr:pre n:NAhE ps:post			{ baheFree NAhE NAhEF
								BNAhE BNAhEF
								pr n ps }

NIhE_ :: Initiator = pr:pre n:NIhE ps:post		{ baheFree Init InitF
								BInit BInitF
								pr n ps }

NIhO_ :: Separator = pr:pre n:NIhO ps:post		{ baheFree Sep SepF
								BSep BSepF
								pr n ps }

NOI_ :: Initiator = pr:pre n:NOI ps:post		{ baheFree Init InitF
								BInit BInitF
								pr n ps }

NU_ :: Initiator = pr:pre n:NU ps:post			{ baheFree Init InitF
								BInit BInitF
								pr n ps }

PA_ :: Mex = pr:pre p:PA ps:number_post			{ baheFree P1 P1F
								BP1 BP1F
								pr p ps }

ROI_ :: Suffix = pr:pre r:ROI ps:post			{ baheFree Suffix SuffixF
								BSuffix BSuffixF
								pr r ps }

SE_ :: Prefix = pr:pre s:SE ps:post			{ baheFree SE SEF
								BSE BSEF
								pr s ps }

SEI_ :: Initiator = pr:pre s:SEI ps:post		{ baheFree Init InitF
								BInit BInitF
								pr s ps }

SEhU_ :: Terminator = pr:pre s:SEhU ps:post		{ baheFree Term TermF
								BTerm BTermF
								pr s ps }

TEhU_ :: Terminator = pr:pre t:TEhU ps:post		{ baheFree Term TermF
								BTerm BTermF
								pr t ps }

TO_ :: Initiator = pr:pre t:TO ps:post			{ baheFree Init InitF
								BInit BInitF
								pr t ps }

TOI_ :: Terminator = pr:pre t:TOI ps:post		{ baheFree Term TermF
								BTerm BTermF
								pr t ps }

TUhE_ :: Initiator = pr:pre t:TUhE ps:post		{ baheFree Init InitF
								BInit BInitF
								pr t ps }

TUhU_ :: Terminator = pr:pre t:TUhU ps:post		{ baheFree Term TermF
								BTerm BTermF
								pr t ps }

UI_ :: Free = pr:pre u:UI ps:post			{ baheFree UI UIF
								BUI BUIF
								pr u ps }

VAU_ :: Terminator = pr:pre v:VAU ps:post		{ baheFree Term TermF
								BTerm BTermF
								pr v ps }

VEI_ :: Initiator = pr:pre v:VEI ps:post		{ baheFree Init InitF
								BInit BInitF
								pr v ps }

VEhO_ :: Terminator = pr:pre v:VEhO ps:post		{ baheFree Term TermF
								BTerm BTermF
								pr v ps }

VUhO_ :: VUhO = pr:pre v:VUhO ps:post			{ baheFree VUhO VUhOF
								BVUhO BVUhOF
								pr v ps }

XI_ :: Prefix = pr:pre x:XI ps:post			{ baheFree XI XIF
								BXI BXIF
								pr x ps }

ZIhE_ :: Separator = pr:pre z:ZIhE ps:post		{ baheFree Sep SepF
								BSep BSepF
								pr z ps }

ZOhU_ :: Terminator = pr:pre z:ZOhU ps:post		{ baheFree Term TermF
								BTerm BTermF
								pr z ps }

--- 12. Pseudo SELMAhO ---

word_ZEI_word_ :: Text = pr:pre z:word_ZEI_word ps:post	{ baheFree Clause ClauseF
								BClause BClauseF
								pr z ps }

word_BU_ :: Lerfu = pr:pre b:word_BU ps:post		{ baheFree Lerfu LerfuF
								BLerfu BLerfuF
								pr b ps }

ZO_word_ :: Text = pr:pre z:ZO_word ps:post		{ baheFree Clause ClauseF
								BClause BClauseF
								pr z ps }

LOhU_words_LEhU_ :: Text = pr:pre l:LOhU_words_LEhU ps:post
							{ baheFree Clause ClauseF
								BClause BClauseF
								pr l ps }

ZOI_anything_ :: Text = pr:pre z:ZOI_anything ps:post	{ baheFree Clause ClauseF
								BClause BClauseF
								pr z ps }

--- 13. Word Modifiers ---

pre :: [String] = bs:(_:word_SI* b:BAhE { b })* _:word_SI*
							{ bs }

post :: Maybe Free = !_:BU_tail !_:ZEI_tail f:free?	{ f }

number_post :: Maybe Free = !_:BU_tail !_:ZEI_tail fr:(!_:PA_ f:free { f })?
							{ fr }

lerfu_post :: Maybe Free = !_:BU_tail !_:ZEI_tail fr:(!_:lerfu f:free { f })?
							{ fr }

vocative_post :: Maybe Free = !_:BU_tail !_:ZEI_tail fr:(!_:vocative f:free { f })?
							{ fr }

-- ****** C. MAGIC WORD CONSTRUCTS ******

words_SU :: [Word] = ws:
	( z:word_ZEI_word			{ z }
	/ b:word_BU				{ b }
	/ z:ZO_word				{ z }
	/ l:LOhU_words_LEhU			{ l }
	/ z:ZOI_anything			{ z }
	/ !_:SU a:any_word			{ Word a }
 )* _:SU_tail						{ ws }

word_SI :: Word = w:
	( z:word_ZEI_word			{ z }
	/ b:word_BU				{ b }
	/ z:ZO_word				{ z }
	/ l:LOhU_words_LEhU			{ l }
	/ z:ZOI_anything			{ z }
	/ a:any_word				{ Word a }
 ) _:SI_tail						{ w }

word_BU :: Word = w:
	( z:ZO_word				{ z }
	/ l:LOhU_words_LEhU			{ l }
	/ z:ZOI_anything			{ z }
	/ a:any_word				{ Word a }
 ) bzs:
 	( bts:(bt:BU_tail { bt })*
		zts:(zt:ZEI_tail { zt })+	{ (bts, zts) }
 )* b:BU_tail+						{ BUStr w bzs b }

word_ZEI_word :: Word = w:
	( z:ZO_word				{ z }
	/ l:LOhU_words_LEhU			{ l }
	/ z:ZOI_anything			{ z }
	/ a:any_word				{ Word a }
 ) zbs:
	( zts:(zt:ZEI_tail { zt } )*
		bts:(bt:BU_tail { bt })+	{ (zts, bts) }
 )* z:ZEI_tail+						{ ZEIStr w zbs z }

SU_tail :: () = _:word_SI* _:SU

SI_tail :: () = _:word_SI* _:SI

BU_tail :: BU = _:word_SI* _:BU				{ BU }

ZEI_tail :: ZEI = _:word_SI* _:ZEI w:any_word		{ ZEI w }

ZO_word :: Word = _:ZO w:any_word			{ ZO w }

LOhU_words_LEhU :: Word = _:LOhU ws:
	( !_:LEhU w:any_word			{ w }
 )* _:LEhU						{ LOhU ws }

ZOI_anything :: Word = z:ZOI sep:any_word str:
	( !w:any_word[w == sep] c:anything	{ c }
 )* sep':any_word[sep == sep']				{ ZOI z sep $ unwords str }

-- ****** D. WORDS ******

anything :: String = _:Y* ns:non_space+			{ ns }

any_word :: String
	= c:CMEVLA					{ c }
	/ b:BRIVLA					{ b }
	/ c:CMAVO					{ c }

CMEVLA :: String = _:Y* c:cmevla			{ c }
BRIVLA :: String = _:Y* b:brivla			{ b }
CMAVO :: String = _:Y* c:cmavo				{ c }

BAI :: String = _:Y* &_:cmavo r:
	( c:c a:a h:h a':a			{ [c, a, h, a'] }
	/ p:p u:u h:h i:i			{ [p, u, h, i] }
	/ n:n u:u h:h o:o			{ [n, u, h, o] }
	/ k:k a:a h:h e:e			{ [k, a, h, e] }
	/ c:c u:u h:h e:e			{ [c, u, h, e] }
	/ n:n a:a u:u				{ [n, a, u] }
	/ r':r u:u h:h i:i			{ [r', u, h, i] }
	/ t:t a:a h:h e:e			{ [t, a, h, e] }
	/ d:d i:i h:h i':i			{ [d, i, h, i'] }
	/ n:n a:a h:h o:o			{ [n, a, h, o] }
	/ v:v e:e h:h u:u			{ [v, e, h, u] }
	/ v:v e:e h:h a:a			{ [v, e, h, a] }
	/ v:v e:e h:h i:i			{ [v, e, h, i] }
	/ v:v e:e h:h e':e			{ [v, e, h, e'] }
	/ v:v i:i h:h i':i			{ [v, i, h, i'] }
	/ v:v i:i h:h a:a			{ [v, i, h, a] }
	/ v:v i:i h:h u:u			{ [v, i, h, u] }
	/ v:v i:i h:h e:e			{ [v, i, h, e] }
	/ v:v i:i !_:h !_:nucleus		{ [v, i] }
	/ v:v a:a !_:h !_:nucleus		{ [v, a] }
	/ v:v u:u !_:h !_:nucleus		{ [v, u] }
	/ d:d u:u h:h a:a			{ [d, u, h, a] }
	/ b:b e:e h:h a:a			{ [b, e, h, a] }
	/ n:n e:e h:h u:u			{ [n, e, h, u] }
	/ v:v u:u h:h a:a			{ [v, u, h, a] }
	/ g:g a:a h:h u:u			{ [g, a, h, u] }
	/ t:t i:i h:h a:a			{ [t, i, h, a] }
	/ n:n i:i h:h a:a			{ [n, i, h, a] }
	/ c:c a:a h:h u:u			{ [c, a, h, u] }
	/ z:z u:u h:h a:a			{ [z, u, h, a] }
	/ r:r i:i h:h u:u			{ [r, i, h, u] }
	/ r:r u:u h:h u':u			{ [r, u, h, u'] }
	/ r:r e:e h:h o:o			{ [r, e, h, o] }
	/ t:t e:e h:h e':e			{ [t, e, h, e'] }
	/ b:b u:u h:h u':u			{ [b, u, h, u'] }
	/ n:n e:e h:h a:a			{ [n, e, h, a] }
	/ p:p a:a h:h o:o			{ [p, a, h, o] }
	/ n:n e:e h:h i:i			{ [n, e, h, i] }
	/ t:t o:o h:h o':o			{ [t, o, h, o'] }
	/ z:z o:o h:h i:i			{ [z, o, h, i] }
	/ z:z e:e h:h o:o			{ [z, e, h, o] }
	/ z:z o:o h:h a:a			{ [z, o, h, a] }
	/ f:f a:a h:h a':a			{ [f, a, h, a'] }
	/ z:z e:e h:h u:u			{ [z, e, h, u] }
	/ z:z e:e h:h a:a			{ [z, e, h, a] }
	/ z:z e:e h:h i:i			{ [z, e, h, i] }
	/ z:z e:e h:h e':e			{ [z, e, h, e'] }
	/ c:c o:o h:h i:i			{ [c, o, h, i] }
	/ p:p u:u h:h o:o			{ [p, u, h, o] }
	/ c:c o:o h:h u:u			{ [c, o, h, u] }
	/ m:m o:o h:h u:u			{ [m, o, h, u] }
	/ c:c a:a h:h o:o			{ [c, a, h, o] }
	/ c:c o:o h:h a:a			{ [c, o, h, a] }
	/ d:d e:e h:h a:a			{ [d, e, h, a] }
	/ b:b a:a h:h o:o			{ [b, a, h, o] }
	/ d:d i:i h:h a:a			{ [d, i, h, a] }
	/ z:z a:a h:h o:o			{ [z, a, h, o] }
	/ z:z u:u !_:h !_:nucleus		{ [z, u] }
	/ z:z a:a !_:h !_:nucleus		{ [z, a] }
	/ z:z i:i !_:h !_:nucleus		{ [z, i] }
	/ b:b a:a !_:h !_:nucleus		{ [b, a] }
	/ p:p u:u !_:h !_:nucleus		{ [p, u] }
	/ c:c a:a !_:h !_:nucleus		{ [c, a] }
	/ k:k i:i !_:h !_:nucleus		{ [k, i] }
	/ d:d u:u h:h o:o			{ [d, u, h, o] }
	/ s:s i:i h:h u:u			{ [s, i, h, u] }
	/ z:z a:a u:u				{ [z, a, u] }
	/ k:k i:i h:h i':i			{ [k, i, h, i'] }
	/ d:d u:u h:h i:i			{ [d, u, h, i] }
	/ c:c u:u h:h u':u			{ [c, u, h, u'] }
	/ t:t u:u h:h i:i			{ [t, u, h, i] }
	/ t:t i:i h:h u:u			{ [t, i, h, u] }
	/ d:d i:i h:h o:o			{ [d, i, h, o] }
	/ j:j i:i h:h u:u			{ [j, i, h, u] }
	/ r:r i:i h:h a:a			{ [r, i, h, a] }
	/ n:n i:i h:h i':i			{ [n, i, h, i'] }
	/ m:m u:u h:h i:i			{ [m, u, h, i] }
	/ k:k i:i h:h u:u			{ [k, i, h, u] }
	/ v:v a:a h:h u:u			{ [v, a, h, u] }
	/ k:k o:o i:i				{ [k, o, i] }
	/ c:c a:a h:h i:i			{ [c, a, h, i] }
	/ t:t a:a h:h i:i			{ [t, a, h, i] }
	/ p:p u:u h:h e:e			{ [p, u, h, e] }
	/ j:j a:a h:h i:i			{ [j, a, h, i] }
	/ k:k a:a i:i				{ [k, a, i] }
	/ b:b a:a i:i				{ [b, a, i] }
	/ f:f i:i h:h e:e			{ [f, i, h, e] }
	/ d:d e:e h:h i:i			{ [d, e, h, i] }
	/ c:c i:i h:h o:o			{ [c, i, h, o] }
	/ m:m a:a u:u				{ [m, a, u] }
	/ m:m u:u h:h u':u			{ [m, u, h, u'] }
	/ r:r i:i h:h i':i			{ [r, i, h, i'] }
	/ r:r a:a h:h i:i			{ [r, a, h, i] }
	/ k:k a:a h:h a':a			{ [k, a, h, a'] }
	/ p:p a:a h:h u:u			{ [p, a, h, u] }
	/ p:p a:a h:h a':a			{ [p, a, h, a'] }
	/ l:l e:e h:h a:a			{ [l, e, h, a] }
	/ k:k u:u h:h u':u			{ [k, u, h, u'] }
	/ t:t a:a i:i				{ [t, a, i] }
	/ b:b a:a u:u				{ [b, a, u] }
	/ m:m a:a h:h i:i			{ [m, a, h, i] }
	/ c:c i:i h:h e:e			{ [c, i, h, e] }
	/ f:f a:a u:u				{ [f, a, u] }
	/ p:p o:o h:h i:i			{ [p, o, h, i] }
	/ c:c a:a u:u				{ [c, a, u] }
	/ m:m a:a h:h e:e			{ [m, a, h, e] }
	/ c:c i:i h:h u:u			{ [c, i, h, u] }
	/ r:r a:a h:h a':a			{ [r, a, h, a'] }
	/ p:p u:u h:h a:a			{ [p, u, h, a] }
	/ l:l i:i h:h e:e			{ [l, i, h, e] }
	/ l:l a:a h:h u:u			{ [l, a, h, u] }
	/ b:b a:a h:h i:i			{ [b, a, h, i] }
	/ k:k a:a h:h i:i			{ [k, a, h, i] }
	/ s:s a:a u:u				{ [s, a, u] }
	/ f:f a:a h:h e:e			{ [f, a, h, e] }
	/ b:b e:e h:h i:i			{ [b, e, h, i] }
	/ t:t i:i h:h i':i			{ [t, i, h, i'] }
	/ j:j a:a h:h e:e			{ [j, a, h, e] }
	/ g:g a:a h:h a':a			{ [g, a, h, a'] }
	/ v:v a:a h:h o:o			{ [v, a, h, o] }
	/ j:j i:i h:h o:o			{ [j, i, h, o] }
	/ m:m e:e h:h a:a			{ [m, e, h, a] }
	/ d:d o:o h:h e:e			{ [d, o, h, e] }
	/ j:j i:i h:h e:e			{ [j, i, h, e] }
	/ p:p i:i h:h o:o			{ [p, i, h, o] }
	/ g:g a:a u:u				{ [g, a, u] }
	/ z:z u:u h:h e:e			{ [z, u, h, e] }
	/ m:m e:e h:h e':e			{ [m, e, h, e'] }
	/ r:r a:a i:i				{ [r, a, i] }
 ) &_:post_cmavo					{ r }

BAhE :: String = _:Y* &_:cmavo r:
	( b:b a:a h:h e:e			{ [b, a, h, e] }
	/ z:z a:a h:h e:e			{ [z, a, h, e] }
 ) &_:post_cmavo
							{ r }

BE :: String = _:Y* &_:cmavo r:(b:b e:e { [b, e] }) &_:post_cmavo
							{ r }

BEI :: String = _:Y* &_:cmavo r:(b:b e:e i:i { [b, e, i] }) &_:post_cmavo
							{ r }

BEhO :: String = _:Y* &_:cmavo r:(b:b e:e h:h o:o { [b, e, h, o] }) &_:post_cmavo
							{ r }

BO :: String = _:Y* &_:cmavo r:(b:b o:o { [b, o] }) &_:post_cmavo
							{ r }

BOI :: String = _:Y* &_:cmavo r:(b:b o:o i:i { [b, o, i] }) &_:post_cmavo
							{ r }

BU :: String = _:Y* &_:cmavo r:(b:b u:u { [b, u] }) &_:post_cmavo
							{ r }

BY :: String = _:Y* &_:cmavo r:
	( t:t e:e i:i				{ [t, e, i] }
	/ f:f o:o i:i				{ [f, o, i] }
	/ c:c e:e h:h a:a			{ [c, e, h, a] }
	/ l:l a:a u:u				{ [l, a, u] }
	/ z:z a:a i:i				{ [z, a, i] }
	/ t:t a:a u:u				{ [t, a, u] }
	/ j:j o:o h:h o':o			{ [j, o, h, o'] }
	/ r:r u:u h:h o:o			{ [r, u, h, o] }
	/ g:g e:e h:h o:o			{ [g, e, h, o] }
	/ j:j e:e h:h o:o			{ [j, e, h, o] }
	/ l:l o:o h:h a:a			{ [l, o, h, a] }
	/ n:n a:a h:h a':a			{ [n, a, h, a'] }
	/ s:s e:e h:h e':e			{ [s, e, h, e'] }
	/ t:t o:o h:h a:a			{ [t, o, h, a] }
	/ g:g a:a h:h e:e			{ [g, a, h, e] }
	/ y:y h:h y':y				{ [y, h, y'] }
	/ b:b y:y				{ [b, y] }
	/ c:c y:y				{ [c, y] }
	/ d:d y:y				{ [d, y] }
	/ f:f y:y				{ [f, y] }
	/ g:g y:y				{ [g, y] }
	/ j:j y:y				{ [j, y] }
	/ k:k y:y				{ [k, y] }
	/ l:l y:y				{ [l, y] }
	/ m:m y:y				{ [m, y] }
	/ n:n y:y				{ [n, y] }
	/ p:p y:y				{ [p, y] }
	/ r:r y:y				{ [r, y] }
	/ s:s y:y				{ [s, y] }
	/ t:t y:y				{ [t, y] }
	/ v:v y:y				{ [v, y] }
	/ x:x y:y				{ [x, y] }
	/ z:z y:y				{ [z, y] }
 ) &_:post_cmavo					{ r }

CEI :: String = _:Y* &_:cmavo r:(c:c e:e i:i { [c, e, i] }) &_:post_cmavo
							{ r }

CO :: String = _:Y* &_:cmavo r:(c:c o:o { [c, o] }) &_:post_cmavo
							{ r }

COI :: String = _:Y* &_:cmavo r:
	( j:j u:u h:h i:i				{ [j, u, h, i] }
	/ c:c o:o i:i					{ [c, o, i] }
	/ f:f i:i h:h i':i				{ [f, i, h, i'] }
	/ t:t a:a h:h a':a				{ [t, a, h, a'] }
	/ m:m u:u h:h o:o				{ [m, u, h, o] }
	/ f:f e:e h:h o:o				{ [f, e, h, o] }
	/ c:c o:o h:h o':o				{ [c, o, h, o'] }
	/ p:p e:e h:h u:u				{ [p, e, h, u] }
	/ k:k e:e h:h o:o				{ [k, e, h, o] }
	/ n:n u:u h:h e:e				{ [n, u, h, e] }
	/ r:r e:e h:h i:i				{ [r, e, h, i] }
	/ b:b e:e h:h e':e				{ [b, e, h, e'] }
	/ j:j e:e h:h e':e				{ [j, e, h, e'] }
	/ m:m i:i h:h e:e				{ [m, i, h, e] }
	/ k:k i:i h:h e:e				{ [k, i, h, e] }
	/ v:v i:i h:h o:o				{ [v, i, h, o] }
 ) &_:post_cmavo					{ r }

CU :: String = _:Y* &_:cmavo r:(c:c u:u { [c, u] }) &_:post_cmavo
							{ r }

DOI :: String = _:Y* &_:cmavo r:(d:d o:o i:i { [d, o, i] }) &_:post_cmavo
							{ r }

DOhU :: String = _:Y* &_:cmavo r:(d:d o:o h:h u:u { [d, o, h, u] }) &_:post_cmavo
							{ r }

FA :: String = _:Y* &_:cmavo r:
	( f:f a:a i:i				{ [f, a, i] }
	/ f:f a:a				{ [f, a] }
	/ f:f e:e				{ [f, e] }
	/ f:f o:o				{ [f, o] }
	/ f:f u:u				{ [f, u] }
	/ f:f i:i h:h a:a			{ [f, i, h, a] }
	/ f:f i:i				{ [f, i] }
 ) &_:post_cmavo					{ r }

FAhO :: String = _:Y* &_:cmavo r:(f:f a:a h:h o:o { [f, a, h, o] }) &_:post_cmavo
							{ r }

FEhU :: String = _:Y* &_:cmavo r:(f:f e:e h:h u:u { [f, e, h, u] }) &_:post_cmavo
							{ r }

FIhO :: String = _:Y* &_:cmavo r:(f:f i:i h:h o:o { [f, i, h, o] }) &_:post_cmavo
							{ r }

GA :: String = _:Y* &_:cmavo r:
	( g:g e:e h:h i:i			{ [g, e, h, i] }
	/ g:g e:e				{ [g, e] }
	/ g:g o:o				{ [g, o] }
	/ g:g a:a				{ [g, a] }
	/ g:g u:u				{ [g, u] }
 ) &_:post_cmavo					{ r }

GEhU :: String = _:Y* &_:cmavo r:(g:g e:e h:h u:u { [g, e, h, u] }) &_:post_cmavo
							{ r }

GI :: String = _:Y* &_:cmavo r:(g:g i:i { [g, i] }) &_:post_cmavo
							{ r }

GIhA :: String = _:Y* &_:cmavo r:
	( g:g i:i h:h e:e			{ [g, i, h, e] }
	/ g:g i:i h:h i':i			{ [g, i, h, i'] }
	/ g:g i:i h:h o:o			{ [g, i, h, o] }
	/ g:g i:i h:h a:a			{ [g, i, h, a] }
	/ g:g i:i h:h u:u			{ [g, i, h, u] }
 ) &_:post_cmavo					{ r }

GOI :: String = _:Y* &_:cmavo r:
	( n:n o:o h:h u:u			{ [n, o, h, u] }
	/ n:n e:e				{ [n, e] }
	/ g:g o:o i:i				{ [g, o, i] }
	/ p:p o:o h:h u:u			{ [p, o, h, u] }
	/ p:p e:e				{ [p, e] }
	/ p:p o:o 				{ [p, o] }
 ) &_:post_cmavo					{ r }

GOhA :: String = _:Y* &_:cmavo r:
	( m:m o:o				{ [m, o] }
	/ n:n e:e i:i				{ [n, e, i] }
	/ g:g o:o h:h u:u			{ [g, o, h, u] }
	/ g:g o:o h:h o':o			{ [g, o, h, o'] }
	/ g:g o:o h:h i:i			{ [g, o, h, i] }
	/ n:n o:o h:h a:a			{ [n, o, h, a] }
	/ g:g o:o h:h e:e			{ [g, o, h, e] }
	/ g:g o:o h:h a:a			{ [g, o, h, a] }
	/ d:d u:u				{ [d, u] }
	/ b:b u:u h:h a:a			{ [b, u, h, a] }
	/ b:b u:u h:h e:e			{ [b, u, h, e] }
	/ b:b u:u h:h i:i			{ [b, u, h, i] }
	/ c:c o:o h:h e:e			{ [c, o, h, e] }
 ) &_:post_cmavo					{ r }

SelmahoI :: String = _:Y* &_:cmavo r:(i:i { [i] }) &_:post_cmavo	{ r }

JAI :: String = _:Y* &_:cmavo r:(j:j a:a i:i { [j, a, i] }) &_:post_cmavo
							{ r }

JOI :: String = _:Y* &_:cmavo r:
	( f:f a:a h:h u:u			{ [f, a, h, u] }
	/ p:p i:i h:h u:u			{ [p, i, h, u] }
	/ j:j o:o i:i				{ [j, o, i] }
	/ c:c e:e h:h o:o			{ [c, e, h, o] }
	/ c:c e:e				{ [c, e] }
	/ j:j o:o h:h u:u			{ [j, o, h, u] }
	/ k:k u:u h:h a:a			{ [k, u, h, a] }
	/ j:j o:o h:h e:e			{ [j, o, h, e] }
	/ j:j u:u h:h e:e			{ [j, u, h, e] }
	/ j:j o:o h:h i:i			{ [j, o, h, i] }
	/ j:j e:e h:h i:i			{ [j, e, h, i] }
	/ j:j e:e				{ [j, e] }
	/ j:j o:o				{ [j, o] }
	/ j:j a:a				{ [j, a] }
	/ j:j u:u				{ [j, u] }
	/ a:a					{ [a] }
	/ e:e					{ [e] }
	/ j:j i:i				{ [j, i] }
	/ o:o					{ [o] }
	/ u:u					{ [u] }
	/ m:m i:i h:h i':i			{ [m, i, h, i'] }
	/ b:b i:i h:h o:o			{ [b, i, h, o] }
	/ b:b i:i h:h i':i			{ [b, i, h, i'] }
	/ g:g e:e h:h a:a			{ [g, e, h, a] }
	/ f:f u:u h:h u':u			{ [f, u, h, u'] }
	/ p:p i:i h:h i':i			{ [p, i, h, i'] }
	/ f:f e:e h:h i:i			{ [f, e, h, i] }
	/ v:v u:u h:h u':u			{ [v, u, h, u'] }
	/ s:s u:u h:h i:i			{ [s, u, h, i] }
	/ j:j u:u h:h u':u			{ [j, u, h, u'] }
	/ g:g e:e i:i				{ [g, e, i] }
	/ p:p a:a h:h i:i			{ [p, a, h, i] }
	/ f:f a:a h:h i:i			{ [f, a, h, i] }
	/ t:t e:e h:h a:a			{ [t, e, h, a] }
	/ c:c u:u h:h a:a			{ [c, u, h, a] }
	/ v:v a:a h:h a':a			{ [v, a, h, a'] }
	/ n:n e:e h:h o:o			{ [n, e, h, o] }
	/ d:d e:e h:h o:o			{ [d, e, h, o] }
	/ f:f e:e h:h a:a			{ [f, e, h, a] }
	/ s:s a:a h:h o:o			{ [s, a, h, o] }
	/ r:r e:e h:h a:a			{ [r, e, h, a] }
	/ r:r i:i h:h o:o			{ [r, i, h, o] }
	/ s:s a:a h:h i:i			{ [s, a, h, i] }
	/ p:p i:i h:h a:a			{ [p, i, h, a] }
	/ s:s i:i h:h i':i			{ [s, i, h, i'] }
 ) &_:post_cmavo					{ r }

KE :: String = _:Y* &_:cmavo r:(k:k e:e { [k, e] }) &_:post_cmavo
							{ r }

KEhE :: String = _:Y* &_:cmavo r:(k:k e:e h:h e':e { [k, e, h, e'] }) &_:post_cmavo
							{ r }

KEI :: String = _:Y* &_:cmavo r:(k:k e:e i:i { [k, e, i] }) &_:post_cmavo
							{ r }

KOhA :: String = _:Y* &_:cmavo r:
	( d:d a:a h:h u:u			{ [d, a, h, u] }
	/ d:d a:a h:h e:e			{ [d, a, h, e] }
	/ d:d i:i h:h u:u			{ [d, i, h, u] }
	/ d:d i:i h:h e:e			{ [d, i, h, e] }
	/ d:d e:e h:h u:u			{ [d, e, h, u] }
	/ d:d e:e h:h e':e			{ [d, e, h, e'] }
	/ d:d e:e i:i				{ [d, e, i] }
	/ d:d o:o h:h i:i			{ [d, o, h, i] }
	/ m:m i:i h:h o:o			{ [m, i, h, o] }
	/ m:m a:a h:h a':a			{ [m, a, h, a'] }
	/ m:m i:i h:h a:a			{ [m, i, h, a] }
	/ d:d o:o h:h o':o			{ [d, o, h, o'] }
	/ k:k o:o h:h a:a			{ [k, o, h, a] }
	/ f:f o:o h:h u:u			{ [f, o, h, u] }
	/ k:k o:o h:h e:e			{ [k, o, h, e] }
	/ k:k o:o h:h i:i			{ [k, o, h, i] }
	/ k:k o:o h:h o':o			{ [k, o, h, o'] }
	/ k:k o:o h:h u:u			{ [k, o, h, u] }
	/ f:f o:o h:h a:a			{ [f, o, h, a] }
	/ f:f o:o h:h e:e			{ [f, o, h, e] }
	/ f:f o:o h:h i:i			{ [f, o, h, i] }
	/ f:f o:o h:h o':o			{ [f, o, h, o'] }
	/ v:v o:o h:h a:a			{ [v, o, h, a] }
	/ v:v o:o h:h e:e			{ [v, o, h, e] }
	/ v:v o:o h:h i:i			{ [v, o, h, i] }
	/ v:v o:o h:h o':o			{ [v, o, h, o'] }
	/ v:v o:o h:h u:u			{ [v, o, h, u] }
	/ r:r u:u				{ [r, u] }
	/ r:r i:i				{ [r, i] }
	/ r:r a:a				{ [r, a] }
	/ t:t a:a				{ [t, a] }
	/ t:t u:u				{ [t, u] }
	/ t:t i:i				{ [t, i] }
	/ z:z i:i h:h o:o			{ [z, i, h, o] }
	/ k:k e:e h:h a:a			{ [k, e, h, a] }
	/ m:m a:a				{ [m, a] }
	/ z:z u:u h:h i:i			{ [z, u, h, i] }
	/ z:z o:o h:h e:e			{ [z, o, h, e] }
	/ c:c e:e h:h u:u			{ [c, e, h, u] }
	/ d:d a:a				{ [d, a] }
	/ d:d e:e				{ [d, e] }
	/ d:d i:i				{ [d, i] }
	/ k:k o:o				{ [k, o] }
	/ m:m i:i				{ [m, i] }
	/ d:d o:o				{ [d, o] }
 ) &_:post_cmavo					{ r }

KU :: String = _:Y* &_:cmavo r:(k:k u:u { [k, u] }) &_:post_cmavo
							{ r }

KUhO :: String = _:Y* &_:cmavo r:(k:k u:u h:h o:o { [k, u, h, o] }) &_:post_cmavo
							{ r }

LAhE :: String = _:Y* &_:cmavo r:
	( t:t u:u h:h a:a			{ [t, u, h, a] }
	/ l:l u:u h:h a:a			{ [l, u, h, a] }
	/ l:l u:u h:h o:o			{ [l, u, h, o] }
	/ l:l a:a h:h e:e			{ [l, a, h, e] }
	/ v:v u:u h:h i:i			{ [v, u, h, i] }
	/ l:l u:u h:h i:i			{ [l, u, h, i] }
	/ l:l u:u h:h e:e			{ [l, u, h, e] }
 ) &_:post_cmavo					{ r }

LE :: String = _:Y* &_:cmavo r:
	( l:l a:a i:i				{ [l, a, i] }
	/ l:l a:a h:h i:i			{ [l, a, h, i] }
	/ l:l a:a				{ [l, a] }
	/ l:l e:e i:i				{ [l, e, i] }
	/ l:l o:o i:i				{ [l, o, i] }
	/ l:l e:e h:h i:i			{ [l, e, h, i] }
	/ l:l o:o h:h i:i			{ [l, o, h, i] }
	/ l:l o:o				{ [l, o] }
	/ l:l e:e				{ [l, e] }
 ) &_:post_cmavo					{ r }

LEhU :: String = _:Y* &_:cmavo r:(l:l e:e h:h u:u { [l, e, h, u] }) &_:post_cmavo
							{ r }

LI :: String = _:Y* &_:cmavo r:(l:l i:i { [l, i] }) &_:post_cmavo
							{ r }

LIhU :: String = _:Y* &_:cmavo r:(l:l i:i h:h u:u { [l, i, h, u] }) &_:post_cmavo
							{ r }

LOhO :: String = _:Y* &_:cmavo r:(l:l o:o h:h o':o { [l, o, h, o'] }) &_:post_cmavo
							{ r }

LOhU :: String = _:Y* &_:cmavo r:(l:l o:o h:h u:u { [l, o, h, u] }) &_:post_cmavo
							{ r }

LU :: String = _:Y* &_:cmavo r:(l:l u:u { [l, u] }) &_:post_cmavo
							{ r }

LUhU :: String = _:Y* &_:cmavo r:(l:l u:u h:h u':u { [l, u, h, u'] }) &_:post_cmavo
							{ r }

MAI :: String = _:Y* &_:cmavo r:
	( m:m o:o h:h o':o			{ [m, o, h, o'] }
	/ m:m a:a i:i				{ [m, a, i] }
 ) &_:post_cmavo					{ r }

ME :: String = _:Y* &_:cmavo r:
	( m:m e:e				{ [m, e] }
	/ n:n u:u h:h a:a			{ [n, u, h, a] }
 ) &_:post_cmavo					{ r }

MEhU :: String = _:Y* &_:cmavo r:(m:m e:e h:h u:u { [m, e, h, u] }) &_:post_cmavo
							{ r }

MOI :: String = _:Y* &_:cmavo r:
	( m:m e:e h:h u:u			{ [m, e, h, u] }
	/ m:m e:e i:i				{ [m, e, i] }
	/ m:m o:o i:i				{ [m, o, i] }
	/ s:s i:i h:h e:e			{ [s, i, h, e] }
	/ c:c u:u h:h o:o			{ [c, u, h, o] }
	/ v:v a:a h:h e:e			{ [v, a, h, e] }
 ) &_:post_cmavo					{ r }

MOhE :: String = _:Y* &_:cmavo r:(m:m o:o h:h e:e { [m, o, h, e] }) &_:post_cmavo
							{ r }

NA :: String = _:Y* &_:cmavo r:
	( j:j a:a h:h a':a			{ [j, a, h, a'] }
	/ n:n a:a				{ [n, a] }
 ) &_:post_cmavo					{ r }

NAhE :: String = _:Y* &_:cmavo r:
	( f:f e:e h:h e':e			{ [f, e, h, e'] }
	/ m:m o:o h:h i:i			{ [m, o, h, i] }
	/ t:t o:o h:h e:e			{ [t, o, h, e] }
	/ j:j e:e h:h a:a			{ [j, e, h, a] }
	/ n:n a:a h:h e:e			{ [n, a, h, e] }
	/ n:n o:o h:h e:e			{ [n, o, h, e] }
 ) &_:post_cmavo					{ r }

NIhE :: String = _:Y* &_:cmavo r:(n:n i:i h:h e:e { [n, i, h, e] }) &_:post_cmavo
							{ r }

NIhO :: String = _:Y* &_:cmavo r:
	( n:n i:i h:h o:o			{ [n, i, h, o] }
	/ n:n o:o h:h i:i			{ [n, o, h, i] }
 ) &_:post_cmavo					{ r }

NOI :: String = _:Y* &_:cmavo r:
	( v:v o:o i:i				{ [v, o, i] }
	/ n:n o:o i:i				{ [n, o, i] }
	/ p:p o:o i:i				{ [p, o, i] }
 ) &_:post_cmavo					{ r }

NU :: String = _:Y* &_:cmavo r:
	( n:n i:i				{ [n, i] }
	/ d:d u:u h:h u':u			{ [d, u, h, u'] }
	/ s:s i:i h:h o:o			{ [s, i, h, o] }
	/ n:n u:u				{ [n, u] }
	/ l:l i:i h:h i':i			{ [l, i, h, i'] }
	/ k:k a:a				{ [k, a] }
	/ j:j e:e i:i				{ [j, e, i] }
	/ s:s u:u h:h u':u			{ [s, u, h, u'] }
	/ z:z u:u h:h o:o			{ [z, u, h, o] }
	/ m:m u:u h:h e:e			{ [m, u, h, e] }
	/ p:p u:u h:h u':u			{ [p, u, h, u'] }
	/ z:z a:a h:h i:i			{ [z, a, h, i] }
 ) &_:post_cmavo					{ r }

PA :: String = _:Y* &_:cmavo r:
	( d:d a:a u:u				{ [d, a, u] }
	/ f:f e:e i:i				{ [f, e, i] }
	/ g:g a:a i:i				{ [g, a, i] }
	/ j:j a:a u:u				{ [j, a, u] }
	/ r:r e:e i:i				{ [r, e, i] }
	/ v:v a:a i:i				{ [v, a, i] }
	/ p:p i:i h:h e:e			{ [p, i, h, e] }
	/ p:p i:i				{ [p, i] }
	/ f:f i:i h:h u:u			{ [f, i, h, u] }
	/ z:z a:a h:h u:u			{ [z, a, h, u] }
	/ m:m e:e h:h i:i			{ [m, e, h, i] }
	/ n:n i:i h:h u:u			{ [n, i, h, u] }
	/ k:k i:i h:h o:o			{ [k, i, h, o] }
	/ c:c e:e h:h i:i			{ [c, e, h, i] }
	/ m:m a:a h:h u:u			{ [m, a, h, u] }
	/ r:r a:a h:h e:e			{ [r, a, h, e] }
	/ d:d a:a h:h a':a			{ [d, a, h, a'] }
	/ s:s o:o h:h a:a			{ [s, o, h, a] }
	/ j:j i:i h:h i':i			{ [j, i, h, i'] }
	/ s:s u:u h:h o:o			{ [s, u, h, o] }
	/ s:s u:u h:h e:e			{ [s, u, h, e] }
	/ r:r o:o				{ [r, o] }
	/ r:r a:a u:u				{ [r, a, u] }
	/ s:s o:o h:h u:u			{ [s, o, h, u] }
	/ s:s o:o h:h i:i			{ [s, o, h, i] }
	/ s:s o:o h:h e:e			{ [s, o, h, e] }
	/ s:s o:o h:h o':o			{ [s, o, h, o'] }
	/ m:m o:o h:h a:a			{ [m, o, h, a] }
	/ d:d u:u h:h e:e			{ [d, u, h, e] }
	/ t:t e:e h:h o:o			{ [t, e, h, o] }
	/ x:x o:o				{ [x, o] }
	/ p:p a:a i:i				{ [p, a, i] }
	/ n:n o:o h:h o':o			{ [n, o, h, o'] }
	/ n:n o:o				{ [n, o] }
	/ p:p a:a				{ [p, a] }
	/ r:r e:e				{ [r, e] }
	/ c:c i:i				{ [c, i] }
	/ v:v o:o				{ [v, o] }
	/ m:m u:u				{ [m, u] }
	/ x:x a:a				{ [x, a] }
	/ z:z e:e				{ [z, e] }
	/ b:b i:i				{ [b, i] }
	/ s:s o:o				{ [s, o] }
 ) &_:post_cmavo					{ r }

ROI :: String = _:Y* &_:cmavo r:
	( r:r e:e h:h u:u			{ [r, e, h, u] }
	/ r:r o:o i:i				{ [r, o, i] }
 ) &_:post_cmavo					{ r }

SE :: String = _:Y* &_:cmavo r:
	( s:s e:e				{ [s, e] }
	/ t:t e:e				{ [t, e] }
	/ v:v e:e				{ [v, e] }
	/ x:x e:e				{ [x, e] }
 ) &_:post_cmavo					{ r }

SEI :: String = _:Y* &_:cmavo r:(s:s e:e i:i { [s, e, i] }) &_:post_cmavo
							{ r }

SEhU :: String = _:Y* &_:cmavo r:(s:s e:e h:h u:u { [s, e, h, u] }) &_:post_cmavo
							{ r }

SI :: String = _:Y* &_:cmavo r:(s:s i:i { [s, i] }) &_:post_cmavo
							{ r }

SU :: String = _:Y* &_:cmavo r:(s:s u:u { [s, u] }) &_:post_cmavo
							{ r }

TEhU :: String = _:Y* &_:cmavo r:(t:t e:e h:h u:u { [t, e, h, u] }) &_:post_cmavo
							{ r }

TO :: String = _:Y* &_:cmavo r:(t:t o:o { [t, o] }) &_:post_cmavo
							{ r }

TOI :: String = _:Y* &_:cmavo r:(t:t o:o i:i { [t, o, i] }) &_:post_cmavo
							{ r }

TUhE :: String = _:Y* &_:cmavo r:(t:t u:u h:h e:e { [t, u, h, e] }) &_:post_cmavo
							{ r }

TUhU :: String = _:Y* &_:cmavo r:(t:t u:u h:h u':u { [t, u, h, u'] }) &_:post_cmavo
							{ r }

UI :: String = _:Y* &_:cmavo result:
	( r:r a:a h:h o:o			{ [r, a, h, o] }
	/ k:k e:e h:h i:i			{ [k, e, h, i] }
	/ g:g a:a h:h o:o			{ [g, a, h, o] }
	/ n:n a:a i:i				{ [n, a, i] }
	/ p:p e:e i:i				{ [p, e, i] }
	/ c:c a:a i:i				{ [c, a, i] }
	/ c:c u:u h:h i:i			{ [c, u, h, i] }
	/ s:s a:a i:i				{ [s, a, i] }
	/ r:r u:u h:h e:e			{ [r, u, h, e] }
	/ d:d a:a h:h o:o			{ [d, a, h, o] }
	/ f:f u:u h:h e:e			{ [f, u, h, e] }
	/ f:f u:u h:h o:o			{ [f, u, h, o] }
	/ i:i h:h a:a				{ [i, h, a] }
	/ i:i e:e				{ [i, e] }
	/ a:a h:h e:e				{ [a, h, e] }
	/ u:u h:h i:i				{ [u, h, i] }
	/ i:i h:h o:o				{ [i, h, o] }
	/ i:i h:h e:e				{ [i, h, e] }
	/ a:a h:h a':a				{ [a, h, a'] }
	/ i:i a:a				{ [i, a] }
	/ o:o h:h i:i				{ [o, h, i] }
	/ o:o h:h e:e				{ [o, h, e] }
	/ e:e h:h e':e				{ [e, h, e'] }
	/ o:o i:i				{ [o, i] }
	/ u:u o:o				{ [u, o] }
	/ e:e h:h i:i				{ [e, h, i] }
	/ u:u h:h o:o				{ [u, h, o] }
	/ a:a u:u				{ [a, u] }
	/ u:u a:a				{ [u, a] }
	/ a:a h:h i:i				{ [a, h, i] }
	/ i:i h:h u:u				{ [i, h, u] }
	/ i:i i':i				{ [i, i'] }
	/ u:u h:h a:a				{ [u, h, a] }
	/ u:u i:i				{ [u, i] }
	/ a:a h:h o:o				{ [a, h, o] }
	/ a:a i:i				{ [a, i] }
	/ a:a h:h u:u				{ [a, h, u] }
	/ i:i u:u				{ [i, u] }
	/ e:e i:i				{ [e, i] }
	/ o:o h:h o':o				{ [o, h, o'] }
	/ e:e h:h a:a				{ [e, h, a] }
	/ u:u u':u				{ [u, u'] }
	/ o:o h:h a:a				{ [o, h, a] }
	/ o:o h:h u:u				{ [o, h, u] }
	/ u:u h:h u':u				{ [u, h, u'] }
	/ e:e h:h o:o				{ [e, h, o] }
	/ i:i o:o				{ [i, o] }
	/ e:e h:h u:u				{ [e, h, u] }
	/ u:u e:e				{ [u, e] }
	/ i:i h:h i':i				{ [i, h, i'] }
	/ u:u h:h e:e				{ [u, h, e] }
	/ b:b a:a h:h a':a			{ [b, a, h, a'] }
	/ j:j a:a h:h o:o			{ [j, a, h, o] }
	/ c:c a:a h:h e:e			{ [c, a, h, e] }
	/ s:s u:u h:h a:a			{ [s, u, h, a] }
	/ t:t i:i h:h e:e			{ [t, i, h, e] }
	/ k:k a:a h:h u:u			{ [k, a, h, u] }
	/ s:s e:e h:h o:o			{ [s, e, h, o] }
	/ z:z a:a h:h a':a			{ [z, a, h, a'] }
	/ p:p e:e h:h i:i			{ [p, e, h, i] }
	/ r:r u:u h:h a:a			{ [r, u, h, a] }
	/ j:j u:u h:h a:a			{ [j, u, h, a] }
	/ t:t a:a h:h o:o			{ [t, a, h, o] }
	/ r:r a:a h:h u:u			{ [r, a, h, u] }
	/ l:l i:i h:h a:a			{ [l, i, h, a] }
	/ b:b a:a h:h u:u			{ [b, a, h, u] }
	/ m:m u:u h:h a:a			{ [m, u, h, a] }
	/ d:d o:o h:h a:a			{ [d, o, h, a] }
	/ t:t o:o h:h u:u			{ [t, o, h, u] }
	/ v:v a:a h:h i:i			{ [v, a, h, i] }
	/ p:p a:a h:h e:e			{ [p, a, h, e] }
	/ z:z u:u h:h u':u			{ [z, u, h, u'] }
	/ s:s a:a h:h e:e			{ [s, a, h, e] }
	/ l:l a:a h:h a':a			{ [l, a, h, a'] }
	/ k:k e:e h:h u:u			{ [k, e, h, u] }
	/ s:s a:a h:h u:u			{ [s, a, h, u] }
	/ d:d a:a h:h i:i			{ [d, a, h, i] }
	/ j:j e:e h:h u:u			{ [j, e, h, u] }
	/ s:s a:a h:h a':a			{ [s, a, h, a'] }
	/ k:k a:a u:u				{ [k, a, u] }
	/ t:t a:a h:h u:u			{ [t, a, h, u] }
	/ n:n a:a h:h i:i			{ [n, a, h, i] }
	/ j:j o:o h:h a:a			{ [j, o, h, a] }
	/ b:b i:i h:h u:u			{ [b, i, h, u] }
	/ l:l i:i h:h o:o			{ [l, i, h, o] }
	/ p:p a:a u:u				{ [p, a, u] }
	/ m:m i:i h:h u:u			{ [m, i, h, u] }
	/ k:k u:u h:h i:i			{ [k, u, h, i] }
	/ j:j i:i h:h a:a			{ [j, i, h, a] }
	/ s:s i:i h:h a:a			{ [s, i, h, a] }
	/ p:p o:o h:h o':o			{ [p, o, h, o'] }
	/ p:p e:e h:h a:a			{ [p, e, h, a] }
	/ r:r o:o h:h i:i			{ [r, o, h, i] }
	/ r:r o:o h:h e:e			{ [r, o, h, e] }
	/ r:r o:o h:h o':o			{ [r, o, h, o'] }
	/ r:r o:o h:h u:u			{ [r, o, h, u] }
	/ r:r o:o h:h a:a			{ [r, o, h, a] }
	/ r:r e:e h:h e':e			{ [r, e, h, e'] }
	/ l:l e:e h:h o:o			{ [l, e, h, o] }
	/ j:j u:u h:h o:o			{ [j, u, h, o] }
	/ f:f u:u h:h i:i			{ [f, u, h, i] }
	/ d:d a:a i:i				{ [d, a, i] }
	/ g:g a:a h:h i:i			{ [g, a, h, i] }
	/ z:z o:o h:h o':o			{ [z, o, h, o'] }
	/ b:b e:e h:h u:u			{ [b, e, h, u] }
	/ r:r i:i h:h e:e			{ [r, i, h, e] }
	/ s:s e:e h:h i:i			{ [s, e, h, i] }
	/ s:s e:e h:h a:a			{ [s, e, h, a] }
	/ v:v u:u h:h e:e			{ [v, u, h, e] }
	/ k:k i:i h:h a:a			{ [k, i, h, a] }
	/ x:x u:u				{ [x, u] }
	/ g:g e:e h:h e':e			{ [g, e, h, e'] }
	/ b:b u:u h:h o:o			{ [b, u, h, o] }
 ) &_:post_cmavo { result }

VAU :: String = _:Y* &_:cmavo r:(v:v a:a u:u { [v, a, u] }) &_:post_cmavo
							{ r }

VEI :: String = _:Y* &_:cmavo r:(v:v e:e i:i { [v, e, i] }) &_:post_cmavo
							{ r }

VEhO :: String = _:Y* &_:cmavo r:(v:v e:e h:h o:o { [v, e, h, o] }) &_:post_cmavo
							{ r }

VUhO :: String = _:Y* &_:cmavo r:(v:v u:u h:h o:o { [v, u, h, o] }) &_:post_cmavo
							{ r }

XI :: String = _:Y* &_:cmavo r:(x:x i:i { [x, i] }) &_:post_cmavo
							{ r }

ZEI :: String = _:Y* &_:cmavo r:(z:z e:e i:i { [z, e, i] }) &_:post_cmavo
							{ r }

ZIhE :: String = _:Y* &_:cmavo r:(z:z i:i h:h e:e { [z, i, h, e] }) &_:post_cmavo
							{ r }

ZO :: String = _:Y* &_:cmavo r:(z:z o:o { [z, o] }) &_:post_cmavo
							{ r }

ZOI :: String = _:Y* &_:cmavo r:
	( z:z o:o i:i				{ [z, o, i] }
	/ l:l a:a h:h o:o			{ [l, a, h, o] }
 ) &_:post_cmavo					{ r }

ZOhU :: String = _:Y* &_:cmavo r:(z:z o:o h:h u:u { [z, o, h, u] }) &_:post_cmavo
							{ r }

-- ****** E. MORPHOLOGY ******

cmevla :: String
	= j:jbocme					{ j }
	/ z:zifcme					{ z }

jbocme :: String = &_:zifcme s:
	(o:onset n:nucleus c:coda? { o ++ n ++ maybeToList c })+ &_:space'
							{ concat s }

zifcme :: String = !_:h cs:
	( v:V					{ [v] }
	/ vv:VV					{ vv }
	/ y:y					{ [y] }
	/ i:I					{ [i] }
	/ h:h					{ [h] }
	/ c:C !_:space'				{ [c] }
 )* c:C &_:space'					{ concat cs ++ [c] }

cmavo :: String
	= !_:cmevla !_:CVCy_lujvo c:C? i:I? n:nucleus
		hns:(h:h n':nucleus { h : n' })* &_:post_cmavo
							{ catMaybes [c, i] ++
								n ++ concat hns }

CVCy_lujvo :: ()
	= _:C _:V _:C _:y _:initial_rafsi*
		_:(_:final_rafsi / _:gismu / _:fuhivla / _:type_3_fuhivla)

post_cmavo :: () = _:space' / !_:nucleus _:cmavo / _:brivla

brivla :: String
	= g:gismu					{ g }
	/ !_:h f:fuhivla				{ f }
	/ t3f:type_3_fuhivla				{ t3f }
	/ l:lujvo					{ l }

lujvo :: String = !_:cmavo !_:h ir:initial_rafsi+ b:
	( fr:final_rafsi			{ fr }
	/ g:gismu				{ g }
	/ f:fuhivla				{ f }
	/ t3f:type_3_fuhivla			{ t3f }
 )							{ concat ir ++ b }

type_3_fuhivla :: String
	= !_:cmevla c:classifier s:syllable+ &_:space'
							{ concat $ c : s }

fuhivla :: String
	= !_:cmevla !_:cmavo !_:rafsi_string !_:slinkuhi
		s0:syllable ss:syllable+ &_:space'	{ concat $ s0 : ss }

gismu :: String
	= fr:full_rafsi &_:space'			{ fr }

final_rafsi :: String
	= !_:cmevla sr: short_rafsi &_:space'	{ sr }

initial_rafsi :: String
	= ylr:y_less_rafsi				{ ylr }
	/ yr:y_rafsi					{ yr }
	/ fr:fuhivla_rafsi				{ fr }
	/ t3r:type_3_rafsi				{ t3r }
	/ br:brivla_rafsi				{ br }

brivla_rafsi :: String
	= !_:cmavo !_:slinkuhi s0:syllable ss:syllable+ h:h y:y
							{ concat (s0 : ss) ++
								[h, y] }

type_3_rafsi :: String
	= c:classifier s:syllable* o:onset y:y		{ c ++ concat s ++ o ++
								[y] }

fuhivla_rafsi :: String
	= !_:cmavo !_:rafsi_string !_:slinkuhi s:syllable+ o:onset y:y
							{ concat s ++ o ++ [y] }

slinkuhi :: String
	= c:C rs:rafsi_string				{ c : rs }

rafsi_string :: String = ylrs:y_less_rafsi* b:
	( g:gismu				{ g }
	/ f:final_rafsi				{ f }
	/ yr:y_rafsi				{ yr }
	/ cc:CC y:y				{ cc ++ [y] }
	/ h:h y:y				{ [h, y] }
	/ fr:full_rafsi h:h y:y			{ fr ++ [h, y] }
 )							{ concat ylrs ++ b }

y_less_rafsi :: String
	= sr:short_rafsi &_:rafsi_string		{ sr }

classifier :: String
	= c1:C v:V c2:C cr:CR				{ c1 : v : c2 : cr }
	/ cc:CC v:V cr:CR				{ cc ++ v : cr }
	/ c:C v:V cr:CR					{ c : v : cr }
	/ yr:y_rafsi r:R				{ yr ++ [r] }

full_rafsi :: String
	= c1:C v1:V c2:C c3:C v2:V			{ [c1, v1, c2, c3, v2] }
	/ cc:CC v1:V c:C v2:V				{ cc ++ [v1, c, v2] }

y_rafsi :: String
	= c1:C v:V c2:C c3:C y:y			{ [c1, v, c2, c3, y] }
	/ cc:CC v:V c:C y:y				{ cc ++ [v, c, y] }
	/ c1:C v:V c2:C y:y				{ [c1, v, c2, y] }

short_rafsi :: String
	= c1:C v:V c2:C					{ [c1, v, c2] }
	/ cc:CC v:V					{ cc ++ [v] }
	/ c:C vv:VV r:R?				{ c : vv ++
								maybeToList r }
	/ c:C v1:V h:h v2:V r:R?			{ c : v1 : h : v2 :
								maybeToList r }

syllable :: String
	= o:onset !_:y n:nucleus c:coda?		{ o ++ n ++ maybeToList c }

coda :: Char
	= !_:onset c:C					{ c }

onset :: String = o:
	( h:h					{ [h] }
	/ c:C? i:I				{ maybeCons c [i] }
	/ a:affricate				{ a }
	/ s:sibilant? m:middle? l:liquid?	{ catMaybes [s, m, l] }
 ) &_:nucleus						{ o }

sibilant :: Char
	= c:c						{ c }
	/ s:s !_:x					{ s }
	/ j:j !_:n !_:l !_:r				{ j }
	/ z:z !_:n !_:l !_:r				{ z }

middle :: Char
	= p:p						{ p }
	/ b:b						{ b }
	/ f:f						{ f }
	/ v:v						{ v }
	/ m:m						{ m }
	/ t:t !_:l					{ t }
	/ d:d !_:l					{ d }
	/ n:n !_:l !_:r					{ n }
	/ k:k						{ k }
	/ g:g						{ g }
	/ x:x						{ x }

liquid :: Char
	= l:l						{ l }
	/ r:r						{ r }

CC :: String
	= &_:onset c1:C c2:C			{ [c1, c2] }

CR :: String
	= c:C r:R					{ [c, r] }
	/ r:r n:n &_:C					{ [r, n] }
	/ r:r l:l &_:n					{ [r, l] }
	/ r:r l:l &_:affricate				{ [r, l] }
	/ n:n l:l &_:r					{ [n, l] }

R :: Char
	= r:r &_:C					{ r }
	/ n:n &_:r					{ n }

C :: Char
	= v:voiced					{ v }
	/ u:unvoiced					{ u }
	/ l:l						{ l }
	/ m:m						{ m }
	/ n:n						{ n }
	/ r:r						{ r }

affricate :: String
	= t:t c:c					{ [t, c] }
	/ t:t s:s					{ [t, s] }
	/ d:d j:j					{ [d, j] }
	/ d:d z:z					{ [d, z] }

voiced :: Char
	= b:b						{ b }
	/ d:d						{ d }
	/ g:g						{ g }
	/ j:j						{ j }
	/ v:v						{ v }
	/ z:z						{ z }

unvoiced :: Char
	= c:c						{ c }
	/ f:f						{ f }
	/ k:k						{ k }
	/ p:p						{ p }
	/ s:s						{ s }
	/ t:t						{ t }
	/ x:x						{ x }

l :: Char = 'l' !_:h !_:l				{ 'l' }
m :: Char = 'm' !_:h !_:m !_:z				{ 'm' }
n :: Char = 'n' !_:h !_:n !_:affricate			{ 'n' }
r :: Char = 'r' !_:h !_:r				{ 'r' }

b :: Char = 'b' !_:h !_:b !_:unvoiced			{ 'b' }
d :: Char = 'd' !_:h !_:d !_:unvoiced			{ 'd' }
g :: Char = 'g' !_:h !_:g !_:unvoiced			{ 'g' }
v :: Char = 'v' !_:h !_:v !_:unvoiced			{ 'v' }
j :: Char = 'j' !_:h !_:j !_:z !_:unvoiced		{ 'j' }
z :: Char = 'z' !_:h !_:z !_:j !_:unvoiced		{ 'z' }

s :: Char = 's' !_:h !_:s !_:c !_:voiced		{ 's' }
c :: Char = 'c' !_:h !_:c !_:s !_:x !_:voiced		{ 'c' }
x :: Char = 'x' !_:h !_:x !_:c !_:k !_:voiced		{ 'x' }
k :: Char = 'k' !_:h !_:k !_:x !_:voiced		{ 'k' }
f :: Char = 'f' !_:h !_:f !_:voiced			{ 'f' }
p :: Char = 'p' !_:h !_:p !_:voiced 			{ 'p' }
t :: Char = 't' !_:h !_:t !_:voiced			{ 't' }
h :: Char = '\'' &_:nucleus				{ '\'' }
I :: Char = c:(i:i { i } / u:u { u }) &_:nucleus	{ c }

nucleus :: String
	= v:V						{ [v] }
	/ vv:VV						{ vv }
	/ y:y						{ [y] }

VV :: String = vv:
	( a:a i:i { [a, i] } / a:a u:u { [a, u] }
	/ e:e i:i { [e, i] } / o:o i:i { [o, i] }) !_:nucleus !_:I
							{ vv }
					

V :: Char = v:(a:a { a } / e:e { e } / i:i { i } / o:o { o } / u:u { u })
	!_:nucleus					{ v }

a :: Char = 'a'						{ 'a' }
e :: Char = 'e'						{ 'e' }
i :: Char = 'i'						{ 'i' }
o :: Char = 'o'						{ 'o' }
u :: Char = 'u'						{ 'u' }
y :: Char = 'y' !_:nucleus				{ 'y' }

Y :: String
	= ys:('y' { 'y' })+ !_:nucleus			{ ys }
	/ _:space					{ "" }

non_space :: Char
	= !_:space c					{ c }

space' :: ()
	= _:space / !_

space :: ()
	= c:[c `elem` ".\t\n\r?!\x0020"]

|]
