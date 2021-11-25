{-# LANGUAGE TypeFamilies, QuasiQuotes, PatternGuards #-}

module Language.Eberban.Parser.Experimental.Parser where

-- Reference grammar: https://github.com/eberban/eberban/blob/master/node/grammar/eberban.peg
-- TODO: ParserConfig controlling for example whether to coalesce consecutive identical characters, whether to convert everything to lowercase, whether to treat all hyphens as the same, etc (canonicalHyphen :: Maybe Char)
-- TODO: unit tests at different layers (e.g. particles, consonant pairs, individual letters, etc)
-- TODO: handle EOF

import Prelude hiding (Word)
import Text.Papillon
import Data.Maybe

type Result = Predicate

parse :: String -> Either String Result
parse src
    | Right (r, _) <- parsed = Right r
    | Left l <- parsed = Left $ showParseError l
    where
        parsed = runError $ gerna_result $ gerna_parse src

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

-- Constants
alphaChars :: [Char]
alphaChars= "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

digitChars :: [Char]
digitChars= "0123456789"

hyphenChars :: [Char]
hyphenChars = ['\x2010', '\x2014', '\x002D']

linebreakChars :: [Char]
linebreakChars= ['\r', '\b']

pauseChars :: [Char]
pauseChars= ['’']

-- Data types
data EberbanText = EberbanText -- the entire text (pending)
    deriving (Show)

data TransformedPredicateWithFree = TransformedPredicateWithFree -- pending
    deriving (Show)

data TransformedPredicate = TransformedPredicate -- pending
    deriving (Show)

data Scope = Scope -- pending
    deriving (Show)

data ChainingItem
    = ChainingNegation BI [ChainingItem]
    | ChainingPredicate Predicate [VeScope]

data VeScope = VeScope VeScopeFirst [VeScopeNext] (Maybe VEI)
    deriving (Show)

data VeScopeFirst = VeScopeFirst (Maybe BI) VE Scope
    deriving (Show)

data VeScopeNext = VeScopeNext (Maybe BI) FE Scope
    deriving (Show)

-- maybe rename to SimplePredicate?
data Predicate
    = PredicateRoot String
    | PredicateCompound Compound
    | PredicateBorrowing [String] (Maybe BE)
    | PredicateQuote Quote
    | PredicateNumber Number
    | PredicateBa BA
    | PredicateMi MI
    | PredicateVariable (Maybe BO) Variable
    deriving (Show)

data Number = Number [TI] (Maybe BE)
    deriving (Show)

newtype ForeignQuoteDelimiter = ForeignQuoteDelimiter NativeWord
    deriving (Show)

newtype ForeignQuoteContent = ForeignQuoteContent String
    deriving (Show)

data Quote
    = GrammaticalQuote CA EberbanText CAI
    | OneWordQuote CE Word
    | OneCompoundQuote CE Compound
    | ForeignQuote CO ForeignQuoteDelimiter ForeignQuoteContent
    deriving (Show)

data Variable
    = VariableKi KI
    | VariableGi GI
    | VariableFreeform String
    deriving (Show)

data Override = Override DU TransformedPredicate
    deriving (Show)

data Free
    = FreeMetadata DA
    | FreeSubscript DI Number
    | FreeParenthetical DO EberbanText DOI
    | FreeInterjection DE TransformedPredicate
    deriving (Show)

data Compound = Compound [Word]
    deriving (Show)

data Word
    = Borrowing String
    | Native NativeWord
    deriving (Show)

data NativeWord
    = Root String
    | Particle String
    deriving (Show)

data BA = BA String
    deriving (Show)
data BE = BE String
    deriving (Show)
data BI = BI String [Free]
    deriving (Show)
data BO = BO String
    deriving (Show)
data BU = BU String
    deriving (Show)

data DA = DA String
    deriving (Show)
data DE = DE String
    deriving (Show)
data DI = DI String
    deriving (Show)
data DO = DO String
    deriving (Show)
data DOI = DOI String
    deriving (Show)
data DU = DU String
    deriving (Show)

data SE = SE String (Maybe Override)
    deriving (Show)
data ZI = ZI String (Maybe Override)
    deriving (Show)
data VE = VE String (Maybe Override) [Free]
    deriving (Show)
data FE = FE String (Maybe Override) [Free]
    deriving (Show)
data VEI = VEI String
    deriving (Show)

data GI = GI String
    deriving (Show)
data KI = KI String
    deriving (Show)
data MI = MI String
    deriving (Show)

data PA = PA String [Free]
    deriving (Show)
data PE = PE String [Free]
    deriving (Show)
data PEI = PEI String
    deriving (Show)
data PI = PI String
    deriving (Show)
data PO = PO String [Free]
    deriving (Show)
data PU = PU String [Free]
    deriving (Show)

data TI = TI String
    deriving (Show)

data CA = CA String
    deriving (Show)
data CAI = CAI String
    deriving (Show)
data CE = CE String
    deriving (Show)
data CO = CO String
    deriving (Show)
data CU = CU String
    deriving (Show)

-- Definition of the grammar
[papillon|
prefix: "gerna_"

-- Define the final result
--result :: Result = eberban_text
--result :: Result = i:initial_pair { i } / _:eof { "empty input" }
result :: Result = predicate:predicate { predicate }

-- Overall text
eberban_text :: EberbanText = !_ { EberbanText } -- not yet implemented

-- Text structure (TODO)

-- Scope (TODO)
scope :: Scope = !_ { Scope } -- not yet implemented

-- Chaining and explicit switches
chaining :: [ChainingItem] = x:chaining_item+ {x}
chaining_item :: ChainingItem = chaining_negation:chaining_negation{chaining_negation} / chaining_predicate:chaining_predicate{chaining_predicate}
chaining_negation :: ChainingItem = bi_clause:bi_clause chaining:chaining { ChainingNegation bi_clause chaining }
-- "chaining_predicate" was originally named "chaining_unit"
chaining_predicate :: ChainingItem = predicate:predicate ve_scope:ve_scope* { ChainingPredicate predicate ve_scope }

ve_scope :: VeScope = ve_scope_first:ve_scope_first ve_scope_next:ve_scope_next* vei_clause_elidible:vei_clause_elidible { VeScope ve_scope_first ve_scope_next vei_clause_elidible }
ve_scope_first :: VeScopeFirst = bi_clause:bi_clause? ve_clause:ve_clause scope:scope { VeScopeFirst bi_clause ve_clause scope }
ve_scope_next :: VeScopeNext = bi_clause:bi_clause? fe_clause:fe_clause scope:scope { VeScopeNext bi_clause fe_clause scope }

-- Predicate unit (TODO)
-- "transformed_predicate_with_free" originally named "predicate"
transformed_predicate_with_free :: TransformedPredicateWithFree = !_ { TransformedPredicateWithFree }

-- "transformed_predicate" was originally named "predicate_1"
transformed_predicate :: TransformedPredicate = !_ { TransformedPredicate }

-- "predicate" was originally named "predicate_2" (pending: predicate_scope)
predicate :: Predicate = x:(predicate_ba:predicate_ba{predicate_ba} / predicate_mi:predicate_mi{predicate_mi} / predicate_quote:predicate_quote{predicate_quote} / predicate_variable:predicate_variable{predicate_variable} / predicate_borrowing:predicate_borrowing{predicate_borrowing} / predicate_root:predicate_root{predicate_root} / predicate_number:predicate_number {predicate_number} / predicate_compound:predicate_compound{predicate_compound}) {x}
predicate_ba :: Predicate = ba_clause:ba_clause { PredicateBa ba_clause }
predicate_mi :: Predicate = mi_clause:mi_clause { PredicateMi mi_clause }
predicate_quote :: Predicate = quote:quote { PredicateQuote quote }
predicate_variable :: Predicate = bo_clause:bo_clause? variable:variable { PredicateVariable bo_clause variable }
predicate_borrowing :: Predicate = x:(_:spaces? borrowing:borrowing {borrowing})+ y:be_clause_elidible { PredicateBorrowing x y }
predicate_root :: Predicate = _:spaces? root:root { PredicateRoot root }
predicate_number :: Predicate = _:spaces? number:number { PredicateNumber number }
predicate_compound :: Predicate = _:spaces? compound:compound { PredicateCompound compound }

-- Quotes
quote :: Quote = grammatical_quote:grammatical_quote{grammatical_quote} / one_word_quote:one_word_quote{one_word_quote} / one_compound_quote:one_compound_quote{one_compound_quote} / foreign_quote:foreign_quote{foreign_quote}
grammatical_quote :: Quote = ca_clause:ca_clause eberban_text:eberban_text cai_clause:cai_clause { GrammaticalQuote ca_clause eberban_text cai_clause }
one_word_quote :: Quote = ce_clause:ce_clause _:spaces? word:(native_word:native_word{Native native_word} / borrowing:borrowing{Borrowing borrowing}) { OneWordQuote ce_clause word }
one_compound_quote :: Quote = ce_clause:ce_clause _:spaces? compound:compound { OneCompoundQuote ce_clause compound }
-- Future: consider consuming _:spaces? (or at least _:space_char) before "pause_char foreign_quote_close" to get rid of undesired (I think) spaces?
foreign_quote :: Quote = co_clause:co_clause _:spaces? foreign_quote_open:foreign_quote_open _:space_char foreign_quote_content:foreign_quote_content _:pause_char foreign_quote_close:foreign_quote_close { ForeignQuote co_clause (ForeignQuoteDelimiter foreign_quote_open) (ForeignQuoteContent foreign_quote_content)} -- TOCONFIRM: is this really correct? foreign_quote_open is not necessarily equal to foreign_quote_close
foreign_quote_content :: String = x:(!_:(_:pause_char _:foreign_quote_close) c {c})* { x }

-- Numbers
number :: Number = x:ti_clause+ y:be_clause_elidible { Number x y }

-- Variables
variable :: Variable = ki_clause:ki_clause {VariableKi ki_clause} / gi_clause:gi_clause {VariableGi gi_clause} / _:spaces? freeform_variable:freeform_variable {VariableFreeform freeform_variable}

-- Free afixes
free :: Free = x:(free_metadata:free_metadata{free_metadata} / free_subscript:free_subscript{free_subscript} / free_parenthetical:free_parenthetical{free_parenthetical} / free_interjection:free_interjection{free_interjection}) {x}
free_metadata :: Free = da_clause:da_clause { FreeMetadata da_clause }
free_subscript :: Free = di_clause:di_clause number:number { FreeSubscript di_clause number }
free_parenthetical :: Free = do_clause:do_clause eberban_text:eberban_text doi_clause:doi_clause { FreeParenthetical do_clause eberban_text doi_clause }
free_interjection :: Free = de_clause:de_clause transformed_predicate:transformed_predicate { FreeInterjection de_clause transformed_predicate }

override :: Override = du_clause:du_clause transformed_predicate:transformed_predicate { Override du_clause transformed_predicate }

-- Particle clauses
ba_clause :: BA = _:spaces? ba:ba { BA ba }
be_clause :: BE = _:spaces? be:be { BE be }
bi_clause :: BI = _:spaces? bi:bi free:free* { BI bi free }
bo_clause :: BO = _:spaces? bo:bo { BO bo }
bu_clause :: BU = _:spaces? bu:bu { BU bu }

da_clause :: DA = _:spaces? da:da { DA da }
de_clause :: DE = _:spaces? de:de { DE de }
di_clause :: DI = _:spaces? di:di { DI di }
do_clause :: DO = _:spaces? do_:do_ { DO do_ }
doi_clause :: DOI = _:spaces? doi:doi { DOI doi }
du_clause :: DU = _:spaces? du:du { DU du }

se_clause :: SE = _:spaces? se:se override:override? { SE se override }
zi_clause :: ZI = _:spaces? zi:zi override:override? { ZI zi override }
ve_clause :: VE = _:spaces? ve:ve override:override? free:free* { VE ve override free }
fe_clause :: FE = _:spaces? fe:fe override:override? free:free* { FE fe override free }
vei_clause :: VEI = _:spaces? vei:vei { VEI vei }

gi_clause :: GI = _:spaces? gi:gi { GI gi }
ki_clause :: KI = _:spaces? ki:ki { KI ki }
mi_clause :: MI = _:spaces? mi:mi { MI mi }

pa_clause :: PA = _:spaces? pa:pa free:free* { PA pa free }
pe_clause :: PE = _:spaces? pe:pe free:free* { PE pe free }
pei_clause :: PEI = _:spaces? pei:pei { PEI pei }
pi_clause :: PI = _:spaces? pi:pi { PI pi }
po_clause :: PO = _:spaces? po:po free:free* { PO po free }
pu_clause :: PU = _:spaces? pu:pu free:free* { PU pu free }

ti_clause :: TI = _:spaces? ti:ti { TI ti }

ca_clause :: CA = _:spaces? ca:ca { CA ca }
cai_clause :: CAI = _:spaces? cai:cai { CAI cai }
ce_clause :: CE = _:spaces? ce:ce { CE ce }
co_clause :: CO = _:spaces? co:co { CO co }
cu_clause :: CU = _:spaces? cu:cu { CU cu }

be_clause_elidible :: Maybe BE = be_clause:be_clause? { be_clause }
pa_clause_elidible :: Maybe PA = pa_clause:pa_clause? { pa_clause }
pei_clause_elidible :: Maybe PEI = pei_clause:pei_clause? { pei_clause }
vei_clause_elidible :: Maybe VEI = vei_clause:vei_clause? { vei_clause }

-- Particle families
ba  :: String = &_:particle                                           x:(b:b a:a { concat [b, a] })                                     &_:post_word        { x }
be  :: String = &_:particle                                           x:(b:b &_:e hieaou:hieaou { concat [b, hieaou] })                 &_:post_word        { x }
bi  :: String = &_:particle                                           x:(b:b i:i { concat [b, i] })                                     &_:post_word        { x }
bo  :: String = &_:particle                                           x:(b:b o:o { concat [b, o] })                                     &_:post_word        { x }
bu  :: String = &_:particle                                           x:(b:b &_:u hieaou:hieaou { concat [b, hieaou] })                 &_:post_word        { x }

ca  :: String = &_:particle         !_:(cai:cai &_:post_word)         x:(c:c &_:a hieaou:hieaou { concat [c, hieaou] })                 &_:post_word        { x }
cai :: String = &_:particle                                           x:(c:c a:a i:i { concat [c, a, i] })                              &_:post_word        { x }
ce  :: String = &_:particle                                           x:(c:c &_:e hieaou:hieaou { concat [c, hieaou] })                 &_:post_word        { x }
co  :: String = &_:particle                                           x:(c:c &_:o hieaou:hieaou { concat [c, hieaou] })                 &_:post_word        { x }
cu  :: String = &_:particle                                           x:(c:c u:u { concat [c, u] })                                     &_:post_word        { x }

da  :: String = &_:particle                                           x:(d:d &_:a hieaou:hieaou { concat [d, hieaou] })                 &_:post_word        { x }
de  :: String = &_:particle                                           x:(d:d &_:e hieaou:hieaou { concat [d, hieaou] })                 &_:post_word        { x }
di  :: String = &_:particle                                           x:(d:d i:i { concat [d, i] })                                     &_:post_word        { x }
do_ :: String = &_:particle                                           x:(d:d o:o { concat [d, o] })                                     &_:post_word        { x }
doi :: String = &_:particle                                           x:(d:d o:o i:i { concat [d, o, i] })                              &_:post_word        { x }
du  :: String = &_:particle                                           x:(d:d u:u { concat [d, u] })                                     &_:post_word        { x }

fe  :: String = &_:particle                                           x:(f:f hieaou:hieaou { concat [f, hieaou] })                      &_:post_word        { x }
gi  :: String = &_:particle                                           x:(g:g hieaou:hieaou { concat [g, hieaou] })                      &_:post_word        { x }
ki  :: String = &_:particle                                           x:(k:k hieaou:hieaou { concat [k, hieaou] })                      &_:post_word        { x }
mi  :: String = &_:particle                                           x:(m:m hieaou:hieaou { concat [m, hieaou] })                      &_:post_word        { x }

pa  :: String = &_:particle                                           x:(p:p &_:a hieaou:hieaou { concat [p, hieaou] })                 &_:post_word        { x }
pe  :: String = &_:particle                                           x:(p:p e:e { concat [p, e] })                                     &_:post_word        { x }
pei :: String = &_:particle                                           x:(p:p e:e i:i { concat [p, e, i] })                              &_:post_word        { x }
pi  :: String = &_:particle                                           x:(p:p &_:i hieaou:hieaou { concat [p, hieaou] })                 &_:post_word        { x }
po  :: String = &_:particle                                           x:(p:p &_:o hieaou:hieaou { concat [p, hieaou] })                 &_:post_word        { x }
pu  :: String = &_:particle                                           x:(p:p &_:u hieaou:hieaou { concat [p, hieaou] })                 &_:post_word        { x }

se  :: String = &_:particle                                           x:(s:s hieaou:hieaou { concat [s, hieaou] })                      &_:post_word        { x }

ve  :: String = &_:particle         !_:(vei:vei &_:post_word)         x:(v:v hieaou:hieaou { concat [v, hieaou] })                      &_:post_word        { x }
vei :: String = &_:particle                                           x:(v:v e:e i:i { concat [v, e, i] })                              &_:post_word        { x }

zi  :: String = &_:particle                                           x:(z:z hieaou:hieaou { concat [z, hieaou] })                      &_:post_word        { x }

 -- TOCONFIRM: is the definition of ti correct?
ti :: String = ti_v1:ti_v1{ti_v1} / ti_v2:ti_v2{ti_v2}
ti_v1 :: String = &_:particle x:(t:t hieaou:hieaou { concat [t, hieaou] }) &_:post_word { x }
ti_v2 :: String = digit_char:digit_char &_:post_word { [digit_char] } -- TOCONFIRM: specifically double-check if ti_v2 is correct

-- Foreign text quoting
foreign_quote_open :: NativeWord = native_word:native_word {native_word}
foreign_quote_word :: String = x:(!_:pause_char c {c})+ { x }
foreign_quote_close :: NativeWord = native_word:native_word {native_word}

-- Compounds
compound :: Compound = x:(compound_2:compound_2{compound_2} / compound_3:compound_3{compound_3} / compound_n:compound_n{compound_n}) {x}
compound_2 :: Compound = e:e x:compound_word y:compound_word { Compound [x, y] }
compound_3 :: Compound = a:a x:compound_word y:compound_word z:compound_word { Compound [x, y, z] }
compound_n :: Compound = o:o x:(!_:compound_n_end compound_word:compound_word {compound_word})+ compound_n_end { Compound x }
compound_n_end :: String = _:spaces o:o _:spaces { o }
compound_word :: Word = _:spaces? x:(borrowing:borrowing{Borrowing borrowing} / native_word:native_word{Native native_word}) {x}

-- Free-form words
freeform_variable :: String = i:i x:(spaces:spaces &_:i {spaces} / hyphen:hyphen !_:i {hyphen}) freeform_content:freeform_content freeform_end:freeform_end { concat [i, x, freeform_content, freeform_end] }
borrowing :: String = u:u x:(spaces:spaces &_:u {spaces} / hyphen:hyphen !_:u {hyphen}) freeform_content:freeform_content freeform_end:freeform_end { concat [u, x, freeform_content, freeform_end] }
freeform_content :: String = x:(ip:initial_pair{ip} / consonant:consonant{consonant} / h:h{h})? hieaou:hieaou y:(consonant_cluster:consonant_cluster hieaou:hieaou {concat [consonant_cluster, hieaou]})* z:sonorant? { concat [fromMaybe [] x, hieaou, concat y, fromMaybe [] z] }
freeform_end :: String = pause_char:pause_char{[pause_char]} / space_char:space_char{[space_char]} / _:eof{""}

-- Native words
native_word :: NativeWord = x:(root:root {Root root} / particle:particle {Particle particle}) {x}

particle :: String = !_:sonorant particle_1:particle_1 &_:post_word { particle_1 }
particle_1 :: String = consonant:consonant hieaou:hieaou !_:medial_pair { concat [consonant, hieaou ] }

root :: String = !_:sonorant x:(r1:root_1{r1} / r2:root_2{r2} / r3:root_3{r3}) &_:post_word { x }
root_1 :: String = consonant:consonant hieaou:hieaou x:root_loop+ y:sonorant? { concat [consonant, hieaou, concat x, fromMaybe [] y] }
root_2 :: String = consonant:consonant hieaou:hieaou sonorant:sonorant { concat [consonant, hieaou, sonorant] }
root_3 :: String = ip:initial_pair hieaou:hieaou x:root_loop* y:sonorant? { concat [ip, hieaou, concat x, fromMaybe [] y] }
root_loop :: String = x:(medial_pair:medial_pair{medial_pair} / hyphen:hyphen sonorant:sonorant { concat [hyphen, sonorant] }) hieaou:hieaou { concat [x, hieaou] }

-- Legal clusters
hieaou :: String = ieaou:ieaou x:(hyphen:hyphen h:h ieaou:ieaou { concat [hyphen, h, ieaou ] })* { concat [ieaou, concat x] }
ieaou :: String = vowel:vowel x:(hyphen:hyphen vowel:vowel { concat [hyphen, vowel] })* { concat [vowel, concat x] }

consonant_cluster :: String = x:(consonant_cluster1:consonant_cluster1{consonant_cluster1} / consonant_cluster2:consonant_cluster2{consonant_cluster2} / consonant_cluster3:consonant_cluster3{consonant_cluster3} / consonant_cluster4:consonant_cluster4{consonant_cluster4}) !_:consonant { x }
consonant_cluster1 :: String = &_:medial_pair !_:sonorant consonant:consonant hyphen:hyphen ip:initial_pair { concat [consonant, hyphen, ip] }
consonant_cluster2 :: String = medial_pair:medial_pair {medial_pair}
consonant_cluster3 :: String = x:sonorant? hyphen:hyphen y:(ip:initial_pair{ip} / !_:sonorant consonant:consonant{consonant}) { concat [fromMaybe [] x, hyphen, y] }
consonant_cluster4 :: String = sonorant:sonorant hyphen:hyphen { concat [sonorant, hyphen] }

medial_pair :: String = !_:initial medial_patterns:medial_patterns {medial_patterns}
medial_patterns :: String = x:(medial_n1:medial_n1{medial_n1} / medial_n2:medial_n2{medial_n2} / medial_fv:medial_fv{medial_fv} / medial_plosive:medial_plosive{medial_plosive}) {x}
medial_n1 :: String = x:(m:m{m} / liquid:liquid{liquid}) hyphen:hyphen n:n { concat [x, hyphen, n] }
medial_n2 :: String = n:n hyphen:hyphen liquid:liquid { concat [n, hyphen, liquid] }
medial_fv :: String = x:(f:f{f} / v:v{v}) hyphen:hyphen y:(plosive:plosive{plosive} / sibilant:sibilant{sibilant} / m:m{m}) { concat [x, hyphen, y] }
medial_plosive :: String = plosive:plosive hyphen:hyphen x:(f:f{f} / v:v{v} / plosive:plosive{plosive} / m:m{m}) { concat [plosive, hyphen, x] }

initial_pair :: String = &_:initial c1:consonant c2:consonant !_:consonant { concat [c1, c2] }

initial :: String =
    plosive_or_f_or_v:(plosive:plosive {plosive} / f:f {f} / v:v {v}) hyphen:hyphen sibilant:sibilant { concat [plosive_or_f_or_v, hyphen, sibilant] } /
    sibilant:sibilant hyphen:hyphen other:other { concat [sibilant, hyphen, other] } /
    sibilant:sibilant hyphen:hyphen sonorant:sonorant { concat [sibilant, hyphen, sonorant] } /
    other:other hyphen:hyphen sonorant:sonorant { concat [other, hyphen, sonorant] }

other :: String =
    p:p !_:n {p} /
    b:b !_:n {b} /
    t:t !_:n !_:l {t} /
    d:d !_:n !_:l {d} /
    v:v {v} /
    f:f {f} /
    k:k {k} /
    g:g {g} /
    m:m {m} /
    n:n !_:liquid {n}

plosive :: String =
    t:t {t} /
    d:d {d} /
    k:k {k} /
    g:g {g} /
    p:p {p} /
    b:b {b}

sibilant :: String =
    c:c {c} /
    s:s {s} /
    j:j {j} /
    z:z {z}

sonorant :: String =
    n:n {n} /
    r:r {r} /
    l:l {l}

consonant :: String =
    voiced:voiced {voiced} /
    unvoiced:unvoiced {unvoiced} /
    liquid:liquid {liquid} /
    nasal:nasal {nasal}

nasal :: String =
    m:m {m} /
    n:n {n}

liquid :: String =
    l:l {l} /
    r:r {r}

voiced :: String =
    b:b {b} /
    d:d {d} /
    g:g {g} /
    v:v {v} /
    z:z {z} /
    j:j {j}

unvoiced :: String =
    p:p {p} /
    t:t {t} /
    k:k {k} /
    f:f {f} /
    s:s {s} /
    c:c {c}

vowel :: String =
    i:i {i} /
    e:e {e} /
    a:a {a} /
    o:o {o} /
    u:u {u}

-- Legal Letters
i :: String = is:('i' {'i'} / 'I' {'I'})+ { is }
e :: String = es:('e' {'e'} / 'E' {'E'})+ { es }
a :: String = as:('a' {'a'} / 'A' {'A'})+ { as }
o :: String = os:('o' {'o'} / 'O' {'O'})+ { os }
u :: String = us:('u' {'u'} / 'U' {'U'})+ { us }

h :: String = hs:('h' {'h'} / 'H' {'H'})+ { hs }
n :: String = ns:('n' {'n'} / 'N' {'N'})+ { ns }
r :: String = rs:('r' {'r'} / 'R' {'R'})+ { rs }
l :: String = ls:('l' {'l'} / 'L' {'L'})+ { ls }

m :: String = ms:('m' {'m'} / 'M' {'M'})+ { ms }
p :: String = ps:('p' {'p'} / 'P' {'P'})+ !_:voiced { ps }
b :: String = bs:('b' {'b'} / 'B' {'B'})+ !_:unvoiced { bs }
f :: String = fs:('f' {'f'} / 'F' {'F'})+ !_:voiced { fs }
v :: String = vs:('v' {'v'} / 'V' {'V'})+ !_:unvoiced { vs }
t :: String = ts:('t' {'t'} / 'T' {'T'})+ !_:voiced { ts }
d :: String = ds:('d' {'d'} / 'D' {'D'})+ !_:unvoiced { ds }

s :: String = ss:('s' {'s'} / 'S' {'S'})+ !_:c !_:voiced { ss }
z :: String = zs:('z' {'z'} / 'Z' {'Z'})+ !_:j !_:unvoiced { zs }
c :: String = cs:('c' {'c'} / 'C' {'C'})+ !_:s !_:voiced { cs }
j :: String = js:('j' {'j'} / 'J' {'J'})+ !_:z !_:unvoiced { js }

g :: String = gs:('g' {'g'} / 'G' {'G'})+ !_:unvoiced { gs }
k :: String = ks:('k' {'k'} / 'K' {'K'})+ !_:voiced { ks }

-- Spaces / pauses
post_word :: String = x:(pause_char:pause_char &_:vowel { [pause_char] } / !_:sonorant &_:consonant { "" } / spaces:spaces {spaces}) {x}

spaces :: String = x:(spaces_1:spaces_1{spaces_1} / spaces_2:spaces_2{spaces_2} / spaces_3:spaces_3{spaces_3}) {x}
spaces_1 :: String = x:space_char+ y:hesitation? z:spaces_2? { concat [x, fromMaybe [] y, fromMaybe [] z] }
spaces_2 :: String = pause_char:pause_char &_:vowel { [pause_char] }
spaces_3 :: String = _:eof { "" }

hesitation :: String = x:hesitation_loop+ { concat x }
hesitation_loop :: String = n:n x:(y:space_char+ { y } / _:eof { "" }) { concat [n, x] }

-- Special characters
hyphen :: String = h:hyphen_required? {fromMaybe [] h}
hyphen_required :: String = h:hyphen_char lbs:(lb:linebreak_char {lb})* { h : lbs }

pause_char :: Char = p:[p `elem` pauseChars] !_:pause_char { p }
space_char :: Char = !_:(pause_char:pause_char{pause_char} / digit_char:digit_char{digit_char} / hyphen_char:hyphen_char{hyphen_char} / alpha_char:alpha_char{alpha_char}) c {c}

alpha_char :: Char = a:[a `elem` alphaChars] { a }
digit_char :: Char = d:[d `elem` digitChars] { d }
hyphen_char :: Char = h:[h `elem` hyphenChars] { h }
linebreak_char :: Char = lb:[lb `elem` linebreakChars] { lb }

eof :: () = !_ { () }

|]
