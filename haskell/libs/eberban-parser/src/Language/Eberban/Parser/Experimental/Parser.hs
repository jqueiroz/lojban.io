{-# LANGUAGE TypeFamilies, QuasiQuotes, PatternGuards #-}

module Language.Eberban.Parser.Experimental.Parser where

-- Reference grammar: https://github.com/eberban/eberban/blob/master/node/grammar/eberban.peg
-- TODO: ParserConfig controlling for example whether to coalesce consecutive identical characters, whether to convert everything to lowercase, whether to treat all hyphens as the same, etc (canonicalHyphen :: Maybe Char)
-- TODO: unit tests at different layers (e.g. particles, consonant pairs, individual letters, etc)
-- TODO: handle EOF

import Prelude hiding (Word)
import Text.Papillon
import Data.Maybe

type Result = NativeWord

parse :: String -> Either String Result
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
data NativeWord
    = Root String
    | Particle String
    deriving (Show)

-- Definition of the grammar
[papillon|

prefix: "gerna_"

--textAll :: String = i:initial_pair { i } / _:eof { "empty input" }
textAll :: Result = native_word:native_word { native_word }

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
do  :: String = &_:particle                                           x:(d:d o:o { concat [d, o] })                                     &_:post_word        { x }
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
--ti  :: String = &_:particle                                           x:(t:t hieaou:hieaou { concat [s, hieaou] })                      &_:post_word        { x } -- TODO: is the definition of ti correct in the reference grammar?

ve  :: String = &_:particle         !_:(vei:vei &_:post_word)         x:(v:v hieaou:hieaou { concat [v, hieaou] })                      &_:post_word        { x }
vei :: String = &_:particle                                           x:(v:v e:e i:i { concat [v, e, i] })                              &_:post_word        { x }

zi  :: String = &_:particle                                           x:(z:z hieaou:hieaou { concat [z, hieaou] })                      &_:post_word        { x }

-- TODO: Foreign text quoting

-- TODO: Compounds

-- TODO: Free-form words

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
