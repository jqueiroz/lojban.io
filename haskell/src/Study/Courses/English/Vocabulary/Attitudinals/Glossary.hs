{-# LANGUAGE OverloadedStrings #-}

-- | This module establishes a glossary of attitudinals and attitudinal modifiers.
--
-- The definitions have been sourced from <https://en.wikibooks.org/wiki/Lojban/Attitudinals>.
module Study.Courses.English.Vocabulary.Attitudinals.Glossary where

import Study.Courses.English.Vocabulary.Attitudinals.Model

-- * Simple propositional emotions
-- | Attitudinal: .a'a
a'a :: Attitudinal
a'a = Attitudinal ".a'a" PropositionalEmotion ("attentive") (Just "inattentive") (Just "avoiding")

-- | Attitudinal: .a'e
a'e :: Attitudinal
a'e = Attitudinal ".a'e" PropositionalEmotion ("alertness") (Nothing) (Just "exhaustion")

-- | Attitudinal: .ai
ai :: Attitudinal
ai = Attitudinal ".ai" PropositionalEmotion ("intent") (Just "indecision") (Just "refusal")

-- | Attitudinal: .a'i
a'i :: Attitudinal
a'i = Attitudinal ".a'i" PropositionalEmotion ("effort") (Just "lack of effort") (Just "repose")

-- | Attitudinal: .a'o
a'o :: Attitudinal
a'o = Attitudinal ".a'o" PropositionalEmotion ("hope") (Nothing) (Just "despair")

-- | Attitudinal: .au
au :: Attitudinal
au = Attitudinal ".au" PropositionalEmotion ("desire") (Just "indifference") (Just "reluctance")

-- | Attitudinal: .a'u
a'u :: Attitudinal
a'u = Attitudinal ".a'u" PropositionalEmotion ("interest") (Just "no interest") (Just "repulsion")

-- * Complex propositional emotions
-- | Attitudinal: .e'a
e'a :: Attitudinal
e'a = Attitudinal ".e'a" PropositionalEmotion ("permission") (Nothing) (Just "prohibition")

-- | Attitudinal: .e'e
e'e :: Attitudinal
e'e = Attitudinal ".e'e" PropositionalEmotion ("competence") (Nothing) (Just "incompetence")

-- | Attitudinal: .ei
ei :: Attitudinal
ei = Attitudinal ".ei" PropositionalEmotion ("obligation") (Nothing) (Just "freedom")

-- | Attitudinal: .e'i
e'i :: Attitudinal
e'i = Attitudinal ".e'i" PropositionalEmotion ("constraint") (Just "independence") (Just "resistance to constraint")

-- | Attitudinal: .e'o
e'o :: Attitudinal
e'o = Attitudinal ".e'o" PropositionalEmotion ("request") (Nothing) (Just "negative request")

-- | Attitudinal: .e'u
e'u :: Attitudinal
e'u = Attitudinal ".e'u" PropositionalEmotion ("suggestion") (Just "no suggestion") (Just "warning")

-- | Attitudinal: .ia
ia :: Attitudinal
ia = Attitudinal ".ia" PropositionalEmotion ("belief") (Just "skepticism") (Just "disbelief")

-- | Attitudinal: .i'a
i'a :: Attitudinal
i'a = Attitudinal ".i'a" PropositionalEmotion ("acceptance") (Nothing) (Just "blame")

-- | Attitudinal: .ie
ie :: Attitudinal
ie = Attitudinal ".ie" PropositionalEmotion ("agreement") (Just "neutrality") (Just "disagreement")

-- | Attitudinal: .i'e
i'e :: Attitudinal
i'e = Attitudinal ".i'e" PropositionalEmotion ("approval") (Just "non-approval") (Just "disapproval")

-- * Miscellaneous pure emotions
-- | Attitudinal: .ii
ii :: Attitudinal
ii = Attitudinal ".ii" PureEmotion ("fear") (Just "nervousness") (Just "security")

-- | Attitudinal: .i'i
i'i :: Attitudinal
i'i = Attitudinal ".i'i" PureEmotion ("togetherness") (Nothing) (Just "privacy")

-- | Attitudinal: .io
io :: Attitudinal
io = Attitudinal ".io" PureEmotion ("respect") (Just "lack of respect") (Just "disrespect")

-- | Attitudinal: .i'o
i'o :: Attitudinal
i'o = Attitudinal ".i'o" PureEmotion ("appreciation") (Just "lack of appreciation") (Just "envy")

-- | Attitudinal: .iu
iu :: Attitudinal
iu = Attitudinal ".iu" PureEmotion ("love") (Just "lack of love") (Just "hatred/fear")

-- | Attitudinal: .i'u
i'u :: Attitudinal
i'u = Attitudinal ".i'u" PureEmotion ("familiarity") (Nothing) (Just "mystery")

-- * Complex pure emotions
-- | Attitudinal: .o'a
o'a :: Attitudinal
o'a = Attitudinal ".o'a" PureEmotion ("pride") (Just "modesty") (Just "shame")

-- | Attitudinal: .o'e
o'e :: Attitudinal
o'e = Attitudinal ".o'e" PureEmotion ("closeness") (Just "detachment") (Just "distance")

-- | Attitudinal: .oi
oi :: Attitudinal
oi = Attitudinal ".oi" PureEmotion ("complaint/pain") (Just "no complaints") (Just "pleasure")

-- | Attitudinal: .o'i
o'i :: Attitudinal
o'i = Attitudinal ".o'i" PureEmotion ("caution") (Just "boldness") (Just "rashness")

-- | Attitudinal: .o'o
o'o :: Attitudinal
o'o = Attitudinal ".o'o" PureEmotion ("patience") (Just "mere tolerance") (Just "anger")

-- | Attitudinal: .o'u
o'u :: Attitudinal
o'u = Attitudinal ".o'u" PureEmotion ("relaxation") (Just "composture") (Just "stress")

-- * Simple pure emotions
-- | Attitudinal: .ua
ua :: Attitudinal
ua = Attitudinal ".ua" PureEmotion ("discovery") (Nothing) (Just "confusion")

-- | Attitudinal: .u'a
u'a :: Attitudinal
u'a = Attitudinal ".u'a" PureEmotion ("gain") (Nothing) (Just "loss")

-- | Attitudinal: .ue
ue :: Attitudinal
ue = Attitudinal ".ue" PureEmotion ("surprise") (Just "no surprise") (Just "expectation")

-- | Attitudinal: .u'e
u'e :: Attitudinal
u'e = Attitudinal ".u'e" PureEmotion ("wonder") (Nothing) (Just "commonplace")

-- | Attitudinal: .ui
ui :: Attitudinal
ui = Attitudinal ".ui" PureEmotion ("happiness") (Nothing) (Just "unhappiness")

-- | Attitudinal: .u'i
u'i :: Attitudinal
u'i = Attitudinal ".u'i" PureEmotion ("amusement") (Nothing) (Just "weariness")

-- | Attitudinal: .uo
uo :: Attitudinal
uo = Attitudinal ".uo" PureEmotion ("completion") (Nothing) (Just "incompleteness")

-- | Attitudinal: .u'o
u'o :: Attitudinal
u'o = Attitudinal ".u'o" PureEmotion ("courage") (Just "timidity") (Just "cowardice")

-- | Attitudinal: .uu
uu :: Attitudinal
uu = Attitudinal ".uu" PureEmotion ("pity") (Nothing) (Just "cruelty")

-- | Attitudinal: .u'u
u'u :: Attitudinal
u'u = Attitudinal ".u'u" PureEmotion ("repentance") (Just "lack of regret") (Just "innocence")
