{-# LANGUAGE OverloadedStrings #-}

module Courses.English.Vocabulary.Attitudinals.Glossary where

import Courses.English.Vocabulary.Attitudinals.Model

-- Source: https://en.wikibooks.org/wiki/Lojban/Attitudinals#Simple_propositional_emotions:

-- Simple propositional emotions
a'a :: Attitudinal
a'a = Attitudinal ".a'a" PropositionalEmotion ("attentive") (Just "inattentive") (Just "avoiding")

a'e :: Attitudinal
a'e = Attitudinal ".a'e" PropositionalEmotion ("alertness") (Nothing) (Just "exhaustion")

ai :: Attitudinal
ai = Attitudinal ".ai" PropositionalEmotion ("intent") (Just "indecision") (Just "refusal")

a'i :: Attitudinal
a'i = Attitudinal ".a'i" PropositionalEmotion ("effort") (Just "lack of effort") (Just "repose")

a'o :: Attitudinal
a'o = Attitudinal ".a'o" PropositionalEmotion ("hope") (Nothing) (Just "despair")

au :: Attitudinal
au = Attitudinal ".au" PropositionalEmotion ("desire") (Just "indifference") (Just "reluctance")

a'u :: Attitudinal
a'u = Attitudinal ".a'u" PropositionalEmotion ("interest") (Just "no interest") (Just "repulsion")

-- Complex propositional emotions
e'a :: Attitudinal
e'a = Attitudinal ".e'a" PropositionalEmotion ("permission") (Nothing) (Just "prohibition")

e'e :: Attitudinal
e'e = Attitudinal ".e'e" PropositionalEmotion ("competence") (Nothing) (Just "incompetence")

ei :: Attitudinal
ei = Attitudinal ".ei" PropositionalEmotion ("obligation") (Nothing) (Just "freedom")

e'i :: Attitudinal
e'i = Attitudinal ".e'i" PropositionalEmotion ("constraint") (Just "independence") (Just "resistance to constraint")

e'o :: Attitudinal
e'o = Attitudinal ".e'o" PropositionalEmotion ("request") (Nothing) (Just "negative request")

e'u :: Attitudinal
e'u = Attitudinal ".e'u" PropositionalEmotion ("suggestion") (Just "no suggestion") (Just "warning")

ia :: Attitudinal
ia = Attitudinal ".ia" PropositionalEmotion ("belief") (Just "skepticism") (Just "disbelief")

i'a :: Attitudinal
i'a = Attitudinal ".i'a" PropositionalEmotion ("acceptance") (Nothing) (Just "blame")

ie :: Attitudinal
ie = Attitudinal ".ie" PropositionalEmotion ("agreement") (Just "neutrality") (Just "disagreement")

i'e :: Attitudinal
i'e = Attitudinal ".i'e" PropositionalEmotion ("approval") (Just "non-approval") (Just "disapproval")

-- Miscellaneous pure emotions
ii :: Attitudinal
ii = Attitudinal ".ii" PureEmotion ("fear") (Just "nervousness") (Just "security")

i'i :: Attitudinal
i'i = Attitudinal ".i'i" PureEmotion ("togetherness") (Nothing) (Just "privacy")

io :: Attitudinal
io = Attitudinal ".io" PureEmotion ("respect") (Just "lack of respect") (Just "disrespect")

i'o :: Attitudinal
i'o = Attitudinal ".i'o" PureEmotion ("appreciation") (Just "lack of appreciation") (Just "envy")

iu :: Attitudinal
iu = Attitudinal ".iu" PureEmotion ("love") (Just "lack of love") (Just "hatred/fear")

i'u :: Attitudinal
i'u = Attitudinal ".i'u" PureEmotion ("familiarity") (Nothing) (Just "mystery")

-- Complex pure emotions
o'a :: Attitudinal
o'a = Attitudinal ".o'a" PureEmotion ("pride") (Just "modesty") (Just "shame")

o'e :: Attitudinal
o'e = Attitudinal ".o'e" PureEmotion ("closeness") (Just "detachment") (Just "distance")

oi :: Attitudinal
oi = Attitudinal ".oi" PureEmotion ("complaint/pain") (Just "no complaints") (Just "pleasure")

o'i :: Attitudinal
o'i = Attitudinal ".o'i" PureEmotion ("caution") (Just "boldness") (Just "rashness")

o'o :: Attitudinal
o'o = Attitudinal ".o'o" PureEmotion ("patience") (Just "mere tolerance") (Just "anger")

o'u :: Attitudinal
o'u = Attitudinal ".o'u" PureEmotion ("relaxation") (Just "composture") (Just "stress")

-- Simple pure emotions
ua :: Attitudinal
ua = Attitudinal ".ua" PureEmotion ("discovery") (Nothing) (Just "confusion")

u'a :: Attitudinal
u'a = Attitudinal ".u'a" PureEmotion ("gain") (Nothing) (Just "loss")

ue :: Attitudinal
ue = Attitudinal ".ue" PureEmotion ("surprise") (Just "no surprise") (Just "expectation")

u'e :: Attitudinal
u'e = Attitudinal ".u'e" PureEmotion ("wonder") (Nothing) (Just "commonplace")

ui :: Attitudinal
ui = Attitudinal ".ui" PureEmotion ("happiness") (Nothing) (Just "unhappiness")

u'i :: Attitudinal
u'i = Attitudinal ".u'i" PureEmotion ("amusement") (Nothing) (Just "weariness")

uo :: Attitudinal
uo = Attitudinal ".uo" PureEmotion ("completion") (Nothing) (Just "incompleteness")

u'o :: Attitudinal
u'o = Attitudinal ".u'o" PureEmotion ("courage") (Just "timidity") (Just "cowardice")

uu :: Attitudinal
uu = Attitudinal ".uu" PureEmotion ("pity") (Nothing) (Just "cruelty")

u'u :: Attitudinal
u'u = Attitudinal ".u'u" PureEmotion ("repentance") (Just "lack of regret") (Just "innocence")
