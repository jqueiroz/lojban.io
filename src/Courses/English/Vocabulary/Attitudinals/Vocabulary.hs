-- | This module defines vocabulary for the course.
module Courses.English.Vocabulary.Attitudinals.Vocabulary where

import Courses.English.Vocabulary.Attitudinals.Model
import Courses.English.Vocabulary.Attitudinals.Glossary as G

-- Modifiers: nai, ru'e, sai
-- Question: pei
-- Others: zo'o, dai, ju'o

-- | List of attitudinals for the first lesson (consists solely of pure emotions).
attitudinals1 :: [Attitudinal]
attitudinals1 = [G.u'i, G.ua, G.oi, G.ui, G.ue, G.u'u]

-- | List of attitudinals for the second lesson (consists solely of propositional emotions).
attitudinals2 :: [Attitudinal]
attitudinals2 = [G.ie, G.i'e, G.ai, G.au, G.e'u, G.a'o]
