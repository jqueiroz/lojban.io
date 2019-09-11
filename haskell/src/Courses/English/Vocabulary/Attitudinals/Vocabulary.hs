-- | This module defines vocabulary for the course.
module Courses.English.Vocabulary.Attitudinals.Vocabulary where

import Courses.English.Vocabulary.Attitudinals.Model
import Courses.English.Vocabulary.Attitudinals.Glossary as G

-- Modifiers: nai, ru'e, sai
-- Question: pei
-- Others: zo'o, dai, ju'o

-- | List of attitudinals introduced in the first lesson (consists solely of pure emotions).
attitudinals1 :: [Attitudinal]
attitudinals1 = [G.u'i, G.ua, G.oi, G.ui, G.ue, G.u'u]

-- | List of attitudinals up to the first lesson.
attitudinals1_cumulative :: [Attitudinal]
attitudinals1_cumulative = attitudinals1

-- | List of attitudinals introduced in the second lesson (consists solely of propositional emotions).
attitudinals2 :: [Attitudinal]
attitudinals2 = [G.ie, G.ei, G.i'e, G.ai, G.au, G.e'u]

-- | List of attitudinals up to the second lesson.
attitudinals2_cumulative :: [Attitudinal]
attitudinals2_cumulative = attitudinals1_cumulative ++ attitudinals2

-- | List of attitudinals introduced in the third lesson.
attitudinals3 :: [Attitudinal]
attitudinals3 = []

-- | List of attitudinals up to the third lesson.
attitudinals3_cumulative :: [Attitudinal]
attitudinals3_cumulative = attitudinals2_cumulative ++ attitudinals3

-- | List of attitudinals introduced in the fourth lesson (consists solely of propositional emotions).
attitudinals4 :: [Attitudinal]
attitudinals4 = [G.a'o]

-- | List of attitudinals up to the fourth lesson.
attitudinals4_cumulative :: [Attitudinal]
attitudinals4_cumulative = attitudinals3_cumulative ++ attitudinals4
