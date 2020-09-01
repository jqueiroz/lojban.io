{-# LANGUAGE OverloadedStrings #-}

-- | This module defines vocabulary for the course.
module Courses.English.Grammar.Introduction.Vocabulary where

import Core

-- | Vocabulary for the appropriate lesson.
--
-- * Starting brivla: dunda, pelxu, zdani
-- * Starting cmavo: mi, do, ti, ta.
vocabulary1 :: Vocabulary
vocabulary1 = Vocabulary
    -- Brivla
    [
        "dunda", "pelxu", "zdani"
    ]
    -- Cmavo
    [
        "mi", "do",
        "ti", "ta"
    ]
    -- Cmene
    [
    ]

-- | Cumulative vocabulary up to the appropriate lesson.
vocabulary1_cumulative :: Vocabulary
vocabulary1_cumulative = vocabulary1

-- | Vocabulary for the appropriate lesson.
--
-- * New brivla: tavla, pendo, prenu, mlatu.
-- * New cmavo: zo'e.
vocabulary2 :: Vocabulary
vocabulary2 = Vocabulary
    -- Brivla
    [
        "tavla", "pendo", "prenu", "mlatu"
    ]
    -- Cmavo
    [
        "zo'e"
    ]
    -- Cmene
    [
    ]

-- | Cumulative vocabulary up to the appropriate lesson.
vocabulary2_cumulative :: Vocabulary
vocabulary2_cumulative = vocabulary1_cumulative <> vocabulary2

-- | Vocabulary for the appropriate lesson.
--
-- * New brivla: ctuca, nelci, gerku, melbi.
-- * New cmavo: lo, ku, fa\/fe\/fi\/fo\/fu, se\/te\/ve\/xe.
vocabulary3 :: Vocabulary
vocabulary3 = Vocabulary
    -- Brivla
    [
        "ctuca", "nelci", "gerku", "melbi"
    ]
    -- Cmavo
    [
        "lo", "ku",
        "fa", "fe", "fi", "fo", "fu",
        "se", "te", "ve", "xe"
    ]
    -- Cmene
    [
    ]

-- | Cumulative vocabulary up to the appropriate lesson.
vocabulary3_cumulative :: Vocabulary
vocabulary3_cumulative = vocabulary2_cumulative <> vocabulary3

-- | Vocabulary for the appropriate lesson.
--
-- * New brivla: NONE.
-- * New cmavo: NONE.
vocabulary4 :: Vocabulary
vocabulary4 = Vocabulary
    -- Brivla
    [
        "sutra", "lojbo", "ciska"
    ]
    -- Cmavo
    [
    ]
    -- Cmene
    [
    ]

-- | Cumulative vocabulary up to the appropriate lesson.
vocabulary4_cumulative :: Vocabulary
vocabulary4_cumulative = vocabulary3_cumulative <> vocabulary4


-- | Vocabulary for the appropriate lesson.
--
-- * New brivla: ciska, djuno.
-- * New cmavo: xu, ma, mo.
vocabulary5 :: Vocabulary
vocabulary5 = Vocabulary
    -- Brivla
    [
        "djuno"
    ]
    -- Cmavo
    [
        "xu", "ma", "mo",
        "na", "go'i"
    ]
    -- Cmene
    [
    ]

-- | Cumulative vocabulary up to the appropriate lesson.
vocabulary5_cumulative :: Vocabulary
vocabulary5_cumulative = vocabulary4_cumulative <> vocabulary5

-- | Vocabulary for the appropriate lesson.
--
-- * New brivla: nupre, cusku, gleki.
-- * New cmavo: nu, du'u, sedu'u, kei.
vocabulary6 :: Vocabulary
vocabulary6 = Vocabulary
    -- Brivla
    [
        "nupre", "cusku", "gleki"
    ]
    -- Cmavo
    [
        "su'u", "nu", "du'u", "sedu'u", "kei"
    ]
    -- Cmene
    [
    ]

-- | Cumulative vocabulary up to the appropriate lesson.
vocabulary6_cumulative :: Vocabulary
vocabulary6_cumulative = vocabulary5_cumulative <> vocabulary6

-- | Vocabulary for the appropriate lesson.
--
-- * New brivla: NONE.
-- * New cmavo: vau, cu.
vocabulary7 :: Vocabulary
vocabulary7 = Vocabulary
    -- Brivla
    [
    ]
    -- Cmavo
    [
        "vau", "cu"
    ]
    -- Cmene
    [
    ]

-- | Cumulative vocabulary up to the appropriate lesson.
vocabulary7_cumulative :: Vocabulary
vocabulary7_cumulative = vocabulary6_cumulative <> vocabulary7

-- | Vocabulary for the appropriate lesson.
--
-- * New brivla: plise, vecnu, skami, pilno.
-- * New cmavo: poi, noi, ke'a, ku'o.
vocabulary9 :: Vocabulary
vocabulary9 = Vocabulary
    -- Brivla
    [
        "plise", "vecnu", "skami", "pilno"
    ]
    -- Cmavo
    [
        "poi", "noi", "ke'a", "ku'o"
    ]
    -- Cmene
    [
    ]

-- | Cumulative vocabulary up to the appropriate lesson.
vocabulary9_cumulative :: Vocabulary
vocabulary9_cumulative = vocabulary7_cumulative <> vocabulary9

-- | Vocabulary for the appropriate lesson.
--
-- * New brivla: cmene, bangu.
-- * New cmavo: be, bei, be'o.
vocabulary10 :: Vocabulary
vocabulary10 = Vocabulary
    -- Brivla
    [
        "cmene", "bangu"
    ]
    -- Cmavo
    [
        "be", "bei", "be'o"
    ]
    -- Cmene
    [
    ]

-- | Cumulative vocabulary up to the appropriate lesson.
vocabulary10_cumulative :: Vocabulary
vocabulary10_cumulative = vocabulary9_cumulative <> vocabulary10

-- | Vocabulary for the appropriate lesson.
--
-- * New brivla: NONE.
--
--     * The following words were deliberately not included: mukti, gasnu.
--
--         The reason is that there are no sentences using these word directly, even though the corresponding BAI are used.
--
-- * New cmavo: pi'o, mu'i, gau.
--
-- Other potentially interesting words: zgana, finti, srana, lifri, stidi, xamgu.
vocabulary11 :: Vocabulary
vocabulary11 = Vocabulary
    -- Brivla
    [
       "fanva", -- actually used in sentences
       "mukti", "gasnu" -- used just in isolated exercises, to help the user remember the meaning and places of mu'i and gau
    ]
    -- Cmavo
    [
        "pi'o", "mu'i", "gau"
    ]
    -- Cmene
    [
    ]

-- | Cumulative vocabulary up to the appropriate lesson.
vocabulary11_cumulative :: Vocabulary
vocabulary11_cumulative = vocabulary10_cumulative <> vocabulary11

-- | Vocabulary for the appropriate lesson.
--
-- * New brivla: NONE.
-- * New cmavo: pu, ca, ba.
vocabulary12 :: Vocabulary
vocabulary12 = Vocabulary
    -- Brivla
    [
    ]
    -- Cmavo
    [
        "pu", "ca", "ba"
    ]
    -- Cmene
    [
    ]

-- | Cumulative vocabulary up to the appropriate lesson.
vocabulary12_cumulative :: Vocabulary
vocabulary12_cumulative = vocabulary11_cumulative <> vocabulary12

-- | Vocabulary for the appropriate lesson.
--
-- * New brivla: NONE.
-- * New cmavo: zo, lu, li'u
vocabulary14 :: Vocabulary
vocabulary14 = Vocabulary
    -- Brivla
    [
    ]
    -- Cmavo
    [
        "zo", "lu", "li'u"
    ]
    -- Cmene
    [
    ]

-- | Cumulative vocabulary up to the appropriate lesson.
vocabulary14_cumulative :: Vocabulary
vocabulary14_cumulative = vocabulary12_cumulative <> vocabulary14

-- | Vocabulary for the appropriate lesson.
--
-- * New brivla: NONE.
-- * New cmavo: ne, pe, po, po'e
vocabulary15 :: Vocabulary
vocabulary15 = Vocabulary
    -- Brivla
    [
    ]
    -- Cmavo
    [
        "ne", "pe", "po", "po'e"
    ]
    -- Cmene
    [
    ]

-- | Cumulative vocabulary up to the appropriate lesson.
vocabulary15_cumulative :: Vocabulary
vocabulary15_cumulative = vocabulary14_cumulative <> vocabulary15

-- | Vocabulary for the appropriate lesson.
--
-- * New brivla: NONE.
-- * New cmavo: .a, .e, .o, .u
vocabulary16 :: Vocabulary
vocabulary16 = Vocabulary
    -- Brivla
    [
    ]
    -- Cmavo
    [
        ".a", ".e", ".o", ".u"
    ]
    -- Cmene
    [
    ]

-- | Cumulative vocabulary up to the appropriate lesson.
vocabulary16_cumulative :: Vocabulary
vocabulary16_cumulative = vocabulary15_cumulative <> vocabulary16

-- | Vocabulary for the appropriate lesson.
--
-- * New brivla: NONE.
-- * New cmavo: na, na'e, to'e, no'e
vocabulary17 :: Vocabulary
vocabulary17 = Vocabulary
    -- Brivla
    [
    ]
    -- Cmavo
    [
        "na", "na'e", "to'e", "no'e"
    ]
    -- Cmene
    [
    ]

-- | Cumulative vocabulary up to the appropriate lesson.
vocabulary17_cumulative :: Vocabulary
vocabulary17_cumulative = vocabulary16_cumulative <> vocabulary17

-- | Vocabulary for the appropriate lesson.
--
-- * New brivla: NONE.
-- * New cmavo: tu'a, jai
vocabulary18 :: Vocabulary
vocabulary18 = Vocabulary
    -- Brivla
    [
        "lerci"
    ]
    -- Cmavo
    [
        "tu'a", "jai"
    ]
    -- Cmene
    [
    ]

-- | Cumulative vocabulary up to the appropriate lesson.
vocabulary18_cumulative :: Vocabulary
vocabulary18_cumulative = vocabulary17_cumulative <> vocabulary18
