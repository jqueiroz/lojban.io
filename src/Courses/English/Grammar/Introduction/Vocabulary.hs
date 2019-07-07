{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | This module defines vocabulary for the course.
module Courses.English.Grammar.Introduction.Vocabulary where

import Courses.Util.Vocabulary

-- | Vocabulary for the first lesson.
--
-- * Starting brivla: tavla, dunda, pendo, prenu, zdani, mlatu.
-- * Starting cmavo: mi, do, ti, ta.
vocabularyGenerator1 :: VocabularyBuilder
vocabularyGenerator1 = createVocabularyBuilder
    -- Selbri
    [
        ("actions", (1,) <$> ["tavla", "dunda"]),
        ("relations", (1,) <$> ["pendo"]),
        ("properties", (1,) <$> ["prenu", "zdani", "mlatu"])
    ]
    -- Sumti
    [
        ("genericPersons", (1,) <$> ["mi", "do"]),
        ("genericPointable", (1,) <$> ["ti", "ta"])
    ]

-- | Vocabulary for the second lesson.
--
-- * New brivla: ctuca, nelci, gerku, melbi.
-- * New cmavo: lo, ku, fa\/fe\/fi\/fo\/fu, se\/te\/ve\/xe.
vocabularyGenerator2 :: VocabularyBuilder
vocabularyGenerator2 = createVocabularyBuilder
    -- Selbri
    [
        ("actions", ((1,) <$> ["tavla", "dunda"]) ++ ((2,) <$> ["ctuca"])),
        ("relations", ((1,) <$> ["pendo"]) ++ ((2,) <$> ["nelci"])),
        ("properties", ((1,) <$> ["prenu", "zdani", "mlatu"]) ++ ((2,) <$> ["gerku", "melbi"]))
    ]
    -- Sumti
    [
        ("genericPersons", (1,) <$> ["mi", "do", "lo prenu ku"]),
        ("semiGenericPersons", (1,) <$> ["lo tavla ku", "lo se tavla ku", "lo dunda ku", "lo te dunda ku"]),
        ("animals", (1,) <$> ["lo mlatu ku", "lo gerku ku"]),
        ("genericPointable", (1,) <$> ["ti", "ta"]),
        ("places", (1,) <$> ["lo zdani ku"]),
        ("subjects", (1,) <$> ["lo zdani ku", "lo mlatu ku", "lo gerku ku", "lo se dunda ku"])
    ]

-- | Vocabulary for the third lesson.
--
-- * New brivla: ciska, djuno.
-- * New cmavo: xu, ma, mo.
vocabularyGenerator3 :: VocabularyBuilder
vocabularyGenerator3 = createVocabularyBuilder
    -- Selbri
    [
        ("actions", ((0,) <$> ["tavla", "dunda"]) ++ ((1,) <$> ["ctuca"]) ++ ((2,) <$> ["ciska", "djuno"])),
        ("relations", (0,) <$> ["pendo", "nelci"]),
        ("properties", (0,) <$> ["prenu", "zdani", "mlatu", "gerku", "melbi"])
    ]
    -- Sumti
    [
        ("genericPersons", (1,) <$> ["mi", "do", "lo prenu ku"]),
        ("semiGenericPersons", (1,) <$> ["lo tavla ku", "lo se tavla ku", "lo dunda ku", "lo te dunda ku", "lo ciska ku"]),
        ("animals", (1,) <$> ["lo mlatu ku", "lo gerku ku"]),
        ("genericPointable", (1,) <$> ["ti", "ta"]),
        ("places", (1,) <$> ["lo zdani ku"]),
        ("subjects", (1,) <$> ["lo zdani ku", "lo mlatu ku", "lo gerku ku", "lo se dunda ku"])
    ]

-- | Vocabulary for the fourth lesson.
--
-- * New brivla: nupre, cusku, gleki.
-- * New cmavo: nu, du'u, sedu'u, kei.
vocabularyGenerator4 :: VocabularyBuilder
vocabularyGenerator4 = createVocabularyBuilder
    -- Selbri
    [
        ("actions", ((0,) <$> ["tavla", "dunda"]) ++ ((1,) <$> ["ctuca", "ciska", "djuno"]) ++ ((2,) <$> ["nupre", "cusku"])),
        ("relations", ((0,) <$> ["pendo", "nelci"]) ++ ((1,) <$> ["gleki"])),
        ("properties", (0,) <$> ["prenu", "zdani", "mlatu", "gerku", "melbi"])
    ]
    -- Sumti
    [
        ("genericPersons", (1,) <$> ["mi", "do", "lo prenu ku"]),
        ("semiGenericPersons", (1,) <$> ["lo tavla ku", "lo se tavla ku", "lo dunda ku", "lo te dunda ku", "lo ciska ku"]),
        ("animals", (1,) <$> ["lo mlatu ku", "lo gerku ku"]),
        ("genericPointable", (1,) <$> ["ti", "ta"]),
        ("places", (1,) <$> ["lo zdani ku"]),
        ("subjects", (1,) <$> ["lo zdani ku", "lo mlatu ku", "lo gerku ku", "lo se dunda ku"])
    ]

-- | Vocabulary for the fifth lesson.
--
-- * New brivla: NONE.
-- * New cmavo: cu.
vocabularyGenerator5 :: VocabularyBuilder
vocabularyGenerator5 = vocabularyGenerator4

-- | Vocabulary for the seventh lesson.
--
-- * New brivla: plise, vecnu, skami, pilno.
-- * New cmavo: poi, noi, ke'a, ku'o.
vocabularyGenerator7 :: VocabularyBuilder
vocabularyGenerator7 = createVocabularyBuilder
    -- Selbri
    [
        ("actions", ((0,) <$> ["tavla", "dunda"]) ++ ((1,) <$> ["ctuca", "ciska", "djuno", "nupre", "cusku"]) ++ ((3,) <$> ["vecnu", "pilno"])),
        ("relations", ((0,) <$> ["pendo", "nelci", "gleki"])),
        ("properties", (0,) <$> ["prenu", "zdani", "mlatu", "gerku", "melbi", "plise", "skami"])
    ]
    -- Sumti
    [
    ]

-- | Vocabulary for the eighth lesson.
--
-- * New brivla: cmene, bangu.
-- * New cmavo: be, bei, be'o.
vocabularyGenerator8 :: VocabularyBuilder
vocabularyGenerator8 = createVocabularyBuilder
    -- Selbri
    [
        ("actions", ((0,) <$> ["tavla", "dunda"]) ++ ((1,) <$> ["ctuca", "ciska", "djuno", "nupre", "cusku"]) ++ ((2,) <$> ["vecnu", "pilno"])),
        ("relations", ((0,) <$> ["pendo", "nelci", "gleki"]) ++ ((3,) <$> ["cmene", "bangu"])),
        ("properties", (0,) <$> ["prenu", "zdani", "mlatu", "gerku", "melbi", "plise", "skami"])
    ]
    -- Sumti
    [
    ]

-- | Vocabulary for the nineth lesson.
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
vocabularyGenerator9 :: VocabularyBuilder
vocabularyGenerator9 = createVocabularyBuilder
    -- Selbri
    [
        ("actions", ((0,) <$> ["tavla", "dunda"]) ++ ((1,) <$> ["ctuca", "ciska", "djuno", "nupre", "cusku", "vecnu"]) ++ ((4,) <$> ["pilno"])),
        ("relations", ((0,) <$> ["pendo", "nelci", "gleki"]) ++ ((1,) <$> ["cmene", "bangu"])),
        ("properties", (0,) <$> ["prenu", "zdani", "mlatu", "gerku", "melbi", "plise", "skami"])
    ]
    -- Sumti
    [
    ]

-- | Vocabulary for the tenth lesson.
--
-- * New brivla: NONE.
-- * New cmavo: pu, ca, ba.
vocabularyGenerator10 :: VocabularyBuilder
vocabularyGenerator10 = createVocabularyBuilder
    -- Selbri
    [
        ("actions", ((0,) <$> ["tavla", "dunda"]) ++ ((1,) <$> ["ctuca", "ciska", "djuno", "nupre", "cusku", "vecnu", "pilno"])),
        ("relations", ((0,) <$> ["pendo", "nelci", "gleki"]) ++ ((1,) <$> ["cmene", "bangu"])),
        ("properties", (0,) <$> ["prenu", "zdani", "mlatu", "gerku", "melbi", "plise", "skami"])
    ]
    -- Sumti
    [
    ]

-- | Vocabulary for the eleventh lesson.
--
-- * New brivla: NONE.
-- * New cmavo: NONE.
vocabularyGenerator11 :: VocabularyBuilder
vocabularyGenerator11 = createVocabularyBuilder
    -- Selbri
    [
        ("actions", ((0,) <$> ["tavla", "dunda"]) ++ ((1,) <$> ["ctuca", "ciska", "djuno", "nupre", "cusku", "vecnu", "pilno"])),
        ("relations", ((0,) <$> ["pendo", "nelci", "gleki"]) ++ ((1,) <$> ["cmene", "bangu"])),
        ("properties", (0,) <$> ["prenu", "zdani", "mlatu", "gerku", "melbi", "plise", "skami"])
    ]
    -- Sumti
    [
    ]
