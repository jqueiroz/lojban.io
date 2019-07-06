{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Courses.English.Grammar.Introduction.Vocabulary where

import Courses.Util.Vocabulary

-------- Vocabulary
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

-- New words: ctuca; nelci; gerku, melbi
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

-- New words: ciska, djuno
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

-- New words: nupre, cusku; gleki
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

-- New words: cu
vocabularyGenerator5 :: VocabularyBuilder
vocabularyGenerator5 = vocabularyGenerator4

-- New words: plise, vecnu, skami, pilno
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

-- New words: cmene, bangu, PENDING
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

-- New words: NONE
-- Deliberately excluded words: mukti, gasnu (there are no sentences using the unabbreviated word)
-- More words: gasnu+, zgana*, finti*, srana?, lifri?, stidi?, xamgu?
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

-- New words: PENDING?
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
