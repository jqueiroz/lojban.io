module Courses.Framework.ExerciseUtils
( simplifyCanonicalAnswer
) where

import Core
import Language.Lojban.Refinement (simplifyTerminatorsInSentence)

-- * Terminator ellisis
-- | Decorates an exercise so that 'simplifyTerminatorsInSentence' is applied to its canonical answer.
simplifyCanonicalAnswer :: Exercise -> Exercise
simplifyCanonicalAnswer (TypingExercise title sentences validate canonicalAnswer) = TypingExercise title sentences validate (simplifyTerminatorsInSentence canonicalAnswer)
simplifyCanonicalAnswer x = x
