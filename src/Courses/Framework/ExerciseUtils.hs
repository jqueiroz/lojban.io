module Courses.Framework.ExerciseUtils
( simplifyTerminatorsInCanonicalAnswer
) where

import Core
import Language.Lojban.Refinement (simplifyTerminatorsInSentence)

-- * Terminator ellisis
-- | Decorates an exercise so that 'simplifyTerminatorsInSentence' is applied to its canonical answer.
simplifyTerminatorsInCanonicalAnswer :: Exercise -> Exercise
simplifyTerminatorsInCanonicalAnswer (TypingExercise title sentences validate canonicalAnswer) = TypingExercise title sentences validate (simplifyTerminatorsInSentence canonicalAnswer)
simplifyTerminatorsInCanonicalAnswer x = x
