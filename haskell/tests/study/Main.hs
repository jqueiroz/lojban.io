module Main where

import Test.Hspec
import Study.Framework.ExerciseGenerators.Tests as EG

main :: IO ()
main = hspec EG.tests
